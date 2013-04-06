package tshrdlu.twitter.index

/**
 * Copyright 2013 Nick Wilson
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import twitter4j.{Status,TwitterStreamFactory}

import tshrdlu.twitter.{Filterable,StatusListenerAdaptor}
import tshrdlu.twitter.{IdFilter,LocationFilter,TermFilter}
import tshrdlu.util.English
import tshrdlu.util.index._


/**
 * Provides a main method to perform searches on indexes.
 */
object LocationInfoSearcher
extends ObjectIndexSearcher[LocationInfo](LocationInfoReaderFactory) {
  def resultToString(locationInfo: LocationInfo, score: Float): String = {
    val (lat, long) = locationInfo.statusLocation match {
      case Some(location) => location
      case None => ("Unavailable", "Unavailable")
    }
    val lines = List(
      score + ": " + locationInfo.statusText,
      "Lat: " + lat + ", Long: "+ long,
      "User profile location: " + locationInfo.userLocation,
      ""
    )
    lines.mkString("\n")
  }
}


/**
 * Handles incoming status messages by indexing their location info. Accepts
 * incoming statuses, writes their location info to an index, and prints the
 * status text.
 */
class LocationInfoIndexListener extends StatusListenerAdaptor {
  private var writer: ObjectWriter[LocationInfo] = null

  override def onStatus(status: Status) {
    println("INCOMING " + status.getId + " " + status.getText)
    require(writer != null)
    val locationInfo = new LocationInfo(status)
    writer.add(locationInfo)
    println("INDEXED " + status.getId)
  }

  /**
   * Set the writer that contains everything needed to write to an index.
   *
   * @param locationInfoWriter the index writer
   */
  def setLocationInfoWriter(locationIndexWriter: ObjectWriter[LocationInfo]) {
    writer = locationIndexWriter
  }
}


class BaseLocationInfoIndexStreamer extends LocationInfoIndexListener {
  val twitterStream = new TwitterStreamFactory().getInstance
  twitterStream.addListener(this)

  /**
   * Samples statuses and writes their location info to an index.
   *
   * @param args takes one argument, a name of an index (see
   *             [[tshrdlu.util.index.Settings.BaseIndexPath]] for a
   *             decription of where the index is located on disk) or a full
   *             path to an index
   */
  def main(args: Array[String]) {
    setLocationInfoWriter(LocationInfoWriterFactory(args(0)))
    twitterStream.sample
  }
}


/**
 * Takes status messages from the Twitter sample stream and indexes their
 * location info. Accepts incoming statuses, writes their location info to an
 * index, and prints the status text.
 */
object LocationInfoIndexer extends BaseLocationInfoIndexStreamer


abstract class FilteredLocationInfoIndexStreamer
extends BaseLocationInfoIndexStreamer with Filterable {

  /**
   * Get a filtered stream based on the FilterQuery provided by extending
   * classes.
   *
   * @param args the first argument is a name of an index (see
   *             [[tshrdlu.util.index.Settings.BaseIndexPath]] for a
   *             decription of where the index is located on disk) or a full
   *             path to an index. All remaining arguments are used as the
   *             filter query.
   */
  override def main(args: Array[String]) {
    setLocationInfoWriter(LocationInfoWriterFactory(args(0)))

    val queryArgs = args.slice(1, args.length)
    twitterStream.filter(getQuery(queryArgs))
  }
}


/**
 * An indexer that indexes a status message's location info by filtering the
 * stream from locations based on the provided bounding boxes.
 */
object LocationInfoLocationIndexer
extends FilteredLocationInfoIndexStreamer with LocationFilter
