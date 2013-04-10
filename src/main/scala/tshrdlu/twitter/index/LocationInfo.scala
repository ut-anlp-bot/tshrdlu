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

import org.apache.lucene.document._
import twitter4j.Place
import twitter4j.Status

import tshrdlu.util.index._


/**
 * Holds information related to a Status and User that may be helpful for
 * determining location.
 */
class LocationInfo(status: Status) extends Serializable {
  val statusId: Long = status.getId()
  val statusCreatedAt: Long = status.getCreatedAt().getTime()
  val statusLocation: Option[(Double, Double)]  = Option(status.getGeoLocation) match {
    case Some(location) => Some((location.getLatitude, location.getLongitude))
    case None => None
  }
  val statusText: String = status.getText()
  val statusPlace: Place = status.getPlace()
  val userDescription: String = status.getUser().getDescription()
  val userIsGeoEnabled: Boolean = status.getUser().isGeoEnabled()
  val userLang: String = status.getUser().getLang()
  val userLocation: String = status.getUser().getLocation()
  val userName: String = status.getUser().getName()
  val userTimeZone: String = Option(status.getUser().getTimeZone()).getOrElse("")
  val userURL: String = status.getUser().getURL()
  val userUtcOffset: Int = status.getUser().getUtcOffset()
}


object LocationInfoToDocument extends ObjectToDocument[LocationInfo] {

  /**
   * Adds fields to the Lucene <code>Document</code> so they can be searched.
   *
   * @param document the document to add fields to
   * @param lodcationInfo the object to extract field values from
   */
  def addFields(document: Document, locationInfo: LocationInfo) {
    document.add(new LongField("statusCreatedAt", locationInfo.statusCreatedAt, Field.Store.NO))
    locationInfo.statusLocation match {
      case Some((latitude, longitude)) => {
        document.add(new DoubleField("statusLatitude", latitude, Field.Store.NO))
        document.add(new DoubleField("statusLongitude", longitude, Field.Store.NO))
      }
      case None =>
    }
    document.add(new TextField("statusText", locationInfo.statusText, Field.Store.NO))
    // TODO: Index info from locationInfo.statusPlace?
    val geoEnabled = if (locationInfo.userIsGeoEnabled) "yes" else "no"
    document.add(new StringField("userIsGeoEnabled", geoEnabled, Field.Store.NO))
    document.add(new TextField("userLang", locationInfo.userLang, Field.Store.NO))
    document.add(new TextField("userLocation", locationInfo.userLocation, Field.Store.NO))
    document.add(new TextField("userTimeZone", locationInfo.userTimeZone, Field.Store.NO))
    document.add(new IntField("userUtcOffet", locationInfo.userUtcOffset, Field.Store.NO))
  }

  /**
   * Returns the Twitter status ID as the ID to store in the index.
   */
  def getId(locationInfo: LocationInfo): Option[Long] = Some(locationInfo.statusId)
}


/**
 * Creates objects to read LocationInfo objects from an index.
 */
object LocationInfoReaderFactory extends ObjectReaderFactory[LocationInfo](
    LocationInfoToDocument,
    defaultSearchField = "userLocation")


/**
 * Creates objects to write LocationInfo objects to an index.
 */
object LocationInfoWriterFactory
extends ObjectWriterFactory[LocationInfo](LocationInfoToDocument)
