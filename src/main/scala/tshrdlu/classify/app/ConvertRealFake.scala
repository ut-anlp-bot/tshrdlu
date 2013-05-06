package tshrdlu.classify.app

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

import scala.xml

import tshrdlu.classify.LabeledTweet
import tshrdlu.classify.TextLabeledTweetReader


/**
 * Converts a text file of tweets (one status message per line) into an XML
 * format for use by the classifier. Each file is given a class label that
 * applies to all the tweets it contains.
 */
object ConvertRealFake {
  def main(args: Array[String]) {
    // Parse and get the command-line options
    val opts = ConvertRealFakeOpts(args)

    val realTweets = readLabeledTweets(opts.real(), "real")
    val fakeTweets = readLabeledTweets(opts.fake(), "fake")

    val dataset =
      <dataset>
        {realTweets.map(_.toXML)}
        {fakeTweets.map(_.toXML)}
      </dataset>

    val xmlPrinter = new xml.PrettyPrinter(1000, 2)
    println("<?xml version=\"1.0\"?>")
    println(new xml.PrettyPrinter(1000, 2).format(dataset))
  }

  /**
   * Read in tweets from a file and give them all the same label.
   *
   * @param filenames one or more paths to files to read from
   * @param label the label to apply to the tweets
   * @return the labeled tweets
   */
  def readLabeledTweets(filenames: List[String], label: String): Iterator[LabeledTweet] = {
    filenames.toIterator.flatMap { filename =>
      new TextLabeledTweetReader(label)(filename)
    }
  }
}

/**
 * Command line option parser for the ConvertRealFake application.
 */
object ConvertRealFakeOpts {
  import org.rogach.scallop._

  def apply(args: Array[String]) = new ScallopConf(args) {
    val real = opt[List[String]](
      "real",
      short = 'r',
      descr = "The files containing real tweets.")
    val fake = opt[List[String]](
      "fake",
      short = 'f',
      descr = "The files containing fake tweets.")
  }
}
