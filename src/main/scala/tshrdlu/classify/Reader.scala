package tshrdlu.classify

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

import scala.xml._


/**
 * A tweet labeled for classification.
 *
 * @param label the tweet's classification label
 * @param content the content of the tweet
 */
case class LabeledTweet(label: String, content: String) {
  /**
   * Converts the labeled tweet to XML.
   *
   * @return an XML element representing the labeled tweet
   */
  def toXML =
    <item label={label}>
      <content>{content}</content>
    </item>
}

/**
 * Takes a filename and returns an iterator of LabeledTweet objects from the
 * file.
 */
trait LabeledTweetReader extends (String => Iterator[LabeledTweet])

/**
 * Produces LabeledTweet objects from a text file that has the content of one
 * tweet on each line.
 *
 * @param label the classification label of the tweet
 */
class TextLabeledTweetReader(label: String) extends LabeledTweetReader {

  /**
   * Produce the LabeledTweet objects.
   *
   * @param filename the file to read from
   */
  def apply(filename: String): Iterator[LabeledTweet] = {
    val lines = scala.io.Source.fromFile(filename).getLines
    for (content <- lines)
      yield LabeledTweet(label, content)
  }
}

/**
 * Produces LabeledTweet objects from an XML file.
 */
object XMLLabeledTweetReader extends LabeledTweetReader {
  /**
   * Produces the LabeledTweet objects.
   *
   * @param filename the file to read from
   */
  def apply(filename: String): Iterator[LabeledTweet] = {
    val items = (scala.xml.XML.loadFile(filename) \\ "item").toIterator
    for (tweetXml <- items)
      yield createLabeledTweet(tweetXml)
  }

  // Create a LabeledTweet from an XML element
  private def createLabeledTweet(tweetXml: NodeSeq) = {
    val label = (tweetXml \ "@label").text
    val content = (tweetXml \ "content").text
    LabeledTweet(label, content)
  }
}
