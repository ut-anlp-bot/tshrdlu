package tshrdlu.classify

import scala.xml._


/**
 * A labeled tweet.
 */
case class LabeledTweet(label: String, content: String) {
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
 */
class TextLabeledTweetReader(label: String) extends LabeledTweetReader {
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
  def apply(filename: String): Iterator[LabeledTweet] = {
    val items = (scala.xml.XML.loadFile(filename) \\ "item").toIterator
    for (tweetXml <- items)
      yield createLabeledTweet(tweetXml)
  }

  private def createLabeledTweet(tweetXml: NodeSeq) = {
    val label = (tweetXml \ "@label").text
    val content = (tweetXml \ "content").text
    LabeledTweet(label, content)
  }
}
