package tshrdlu.classify.app

import scala.xml

import tshrdlu.classify.LabeledTweet
import tshrdlu.classify.TextLabeledTweetReader


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

  def readLabeledTweets(filenames: List[String], label: String): Iterator[LabeledTweet] = {
    filenames.toIterator.flatMap { filename =>
      new TextLabeledTweetReader(label)(filename)
    }
  }
}

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
