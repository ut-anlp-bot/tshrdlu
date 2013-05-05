package tshrdlu.classify

import chalk.lang.eng.Twokenize
import nak.data.{Featurizer, FeatureObservation}

import tshrdlu.util.English


case class PreprocessedTweet(
  origContent: String,
  tokens: Seq[String],
  lowerTokens: Seq[String],
  lowerTokensNoStopwords: Seq[String]
)

object PreprocessTweet extends (String => PreprocessedTweet) {
  def apply(content: String): PreprocessedTweet = {
    val tokens = Twokenize(content)
    val lowerTokens = tokens.map(_.toLowerCase)
    val lowerTokensNoStopwords = lowerTokens.filterNot(English.stopwords)
    PreprocessedTweet(content, tokens, lowerTokens, lowerTokensNoStopwords)
  }
}

trait TweetFeaturizer extends Featurizer[PreprocessedTweet, String]

class CombinedTweetFeaturizers(featurizers: Seq[TweetFeaturizer])
extends Featurizer[String, String] {
  def apply(content: String): Seq[FeatureObservation[String]] = {
    val tweet = PreprocessTweet(content)
    featurizers.flatMap { featurizer =>
      featurizer(tweet)
    }
  }
}

class NGramFeaturizer(n: Int, name: String, stopwords: Boolean = true)
extends TweetFeaturizer {
  def apply(tweet: PreprocessedTweet): Seq[FeatureObservation[String]] = {
    val tokens = if(stopwords) tweet.lowerTokens else tweet.lowerTokensNoStopwords
    tokens.sliding(n).map { ngram =>
      FeatureObservation(name + "=" + ngram.mkString("+"))
    }.toSeq
  }
}
