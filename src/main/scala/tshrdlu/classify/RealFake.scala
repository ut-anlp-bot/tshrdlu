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
    val boundary = List.fill(n - 1)("<boundary>")
    (boundary ++ tokens ++ boundary).sliding(n).map { ngram =>
      FeatureObservation(name + "=" + ngram.mkString("+"))
    }.toSeq
  }
}

class GappyBigramFeaturizer(gap: Int) extends TweetFeaturizer {
  def apply(tweet: PreprocessedTweet): Seq[FeatureObservation[String]] = {
    val length = gap + 2
    tweet.lowerTokens.sliding(length).filter(_.length == length).map { tokens =>
      val token1 = tokens(0)
      val token2 = tokens(tokens.length - 1)
      val parts = (List(token1) ++ List.fill(gap)("_") ++ List(token2))
      FeatureObservation("gappy=" + parts.mkString("+"))
    }.toSeq
  }
}

class StandardGappyBigramFeaturizer(maxGap: Int) extends TweetFeaturizer {
  def apply(tweet: PreprocessedTweet): Seq[FeatureObservation[String]] = {
    (1 to maxGap).flatMap { gap =>
      val length = gap + 2
      tweet.lowerTokens.sliding(length).filter(_.length == length).map { tokens =>
        val token1 = tokens(0)
        val token2 = tokens(tokens.length - 1)
        val parts = List(token1, "___", token2)
        FeatureObservation("standard_gappy=" + parts.mkString("+"))
      }
    }.toSeq
  }
}

object FractionStopwordsFeaturizer extends TweetFeaturizer {
  def apply(tweet: PreprocessedTweet): Seq[FeatureObservation[String]] = {
    val numStopwords = tweet.lowerTokens.length - tweet.lowerTokensNoStopwords.length
    val pattern = "[a-z0-9].*".r.pattern
    val numContentWords = tweet.lowerTokens.count(pattern.matcher(_).matches)
    val fractionStopwords = if(numContentWords == 0)
      0.0
    else
      numStopwords.toDouble / numContentWords
    Seq(FeatureObservation("fraction_stopwords", fractionStopwords))
  }
}

class FractionDuplicateNGramFeaturizer(n: Int) extends TweetFeaturizer {
  def apply(tweet: PreprocessedTweet): Seq[FeatureObservation[String]] = {
    val ngrams = tweet.lowerTokens.sliding(n).filter(_.length == n).toArray
    val numDups = ngrams.length - ngrams.distinct.length
    val fraction = if(ngrams.length == 0)
      0.0
    else
      numDups.toDouble / ngrams.length
    Seq(FeatureObservation("frac_dup_" + n + "_grams", fraction))
  }
}
