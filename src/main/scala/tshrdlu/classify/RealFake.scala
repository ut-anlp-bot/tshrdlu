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

import chalk.lang.eng.Twokenize
import nak.data.{Featurizer, FeatureObservation}

import tshrdlu.util.English


/**
 * A tweet preprocessed for feature extraction.
 *
 * @param origContent the original content of the tweet
 * @param tokens the tweek tokenized with Twokenize
 * @param lowerTokens lowercase version of the tokens
 * @param lowerTokensNoStopwords lowercase version of the tokens without stopwords
 */
case class PreprocessedTweet(
  origContent: String,
  tokens: Seq[String],
  lowerTokens: Seq[String],
  lowerTokensNoStopwords: Seq[String]
)

/**
 * Preprocesses a tweet for feature extraction.
 */
object PreprocessTweet extends (String => PreprocessedTweet) {

  /**
   * Preprocess the tweet.
   *
   * @param content the tweet to preprocess
   * @return the preprocessed tweet
   */
  def apply(content: String): PreprocessedTweet = {
    val tokens = Twokenize(content)
    val lowerTokens = tokens.map(_.toLowerCase)
    val lowerTokensNoStopwords = lowerTokens.filterNot(English.stopwords)
    PreprocessedTweet(content, tokens, lowerTokens, lowerTokensNoStopwords)
  }
}

/**
 * A custom featurizer that works on preprocessed tweets.
 */
trait TweetFeaturizer extends Featurizer[PreprocessedTweet, String]


/**
 * A single TweetFeaturizer that combines several others.
 *
 * @param featurizers the featurizers to combine
 */
class CombinedTweetFeaturizers(featurizers: Seq[TweetFeaturizer])
extends Featurizer[String, String] {

  /**
   * Apply all the featurizers to the tweet.
   *
   * @param content the tweet to featurize
   * @return the observed features
   */
  def apply(content: String): Seq[FeatureObservation[String]] = {
    val tweet = PreprocessTweet(content)
    featurizers.flatMap { featurizer =>
      featurizer(tweet)
    }
  }
}

/**
 * Extract n-gram features from a tweet.
 *
 * @param n the number of grams
 * @param name used to construct the feature names
 * @param stopwords true to include stopwords (default), false otherwise
 */
class NGramFeaturizer(n: Int, name: String, stopwords: Boolean = true)
extends TweetFeaturizer {
  val stripPunctuationPattern = "[a-z0-9#@].*".r.pattern

  def apply(tweet: PreprocessedTweet): Seq[FeatureObservation[String]] = {
    val baseTokens = if(stopwords) tweet.lowerTokens else tweet.lowerTokensNoStopwords
    val tokens = if (n == 1) {
      // Exclude punctuation from bag-of-words features
      baseTokens.filter(t => stripPunctuationPattern.matcher(t).matches)
    } else {
      baseTokens
    }
    val boundary = List.fill(n - 1)("<boundary>")
    val ngrams = (boundary ++ tokens ++ boundary).sliding(n).toList
    ngrams.map { ngram =>
      FeatureObservation(name + "=" + ngram.mkString("+"), 1.0 / ngrams.length)
    }.toSeq
  }
}

/**
 * Extract a 'length' feature from a tweet whose value is the number of tokens.
 */
object LengthFeaturizer extends TweetFeaturizer {
  def apply(tweet: PreprocessedTweet): Seq[FeatureObservation[String]] = {
    Seq(FeatureObservation("length", tweet.lowerTokens.length))
  }
}

/**
 * Extract gappy bigram features from a tweet. The number of tokens in the gap
 * is encoded in the feature.
 *
 * @param gap the size of the gap
 */
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

/**
 * Extract standard gappy bigram features from a tweet. The number of tokens in
 * the gap is not encoded in the feature.
 *
 * @param gap the size of the gap
 */
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

/**
 * Extract a feature for the fraction of the words in the tweet that are stopwords.
 */
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

/**
 * Extract a feature representing the number of a tweet's n-grams that are
 * duplicates.
 *
 * @param n the number of grams
 */
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
