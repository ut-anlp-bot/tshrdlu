package tshrdlu.classify

import chalk.lang.eng.Twokenize
import nak.data.{Featurizer, FeatureObservation}

import tshrdlu.util.POSTagger


/*
 * Basic featurizer for real tweet detection.
 */
object BasicRealFakeFeaturizer extends Featurizer[String, String] {
  def apply(content: String): Seq[FeatureObservation[String]] = {
    val tokens = Twokenize(content)
    val bowFeatures = tokens.map { token =>
      FeatureObservation("word=" + token)
    }
    bowFeatures
  }
}

/*
 * Extended featurizer for real tweet detection.
 */
object ExtendedRealFakeFeaturizer extends Featurizer[String, String] {
  def apply(content: String): Seq[FeatureObservation[String]] = {
    val tokensAndTags = POSTagger(content).map { taggedToken =>
      (taggedToken.token, taggedToken.tag)
    }
    val (tokens, posTags) = tokensAndTags.unzip

    // Part-of-Speech N-Grams
    val begin = List("<begin>")
    val end = List("<end>")
    val posUnigramFeatures = posTags.map { tag =>
      FeatureObservation("pos_unigram=" + tag)
    }
    val posBigramFeatures = (begin ++ posTags ++ end).sliding(2).map {
      case List(tag1, tag2) => FeatureObservation("pos_bigram=" + tag1 + tag2)
    }
    val posTrigramFeatures = (begin ++ begin ++ posTags ++ end ++ end).sliding(3).map {
      case List(tag1, tag2, tag3) => FeatureObservation("pos_trigram=" + tag1 + tag2 + tag3)
    }
    val posNgramFeatures = posUnigramFeatures ++ posBigramFeatures ++ posTrigramFeatures

    // Fraction of POS tags for each tag
    val posFractions = posTags.groupBy(identity).mapValues(_.length).map {
      case (tag, count) => FeatureObservation("pos_fraction_" + tag, count.toDouble / posTags.length)
    }

    val features = posNgramFeatures ++ posFractions
    //features.foreach(println)
    //println("")
    features
  }
}
