package tshrdlu.classify

import chalk.lang.eng.Twokenize
import nak.data.{Featurizer, FeatureObservation}


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
    BasicRealFakeFeaturizer(content)
  }
}
