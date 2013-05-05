package tshrdlu.classify.app

import scala.util.Random

import nak.NakContext
import nak.data.Example
import nak.liblinear
import nak.util.ConfusionMatrix

import tshrdlu.classify._


object RealFake {
  def main(args: Array[String]) {
    // Parse and get the command-line options
    val opts = RealFakeOpts(args)

    // Read the datasets
    val trainExamples = readExamples(opts.train()).toIndexedSeq
    val evalExamples = readExamples(opts.eval()).toIndexedSeq

    // Build the featurizer
    opts.featurizers().foreach(println)
    val featurizers = opts.featurizers().map {
      case "unigram" => new NGramFeaturizer(1, "unigram", stopwords = false)
      case "bigram" => new NGramFeaturizer(2, "bigram")
      case "trigram" => new NGramFeaturizer(3, "trigram")
    }
    val combinedFeaturizer = new CombinedTweetFeaturizers(featurizers)

    // Display features from a random sample of tweets
    if (opts.sampleFeatures()) {
      Random.shuffle(trainExamples).take(5).map(_.features).map { content =>
        println("\nContent: " + content)
        combinedFeaturizer(content).foreach { feature =>
          println("  " + feature.feature + " = " + feature.magnitude)
        }
      }
    }

    // Train the classifier
    val method = opts.method()
    val classifier = method match {
      case _ => {   // Assume Liblinear solver
        // Configure and train
        val solverType = liblinear.Solver(method)
        val config = liblinear.LiblinearConfig(
          solverType = solverType,
          cost = opts.cost(),
          showDebug = opts.verbose())
        NakContext.trainClassifier(config, combinedFeaturizer, trainExamples)
      }
    }

    // Predict the evaluation data
    val comparisons = {
      val maxLabel = NakContext.maxLabel(classifier.labels) _
      for (ex <- evalExamples)
        yield (ex.label, maxLabel(classifier.evalRaw(ex.features)), ex.features)
    }

    // Print the confusion matrix
    val (goldLabels, predictions, inputs) = comparisons.toSeq.unzip3
    val confusion = ConfusionMatrix(goldLabels, predictions, inputs)
    val output = if (opts.detailed()) confusion.detailedOutput else confusion.toString
    println(output)

    // Save the classifier to a file if requested
    opts.save.get match {
      case Some(path) => {
        NakContext.saveClassifier(classifier, path)
        println("\nClassifier saved to " + path)
      }
      case _ =>
    }
  }

  def readExamples(filenames: List[String]): Iterator[Example[String, String]] = {
    val labeledTweets = filenames.toIterator.flatMap { filename =>
      XMLLabeledTweetReader(filename)
    }
    labeledTweets.map { labeledTweet =>
      Example(labeledTweet.label, labeledTweet.content)
    }
  }
}

/**
 * An object that sets of the configuration for command-line options using
 * Scallop and returns the options, ready for use.
 */
object RealFakeOpts {
  import org.rogach.scallop._

  def apply(args: Array[String]) = new ScallopConf(args) {
    val featurizerTypes = Set("unigram", "bigram", "trigram")

    banner("""
Classification application.

For usage see below:
""")

    val cost = opt[Double](
      "cost",
      short = 'c',
      default = Some(1.0),
      descr = "The cost parameter C. Bigger values means less regularization (more fidelity to the training set).")
    val detailed = opt[Boolean](
      "detailed",
      short = 'd')
    val eval = opt[List[String]](
      "eval",
      short = 'e',
      descr = "The files containing evalualation events.")
    val method = opt[String](
      "method",
      short = 'm',
      default = Some("L2R_LR"),
      descr = "The type of solver to use. Possible values: any liblinear solver type.")
    val train = opt[List[String]](
      "train",
      short = 't',
      descr = "The files containing training events.")
    val featurizers = opt[List[String]](
      "featurizers",
      noshort = true,
      validate = validateList(featurizerTypes) _,
      descr = "Featurizers to use. Possible values: " + featurizerTypes.toSeq.sorted.mkString(", "))
    val save = opt[String](
      "save",
      noshort = true,
      descr = "Save the classify to the path provided"
    )
    val sampleFeatures = opt[Boolean](
      "sampleFeatures",
      noshort = true,
      descr = "Show features for a random sample training tweets."
    )
    val help = opt[Boolean](
      "help",
      noshort = true,
      descr = "Show this message")
    val verbose = opt[Boolean](
      "verbose",
      short = 'v')
  }

  def validateList(validator: (String => Boolean))(list: List[String]): Boolean = {
    list.map(validator).count(_ == false) == 0
  }
}
