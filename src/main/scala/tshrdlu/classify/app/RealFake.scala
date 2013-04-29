package tshrdlu.classify.app

import nak.NakContext
import nak.data.Example
import nak.liblinear
import nak.util.ConfusionMatrix

import tshrdlu.classify.{BasicRealFakeFeaturizer, ExtendedRealFakeFeaturizer}
import tshrdlu.classify.XMLLabeledTweetReader


object RealFake {
  def main(args: Array[String]) {
    // Parse and get the command-line options
    val opts = RealFakeOpts(args)

    // Read the datasets
    val trainExamples = readExamples(opts.train())
    val evalExamples = readExamples(opts.eval())

    // Train the classifier
    val method = opts.method()
    val classifier = method match {
      case _ => {   // Assume Liblinear solver
        // Construct appropriate featurizer
        val featurizer = {
          if (opts.extended()) {
            ExtendedRealFakeFeaturizer
          } else {
            BasicRealFakeFeaturizer
          }
        }

        // Configure and train
        val solverType = liblinear.Solver(method)
        val config = liblinear.LiblinearConfig(
          solverType = solverType,
          cost = opts.cost(),
          showDebug = opts.verbose())
        NakContext.trainClassifier(config, featurizer, trainExamples.toList)
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
    val extended = opt[Boolean](
      "extended",
      short = 'x',
      descr = "Use extended features.")
    val method = opt[String](
      "method",
      short = 'm',
      default = Some("L2R_LR"),
      descr = "The type of solver to use. Possible values: any liblinear solver type.")
    val train = opt[List[String]](
      "train",
      short = 't',
      descr = "The files containing training events.")
    val save = opt[String](
      "save",
      noshort = true,
      descr = "Save the classify to the path provided"
    )
    val help = opt[Boolean](
      "help",
      noshort = true,
      descr = "Show this message")
    val verbose = opt[Boolean](
      "verbose",
      short = 'v')
  }
}
