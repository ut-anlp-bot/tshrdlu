package tshrdlu.twitter

import akka.actor._
import twitter4j._


/**
 * An actor that constructs replies to a given status.
 */
trait BaseReplier extends Actor with ActorLogging {
  import Bot._
  import TwitterRegex._
  import tshrdlu.util.SimpleTokenizer

  import context.dispatcher
  import scala.concurrent.Future
  import akka.pattern.pipe

  def receive = {
    case ReplyToStatus(status, user, locationResolver) =>
      val replyName = status.getUser.getScreenName
      val candidatesFuture = getReplies(status, 138-replyName.length, user, locationResolver)
      candidatesFuture.map { candidates =>
        candidates.toSet.headOption.map({ replyText:String => 
          val reply = "@" + replyName + " " + replyText
          log.info("Candidate reply: " + reply)
          new StatusUpdate(reply).inReplyToStatusId(status.getId)
        })
      } pipeTo sender
  }

  def getReplies(status: Status, maxLength: Int, user: String, locationResolver: ActorRef): Future[Seq[String]]

}

/**
 * An actor that constructs replies to a given status.
 */
class GeoReplier extends BaseReplier {
  import Bot._
  import LocationResolver._
  import TwitterRegex._
  import tshrdlu.util.SimpleTokenizer
  import twitter4j._
  import scala.util.Random
  import nak._
  import nak.NakContext._
  import nak.core._
  import nak.data._
  import nak.liblinear.LiblinearConfig

  import java.util.{Timer,TimerTask}
  import java.io.{File, FileWriter,PrintWriter}
  import context.dispatcher
  import akka.pattern.ask
  import akka.util._
  import scala.concurrent.duration._
  import scala.concurrent.Await
  import scala.concurrent.Future
  import tshrdlu.twitter.LocationResolver
  import math.ceil
  import tshrdlu.util._
  import tshrdlu.twitter._

  implicit val timeout = Timeout(100 seconds)

  /**
   * Produce a reply to a status using geography
   */
  def getReplies(status: Status, maxLength: Int, user: String, locResolver: ActorRef): Future[Seq[String]] = {
    log.info("Trying to reply through GeoReplier")

    // Try to fetch the user's Location
    val locationResolver = (locResolver ? LocateStatus(status)).mapTo[Option[LocationConfidence]]
    val result = Await.result(locationResolver, 15.seconds).asInstanceOf[Option[LocationConfidence]]
    result match {
      case Some(loc) =>
        val latitude =loc.latitude
        val longitude = loc.longitude
        log.info("Latitude: " + loc.latitude + ", Longitude: " + loc.longitude + " -- " + loc.confidence)

        val text = stripLeadMention(status.getText).toLowerCase
        val tweetBuffer = collection.mutable.ListBuffer[String]()
        var numTweetsSeen = 0

        def simpleStatusListener = new StatusListener() {
          def onStatus(status: Status) {
            tweetBuffer += status.getText
            numTweetsSeen += 1
            println(status.getText)
          }
          def onDeletionNotice(statusDeletionNotice: StatusDeletionNotice) {}
          def onTrackLimitationNotice(numberOfLimitedStatuses: Int) {}
          def onException(ex: Exception) { ex.printStackTrace }
          def onScrubGeo(arg0: Long, arg1: Long) {}
          def onStallWarning(warning: StallWarning) {}
        }

        // Fetch tweets from set User's timeline, at the most 200
        val userScreenName = user
        val futureTweets = (context.parent ? UserTimeline(userScreenName)).mapTo[Seq[Status]]
        val tweets = Await.result(futureTweets, 15.seconds).asInstanceOf[Seq[Status]]

        // Strip Leading Mentions and replace other mentions with JustinBieber
        val userText = tweets.map(_.getText).toList.map {
          case StripMentionsRE(rest) => rest
          case x => x
        }
        .filter(tshrdlu.util.English.isSafe) 
        .map(x => x.replaceAll("""(?:RT\s)?(?:@[0-9]*[A-Za-z]+[0-9'_\.]*[A-Za-z\.]*:?\s)+""", ""))
        .map(x => x.replaceAll("""(?:@[0-9]*[A-Za-z]+[0-9'_\.]*[A-Za-z\.]*:?)""", "JustinBieber "))
        .map(x => x.replaceAll("""(?:\\s\\s)+""","\\s"))
        .filter(tshrdlu.util.English.isEnglish)
        println("////////////////")
        userText.foreach(println)
        println("////////////////")
        // Create a Bigram map of the user's tweets
        val tweetSet = userText
            .flatMap(x => Twokenize.tokenize("^ "+x.toLowerCase+" $$").filterNot(_.startsWith("http"))).toList
        val userFilter = tweetSet.sliding(2).flatMap{case List(p,q) => List(p+" "+q)}
        val userMap = userFilter.toList.groupBy(x=>x).mapValues(x=>x.length.toDouble).toMap

        val twitter = new TwitterFactory().getInstance

        // Collect Trends closest to the user's Location
        val locList = twitter.getClosestTrends(new GeoLocation(latitude,longitude))
        val loctrend = twitter.getPlaceTrends(locList.get(0).getWoeid).getTrends
        //val allTrend = twitter.getCurrentTrends(false).getTrends
        var trendList = List[String]()
        for(i <- 0 until loctrend.size) {
          trendList = loctrend(i).getName :: trendList
        }
        println(trendList)

        // Randomly select a trend close to the user
        val filterTrends = trendList.filter(x => x.startsWith("#"))
        val trending = Random.shuffle(filterTrends).head
        val trend = Array(trending)
        val tweetSearch  = twitter.search(new Query("\""+trending+"\"")).getTweets
        var tweetText = List[String]()
        for (i <- 0 to (tweetSearch.size - 1))
          tweetText =  tweetSearch.get(i).getText :: tweetText
        tweetText = tweetText.map {
          case StripMentionsRE(rest) => rest
          case x => x
        }
        .filter(tshrdlu.util.English.isSafe)
        .map(x => x.replaceAll("""(?:RT\s)?(?:@[0-9]*[A-Za-z]+[0-9'_\.]*[A-Za-z\.]*:?\s)+""", ""))
        .map(x => x.replaceAll("""(?:@[0-9]*[A-Za-z]+[0-9'_\.]*[A-Za-z\.]*:?)""", "JustinBieber "))
        .map(x => x.replaceAll("""(?:\\s\\s)+""","\\s"))
        .filter(tshrdlu.util.English.isEnglish)
        tweetText.foreach(println)
        val trainSet = tweetText.flatMap(x => Twokenize.tokenize("^ "+x.toLowerCase+" $$").filterNot(_.startsWith("http"))).toList
        val filterTrain = trainSet.sliding(2).flatMap{case List(p,q) => List(p+" "+q)}

        // Make a Bigram Map of the offline tweets collected along with their probabilities
        val normalizedNew = scala.io.Source.fromFile("src/main/resources/lang/eng/lexicon/twitterBigramfreq.txt").getLines
          .flatMap(t => t.split("\t") match {
            case Array(str1, str2) => Map(str1.toLowerCase -> str2.toDouble)
          }) toMap

        val normalizedMap = normalizedNew.toMap
        val markovMap = filterTrain.toList.groupBy(x=>x).mapValues(x=>x.length.toDouble).toMap

        val realSet = tweetText ++ userText
        val realTrend = realSet.map{x => if(!x.contains(trending)) x+" "+trending else x}
        val realTweets = realTrend.filterNot(x => (x=="" || x.contains("\n"))).sortBy(-_.length).distinct

      // Combine all three models using weights
      val finalMap = (markovMap /: userMap) {
        case (map, (k,v)) => map + ( k -> (0.5*v + 0.5*map.getOrElse(k, 0.0)))
      }

      val compreMap = (normalizedMap /: finalMap) {
        case (map, (k,v)) => map + ( k -> (0.80*v + 0.20*map.getOrElse(k, 0.0)))
      }

      // Generate 500 tweets using the aggregate model
      var replySample = List[String]()
      val now = System.nanoTime
      var seconds = 0.0
      var candidateCount = 0
      while(seconds < 20) {
        replySample = generateSentence(trending,compreMap,maxLength)::replySample
        seconds = (System.nanoTime - now) / 1000000000
        candidateCount +=1
      }
      //val replyList = Await.result(replySample, 20.seconds).asInstanceOf(List[String])
      val genReply = replySample.map(x => postProcess(x)).filter(x => x.length < maxLength)
      val sortReply = genReply.sortBy(-_.length).distinct

      // Printing distinct generated candidate replies sorted by length
      println("************************************")
      sortReply.foreach(println)
      println("************************************")

      println(candidateCount)
      // Load the classifier and run it on the generated candidate replies and
      // print top 10 tweets
      val classifier = NakContext.loadClassifier[FeaturizedClassifier[String,String]]("real_fake.obj")
      val comparisons = for (ex <- sortReply) yield
        classifier.evalRaw(ex)
      val lengthFilter = sortReply.filter(x => ((x.length > 40 && x.length < maxLength) && (x.split("\\s+").toList.length > 3)))
      val realIndex = classifier.labels.toList.indexOf("real")
      val results = comparisons.map(x => x.toList)
      val realList = results.map { x =>
        if(realIndex == 0) (x(0)-x(1)) else (x(1)-x(0))
      }
      val predictResult = realList.zip(lengthFilter).sortBy(-_._1)
      val topTen = predictResult.take(10)
      for(i <- 0 to (topTen.size - 1))
        println(topTen(i))
      if(lengthFilter.length == 0)
        Future{Seq(sortReply.head)}
      else
        Future{Seq(predictResult(0)._2)}
      case None => 
        log.info("Defaulting to Austin as location")
        //Future{Seq()}
        val latitude =30.26
        val longitude = -97.74
        log.info("Latitude: " + latitude + ", Longitude: " + longitude)

        val text = stripLeadMention(status.getText).toLowerCase
        val tweetBuffer = collection.mutable.ListBuffer[String]()
        var numTweetsSeen = 0

        def simpleStatusListener = new StatusListener() {
          def onStatus(status: Status) {
            tweetBuffer += status.getText
            numTweetsSeen += 1
            println(status.getText)
          }
          def onDeletionNotice(statusDeletionNotice: StatusDeletionNotice) {}
          def onTrackLimitationNotice(numberOfLimitedStatuses: Int) {}
          def onException(ex: Exception) { ex.printStackTrace }
          def onScrubGeo(arg0: Long, arg1: Long) {}
          def onStallWarning(warning: StallWarning) {}
        }

        // Fetch tweets from set User's timeline, at the most 200
        val userScreenName = user
        val futureTweets = (context.parent ? UserTimeline(userScreenName)).mapTo[Seq[Status]]
        val tweets = Await.result(futureTweets, 15.seconds).asInstanceOf[Seq[Status]]

        // Strip Leading Mentions and replace other mentions with JustinBieber
        val userText = tweets.map(_.getText).toList.map {
          case StripMentionsRE(rest) => rest
          case x => x
        }
        .filter(tshrdlu.util.English.isSafe) 
        .map(x => x.replaceAll("""(?:RT\s)?(?:@[0-9]*[A-Za-z]+[0-9'_\.]*[A-Za-z\.]*:?\s)+""", ""))
        .map(x => x.replaceAll("""(?:@[0-9]*[A-Za-z]+[0-9'_\.]*[A-Za-z\.]*:?)""", "JustinBieber "))
        .map(x => x.replaceAll("""(?:\\s\\s)+""","\\s"))
        .filter(tshrdlu.util.English.isEnglish)
        println("////////////////")
        userText.foreach(println)
        println("////////////////")
        // Create a Bigram map of the user's tweets
        val tweetSet = userText
            .flatMap(x => Twokenize.tokenize("^ "+x.toLowerCase+" $$").filterNot(_.startsWith("http"))).toList
        val userFilter = tweetSet.sliding(2).flatMap{case List(p,q) => List(p+" "+q)}
        val userMap = userFilter.toList.groupBy(x=>x).mapValues(x=>x.length.toDouble).toMap

        val twitter = new TwitterFactory().getInstance

        // Collect Trends closest to the user's Location
        val locList = twitter.getClosestTrends(new GeoLocation(latitude,longitude))
        val loctrend = twitter.getPlaceTrends(locList.get(0).getWoeid).getTrends
        //val allTrend = twitter.getCurrentTrends
        var trendList = List[String]()
        for(i <- 0 until loctrend.size) {
          trendList = loctrend(i).getName :: trendList
        }
        println(trendList)

        // Randomly select a trend close to the user
        val filterTrends = trendList.filter(x => x.startsWith("#"))
        val trending = Random.shuffle(filterTrends).head
        val trend = Array(trending)
        val tweetSearch  = twitter.search(new Query("\""+trending+"\"")).getTweets
        var tweetText = List[String]()
        for (i <- 0 to (tweetSearch.size - 1))
          tweetText =  tweetSearch.get(i).getText :: tweetText
        tweetText = tweetText.map {
          case StripMentionsRE(rest) => rest
          case x => x
        }
        .filter(tshrdlu.util.English.isSafe)
        .map(x => x.replaceAll("""(?:RT\s)?(?:@[0-9]*[A-Za-z]+[0-9'_\.]*[A-Za-z\.]*:?\s)+""", ""))
        .map(x => x.replaceAll("""(?:@[0-9]*[A-Za-z]+[0-9'_\.]*[A-Za-z\.]*:?)""", "JustinBieber "))
        .map(x => x.replaceAll("""(?:\\s\\s)+""","\\s"))
        .filter(tshrdlu.util.English.isEnglish)
        tweetText.foreach(println)
        val trainSet = tweetText.flatMap(x => Twokenize.tokenize("^ "+x.toLowerCase+" $$").filterNot(_.startsWith("http"))).toList
        val filterTrain = trainSet.sliding(2).flatMap{case List(p,q) => List(p+" "+q)}

        // Make a Bigram Map of the offline tweets collected along with their probabilities
        val normalizedNew = scala.io.Source.fromFile("src/main/resources/lang/eng/lexicon/twitterBigramfreq.txt").getLines
          .flatMap(t => t.split("\t") match {
            case Array(str1, str2) => Map(str1.toLowerCase -> str2.toDouble)
          }) toMap

        val normalizedMap = normalizedNew.toMap
        val markovMap = filterTrain.toList.groupBy(x=>x).mapValues(x=>x.length.toDouble).toMap

        val realSet = tweetText ++ userText
        val realTrend = realSet.map{x => if(!x.contains(trending)) x+" "+trending else x}
        val realTweets = realTrend.filterNot(x => (x=="" || x.contains("\n"))).sortBy(-_.length).distinct

      // Combine all three models using weights
      val finalMap = (markovMap /: userMap) {
        case (map, (k,v)) => map + ( k -> (0.5*v + 0.5*map.getOrElse(k, 0.0)))
      }

      val compreMap = (normalizedMap /: finalMap) {
        case (map, (k,v)) => map + ( k -> (0.80*v + 0.20*map.getOrElse(k, 0.0)))
      }

      // Generate 500 tweets using the aggregate model
      var replySample = List[String]()
      val now = System.nanoTime
      var seconds = 0.0
      var candidateCount = 0
      while(seconds < 20) {
        replySample = generateSentence(trending,compreMap,maxLength)::replySample
        seconds = (System.nanoTime - now) / 1000000000
        candidateCount +=1
      }
      //val replyList = Await.result(replySample, 20.seconds).asInstanceOf(List[String])
      val genReply = replySample.map(x => postProcess(x)).filter(x => x.length < maxLength)
      val sortReply = genReply.sortBy(-_.length).distinct

      // Printing distinct generated candidate replies sorted by length
      println("************************************")
      sortReply.foreach(println)
      println("************************************")

      println(candidateCount)
      // Load the classifier and run it on the generated candidate replies and
      // print top 10 tweets
      val classifier = NakContext.loadClassifier[FeaturizedClassifier[String,String]]("real_fake.obj")
      val comparisons = for (ex <- sortReply) yield
        classifier.evalRaw(ex)
      val lengthFilter = sortReply.filter(x => ((x.length > 40 && x.length < maxLength) && (x.split("\\s+").toList.length > 3)))
      val realIndex = classifier.labels.toList.indexOf("real")
      val results = comparisons.map(x => x.toList)
      val realList = results.map { x =>
        if(realIndex == 0) (x(0)-x(1)) else (x(1)-x(0))
      }
      val predictResult = realList.zip(lengthFilter).sortBy(-_._1)
      val topTen = predictResult.take(10)
      for(i <- 0 to (topTen.size - 1))
        println(topTen(i))
      if(lengthFilter.length == 0)
        Future{Seq(sortReply.head)}
      else
        Future{Seq(predictResult(0)._2)}
    }
  }

  def stringLength(s1:Int,s2:Int) : Int = {
    s1+s2+1
  }

  /**
    * Post processing of candidate replies.
    */
  def postProcess(reply:String): String = {
    var tempReply = reply.trim
    var finalReply = ""
    if(tempReply.contains("."))
    tempReply = tempReply.slice(0,tempReply.indexOf(".")-1) + tempReply.slice(tempReply.indexOf("."),tempReply.length)
    if(tempReply.contains("\" "))
    tempReply = tempReply.slice(0,tempReply.indexOf("\"")) + tempReply.slice(tempReply.indexOf("\"")+2,tempReply.length)
    if(tempReply.contains(","))
    tempReply = tempReply.slice(0,tempReply.indexOf(",")-1) + tempReply.slice(tempReply.indexOf(","),tempReply.length)
    if(tempReply.contains("!"))
    tempReply = tempReply.slice(0,tempReply.indexOf("!")-1) + tempReply.slice(tempReply.indexOf("!"),tempReply.length)
    if(tempReply.contains("?"))
    tempReply = tempReply.slice(0,tempReply.indexOf("?")-1) + tempReply.slice(tempReply.indexOf("?"),tempReply.length)
    if(tempReply.contains(":"))
    tempReply = tempReply.slice(tempReply.indexOf(":")+1,tempReply.length)
    finalReply = tempReply.trim
    if(!finalReply(0).isUpper)
    finalReply = finalReply(0).toString.toUpperCase + finalReply.substring(1, finalReply.length)
    val tReply = finalReply.replaceAll("""\s+([,!\"?\.'':])""","""$1""")
    val r = """[\.!?]\s+[a-z]""".r.replaceAllIn(tReply, _.matched.toUpperCase)
    val p = r.replaceAll("""\s+(?:i')""",""" I'""")
    val g = p.replaceAll(""" i """,""" I """)
    return g
  }

  /**
    * Generate sentence going using forward bigram model, starting with ^ and
    * ending with $$
    */
  def generateSentence(trending:String,compreMap:Map[String,Double],maxLength : Int):String={
    var newSentence = ""
    var symbol = "^"
    var genSentence = ""
    var flag1 = false
    while(symbol != "$$") {
      val wordKey = compreMap.filter(p => p._1.startsWith(symbol+" "))
      val wordCountOld = wordKey.toSeq.sortBy(-_._2)
      val total = wordCountOld.map(_._2).sum
      val wordCount = wordKey.map { v => (v._1 -> v._2*1.0 / total)}.toSeq.sortBy(-_._2)
      val rand = Random.nextDouble()
      var sampleProb = wordCount(0)._2
      var ind = 0
      while(sampleProb < rand ){
        ind +=1
        sampleProb+=wordCount(ind)._2     
      }

      val word = wordCount(ind)._1.split("\\s+").toList
      symbol = word(1)
      if(flag1 == false)
        flag1 = true
      else
        newSentence = newSentence+" "+word(0)
      //println(symbol)
      //println(newSentence)
    }

    if (!newSentence.contains(trending))
      newSentence = newSentence + " "+ trending
      newSentence
    }

  def getText(status: Status): Option[String] = {
    import tshrdlu.util.English.{isEnglish,isSafe}

    val text = status.getText match {
      case StripMentionsRE(rest) => rest
      case x => x
    }

    if (!text.contains('@') && !text.contains('/') && isEnglish(text) && isSafe(text))
      Some(text)
    else
      None
  }

  def Tokenize(text: String): IndexedSeq[String]={
    val starts = """(?:[#@])|\b(?:http)"""
    text
      .toLowerCase
      .replaceAll("""([\?!()\";\|\[\].,':])""", " $1 ")
      .trim
      .split("\\s+")
      .filterNot(x=> x.contains("'"))
      .filterNot(x=> x=="'")
      .filterNot(x=> x=="!")
      .filterNot(x=> x=="?")
      .toIndexedSeq
      .filterNot(x => x.startsWith(starts))
      .filterNot(x => x.startsWith("RT"))
  }

  def using[A <: {def close(): Unit}, B](param: A)(f: A => B): B =
      try { f(param) } finally { param.close() }

  def appendToFile(fileName:String, textData:String) =
    using (new FileWriter(fileName, true)){
      fileWriter => using (new PrintWriter(fileWriter)) {
        printWriter => printWriter.flush
        printWriter.println(textData)
      }
    }
}
