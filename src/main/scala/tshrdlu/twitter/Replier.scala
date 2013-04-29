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
    case ReplyToStatus(status) => 
      val replyName = status.getUser.getScreenName
      val candidatesFuture = getReplies(status, 138-replyName.length)
      candidatesFuture.map { candidates =>
        candidates.toSet.headOption.map({ replyText:String => 
          val reply = "@" + replyName + " " + replyText
          log.info("Candidate reply: " + reply)
          new StatusUpdate(reply).inReplyToStatusId(status.getId)
        })
      } pipeTo sender
  }

  def getReplies(status: Status, maxLength: Int): Future[Seq[String]]

}
/*
//class GeoLocation(lat:double, lon:double){

//}

class SynonymReplier extends BaseReplier {
  import Bot._ 
  import tshrdlu.util.English.synonymize
  import TwitterRegex._

  import context.dispatcher
  import scala.concurrent.Future

  def getReplies(status: Status, maxLength: Int = 140): Future[Seq[String]] = {
    log.info("Trying to reply synonym")
    val text = stripLeadMention(status.getText).toLowerCase
    val synTexts = (0 until 10).map(_ => Future(synonymize(text))) 
    Future.sequence(synTexts).map(_.filter(_.length <= maxLength))
  }

}

/**
 * An actor that constructs replies to a given status.
 * For best results, tweet at me something related to one of the 
 * topics from the "20 Newsgroups" data
 * e.g. Religion, baseball, atheism, windows, hockey, mideast, pc hardware
 */
class TopicModelReplier extends BaseReplier {
  import Bot._ 
  import TwitterRegex._
  import tshrdlu.util.SimpleTokenizer

  import context.dispatcher
  import akka.pattern.ask
  import akka.util._
  import scala.concurrent.duration._
  import scala.concurrent.Future
  implicit val timeout = Timeout(10 seconds)

  val modeler = new TopicModeler("minTopicKeys.txt")

  def getReplies(status: Status, maxLength: Int = 140): Future[Seq[String]] = {
    log.info("Trying to reply via topic models")
    val text = stripLeadMention(status.getText).toLowerCase
    val statusTopicList = SimpleTokenizer(text)
				.filter(_.length > 4)
				.toSet
				.take(3)
				.toList
				.flatMap(w => modeler.wordTopicsMap.get(w))
				.flatten

	val topicWords:List[String] = statusTopicList.map(topic => 
		modeler.topicWordsMap.getOrElse(topic,Set(" "))).take(4).flatten

	val statusQueryList :List[String] = topicWords
				.filter(_.length > 4)
                .filter(_.length < 11)
	        	.sortBy(- _.length)
				.distinct
    
    // Get a sequence of futures of status sequences (one seq for each query)
    val statusSeqFutures: Seq[Future[Seq[Status]]] = 
		if(statusQueryList.length <1) {
			SimpleTokenizer(text)
				.filter(_.length > 3)
				.filter(_.length < 10)
				.filterNot(_.contains('/'))
				.filter(tshrdlu.util.English.isSafe)
				.sortBy(- _.length)
				.take(3) 
				.map(w => (context.parent ? 
					SearchTwitter(new Query(w))).mapTo[Seq[Status]])}
		else { statusQueryList
    			.map(w => (context.parent ? 
					SearchTwitter(new Query(w))).mapTo[Seq[Status]])}

    // Convert this to a Future of a single sequence of candidate replies
    val statusesFuture: Future[Seq[Status]] =
      	Future.sequence(statusSeqFutures).map(_.flatten)

	statusesFuture.map{x => extractText(x, statusTopicList.toSet)}
  }

  /**
   * Go through the list of tweets, gets "proper" tweets, determines
   * topic distribution vectors of said tweets, calculates similarities
   * between original tweet and candidate tweets
   * Returns most similar tweeet
   */
  def extractText(statusList: Seq[Status], statusTopics: Set[String]) = {
    val useableTweets = statusList
      .map(_.getText)
      .map {
			case StripMentionsRE(rest) => rest
			case x => x
      }
      .filterNot(_.contains('@'))
      .filterNot(_.contains('/'))
      .filter(tshrdlu.util.English.isEnglish)
      .filter(tshrdlu.util.English.isSafe)

    //Use topic model to select response
    val topicDistributions = for ( tweet <- useableTweets) yield {
    			SimpleTokenizer(tweet).filter(_.length > 4)
				.toSet
				.take(3)
				.toList
				.flatMap(w => modeler.wordTopicsMap.get(w))
				.flatten}
    
    val topicSimilarity = topicDistributions.map(ids => 
		ids.toSet.intersect(statusTopics).size * {
			if(statusTopics.size -ids.toSet.size ==0 ) 1 
			else (1/math.abs(statusTopics.size - ids.toSet.size)).toDouble})
    
    val topTweet = topicSimilarity.toList.zip(useableTweets).maxBy(_._1)._2

    List(topTweet)
  }

  def getText(status: Status): Option[String] = {
    import tshrdlu.util.English.{isEnglish,isSafe}

    val text = status.getText match {
      case StripMentionsRE(rest) => rest
      case x => x
    }
    
    if (!text.contains('@') && !text.contains('/') && isEnglish(text) && isSafe(text))
      Some(text)
    else None
  }
}


/**
 * An actor that constructs replies to a given status.
 */
class StreamReplier extends BaseReplier {
  import Bot._
  import TwitterRegex._
  import tshrdlu.util.SimpleTokenizer

  import context.dispatcher
  import akka.pattern.ask
  import akka.util._
  import scala.concurrent.duration._
  import scala.concurrent.Future
  implicit val timeout = Timeout(10 seconds)

  /**
   * Produce a reply to a status.
   */
  def getReplies(status: Status, maxLength: Int = 140): Future[Seq[String]] = {
    log.info("Trying to reply stream")

    val text = stripLeadMention(status.getText).toLowerCase
    
    // Get a sequence of futures of status sequences (one seq for each query)
    val statusSeqFutures: Seq[Future[Seq[Status]]] = SimpleTokenizer(text)
    .filter(_.length > 3)
    .filter(_.length < 10)
    .filterNot(_.contains('/'))
    .filter(tshrdlu.util.English.isSafe)
    .sortBy(- _.length)
    .take(3)
    .map(w => (context.parent ? SearchTwitter(new Query(w))).mapTo[Seq[Status]])

    // Convert this to a Future of a single sequence of candidate replies
    val statusesFuture: Future[Seq[Status]] =
      Future.sequence(statusSeqFutures).map(_.flatten)

    // Filter statuses to their text and make sure they are short enough to use.
    statusesFuture.map(_.flatMap(getText).filter(_.length <= maxLength))
  }


  /**
   * Go through the list of Statuses, filter out the non-English ones and
   * any that contain (known) vulgar terms, strip mentions from the front,
   * filter any that have remaining mentions or links, and then return the
   * head of the set, if it exists.
   */
  def getText(status: Status): Option[String] = {
    import tshrdlu.util.English.{isEnglish,isSafe}

    val text = status.getText match {
      case StripMentionsRE(rest) => rest
      case x => x
    }
    
    if (!text.contains('@') && !text.contains('/') && isEnglish(text) && isSafe(text))
      Some(text)
    else None
  }

}


/**
 * An actor that constructs replies to a given status based on synonyms.
 */
class SynonymStreamReplier extends StreamReplier {
  import Bot._
  import tshrdlu.util.SimpleTokenizer

  import context.dispatcher
  import akka.pattern.ask
  import akka.util._
  import scala.concurrent.duration._
  import scala.concurrent.Future

  import tshrdlu.util.English._
  import TwitterRegex._
  override implicit val timeout = Timeout(10000)


  override def getReplies(status: Status, maxLength: Int = 140): Future[Seq[String]] = {
    log.info("Trying to do synonym search")
    val text = stripLeadMention(status.getText).toLowerCase

    // Get two words from the tweet, and get up to 5 synonyms each (including the word itself).
    // Matched tweets must contain one synonym of each of the two words.

    val query:String = SimpleTokenizer(text)
      .filter(_.length > 3)
      .filter(_.length < 10)
      .filterNot(_.contains('/'))
      .filter(tshrdlu.util.English.isSafe)
      .filterNot(tshrdlu.util.English.stopwords(_))
      .take(2).toList
      .map(w => synonymize(w, 5))
      .map(x=>x.mkString(" OR ")).map(x=>"("+x+")").mkString(" AND ")

    log.info("searched for: " + query)

    val futureStatuses = (context.parent ? SearchTwitter(new Query(query))).mapTo[Seq[Status]]

    futureStatuses.map(_.flatMap(getText).filter(_.length <= maxLength))
 }

}


/**
 * An actor that constructs replies to a given status.
 */
class BigramReplier extends BaseReplier {
  import Bot._
  import TwitterRegex._
  import tshrdlu.util.SimpleTokenizer

  import context.dispatcher
  import akka.pattern.ask
  import akka.util._
  import scala.concurrent.duration._
  import scala.concurrent.Future
  implicit val timeout = Timeout(10 seconds)

  /**
   * Produce a reply to a status using bigrams
   */
  lazy val stopwords = tshrdlu.util.English.stopwords_bot
  def getReplies(status: Status, maxLength: Int = 140): Future[Seq[String]] = {
    log.info("Trying to reply stream")

    val text = stripLeadMention(status.getText).toLowerCase
    
    // Get a sequence of futures of status sequences (one seq for each query)

    val bigram = Tokenize(text)
      .sliding(2)
      .filterNot(z => (stopwords.contains(z(0))||stopwords.contains(z(1))))
      .flatMap{case Vector(x,y) => List(x+" "+y)}
      .toList
      .sortBy(-_.length)

    val statusSeqFutures: Seq[Future[Seq[String]]] = bigram
      .takeRight(5)
      .map(w => (context.parent ? SearchTwitter(new Query("\""+w+"\""))).mapTo[Seq[Status]].map(_.flatMap(getText).toSeq))
    
    //statusSeqFutures.foreach(println)
    // Convert this to a Future of a single sequence of candidate replies
    val statusesFuture: Future[Seq[String]] =
      extractText(statusSeqFutures,bigram.toList)

    //statusesFuture.foreach(println)
    // Filter statuses to their text and make sure they are short enough to use.
    statusesFuture.filter(_.length <= maxLength)
  }

  def extractText(statusList: Seq[Future[Seq[String]]],bigram:List[String]): Future[Seq[String]] = {
    val bigramMap = Future.sequence(statusList).map(_.flatten)
    //bigramMap.foreach(println)
    val sortedMap = bigramMap.map { tweet => {
      tweet.flatMap{ x => { 
        Tokenize(x)
          .sliding(2)
          .filterNot(z => (stopwords.contains(z(0))||stopwords.contains(z(1))))
          .map(bg => bg.mkString(" ") -> x) toMap
      }}.filter { case (p,q) => bigram.contains(p)}
    }}

    val bigramSeq = sortedMap.map(_.map(_._2))
    bigramSeq
  }

  def getText(status: Status): Option[String] = {
    import tshrdlu.util.English.{isEnglish,isSafe}

    val text = status.getText match {
      case StripMentionsRE(rest) => rest
      case x => x
    }
    
    if (!text.contains('@') && !text.contains('/') && isEnglish(text) && isSafe(text))
      Some(text)
    else None
  }

  
  def Tokenize(text: String): IndexedSeq[String]={
    val starts = """(?:[#@])|\b(?:http)"""
    text
    .replaceAll("""([\?!()\";\|\[\].,':])""", " $1 ")
    .trim
    .split("\\s+")
    .toIndexedSeq
    .filterNot(x => x.startsWith(starts))
  }

}
*/
/**
 * An actor that constructs replies to a given status.
 */
class GeoReplier(locResolver: ActorRef)extends BaseReplier {
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
  import tshrdlu.classify.{BasicRealFakeFeaturizer, ExtendedRealFakeFeaturizer}
  implicit val timeout = Timeout(100 seconds)
  
  /**
   * Produce a reply to a status using geography
   */

  def getReplies(status: Status, maxLength: Int = 140): Future[Seq[String]] = {
    log.info("Trying to reply through GeoReplier")
    /**
    * Try to fetch the user's Location
    */

    //val locResolver = context.actorFor("akka://TwitterBot/user/LocationResolver")
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
    def onException(ex: Exception) { ex.printStackTrace 
    }
    def onScrubGeo(arg0: Long, arg1: Long) {}
    def onStallWarning(warning: StallWarning) {}
    }

    //val botInst = new Bot
    val twitter = new TwitterFactory().getInstance
    //val twitterStream = botInst.streamer.stream
    //new TwitterStreamFactory().getInstance
   /**
    * Collect Trends closest to the user's Location
    */
    
    //val latitude = 30.26
    //val longitude = -97.74
    val locList = twitter.getClosestTrends(new GeoLocation(latitude,longitude))
    val loctrend = twitter.getPlaceTrends(locList.get(0).getWoeid).getTrends
    var trendList = List[String]()
    for(i <- 0 until loctrend.size){
          trendList = loctrend(i).getName :: trendList
    }
    println(trendList)
    val filterTrends = trendList.filter(x => x.startsWith("#"))
    //var trendFlag = false
    //var trending = ""
    //var trainSet = List[String]()

    //while(trendFlag == false){
    val trending = Random.shuffle(filterTrends).head
    
    //println("-------------->>>>"+trending)
    //if(trending.startsWith("#")) 
      //trending.slice(1,trending.length-1)
    val trend = Array(trending)
    val tweetSearch  = twitter.search(new Query("\""+trending+"\"")).getTweets
    var tweetText = List[String]()
    for(i <-0 to tweetSearch.size-1) 
      tweetText =  tweetSearch.get(i).getText :: tweetText

    tweetText.foreach(println)
    /*
    twitterStream.cleanUp
    twitterStream.addListener(simpleStatusListener)
    val filterQuery = new FilterQuery()
    filterQuery.locations(Array(Array(longitude-2, latitude-2), Array(longitude+2, latitude+2)))
    //println("************")
    filterQuery.track(trend)
    twitterStream.getFilterStream(filterQuery) 
    /**
    * Collect filtered tweets useful for sentence generation
    */
    while(numTweetsSeen < 5){
      Thread.sleep(1000)
      println(numTweetsSeen)
    }
    println(trending)
    twitterStream.shutdown
    */
    val trainSet = tweetText
        .map {
      case StripMentionsRE(rest) => rest
      case x => x
    }
    .filterNot(_.contains('/'))
    .filterNot(_.contains(" : "))
    .filter(tshrdlu.util.English.isSafe) 
    .map(x => x.replaceAll("""@[A-Za-z]+_?[A-Za-z]*""", "Justin Bieber"))
    .filter(tshrdlu.util.English.isEnglish).flatMap(x => Twokenize.tokenize("^ "+x.toLowerCase+" $$"))
   /*
    if(!trainSet.contains(trending.toLowerCase))
      trendFlag = false
    else
      trendFlag = true
}
*/
    val wordCountOnline = trainSet.toList.groupBy(x=>x).mapValues(x=>x.length).toMap
    val filterTrain = trainSet.sliding(2).flatMap{case List(p,q) => List(p+" "+q)}
    /**
    * Make a Bigram Map of the tweets collected along with their probabilities
    */
  val normalizedNew = scala.io.Source.fromFile("src/main/resources/lang/eng/lexicon/twitterBigramfreq.txt").getLines
    .flatMap(t => t.split("\t") match {
      case Array(str1, str2) => Map(str1 -> str2.toDouble)
  } ) toMap
  //val markovMap = filterTrain.toList.groupBy(x=>x).mapValues(x=>x.length).toMap
 //val normalMarkov = markovMap.map(x => (x._1 -> x))
   val normalizedMap = normalizedNew.toMap
   /*for((k,v) <- normalizedNews){
    println(k+" ->"+v)
   }*/
   val markovMap = filterTrain.toList.groupBy(x=>x).mapValues(x=>x.length.toDouble).toMap
   /*
   val markovMapfwd = markovMap.map{ case (t,u) => 
   val wordList = t.split("\\s+").toList
   //val wordKey = markovMap.filter(p => p._1.startsWith(wordList(0)+" "))
   val wordCount = wordCountOnline.getOrElse(wordList(0),0)
   //wordKey.foldLeft(0)( (acc, kv) => acc + kv._2)
   if(wordCount == 0)
    (t -> u*1.0)
   //println(wordCount)
   else
    (t -> u*1.0/(wordCount.toDouble))
   }
   */
   /*
for((k,v) <- markovMapfwd){
    println(k+" ->"+v)
   }

   val markovMapbwd = markovMap.map{ case (t,u) => 
   val wordList = t.split("\\s+").toList
   //val wordKey = markovMap.filter(p => p._1.endsWith(" "+wordList(1)))
   val wordCount = wordCountOnline.getOrElse(wordList(1),0)
   //wordKey.foldLeft(0)( (acc, kv) => acc + kv._2)
   if(wordCount == 0)
    (t -> u*1.0)
   //println(wordCount)
   else
    (t -> 1.0*u/(wordCount.toDouble))
   }
   */
    //val comblist = markovMap.toList ++ normalizedNews.toList
    //val compreMap = comblist.groupBy ( _._1) .map { case (k,v) => k -> v.map(_._2).sum }
   /*
   val normalizedMapfwd = normalizedNews.map{ case (t,u) => 
   val wordList = t.split("\\s+").toList
   val wordKey = markovMap.filter(p => p._1.startsWith(wordList(0)+" "))
   val wordCount = wordKey.foldLeft(0)( (acc, kv) => acc + kv._2)
   if(wordCount == 0)
    (t -> u*1.0)
    else
   //println(wordCount)
    (t -> 1.0*u/(wordCount.toDouble))
   }
   */
   /*
   val normalizedMapbwd = normalizedNews.map{ case (t,u) => 
   val wordList = t.split("\\s+").toList
   val wordKey = markovMap.filter(p => p._1.endsWith(" "+wordList(1)))
   val wordCount = wordKey.foldLeft(0)( (acc, kv) => acc + kv._2)
   //println(wordCount)
   if(wordCount == 0)
    (t -> u*1.0)
    else
  // println(wordCount)
    (t -> 1.0*u/(wordCount.toDouble))
   }

   val compreMapfwd = (normalizedMapfwd /: markovMapfwd) { case (map, (k,v)) =>
    map + ( k -> (0.5*v + 0.5*map.getOrElse(k, 0.0)))
}
*/
  val compreMap = (normalizedMap /: markovMap) { case (map, (k,v)) =>
    map + ( k -> (0.70*v + 0.30*map.getOrElse(k, 0.0)))
}
   /*
    val normalizedMap = markovMap.map{ case (t,u) => 
    val wordList = t.split("\\s+").toList
    val wordKey = markovMap.filterKeys(p => p.startsWith(wordList(0)+" "))
    //val filteredMap = wordKey.map(x => (x._1 -> Math.ceil(x._2).toInt))
    val wordCount = wordKey.foldLeft(0)( (acc, kv) => acc + kv._2)
    (t -> u/wordCount.toDouble)
   }*/
   /**
    * Merge the original map with the 20 news group tokenized corpus and normalize probabilities
    */
    //val combList = markovMap.toList ++ normalizedNews.toList
    //val compreMap = combList.groupBy (_._1).map { case (k,v) => k -> v.map(_._2).sum }
  /*
  for((k,v) <- normalizedMapfwd){
    println(k+" ->"+v)
   }
*/
    var replySample = List[String]()
    for(k <- 1 to 200){  
      replySample = generateSentence(trending,compreMap,maxLength)::replySample
    }
    

    val genReply = replySample.map(x => postProcess(x))
    println("************************************")
    genReply.foreach(println)
    println("************************************")
    val sortReply = genReply.sortBy(-_.length).distinct
    println("************************************")
    sortReply.foreach(println)
    println("************************************")
    val classifier = NakContext.loadClassifier[FeaturizedClassifier[String,String]]("real_fake.obj")
    val comparisons = for (ex <- sortReply) yield 
      classifier.evalRaw(ex)
    //val predictions, input) = comparisons.unzip3
    val lenthFilter = sortReply.filter(x => x.length > 40)
    //print(classifier.labels)
    val realIndex = classifier.labels.toList.indexOf("real")
    val results = comparisons.map(x => x.toList)
    //results.foreach(println)
    val realList = results.map{x => if(realIndex == 0) x(0)
      else x(1)}
    val predictResult = realList.zip(lenthFilter).sortBy(-_._1)
    val topTen = predictResult.take(10)
    for(i <- 0 to topTen.length-1)
      println(topTen(i))
    Future{Seq(predictResult(0)._2)}
    case None => log.info("Couldn't find status location")
        Future{Seq()}
    
    }


//Future{Seq("newSentence")}
  }
  def stringLength(s1:Int,s2:Int) : Int ={
    s1+s2+1
  }
  def postProcess(reply:String): String ={
    var tempReply = reply(0).toString.toUpperCase + reply.substring(1, reply.length)
    if(tempReply.contains("."))
    tempReply = tempReply.slice(0,tempReply.indexOf(".")-1) + tempReply.slice(tempReply.indexOf("."),tempReply.length)
    if(tempReply.contains("\""))
    tempReply = tempReply.slice(0,tempReply.indexOf("\"")-1) + tempReply.slice(tempReply.indexOf("\""),tempReply.length)
    if(tempReply.contains(","))
    tempReply = tempReply.slice(0,tempReply.indexOf(",")-1) + tempReply.slice(tempReply.indexOf(","),tempReply.length)
    if(tempReply.contains("!"))
    tempReply = tempReply.slice(0,tempReply.indexOf("!")-1) + tempReply.slice(tempReply.indexOf("!"),tempReply.length)
    if(tempReply.contains("?"))
    tempReply = tempReply.slice(0,tempReply.indexOf("?")-1) + tempReply.slice(tempReply.indexOf("?"),tempReply.length)
    //if(tempReply.contains("'"))
    //tempReply = tempReply.slice(0,tempReply.indexOf("'")-1) + tempReply.slice(tempReply.indexOf("'"),tempReply.length)
    return tempReply
  }

  def generateSentence(trending:String,compreMap:Map[String,Double],maxLength : Int):String={
    var limit = true
    var newSentence = ""
    var count = 0
    //while(limit==true){
    limit = false
    val trendSymbol = trending.split("\\s+").toList
    var startSym = trendSymbol.last.toLowerCase
    var endSym = trendSymbol.head.toLowerCase
    var symbol = startSym 
    /**
    * Generate sentence going forward and backward starting from a randomly chosen trend in the nearby area
    */
    count = 0
    println(startSym)
    newSentence = trending
    var genSentence = ""
    var flag1 = false
    var flag2 = false
    var fwd = true
    var fwdway = true
    var bwdway = true
    println(newSentence)
    while(stringLength(newSentence.length, symbol.length) < maxLength && (fwdway ==true || bwdway ==true) && count < 20){
      count = count+1
      //println("length: "+stringLength(newSentence.length, symbol.length))
      //println(symbol)
      if(fwd == true && fwdway ==true){
        fwd = false
        //val fwdSet = compreMapfwd.filterKeys(p => p.startsWith(startSym+" "))
        //if(fwdSet.isEmpty)
        //limit = true
        val wordKey = compreMap.filter(p => p._1.startsWith(startSym+" "))
        val wordCountOld = wordKey.toSeq.sortBy(-_._2)
        val total = wordCountOld.map(_._2).sum
        val wordCount = wordKey.map { v => (v._1 -> v._2*1.0 / total)}.toSeq.sortBy(-_._2)
       //println(wordCount)
        val rand = Random.nextDouble()
       // println(rand + " randomfwd")
        var sampleProbf = wordCount(0)._2
        var indf = 0
        while(sampleProbf < rand && wordCount.size >1){
          indf +=1
          if(indf > wordCount.size-1){
        indf = wordCount.size-1
        sampleProbf=2
      }  
      else
          sampleProbf+=wordCount(indf)._2     
        }
        
        val word = wordCount(indf)._1.split("\\s+").toList
        startSym = word(1)
        if(startSym=="$$"){
          fwdway = false
         // if(flag1 == false)
          //newSentence = newSentence+" "+word(0)
        }
        if(flag1 == false)
          flag1 = true
        else
          newSentence = newSentence+" "+word(0)
        symbol = startSym
        //println(symbol)
        println(newSentence)
      }
      else if(bwdway == true){
        fwd = true
        //val bwdSet = compreMapbwd.filterKeys(p => p.endsWith(" "+endSym))
        //if(bwdSet.isEmpty)
        //limit = true
        val wordKey = compreMap.filter(p => p._1.endsWith(" "+endSym))
        val wordCountOld = wordKey.toSeq.sortBy(-_._2)
        val total = wordCountOld.map(_._2).sum
        val wordCount = wordKey.map { v => (v._1 -> v._2*1.0 / total)}.toSeq.sortBy(-_._2)
        //println(wordCount)
        var sampleProbb = wordCount(0)._2
        var indb = 0
        val rand = Random.nextDouble()
          //println(rand + " randombwd")
        while(sampleProbb < rand && wordCount.size >1){
          indb +=1
           if(indb > wordCount.size-1){
        indb = wordCount.size-1 
        sampleProbb= 2
      }   
      else
          sampleProbb+=wordCount(indb)._2 
        }

        val word = wordCount(indb)._1.split("\\s+").toList
      endSym =  word(0)
      if(endSym == "^"){
        bwdway = false
       // if(flag2 == false)
        //newSentence = word(1)+" "+newSentence
      }
      if(flag2 == false)
          flag2 = true
        else
          newSentence = word(1)+" "+newSentence
      symbol = endSym
      //println(symbol)
      println(newSentence)
      }
    }
    if(fwdway ==true || bwdway == true || count > 5)
    limit = true
 // }
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
    else None
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

/*
/**
 * An actor that constructs replies to a given status.
 */
class LuceneReplier extends BaseReplier {
  import Bot._
  import TwitterRegex._
  import tshrdlu.util.{English, Lucene, SimpleTokenizer}

  import context.dispatcher
  import akka.pattern.ask
  import akka.util._
  import scala.concurrent.duration._
  import scala.concurrent.Future

  def getReplies(status: Status, maxLength: Int = 140): Future[Seq[String]] = {
    log.info("Trying to do search replies by Lucene")
    val text = status.getText.toLowerCase
	  val StripLeadMentionRE(withoutMention) = text
	  val query = SimpleTokenizer(withoutMention)
	    .filter(_.length > 2)
	    .toList
	    .mkString(" ")
      val replyLucene = Lucene.read(query)
    Future(replyLucene).map(_.filter(_.length <= maxLength))
  }

}

/**
 * A replier that replies based on unsupervised noun phrase chunking of a given tweet.
 */
class ChunkReplier extends BaseReplier {
  import Bot._
  import tshrdlu.util.{English, Lucene, SimpleTokenizer}
  import jarvis.nlp.TrigramModel
  import jarvis.nlp.util._
  import scala.concurrent.Future  
  import TwitterRegex._
  import akka.pattern.ask
  import akka.util._
  import context.dispatcher
  import scala.concurrent.duration._
  import scala.concurrent.Future
  import java.net.URL

  implicit val timeout = Timeout(10 seconds)

  //A Trigram language model based on a dataset of mostly english tweets
  val LanguageModel = TrigramModel(SPLReader(this.getClass().getResource("/chunking/").getPath()))

  val Chunker = new Chunker()

  def getReplies(status: Status, maxLength: Int = 140): Future[Seq[String]] = {
    log.info("Getting chunk tweets")
    val text = status.getText.toLowerCase
    val StripLeadMentionRE(withoutMention) = text
    val selectedChunks = Chunker(withoutMention)
      .map(c => (LanguageModel(SimpleTokenizer(c)), c))
      .sorted
      .take(2)
      .map(_._2)
    
     val statusList: Seq[Future[Seq[Status]]] = selectedChunks
         .map(chunk => (context.parent ? SearchTwitter(new Query(chunk))).mapTo[Seq[Status]])

    val statusesFuture: Future[Seq[Status]] = Future.sequence(statusList).map(_.flatten)

    statusesFuture
      .map(status => extractText(status))
      .map(_.filter(_.length <= maxLength))
  }

  /**
   * Go through the list of Statuses, filter out the non-English ones,
   * strip mentions from the front, filter any that have remaining
   * mentions, and then return the head of the set, if it exists.
   */
   def extractText(statusList: Seq[Status]): Seq[String] = {
     val useableTweets = statusList
       .map(_.getText)
       .map {
         case StripMentionsRE(rest) => rest
         case x => x
       }.filter(tweet => tshrdlu.util.English.isEnglish(tweet) 
                       &&  tshrdlu.util.English.isSafe(tweet)
                       && !tweet.contains('@')
                       && !tweet.contains('/'))
      .map(t => (LanguageModel(SimpleTokenizer(t)), t))
      .sorted
      .reverse
      .map{ case (k,t) => t}
      //given the set of potential tweets, return the tweet that has
      //the highest probability according to the language model
      Seq(if (useableTweets.isEmpty) "I don't know what to say." else useableTweets.head)
  }
}

/**
 * An actor that responds to requests to make sandwiches.
 *
 * @see <a href="http://xkcd.com/149/">http://xkcd.com/149/</a>
 */
class SudoReplier extends BaseReplier {
  import scala.concurrent.Future
  import context.dispatcher

  lazy val MakeSandwichRE = """(?i)(?:.*(\bsudo\b))?.*\bmake (?:me )?an?\b.*\bsandwich\b.*""".r

  def getReplies(status: Status, maxLength: Int = 140): Future[Seq[String]] = {
    log.info("Checking for sandwich requests")
    val text = TwitterRegex.stripLeadMention(status.getText)
    val replies: Seq[String] = Seq(text) collect {
      case MakeSandwichRE(sudo) => {
        Option(sudo) match {
          case Some(_) => "Okay."
          case None => "What? Make it yourself."
        }
      }
    }
    Future(replies.filter(_.length <= maxLength))
  }
}

/** An actor that responds to a tweet if it can be replied 
* by "Thats what she said". This is based on the work done by 
*Chloe Kiddon and Yuriy Brun , University of Washington
*That's What She Said: Double Entendre Identification
* Dataset from Edwin Chen
*/

class TWSSReplier extends BaseReplier {
  import scala.concurrent.Future
  import context.dispatcher
  import de.bwaldvogel.liblinear._; 
  import scala.collection.mutable.ArrayBuffer
  import tshrdlu.util.{TWSSModel, English,SimpleTokenizer}


  val vocabulary = English.vocabularyTWSS.map(line => line.split(" ")(0)).toIndexedSeq
  val IDFMap:Map[String,Int] = English.vocabularyTWSS.map { line=>
    val tokens = line.split(" ");
    (tokens(0),tokens(1).toInt)
  }.toMap

  def getReplies(status: Status , maxLength:Int = 140): Future[Seq[String]] ={
    log.info("Checking if tweet can be responded with TWSS")
    val tweet = TwitterRegex.stripLeadMention(status.getText.toLowerCase)
    val tweetMap:Map[String,Int] = SimpleTokenizer(tweet)
    .groupBy(x=> x)
    .mapValues(x=> x.length)

    val twssModel = TWSSModel()
    val featureVector = getFeatureVector(tweetMap)
    val prob = Array(0.0,0.0);
    Linear.predictProbability(twssModel, getFeatureVector(tweetMap).toArray,prob);
   // println(prob.toList);
    val response = if(prob.toList(0) > 0.9 ) "Thats what she said !! :-P " else "Thats was exactly what I told him !! "
    Future(Seq(response));
  }
  def getFeatureVector(document:Map[String,Int]): ArrayBuffer[Feature] ={
    val feature = new ArrayBuffer[Feature](vocabulary.size);
    var index=1;

    vocabulary.foreach{ word=>
      
      if(document.contains(word))
      {
      val tf = document(word);
      val idf = Math.log(7887/IDFMap(word));
      val tf_idf = tf*idf;
      val featureNode:Feature = new FeatureNode(index,tf_idf);
      feature += featureNode
      }
      index +=1
    }
    feature
  }
}
*/
