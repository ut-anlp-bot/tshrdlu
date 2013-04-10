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
      val candidatesFuture = getReplies(status, 125-replyName.length)
      candidatesFuture.map { candidates =>
        val reply = "@" + replyName + candidates.toSet.head
        log.info("Candidate reply: " + reply)
        new StatusUpdate(reply).inReplyToStatusId(status.getId)
      } pipeTo sender
  }

  def getReplies(status: Status, maxLength: Int): Future[Seq[String]]

}

//class GeoLocation(lat:double, lon:double){

//}

class GeoReplier extends BaseReplier {
  import Bot._
  import TwitterRegex._
  import tshrdlu.util.SimpleTokenizer
  import twitter4j._
  import scala.util.Random

  import java.util.{Timer,TimerTask}
  import java.io.{File, FileWriter,PrintWriter}
  import context.dispatcher
  import akka.pattern.ask
  import akka.util._
  import scala.concurrent.duration._
  import scala.concurrent.Future
  implicit val timeout = Timeout(10 seconds)
  
  /**
   * Produce a reply to a status using geography
   */
  lazy val stopwords = tshrdlu.util.English.stopwords_bot
  def getReplies(status: Status, maxLength: Int = 140): Future[Seq[String]] = {
    log.info("Trying to reply through GeoReplier")

    val text = stripLeadMention(status.getText).toLowerCase
    val tweetBuffer = collection.mutable.ListBuffer[String]()
    var numTweetsSeen = 0
    def simpleStatusListener = new StatusListener() {
    def onStatus(status: Status) { 
      tweetBuffer += status.getText
      numTweetsSeen += 1 
    }
    def onDeletionNotice(statusDeletionNotice: StatusDeletionNotice) {}
    def onTrackLimitationNotice(numberOfLimitedStatuses: Int) {}
    def onException(ex: Exception) { ex.printStackTrace }
    def onScrubGeo(arg0: Long, arg1: Long) {}
    def onStallWarning(warning: StallWarning) {}
    }

    val latitude = 30.26715
    val longitude = -97.74306
    val twitter = new TwitterFactory().getInstance
    val twitterStream = new TwitterStreamFactory().getInstance
    val locList = twitter.getClosestTrends(new GeoLocation(latitude,longitude))
    val loctrend = twitter.getPlaceTrends(locList.get(0).getWoeid).getTrends
    var trendList = List[String]()
    for(i <- 0 until loctrend.size){
          trendList = loctrend(i).getName :: trendList
    }
    val filterTrends = trendList.filterNot(x => x.startsWith("#"))
    val trending = Random.shuffle(filterTrends).head
    val trend = Array(trending)
    twitterStream.addListener(simpleStatusListener)
    val filterQuery = new FilterQuery()
    filterQuery.locations(Array(Array(longitude-0.05, latitude-0.05), Array(longitude+0.05, latitude+0.05)))
    filterQuery.track(trend)
    twitterStream.filter(filterQuery) 
    
    while(numTweetsSeen < 20){
      println(numTweetsSeen)
      ;
    }
    println(trending)
    twitterStream.cleanUp
    twitterStream.shutdown
    
    val trainSet = tweetBuffer.toSeq.map {
      case StripMentionsRE(rest) => rest
      case x => x
    }
    .filterNot(_.contains('/'))
    .filterNot(_.contains('@'))
    .filter(tshrdlu.util.English.isSafe) 
    .filter(tshrdlu.util.English.isEnglish).flatMap(x => Tokenize("^ "+x))
    val filterTrain = trainSet.sliding(2).flatMap{case List(p,q) => List(p+" "+q)}

  val markovMap = filterTrain.toList.groupBy(x=>x).mapValues(x=>x.length).toMap
   val normalizedMap = markovMap.map{ case (t,u) => 
    val wordList = t.split("\\s+").toList
    val wordKey = markovMap.filterKeys(p => p.startsWith(wordList(0)+" "))
    val wordCount = wordKey.foldLeft(0)( (acc, kv) => acc + kv._2)
    (t -> u/wordCount.toDouble)
   }
    val normalizedNew = scala.io.Source.fromFile("src/main/resources/lang/eng/lexicon/20news_markov.txt").getLines
    .flatMap(t => t.split("\t") match {
      case Array(str1, str2) => Map(str1 -> str2.toDouble)
  } ) toMap

    val normalizedNews = normalizedNew.toMap

    val combList = normalizedMap.toList ++ normalizedNews.toList
    val compreMap = combList.groupBy ( _._1) .map { case (k,v) => k -> v.map(_._2).sum }

    val trendSymbol = trending.split("\\s+").toList
    var startSym = trendSymbol.last.toLowerCase
    var endSym = trendSymbol.head.toLowerCase
    var symbol = startSym 

    var newSentence = trending
    var genSentence = ""
    var flag1 = false
    var flag2 = false
    var fwd = true

    while(newSentence.length+symbol.length <=140 && startSym!="." && endSym != "^"){
      if(fwd == true){
        fwd = false
        val wordKey = compreMap.filterKeys(p => p.startsWith(startSym+" "))
        val wordCount = wordKey.toSeq.sortBy(-_._2)
        val word = wordCount(0)._1.split("\\s+").toList
         if(flag1 == false)
          flag1 = true
        else
          newSentence = newSentence+" "+word(0)
        startSym = word(1)
        symbol = startSym
        println(newSentence)
      }
      else{
        fwd = true
        val wordKey = compreMap.filterKeys(p => p.endsWith(" "+endSym))
        val wordCount = wordKey.toSeq.sortBy(-_._2)
        val word = wordCount(0)._1.split("\\s+").toList
        if(flag2 == false)
          flag2 = true
        else
          newSentence = word(1)+" "+newSentence
      endSym =  word(0)
      symbol = endSym
      println(newSentence)
      }
    
    }

   Future{Seq(newSentence)}
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
