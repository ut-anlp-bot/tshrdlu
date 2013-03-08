package tshrdlu.twitter

/**
 * Copyright 2013 Karthik Padmanabhan, Nazneen Rajani, and Nick Wilson
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

import twitter4j._
import scala.io.Source
import collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer

/**
 * A direct message replier that only responds to "are you alive?".
 */
class AliveDirectMessageReplier(master: BotMaster) extends DirectMessageReplier {
  def attemptDirectMessageReply(directMessage: DirectMessage): Option[String] = {
    if (directMessage.getText.matches("""(?i).*\bare you alive\?.*""")) Some("yes") else None
  }
}


/**
 * A default replier when the bot has nothing interesting to say.
 */
class DefaultReplier(master: BotMaster)
extends StatusReplier with DirectMessageReplier {

  lazy val defaultReplies = Vector(
    "That does not interest me",
    "I really don't care about that",
    "Can we talk about something else?",
    "Let's talk about something else")

  def attemptStatusReply(status: Status): Option[String] = {
    return getReply(status.getText)
  }

  def attemptDirectMessageReply(directMessage: DirectMessage): Option[String] = {
    return getReply(directMessage.getText)
  }

  private def getReply(text: String): Option[String] = {
    return Some(defaultReplies(scala.util.Random.nextInt(defaultReplies.length)))
  }

}


/**
 * A replier for status and direct messages. Responds to requests to make
 * sandwiches.
 *
 * @see <a href="http://xkcd.com/149/">http://xkcd.com/149/</a>
 */
class SudoReplier(master: BotMaster)
extends StatusReplier with DirectMessageReplier {

  lazy val MakeSandwichRE = """(?i)(?:.*(\bsudo\b))?.*\bmake (?:me )?an?\b.*\bsandwich\b.*""".r

  def attemptStatusReply(status: Status): Option[String] = {
    return getReply(status.getText)
  }

  def attemptDirectMessageReply(directMessage: DirectMessage): Option[String] = {
    return getReply(directMessage.getText)
  }

  private def getReply(text: String): Option[String] = {
    val reply = text match {
      case MakeSandwichRE(sudo) => {
        if (sudo == null) Some("What? Make it yourself.") else Some("Okay.")
      }
      case _ => None
    }

    return reply
  }
}

/** A replier that uses the tweets from Users who have tweeted "quotes" about various topics".
  * The idea is that quotes are very much addressed at humans and makes it sound more human
  */

  class WisdomReplier(master:BotMaster)
  extends StatusReplier with DirectMessageReplier 
  {

    import tshrdlu.util.SimpleTokenizer
    import collection.JavaConversions._

    lazy val stopwords = Source.fromFile("src/main/resources/lang/eng/lexicon/stopwordsWisdom").getLines().toSet


  // Pull just the lead mention from a tweet.
    lazy val StripLeadMentionRE = """(?:)^@[a-z_0-9]+\s(.*)$""".r

  // Pull the RT and mentions from the front of a tweet.
    lazy val StripMentionsRE = """(?:)(?:RT\s)?(?:(?:@[a-z]+\s))+(.*)$""".r

    // this will initialize the userList by searching for users with the name "quote"

    val userList = userSearch("quote",master.twitter)

    /*Initliaze the userList
     * The number of users is hardcoded for now, to 20.
     */
  def userSearch(searchWith:String, twitter:Twitter) :ArrayBuffer[String] = 
  {
    var userList = ArrayBuffer[(String,Int)]()
    println("Initializing Wisdom Replier");
    for(page<- 1 to 10)
    { 
      val temp = master.twitter.searchUsers(searchWith,page);
      userList = userList ++ temp.map(x=> (x.getScreenName,x.getStatusesCount))
    }

    println("Bot ready");

    val a = userList.map(x=> x._1).slice(0,15)
    println(a);
    a

  }


  def attemptStatusReply(status:Status) :Option[String] =
  {
    if(Math.random < 0.7) None
    else
    return getReply(status.getText)
  }

  def attemptDirectMessageReply(directMessage:DirectMessage) : Option[String] = 
  {
    return getReply(directMessage.getText)
  }

  // Generates the reply that has to be sent out
  private def getReply(s:String) : Option[String] = 
  {
    val text = s.toLowerCase

    
      try {
        val StripLeadMentionRE(withoutMention) = text
        val statusList = 
        SimpleTokenizer(withoutMention)
        .filter(_.length > 3)
        .filter(_.length < 10)
        .filterNot(_.contains('/'))
        .filter(tshrdlu.util.English.isSafe)
        .sortBy(- _.length)
        .toSet
        .take(3)
        .toList//.flatMap(w => twitter.search(new Query(w)).getTweets)
        // the one above is a list of tokens
        
        //Get the tweets from the users mentioned in userList.This will return only a few tweets , per user
        // the Limit is around 200 / user and need to figure out how to set it to the maximum

        val candidateTweets = userList.map(x=> master.twitter.getUserTimeline(x,new Paging(1,20))).flatten;
        
        println("candidate Tweets size is " + candidateTweets.length);
        
        val candidateTweetsText = candidateTweets.map(x=> x.getText).toSeq
        val reply = generateReply(candidateTweetsText,statusList)
	
	if(reply.equals("#@"))
		None
	else
		Some(reply)

      }
      catch { 
        case  e : Throwable => println(e);None
      }
    }
    
  

    // take a List of tweets , and return the one that is most relevanr
    // Here relevance is just based on matching a word from the original tweet
    // to the tweets to be searched. In the future need to implement other things
    // Bigram , or other similarity measure between tweets can be used

  def generateReply(statusList:Seq[String],tweet : Seq[String]) = {
    val processedCandidates = statusList.map(x=> normalize(x))

    val filterCandidates:Seq[String] = processedCandidates.map {
      case StripMentionsRE(rest) => rest
      case x => x
    }
    .filterNot(_.contains('/'))
    .filterNot(_.contains('@'))
    .filter(tshrdlu.util.English.isSafe) 
    .filter(tshrdlu.util.English.isEnglish)

    val potentialResponse= filterCandidates.map(x=> (x,score(x,tweet)) ).sortBy(x=>x._2).reverse(0)
    if(potentialResponse._2 == 0) "#@" else potentialResponse._1;

  }


  // Score is just based on how many words in the original tweet matches the candidates
  // It does not match stopwords , as they never really give any information regarding
  // the content of the tweet

  def score(x: String , tweet:Seq[String])=
  {
    val candidate = SimpleTokenizer(x).filter(x=> stopwords.contains(x) == false).toSet
    val score = tweet.map(x=> if(candidate.contains(x)) 1 else 0 ).sum
    println("size is  " + candidate.size + "and score is " + score);
    score
  }

  // Just extract the content part of the quote , omit 
  // things like who said and etc.
  def normalize(sentence:String):String  = {
    val normalizedSentence = if(sentence.startsWith("\"")) (sentence.split("\""))(1) 
    else if(sentence.startsWith("\'")) (sentence.split("\'"))(1)
    else (sentence.split("-"))(0)
    normalizedSentence

  }

}
class BigramImplementation(master: BotMaster)
extends StatusReplier with DirectMessageReplier {

    import tshrdlu.util.SimpleTokenizer
    import collection.JavaConversions._

    val username = master.twitter.getScreenName

    lazy val stopwords = tshrdlu.util.English.stopwords_bot
    // Recognize a follow command
    lazy val FollowRE = """(?i)(?<=follow)(\s+(me|@[a-z_0-9]+))+""".r

    // Pull just the lead mention from a tweet.
    lazy val StripLeadMentionRE = """(?:)^@[a-z_0-9]+\s(.*)$""".r

    // Pull the RT and mentions from the front of a tweet.
    lazy val StripMentionsRE = """(?:)(?:RT\s)?(?:(?:@[a-z]+\s))+(.*)$""".r 
    def attemptStatusReply(status: Status): Option[String] = {
    return getReply(status.getText)
   }  

   def attemptDirectMessageReply(directMessage: DirectMessage): Option[String] = {
    return getReply(directMessage.getText)
  }

   private def getReply(text: String): Option[String] = {
//    if(!(text.contains("?")||text.startsWith("Who")||text.startsWith("Where")||text.startsWith("Why")||text.startsWith("How")||text.startsWith("When")||text.startsWith("Which")))
  //  return None
    val newtext = text.toLowerCase    
        try {
          val StripLeadMentionRE(withoutMention) = newtext
          val bigram = Tokenize(withoutMention)
          .sliding(2)
          .flatMap{case Vector(x,y) => List(x+" "+y)}
          .filterNot(z => (stopwords.contains(z(0))||stopwords.contains(z(1))))
          .toList
          .sortBy(_.length)
          val bigramsearch= bigram
              .takeRight(3)
              .toList
              .flatMap(w => master.twitter.search(new Query("\""+w+"\"")).getTweets)
        
              val reply = extractText(bigramsearch,bigram.toList)
              Some(reply)
              } catch { 
                  case _: Throwable => val reply = None
                  None
        }
  }
    def extractText(statusList: List[Status],bigram:List[String]):String = {
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
      if (useableTweets.isEmpty) "NO." else
        { 
        val bigramMap = useableTweets.flatMap{x =>Tokenize(x)
          .sliding(2)
          .filterNot(z => (stopwords.contains(z(0))||stopwords.contains(z(1))))
          .map(bg => bg.mkString(" ") -> x)} toMap
        val sortedMap = bigramMap.filter{case (x,y) => bigram.contains(x)}
        val mostRelevantTweet = sortedMap
        .groupBy(_._2)
        .maxBy(_._2.size)
        val found = for {
            rb <- bigram
            sentence <- bigramMap get rb
            } yield sentence

      if (!mostRelevantTweet._2.isEmpty)
          mostRelevantTweet._1
          else
            found.headOption.get
      }
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

