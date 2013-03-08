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
    if(!(text.contains("?")||text.startsWith("Who")||text.startsWith("Where")||text.startsWith("Why")||text.startsWith("How")||text.startsWith("When")||text.startsWith("Which")))
    return None
    val newtext = text.toLowerCase    
        try {
          val StripLeadMentionRE(withoutMention) = newtext
          val bigram = Tokenize(withoutMention)
          .sliding(2)
          .flatMap{case Vector(x,y) => List(x+" "+y)}
          //.filterNot(z => (stopwords.contains(z(0))||stopwords.contains(z(1))))
          .toList
          .sortBy(_.length)
         // bigram.foreach(println)
          //println(bigram.takeRight(3))
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

      //sortedMap foreach {case (key, value) => println (key + "-->" + value)}
      //println(mostRelevantTweet)
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

