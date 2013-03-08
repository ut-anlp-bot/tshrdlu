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
 * Trait used by sub-bots capable of replying to status messages.
 */
trait StatusReplier {
  /**
   * Take a status message and optionally return a reply.
   *
   * @return <code>Some(reply)</code> to reply or <code>None</code> to pass
   */
  def attemptStatusReply(status: Status): Option[String]
}


/**
 * Trait used by sub-bots capable of replying to direct messages.
 */
trait DirectMessageReplier {
  /**
   * Take a direct message and optionally return a reply.
   *
   * @return <code>Some(reply)</code> to reply or <code>None</code> to pass
   */
  def attemptDirectMessageReply(directMessage: DirectMessage): Option[String]
}


/**
 * A bot that maintains a collection of Repliers (or "sub-bots") that are
 * capable of replying to tweets directed at the bot.
 */
class BotMaster extends TwitterInstance with StreamInstance
with StatusListenerAdaptor with UserStreamListenerAdaptor {

  val username = twitter.getScreenName

  // Listen to the user stream
  stream.addListener(this)
  stream.user

  private val atReplyStatusRepliers = collection.mutable.ListBuffer[StatusReplier]()
  private val directMessageRepliers = collection.mutable.ListBuffer[DirectMessageReplier]()

  /**
   * Add a replier capable of responding to {@literal@}reply tweets (those
   * beginning with "{@literal@}username") sent to the bot.
   */
  def addAtReplyStatusReplier(statusReplier: StatusReplier) {
    atReplyStatusRepliers += statusReplier
  }

  /**
   * Add a replier capable of responding to direct messages sent to the bot.
   */
  def addDirectMessageReplier(directMessageReplier: DirectMessageReplier) {
    directMessageRepliers += directMessageReplier
  }

  /**
   * Process an incoming status message. Upon receiving a status, determine if
   * it is an {@literal@}reply sent to the bot. If so, scan through the
   * collection of possible repliers until we find one willing to handle it.
   */
  override def onStatus(status: Status) {
    val replyName = status.getInReplyToScreenName
    if (replyName == username) {
      // Process a tweet directed at us
      val fromUsername = status.getUser.getScreenName
      println("Incoming @reply from @" + fromUsername + ": " + status.getText)

      atReplyStatusRepliers.foreach { replier =>
        replier.attemptStatusReply(status) match {
          case Some(reply) => {
            println("Using reply generated from " + replier + ": " + reply)
            val text = "@" + status.getUser.getScreenName + " " + reply
            println("Replying: " + text)
            val replyStatus = new StatusUpdate(text).inReplyToStatusId(status.getId)
            twitter.updateStatus(replyStatus)
            return
          }
          case None => {
            println(replier + " chose not to reply")
          }
        }
      }
    }
  }

  /**
   * Process an incoming direct message. Upon receiving a message, scan through
   * the collection of possible repliers until one is found that is willing to
   * handle it.
   */
  override def onDirectMessage(directMessage: DirectMessage) {
    val fromUsername = directMessage.getSenderScreenName
    if (fromUsername != username) {
      println("Incoming DM from @" + fromUsername + ": " + directMessage.getText)

      directMessageRepliers.foreach { replier =>
        replier.attemptDirectMessageReply(directMessage) match {
          case Some(reply) => {
            println("Using reply generated from " + replier + ": " + reply)
            println("Replying: " + reply)
            twitter.sendDirectMessage(directMessage.getSenderId(), reply)
            return
          }
          case _ => {
            println(replier + " chose not to reply")
          }
        }
      }
    }
  }
}


/**
 * Companion object for BotMaster with a main method.
 */
object BotMaster {

  def main(args: Array[String]) {
    val master = new BotMaster

    master.addDirectMessageReplier(new AliveDirectMessageReplier(master))

    val sudoReplier = new SudoReplier(master)
    master.addDirectMessageReplier(sudoReplier)
    master.addAtReplyStatusReplier(sudoReplier)

    val wisdomReplier = new WisdomReplier(master)
    master.addDirectMessageReplier(wisdomReplier)
    master.addAtReplyStatusReplier(wisdomReplier)

    val defaultReplier = new DefaultReplier(master)
    master.addDirectMessageReplier(defaultReplier)
    master.addAtReplyStatusReplier(defaultReplier)


   


  }

}
