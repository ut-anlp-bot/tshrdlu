package tshrdlu.twitter

import twitter4j._

/**
 * Follow all users of {@literal@}appliednlp with usernames ending in "_anlp".
 */
object FollowAnlp extends TwitterInstance with StreamInstance{

  def main(args: Array[String]) {
    val followers = twitter.getFollowersIDs("appliednlp",-1).getIDs.map{id => twitter.showUser(id)}.toSet
    val anlp_names = followers.map{x => x.getScreenName}.toSet.filter{x => (x.endsWith("_anlp") && x!=twitter.getScreenName)}
    anlp_names.foreach(twitter.createFriendship)
  }

}
