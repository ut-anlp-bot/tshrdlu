package tshrdlu.twitter

import twitter4j._

object FollowAnlp extends TwitterInstance with StreamInstance{

  def main(args: Array[String]) {
    val followerIds = twitter.getFollowersIDs("appliednlp",-1).getIDs
    val anlp = followerIds.map{id => twitter.showUser(id)}.toSet
    val names = anlp.map{x => x.getScreenName}.toSet
    val anlp_names = names.filter{x => (x.endsWith("_anlp") && x!=twitter.getScreenName)}
    println(anlp_names)
    anlp_names.foreach(twitter.createFriendship)
  }

}
