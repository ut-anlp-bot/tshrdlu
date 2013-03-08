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
 * Follow all users of {@literal@}appliednlp with usernames ending in "_anlp".
 */
object FollowAnlp extends TwitterInstance with StreamInstance{

  def main(args: Array[String]) {
    val followers = twitter.getFollowersIDs("appliednlp",-1).getIDs.map{id => twitter.showUser(id)}.toSet
    val anlp_names = followers.map{x => x.getScreenName}.toSet.filter{x => (x.endsWith("_anlp") && x!=twitter.getScreenName)}
    anlp_names.foreach(twitter.createFriendship)
  }

}
