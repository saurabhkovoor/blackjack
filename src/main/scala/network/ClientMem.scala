package network

import akka.actor.typed.ActorRef
import model.User
import scalafx.collections.ObservableHashSet

object ClientMem {
  var mute: Boolean = false
  var loadFXMLCount: Int = 0
  var isPlaying: Boolean = false

  var ownRef: Option[ActorRef[GameClient.Command]] = None
  var ownName: String = ""

  var tableList = new ObservableHashSet[User]()
  var hostRef: Option[ActorRef[GameClient.Command]] = None

  def isHost: Boolean = {
    if (ownRef == hostRef) {
      return true
    } else {
      return false
    }
  }

}
