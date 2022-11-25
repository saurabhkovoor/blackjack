package model
import scalafx.beans.property.{BooleanProperty, StringProperty}
import scala.collection.mutable.ListBuffer
import scala.quoted.FromExpr.NoneFromExpr

//might not need ishost attribute
class BlackJackGame(val isHost: Boolean, val tableList: ListBuffer[User], val currentPlayerUser: User) {
//   var gameStatus: StringProperty = new StringProperty()
  var allPlayersConfirmedBet: BooleanProperty = BooleanProperty(false)
  var endOfRound: BooleanProperty = BooleanProperty(false)
  val maxNoOfCardsInHand: Int = 5
  var usersToAct = new ListBuffer[User]
  for (user <- tableList){ 
    usersToAct += user
    }

  val currentPlayer: Option[User] = None
  val housedealer = new HouseDealer()
  val dummyCard = new Card("", "")

  // Initialise player positions
  var noOfUsers: Int = tableList.size
  var playerL: Option[Player] = None
  var playerM: Option[Player] = None
  var playerR: Option[Player] = None

  def roundStart(): Unit = {
    endOfRound.value = false
    // println(s"game.roundStart: roomList: ${roomList}")
    // for all the players in the table empty the cards from their hand
    for (user <- tableList) {
      getPlayer(user).get.emptyCardsinHand()
    }
  }

  def setPlayerPosition(): Unit = {
    val localPlayersList = new ListBuffer[User]()
    for (u <- roomList) {
        localPlayersList += user
    }

    //Middle Player
    localPlayersList = localPlayersList - localPlayer
    mPlayer = Option(new Player(localPlayer))

    //Left Player
    if (localPlayersList.nonEmpty) {
      lPlayer = Option(new Player(localPlayersList.remove(0)))
    }
    //Right Player
    if (localPlayersList.nonEmpty) {
      rPlayer = Option(new Player(localPlayersList.remove(0)))
    }
  }
  
  def returnPlayerSeatPosition(user: User): Int = {
    for (player <- mPlayer) {
      if (player.user == user) {
        return 2
      }
    }
    for (player <- lPlayer) {
      if (player.user == user) {
        return 1
      }
    }
    for (player <- rPlayer) {
      if (player.user == user) {
        return 3
      }
    }
    return 0
  }

  def returnPlayer(user: User): Option[Player] = {
    if (mPlayer.get.user == user) {
      return mPlayer
    } else if (lPlayer.get.user == user) {
      return lPlayer
    } else if (rPlayer.get.user == user) {
      return rPlayer
    } else {
      return null
    }
  }
  def playerConfirmBet(user: User): Unit = {
    getPlayer(user).get.betConfirmed = true

    // Check all player's bet confirmed status
    var allConfirmedBetCheck = true
    for (player <- roomList) {
      if (!getPlayer(player).get.betConfirmed) {
        allConfirmedBetCheck = false
      }
    }
    allPlayersConfirmedBet.value = allConfirmedBetCheck
  }
  def placeBet(user: User): Unit = {
    getPlayer(user).get.betPlaced = true

    // Check all player's bet confirmed status - p
    var allPlayersConfirmedBets = true
    for (player <- roomList) {
      if (!getPlayer(player).get.betConfirmed) {
        allConfirmedBetCheck = false
      }
    }
    allPlayersConfirmedBet.value = allConfirmedBetCheck
  }

}