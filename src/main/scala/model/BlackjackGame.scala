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

  def placeBet(user: User): Unit = {
    getPlayer(user).get.betPlaced = true

    // Check all player's bet confirmed status - p
    var allBetsPlaced = true
    for (p <- tableList) {
      if (!getPlayer(p).get.betConfirmed) {
        allBetsPlaced = false
      }
    }
  }
  def dealCards: ListBuffer[PokerCardHolderInfo] = {
    val PokerCardHolderInfoList = new ListBuffer[PokerCardHolderInfo]() //isPlayer, User, Card, cardPosition
    var tempCard = dummyCard

    // First card for players
    for (u <- roomList) {
      tempCard = housedealer.takeCard
      getPlayer(u).get.addCardToHand(tempCard)
      PokerCardHolderInfoList += PokerCardHolderInfo(isPlayer = true, user, tempCard, 1)
    }
    // First card for dealer, hidden
    tempCard = dealer.getCard
    dealer.addToHand(tempCard)
    PokerCardHolderInfoList += PokerCardHolderInfo(isPlayer = false, localPlayer, dummyCard, 1)

    // Second card for players
    for (user <- roomList) {
      tempCard = dealer.getCard
      getPlayer(user).get.addToHand(tempCard)
      PokerCardHolderInfoList += PokerCardHolderInfo(isPlayer = true, user, tempCard, 2)
    }
    // Second card for dealer, visible
    tempCard = dealer.getCard
    dealer.addToHand(tempCard)
    PokerCardHolderInfoList += PokerCardHolderInfo(isPlayer = false, localPlayer, tempCard, 2)

    return PokerCardHolderInfoList
  }
  
}