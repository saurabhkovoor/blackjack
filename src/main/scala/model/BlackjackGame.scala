package model
import scalafx.beans.property.BooleanProperty

import scala.collection.mutable.ListBuffer

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

  var currentPlayer: Option[User] = None
  val housedealer = new HouseDealer()
  val dummyCard = new PokerCard("", "")

  // Initialise player positions
  var noOfUsers: Int = tableList.size
  var player1: Option[Player] = None
  var player2: Option[Player] = None
  var player3: Option[Player] = None
  var player4: Option[Player] = None

  def roundStart(): Unit = {
    endOfRound.value = false
    // println(s"game.roundStart: roomList: ${roomList}")
    // for all the players in the table empty the cards from their hand
    for (user <- tableList) {
      returnPlayer(user).get.emptyCardsinHand()
    }
  }

  def setPlayerPosition(): Unit = {
    val localPlayersList = new ListBuffer[User]()
    for (u <- tableList) {
        localPlayersList += u
    }

    //Middle Player
    localPlayersList -= currentPlayerUser
    player2 = Option(new Player(currentPlayerUser))

    //Left Player
    if (localPlayersList.nonEmpty) {
      player1 = Option(new Player(localPlayersList.remove(0)))
    }
    //Right Player
    if (localPlayersList.nonEmpty) {
      player3 = Option(new Player(localPlayersList.remove(0)))
    }
    if (localPlayersList.nonEmpty) {
      player4 = Option(new Player(localPlayersList.remove(0)))
    }
  }

  
  def returnPlayerSeatPosition(user: User): Int = {
    for (player <- player1) {
      if (player.user == user) {
        return 1
      }
    }
    for (player <- player2) {
      if (player.user == user) {
        return 2
      }
    }
    for (player <- player3) {
      if (player.user == user) {
        return 3
      }
    }
    for (player <- player4) {
      if (player.user == user) {
        return 4
      }
    }
    return 0
  }

  def returnPlayer(user: User): Option[Player] = {
    if (player1.get.user == user) {
      return player1
    } else if (player2.get.user == user) {
      return player2
    } else if (player3.get.user == user) {
      return player3
    } else if (player4.get.user == user) {
      return player4
    }
    else {
      return null
    }
  }

  // -------------------------
  def playerConfirmBet(user: User): Unit = {
    returnPlayer(user).get.betPlaced = true

    // Check all player's bet confirmed status
    var allConfirmedBetCheck = true
    for (player <- tableList) {
      if (!returnPlayer(player).get.betPlaced) {
        allConfirmedBetCheck = false
      }
    }
    allPlayersConfirmedBet.value = allConfirmedBetCheck
  }

  def placeBet(user: User): Unit = {
    returnPlayer(user).get.betPlaced = true

    // Check all player's bet confirmed status - p
    var allBetsPlaced = true
    for (p <- tableList) {
      if (!returnPlayer(p).get.betPlaced) {
        allBetsPlaced = false
      }
    }
  }
  def dealCards: ListBuffer[PokerCardHolderInfo] = {
    val PokerCardHolderInfoList = new ListBuffer[PokerCardHolderInfo]() //isPlayer, User, Card, cardPosition
    var tempCard = dummyCard

    // First card for players
    for (u <- tableList) {
      tempCard = housedealer.takeCard
      returnPlayer(u).get.addCardToHand(tempCard)
      PokerCardHolderInfoList += new PokerCardHolderInfo(inPlayerHand = true, u, tempCard, 1)
    }
    // First card for dealer, hidden
    tempCard = housedealer.takeCard
    housedealer.addCardToHand(tempCard)
    PokerCardHolderInfoList += new PokerCardHolderInfo(inPlayerHand = false, currentPlayerUser, dummyCard, 1)

    // Second card for players
    for (user <- tableList) {
      tempCard = housedealer.takeCard
      returnPlayer(user).get.addCardToHand(tempCard)
      PokerCardHolderInfoList += new PokerCardHolderInfo(inPlayerHand = true, user, tempCard, 2)
    }
    // Second card for dealer, visible
    tempCard = housedealer.takeCard
    housedealer.addCardToHand(tempCard)
    PokerCardHolderInfoList += new PokerCardHolderInfo(inPlayerHand = false, currentPlayerUser, tempCard, 2)

    return PokerCardHolderInfoList
  }

  def getHandValue(hand: ListBuffer[PokerCard]): Int = {
    var aceCount = 0
    var totalWithoutAces = 0
    val dict = Map("2" -> 2, "3" -> 3, "4" -> 4, "5" -> 5, "6" -> 6, "7" -> 7, "8" -> 8, "9" -> 9, "10" -> 10, "jack" -> 10, "queen" -> 10, "king" -> 10)

    for (card <- hand) {
      val rank = card.value
      if (rank == "ace") {
        aceCount += 1
      } else {
        totalWithoutAces += dict(rank)
      }
    }

    // Return directly for fixed cases
    if (aceCount == 0) {
      return totalWithoutAces
    } else if (aceCount == 2 && hand.size == 2) {
      return 21
    }

    // Counting all possible combinations of aces
    val smallAce = 1
    var bigAce = 11
    if (hand.size > 2) {
      bigAce = 10
    }
    val aceCombination = new ListBuffer[Int]
    for (i <- 0 to aceCount) {
      val tmp = totalWithoutAces + ((i * smallAce) + (aceCount - i) * bigAce)
      if (tmp <= 21) {
        aceCombination += tmp
      }
    }

    if (aceCombination.isEmpty) {
      return totalWithoutAces + aceCount
    } else {
      return aceCombination.max
    }
  }

  def getCurrentUserTurn: Option[User] = {
    if (currentPlayer.isDefined) {
      return currentPlayer
    } else if (usersToAct.nonEmpty) {
      currentPlayer = Option(usersToAct.remove(0))
    }
    return currentPlayer
  }

  def playerHit(user: User): PokerCardHolderInfo = {
    val tempCard = housedealer.takeCard
    val currentPlayer = returnPlayer(user).get
    currentPlayer.addCardToHand(tempCard)

    // If player's hand is full or above 21, no more actions
    if (currentPlayer.cardsInHand.size == maxNoOfCardsInHand || getHandValue(currentPlayer.cardsInHand) >= 21) {
      currentPlayerUser
    }
    return new PokerCardHolderInfo(inPlayerHand = true, user, tempCard, currentPlayer.cardsInHand.size)
  }

  def playerStand(user: User): Unit = {
    currentPlayerUser
  }

  def playerIncreaseBet(user: User): Int = {
    val currentUser = returnPlayer(user).get
    currentUser.increaseBetPlaced(50)
    return currentUser.minBet
  }

  def playerDecreaseBet(user: User): Int = {
    val currentUser = returnPlayer(user).get
    currentUser.increaseBetPlaced(-50)
    return currentUser.minBet
  }

  // Host clear everyone's hand, generate new usersWithAction, reset bet confirmed
  def prepareNewRound(): Unit = {
    usersToAct.clear()
    for (user <- tableList) {
      returnPlayer(user).get.betPlaced = false
      usersToAct += user
    }
    if (usersToAct.nonEmpty) {
      currentPlayer = Option(usersToAct.remove(0))
    }
    housedealer.shuffleDeck()
    allPlayersConfirmedBet.value = false
  }

  def getDealerHand: ListBuffer[PokerCardHolderInfo] = {
    // Dealer draw card if hand value below 15
    while (getHandValue(housedealer.cardsInHand) < 15) {
      housedealer.addCardToHand(housedealer.takeCard)
    }
    // Reveal dealer's cards
    val cardMetaList = new ListBuffer[PokerCardHolderInfo]
    for (card <- housedealer.cardsInHand) {
      cardMetaList += new PokerCardHolderInfo(inPlayerHand = false, currentPlayerUser, card, (cardMetaList.size + 1))
    }
    endOfRound.value = true
    return cardMetaList
  }

  def isHouseWin: Boolean = {
    val dealerHandValue = getHandValue(housedealer.cardsInHand)
    var bestPlayerHandValue = 0
    for (user <- tableList) {
      val playerHandValue = getHandValue(returnPlayer(user).get.cardsInHand)
      if (playerHandValue <= 21 && playerHandValue > bestPlayerHandValue) {
        bestPlayerHandValue = playerHandValue
      }
    }

    if (dealerHandValue <= 21 && dealerHandValue > bestPlayerHandValue) {
      return true
    } else {
      return false
    }
  }

  def getPlayerResult(user: User): String = {
    val currentUser = returnPlayer(user).get
    val currentUserHandValue = getHandValue(currentUser.cardsInHand)
    val dealerHandValue = getHandValue(housedealer.cardsInHand)

    if (currentUserHandValue <= 21 && currentUserHandValue > dealerHandValue) {
      adjustPlayerBalance(currentUser, isPlayerWin = true)
      return "You win!"
    } else if (currentUserHandValue > 21 && dealerHandValue > 21) {
      return "Both bust, it's a tie!"
    } else if (currentUserHandValue == dealerHandValue) {
      return "It's a tie!"
    } else if (currentUserHandValue > 21 && dealerHandValue <= 21) {
      adjustPlayerBalance(currentUser, isPlayerWin = false)
      return "You lose!"
    } else if (dealerHandValue > 21 && currentUserHandValue <= 21) {
      adjustPlayerBalance(currentUser, isPlayerWin = true)
      return "You win!"
    } else if (currentUserHandValue < dealerHandValue) {
      adjustPlayerBalance(currentUser, isPlayerWin = false)
      return "You lose!"
    } else {
      return "An unexpected error occurred."
    }
  }

  def adjustPlayerBalance(player: Player, isPlayerWin: Boolean): Unit = {
    if (isPlayerWin) {
      player.chipAmount += player.minBet
    } else {
      player.chipAmount -= player.minBet
      player.increaseBetPlaced(0)
    }
  }
  
}