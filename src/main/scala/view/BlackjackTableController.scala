package view

import model.{BlackJackGame, PokerCardHolderInfo, User}
import network.{ClientMem, GameClient, Lobby, MainApp}
import scalafx.animation.AnimationTimer
import scalafx.event.ActionEvent
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.{Alert, Button, ButtonType}
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.media.{Media, MediaPlayer}
import scalafx.scene.text.Text
import scalafxml.core.macros.sfxml

import scala.collection.mutable.ListBuffer
@sfxml
class BlackjackTableController(private var player1Name: Text,
                      private var player2Name: Text,
                      private var player3Name: Text,
                      private var player4Name: Text,
                      private var player1BetLabel: Text,
                      private var player1BalLabel: Text,
                      private var player2BetLabel: Text,
                      private var player2BalLabel: Text,
                      private var player3BetLabel: Text,
                      private var player3BalLabel: Text,
                      private var player4BetLabel: Text,
                      private var player4BalLabel: Text,
                      private var player1card1: ImageView,
                      private var player1card2: ImageView,
                      private var player1card3: ImageView,
                      private var player1card4: ImageView,
                      private var player1card5: ImageView,
                      private var player2card1: ImageView,
                      private var player2card2: ImageView,
                      private var player2card3: ImageView,
                      private var player2card4: ImageView,
                      private var player2card5: ImageView,
                      private var player3card1: ImageView,
                      private var player3card2: ImageView,
                      private var player3card3: ImageView,
                      private var player3card4: ImageView,
                      private var player3card5: ImageView,
                      private var player4card1: ImageView,
                      private var player4card2: ImageView,
                      private var player4card3: ImageView,
                      private var player4card4: ImageView,
                      private var player4card5: ImageView,
                      private var dealercard1: ImageView,
                      private var dealercard2: ImageView,
                      private var dealercard3: ImageView,
                      private var dealercard4: ImageView,
                      private var dealercard5: ImageView,
                      private var statusText: Text,
                      private var hitBtn: Button,
                      private var standBtn: Button,
                      private var increaseBetBtn: Button,
                      private var decreaseBetBtn: Button,
                      private var confirmBetBtn: Button,
                      private var player1Bet: Text,
                      private var player2Bet: Text,
                      private var player3Bet: Text,
                      private var player4Bet: Text,
                      private var player1Bal: Text,
                      private var player2Bal: Text,
                      private var player3Bal: Text,
                      private var player4Bal: Text,
                      private var nextRoundBtn: Button,
                      private var playerConfetti: ImageView,
                      private var muteBtn: Button
                     ) {
  // Initialise board state
  val blankImg = new Image(getClass.getResourceAsStream("/blackjack/img/blank.png"))
  clearBoard()
  nextRoundBtn.disable = true
  nextRoundBtn.visible = false
  player2Name.text = ClientMem.ownName

  if (!ClientMem.mute) {
    muteBtn.setStyle("-fx-background-color: #ccdb30;")
  } else {
    muteBtn.setStyle("-fx-background-color: #ff6666;")
  }

  val media = new Media(getClass.getResource(s"/blackjack/sound/CasinoAmbience.mp3").toURI.toString)
  val player = new MediaPlayer(media)
  player.setCycleCount(MediaPlayer.Indefinite)
  if (!ClientMem.mute) player.play()

  private val localPlayer = User(ClientMem.ownName, MainApp.ownRef)
  var game = new BlackJackGame(ClientMem.isHost, ClientMem.tableList.to[ListBuffer], localPlayer)

  // Initialise additional players
  if (game.player1.isDefined) {
    player1Name.text = game.player1.get.user.name
  } else {
    player1BetLabel.setVisible(false)
    player1BalLabel.setVisible(false)
    player1Bet.setVisible(false)
    player1Bal.setVisible(false)
  }
  if (game.player3.isDefined) {
    player3Name.text = game.player3.get.user.name
  } else {
    player3BetLabel.setVisible(false)
    player3BalLabel.setVisible(false)
    player3Bet.setVisible(false)
    player3Bal.setVisible(false)
  }
  if (game.player4.isDefined) {
    player3Name.text = game.player4.get.user.name
  } else {
    player4BetLabel.setVisible(false)
    player4BalLabel.setVisible(false)
    player4Bet.setVisible(false)
    player4Bal.setVisible(false)
  }

  // Host giving cards animation
  var cardMetaList = new ListBuffer[PokerCardHolderInfo]
  var time = 0L
  val timerCard: AnimationTimer = AnimationTimer(t => {
    if (cardMetaList.isEmpty) {
      timerCard.stop
      if (game.getCurrentUserTurn.isDefined && game.allPlayersConfirmedBet.value) {
        announceTurn(game.getCurrentUserTurn.get)
      }
    }
    if ((t - time) > 0.333e9) {
      val currentCard = cardMetaList.remove(0)
      for (user <- ClientMem.tableList) {
        ClientMem.ownRef.foreach(_ ! GameClient.GiveCard(currentCard, user))
      }
      time = t
    }
  })

  // Host starts first round
  if (ClientMem.isHost) {
    game.roundStart()
    roundStart()
    nextRoundBtn.visible = true
  }

  // Host starts distributing cards after all players confirmed bet
  game.allPlayersConfirmedBet.onChange((_, old, newV) => {
    if (game.allPlayersConfirmedBet.value && ClientMem.isHost) {
      cardMetaList = game.dealCards
      timerCard.start
    }
  })

  // Host wait after each action before calling the next user's turn
  val timerTurn: AnimationTimer = AnimationTimer(t => {
    if (time == 0L) {
      time = t
    }
    if ((t - time) > 0.5e9) {
      //Get next player's turn
      val nextPlayer = game.getCurrentUserTurn
      if (nextPlayer.isDefined) {
        announceTurn(nextPlayer.get)
      } else {
        roundEnd()
      }
      timerTurn.stop
    }
  })

  def mute(action: ActionEvent): Unit = {
    ClientMem.mute = !ClientMem.mute
    if (!ClientMem.mute) {
      player.play()
      muteBtn.setStyle("-fx-background-color: #ccdb30;")
    } else {
      player.stop()
      muteBtn.setStyle("-fx-background-color: #ff6666;")
    }
  }

  // Host send round start command
  def roundStart(): Unit = {
    nextRoundBtn.disable = true
    clearBoard()
    playerConfetti.visible = false
    game.roundStart()
    for (user <- ClientMem.tableList) { //Inform clients to restart round
      if (user.ref != ClientMem.hostRef.get) {
        ClientMem.ownRef.foreach(_ ! GameClient.RoundStart(user))
      }
    }
  }

  // Client receive round start command
  def roundStartReceive(): Unit = {
    clearBoard()
    playerConfetti.visible = false
    game.roundStart()
  }

  // Client receive cards to display
  def updateCard(cardMeta: PokerCardHolderInfo): Unit = {
    var playerInt = game.returnPlayerSeatPosition(cardMeta.playerUser)
    val currentPlayer = game.returnPlayer(cardMeta.playerUser).get

    // Dealer card is placed on top
    if (!cardMeta.inPlayerHand) {
      playerInt = 0

      // Make sure dealer's hand fully revealed before enabling next round button
      if (ClientMem.isHost && cardMeta.position == game.housedealer.cardsInHand.size && game.endOfRound.value) {
        nextRoundBtn.disable = false
      }
    }

    // Non-host clients update player object's hand
    if (cardMeta.inPlayerHand && !ClientMem.isHost) {
      currentPlayer.addCardToHand(cardMeta.pokerCard)
    }

    val cardImg = new Image(getClass.getResourceAsStream(s"/blackjack-app/img/cards/${cardMeta.pokerCard.toString}.jpg"))

    if (playerInt == 0) { //Dealer
      if (cardMeta.position == 1) dealercard1.image = cardImg
      else if (cardMeta.position == 2) dealercard2.image = cardImg
      else if (cardMeta.position == 3) dealercard3.image = cardImg
      else if (cardMeta.position == 4) dealercard4.image = cardImg
      else if (cardMeta.position == 5) dealercard5.image = cardImg
    }
    else if (playerInt == 1) { //Left player
      if (cardMeta.position == 1) player1card1.image = cardImg
      else if (cardMeta.position == 2) player1card2.image = cardImg
      else if (cardMeta.position == 3) player1card3.image = cardImg
      else if (cardMeta.position == 4) player1card4.image = cardImg
      else if (cardMeta.position == 5) player1card5.image = cardImg
    }
    else if (playerInt == 2) { //Middle player
      if (cardMeta.position == 1) player2card1.image = cardImg
      else if (cardMeta.position == 2) player2card2.image = cardImg
      else if (cardMeta.position == 3) player2card3.image = cardImg
      else if (cardMeta.position == 4) player2card4.image = cardImg
      else if (cardMeta.position == 5) player2card5.image = cardImg
    }
    else if (playerInt == 3) { //Right player
      if (cardMeta.position == 1) player3card1.image = cardImg
      else if (cardMeta.position == 2) player3card2.image = cardImg
      else if (cardMeta.position == 3) player3card3.image = cardImg
      else if (cardMeta.position == 4) player3card4.image = cardImg
      else if (cardMeta.position == 5) player3card5.image = cardImg
    }
    else if (playerInt == 4) { //Right player
      if (cardMeta.position == 1) player4card1.image = cardImg
      else if (cardMeta.position == 2) player4card2.image = cardImg
      else if (cardMeta.position == 3) player4card3.image = cardImg
      else if (cardMeta.position == 4) player4card4.image = cardImg
      else if (cardMeta.position == 5) player4card5.image = cardImg
    }
  }

  // Host send turn announcement
  def announceTurn(currentUser: User): Unit = {
    for (user <- ClientMem.tableList) {
      ClientMem.ownRef.foreach(_ ! GameClient.AnnounceTurn(currentUser, user))
    }
  }

  // Client receive turn announcement
  def announceTurnReceive(currentUser: User): Unit = {
    if (currentUser.ref == ClientMem.ownRef.get) {
      statusText.text = "It is your turn, choose an action!"
      hitBtn.disable = false
      standBtn.disable = false
    } else {
      statusText.text = s"${currentUser.name}'s turn!"
      hitBtn.disable = true
      standBtn.disable = true
    }
  }

  // Client send player hit action
  def playerHit(action: ActionEvent): Unit = {
    ClientMem.hostRef.foreach(_ ! GameClient.PlayerHit(localPlayer))
    hitBtn.disable = true
    standBtn.disable = true
    increaseBetBtn.disable = true
    decreaseBetBtn.disable = true
  }

  // Host receive player hit action
  def playerHitReceive(currentUser: User): Unit = {
    val cardMeta = game.playerHit(currentUser)
    for (user <- GameClient.tableList) {
      ClientMem.ownRef.foreach(_ ! GameClient.GiveCard(cardMeta, user))
    }
    time = 0L
    timerTurn.start
  }

  // Client send player stand action
  def playerStand(action: ActionEvent): Unit = {
    ClientMem.hostRef.foreach(_ ! GameClient.PlayerStand(localPlayer))
    hitBtn.disable = true
    standBtn.disable = true
    increaseBetBtn.disable = true
    decreaseBetBtn.disable = true
  }

  // Host receive player stand action
  def playerStandReceive(currentUser: User): Unit = {  //Host Receive Player Stand Response
    game.playerStand(currentUser)
    time = 0L
    timerTurn.start
  }

  // Client send increase bet action
  def increaseBet(action: ActionEvent): Unit = {
    ClientMem.ownRef.foreach(_ ! GameClient.IncreaseBetSend(ClientMem.hostRef.get))
  }

  // Client send decrease bet action
  def decreaseBet(action: ActionEvent): Unit = {
    ClientMem.ownRef.foreach(_ ! GameClient.DecreaseBetSend(ClientMem.hostRef.get))
  }

  // Host receive increase bet action
  def increaseBetReceive(user: User): Unit = {
    updateBet(user, game.playerIncreaseBet(user))
  }

  // Host receive decrease bet action
  def decreaseBetReceive(user: User): Unit = {
    updateBet(user, game.playerDecreaseBet(user))
  }

  // Host send updated bet amount
  def updateBet(user: User, amt: Int): Unit = {
    for (target <- ClientMem.tableList) {
      ClientMem.ownRef.foreach(_ ! GameClient.UpdateBet(target, user, amt))
    }
  }

  // Client receive updated bet amount
  def updateBetReceive(user: User, amt: Int): Unit = {
    var playerInt = game.returnPlayerSeatPosition(user)

    if (playerInt == 1) {
      player1Bet.text = s"${amt}"
    } else if (playerInt == 2) {
      player2Bet.text = s"${amt}"
    } else if (playerInt == 3) {
      player3Bet.text = s"${amt}"
    } else if (playerInt == 4) {
      player4Bet.text = s"${amt}"
    }
  }

  def confirmBet(action: ActionEvent): Unit = {
    ClientMem.ownRef.foreach(_ ! GameClient.ConfirmBetSend(ClientMem.hostRef.get))
  }

  def confirmBetReceiveAck(): Unit = {
    increaseBetBtn.disable = true
    decreaseBetBtn.disable = true
    confirmBetBtn.disable = true
  }

  def confirmBetReceive(from: User): Unit = {
    game.playerConfirmBet(from)
  }

  // Host send updated balance and bet amount
  def updateBalAndBet(): Unit = {
    for (player <- ClientMem.tableList) { // For each player's balance and bet amount
      val currentUser = game.returnPlayer(player).get
      for (target <- ClientMem.tableList) { // Send it to all players
        ClientMem.ownRef.foreach(_ ! GameClient.UpdateBalAndBet(target, player, currentUser.chipAmount, currentUser.minBet))
      }
    }
  }

  // Client receive updated balance and bet amount
  def updateBalAndBetReceive(user: User, bal: Int, bet: Int): Unit = {
    val playerInt = game.returnPlayerSeatPosition(user)

    if (playerInt == 1) {
      player1Bal.text = s"${bal}"
      player1Bet.text = s"${bet}"
    } else if (playerInt == 2) {
      player2Bal.text = s"${bal}"
      player2Bet.text = s"${bet}"
    } else if (playerInt == 3) {
      player3Bal.text = s"${bal}"
      player3Bet.text = s"${bet}"
    } else if (playerInt == 4) {
      player4Bal.text = s"${bal}"
      player4Bet.text = s"${bet}"
    }
  }

  //Host send dealer's final hand and announce winner
  def roundEnd(): Unit = {
    this.cardMetaList = game.getDealerHand
    timerCard.start

    val isHouseWin = game.isHouseWin
    for (user <- GameClient.tableList) {
      if (isHouseWin) {
        ClientMem.ownRef.foreach(_ ! GameClient.AnnounceHouseWin(user))
      }
      ClientMem.ownRef.foreach(_ ! GameClient.AnnounceWinResult(user, game.getPlayerResult(user)))
    }
    updateBalAndBet()
    //nextRoundBtn.disable = false
  }

  def announceWinResult(playerResult: String): Unit = {
    statusText.text = playerResult
    if (playerResult == "You won!") {
      playerConfetti.visible = true
    }
  }

  // Host start next round
  def hostStartNextRound(action: ActionEvent): Unit = {
    game.prepareNewRound()
    roundStart()
  }

  // Clear cards and disable buttons
  def clearBoard(): Unit = {
    statusText.text = "Waiting for players to place bets"
    player1card1.image = blankImg
    player1card2.image = blankImg
    player1card3.image = blankImg
    player1card4.image = blankImg
    player1card5.image = blankImg
    player2card1.image = blankImg
    player2card2.image = blankImg
    player2card3.image = blankImg
    player2card4.image = blankImg
    player2card5.image = blankImg
    player3card1.image = blankImg
    player3card2.image = blankImg
    player3card3.image = blankImg
    player3card4.image = blankImg
    player3card5.image = blankImg
    player4card1.image = blankImg
    player4card2.image = blankImg
    player4card3.image = blankImg
    player4card4.image = blankImg
    player4card5.image = blankImg
    dealercard1.image = blankImg
    dealercard2.image = blankImg
    dealercard3.image = blankImg
    dealercard4.image = blankImg
    dealercard5.image = blankImg
    hitBtn.disable = true
    standBtn.disable = true
    increaseBetBtn.disable = false
    decreaseBetBtn.disable = false
    confirmBetBtn.disable = false
  }

  // Client update status text
  def updateStatusText(text: String): Unit = {
    statusText.text = text
  }

  // Client leave game
  def leaveGame(action: ActionEvent): Unit = {
    val alert = new Alert(AlertType.Warning) {
      title = "Leave room"
      headerText = "Leave room"
      contentText = "Are you sure to leave the room?"
    }

    val result = alert.showAndWait()
    result match {
      case Some(ButtonType.OK) =>
        player.stop()
        ClientMem.isPlaying = false
        if (ClientMem.isHost) {
          for (user <- ClientMem.tableList) { //Inform all players in room that I'm leaving
            if (user.ref != MainApp.ownRef) {
              ClientMem.ownRef.foreach(_ ! GameClient.HostLeaveRoom(user))
            }
          }
        } else {
          for (user <- ClientMem.tableList) { //Inform all players in room that I'm leaving
            if (user.ref != MainApp.ownRef) {
              ClientMem.ownRef.foreach(_ ! GameClient.ClientLeaveRoom(user))
            }
          }
        }
        ClientMem.ownRef.foreach(_ ! GameClient.ResetRoomList)
        Lobby.load()
      case _ =>
    }
  }

  // Client receive notice that a player disconnected
  def clientSuddenLeft(user: User): Unit = {
    val playerPosition = game.returnPlayerSeatPosition(user)
    game.tableList -= user

    if (playerPosition == 1) {
      player1Name.setStrikethrough(true)
    } else if (playerPosition == 3) {
      player3Name.setStrikethrough(true)
    }

    if (ClientMem.isHost) {
      nextRoundBtn.disable = false
      new Alert(AlertType.Warning) {
        title = "Oops"
        headerText = s"${user} has left the room"
        contentText = "Please start a new round."
      }.showAndWait()
    }
  }

}