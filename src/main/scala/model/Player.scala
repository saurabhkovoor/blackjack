package model

import scala.collection.mutable.ListBuffer

class Player(val user: User) {
  val cardsInHand: ListBuffer[PokerCard] = ListBuffer[PokerCard]()
  var chipAmount: Int = 500
  var minBet: Int = 10
  var betPlaced: Boolean = false

  // function to clear the cards in hand 
  def emptyCardsinHand(): Unit = {
    cardsInHand.clear()
  }
  
  // function to add a card to the dealer's hand
  def addCardToHand(pokerCard: PokerCard): Unit = {
    cardsInHand += pokerCard
  }

  // function to determine the size of the bet to be placed, 
  def increaseBetPlaced(betSize: Int): Unit = {
    minBet = minBet + betSize
    // if the bet size is bigger than the balance available, all the available balance is used as the bet amount
    if (minBet > chipAmount) {
      minBet = chipAmount
    }
    // if player does not have funds left, minimum bet is 0
    if (minBet < 0) {
      minBet = 0
    }
  }
}