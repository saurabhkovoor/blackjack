package model

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Random

class HouseDealer {
  var pokerCardDeck:ArrayBuffer[PokerCard] = ArrayBuffer.empty[PokerCard]
  val values: List[String] = List("a", "2", "3", "4", "5", "6", "7", "8", "9", "10", "j", "q", "k")
  val suits: List[String] = List("d", "c", "h", "s")
  var cardsInHand = new ListBuffer[PokerCard]()

  // function to create and generate shuffled deck
  def shuffleDeck(): Unit = {

    // generating all possible combinations of values-suits to create cards of a deck
    for (s <- suits) {
      for (v <- values) {
        // adding the unique created poker card to the deck 
        pokerCardDeck += PokerCard(v, s)
      }
    }
    // shuffling the deck of cards so the positioning is random
    pokerCardDeck = Random.shuffle(pokerCardDeck)
  }

  def takeCard: Card = {
    //generate new deck and shuffle if no cards available in current deck
    if (pokerCardDeck.isEmpty) {
      shuffleDeck()
    }
    //pop the first card from the deck
    pokerCardDeck.remove(0)
  }

  // function to add a card to the dealer's hand
  def addCardToHand(pokerCard: PokerCard): Unit = {
    cardsInHand = cardsInHand + pokerCard
  }

  // function to clear the cards in hand 
  def emptyCardsinHand(): Unit = {
    cardsInHand.clear()
  }
}