package model

import protocol.JsonSerializable

class PokerCard(val value: String, val suit: String) extends JsonSerializable {
}

case class PokerCardHolderInfo(inPlayerHand: Boolean, playerUser: User, pokerCard: PokerCard, position:Int)extends JsonSerializable {
  override def toString: String = {
    s"${inPlayerHand}_${playerUser}_${pokerCard}_${position}"
  }
}