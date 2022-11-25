package model

import protocol.JsonSerializable

class PokerCard(val value: String, val suit: String) extends JsonSerializable {
}

class PokerCardHolderInfo(inPlayerHand: Boolean, playerUser: User, pokerCard: PokerCard, position:Int)extends JsonSerializable {
}