package model

import protocol.JsonSerializable

class PokerCard(val ranking: String, val suit: String) extends JsonSerializable {

}

class CardMeta