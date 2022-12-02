package model
import akka.actor.typed.ActorRef
import network.GameClient
import protocol.JsonSerializable

case class User(name: String, ref: ActorRef[GameClient.Command]) extends JsonSerializable {
  override def toString: String = {
    name
  }

}