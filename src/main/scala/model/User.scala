package model
import protocol.JsonSerializable
import akka.actor.typed.ActorRef
import network.Client

case class User(name: String, ref: ActorRef[Client.Command]) extends JsonSerializable {
  override def toString: String = {
    name
  }
}