package model

import akka.actor.typed.ActorRef
import network.Client
import protocol.JsonSerializable
import scalafx.collections.ObservableHashSet

import scala.collection.mutable.ListBuffer

object Table extends JsonSerializable{
  val numberOfSeats = 5
//   var status = "WAITING"
  var tableList = ListBuffer[User]()

//   def setHost(user: User): Unit = {
//     roomList += user
//   }

//   def addPlayer(user: User): Unit = {
//     roomList += user
//   }

//   def removePlayer(user: User): Unit = {
//     roomList -= user
//   }

  def reset(): Unit = {
    tableList = ListBuffer[User]()
  }

//   override def toString: String = {
//     s"blank's room | ${status} | /${maxRoomSize}"
//   }
}
