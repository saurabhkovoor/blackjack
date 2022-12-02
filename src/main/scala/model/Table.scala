package model

import protocol.JsonSerializable
import scala.collection.mutable.ListBuffer

object Table extends JsonSerializable{
  val numberOfSeats = 5
//   var status = "WAITING"
  var tableList = ListBuffer[User]()

//   def setHost(user: User): Unit = {
//     roomList += user
//   }

   def addPlayer(user: User): Unit = {
     tableList += user
   }

   def removePlayer(user: User): Unit = {
     tableList -= user
   }

  def reset(): Unit = {
    tableList = ListBuffer[User]()
  }

//   override def toString: String = {
//     s"blank's room | ${status} | /${maxRoomSize}"
//   }
}
