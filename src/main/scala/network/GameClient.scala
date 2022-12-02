package network

import akka.actor.Address
import akka.actor.typed.receptionist.Receptionist
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior, PostStop}
import akka.cluster.ClusterEvent.{MemberEvent, ReachabilityEvent, ReachableMember, UnreachableMember}
import akka.cluster.typed.{Cluster, Subscribe}
import model.{PokerCardHolderInfo, User}
import protocol.JsonSerializable
import scalafx.application.Platform
import scalafx.collections.ObservableHashSet

object GameClient{
  sealed trait Command extends JsonSerializable
  object start extends Command
  case class StartJoin(username: String) extends Command
  final case class Joined(list: Iterable[User]) extends Command
  final case object FindServer extends Command
  private case class ListingResponse(listing: Receptionist.Listing) extends Command
  private final case class UserUpdate(event: MemberEvent) extends Command
  private final case class ReachabilityChange(reachabilityEvent: ReachabilityEvent) extends Command

  val users = new ObservableHashSet[User]()
  val cantreach = new ObservableHashSet[Address]()
  var tableList = new ObservableHashSet[User]()

  users.onChange { (ns, _) =>
    Platform.runLater {
      Lobby.control.updateList(ns.toList.filter(y => !cantreach.contains(y.ref.path.address)))
    }
  }

  cantreach.onChange { (ns, _) =>
    Platform.runLater {
      Lobby.control.updateList(users.toList.filter(y => !cantreach.contains(y.ref.path.address)))
      for (user <- tableList.toList.filter(y => cantreach.contains(y.ref.path.address))) {
        if (user.ref == ClientMem.hostRef.get) {
          ClientMem.ownRef.foreach(_ ! HostLeaveRoomReceive)
        } else {
          ClientMem.ownRef.foreach(_ ! ClientLeaveRoomReceive(user))
        }
        tableList -= user
      }
    }
  }

  tableList.onChange { (ns, _) =>
    ClientMem.tableList = tableList
    Platform.runLater {
      Lobby.control.updateRoomList(tableList)
    }
  }

  //----------------//
  // Lobby protocol //
  //----------------//
  final case class MemberList(list: Iterable[User]) extends Command

  final case class LobbyList(list: Iterable[User]) extends Command

  final case class RoomList(list: Iterable[User]) extends Command

  final case object NewRoom extends Command

  final case class IsInvitable(from: ActorRef[GameClient.Command]) extends Command

  final case class IsInvitableResponse(userTarget: User, result: Boolean) extends Command

  final case class SendInvitation(target: ActorRef[GameClient.Command]) extends Command

  final case class ReceiveInvitation(from: User) extends Command

  final case class AcceptInvitation(sender: User) extends Command

  final case class RejectInvitation(sender: User) extends Command

  final case class InvitationResponse(choice: Boolean, from: User) extends Command

  final case object ResetRoomList extends Command

  final case class AddToRoomList(from: User) extends Command

  final case class RemoveFromRoomList(from: User) extends Command

  final case class HostLeaveRoom(from: User) extends Command

  final case object HostLeaveRoomReceive extends Command

  final case class ClientLeaveRoom(from: User) extends Command

  final case class ClientLeaveRoomReceive(from: User) extends Command

  final case class GameStart(target: ActorRef[GameClient.Command]) extends Command

  final case object GameStartReceive extends Command

  //---------------//
  // Game protocol //
  //---------------//
  final case class RoundStart(target: User) extends Command

  final case object RoundStartReceive extends Command

  final case class GiveCard(cardMeta: PokerCardHolderInfo, target: User) extends Command

  final case class ReceiveCard(cardMeta: PokerCardHolderInfo) extends Command

  final case class AnnounceTurn(currentUser: User, target: User) extends Command

  final case class AnnounceTurnReceive(currentUser: User) extends Command

  final case class PlayerHit(currentUser: User) extends Command

  final case class PlayerStand(currentUser: User) extends Command

  final case class IncreaseBetSend(hostRef: ActorRef[GameClient.Command]) extends Command

  final case class IncreaseBetReceive(from: User) extends Command

  final case class DecreaseBetSend(hostRef: ActorRef[GameClient.Command]) extends Command

  final case class DecreaseBetReceive(from: User) extends Command

  final case class UpdateBet(target: User, user: User, amt: Int) extends Command

  final case class UpdateBetReceive(user: User, amt: Int) extends Command

  final case class ConfirmBetSend(hostRef: ActorRef[GameClient.Command]) extends Command

  final case class ConfirmBetReceive(from: User) extends Command

  final case object ConfirmBetReceiveAck extends Command

  final case class UpdateBalAndBet(target: User, player: User, bal: Int, bet: Int) extends Command

  final case class UpdateBalAndBetReceive(player: User, bal: Int, bet: Int) extends Command

  final case class AnnounceHouseWin(target: User) extends Command

  final case object AnnounceHouseWinReceive extends Command

  final case class AnnounceWinResult(target: User, playerResult: String) extends Command

  final case class AnnounceWinResultReceive(playerResult: String) extends Command

  var defaultBehavior: Option[Behavior[GameClient.Command]] = None
  var remoteOpt: Option[ActorRef[Server.Command]] = None
  var nameOpt: Option[String] = None

  def messageStarted(): Behavior[GameClient.Command] =
    Behaviors.receive[GameClient.Command] { (context, message) =>
      message match {
        case ReachabilityChange(reachabilityEvent) =>
          reachabilityEvent match {
            case UnreachableMember(member) =>
              cantreach += member.address
              Behaviors.same
            case ReachableMember(member) =>
              cantreach -= member.address
              Behaviors.same
          }

        case MemberList(list: Iterable[User]) =>
          users.clear()
          users ++= list
          Behaviors.same
        case NewRoom =>
          ClientMem.hostRef = Option(context.self)
          tableList += User(ClientMem.ownName, context.self)
          Behaviors.same
        case IsInvitable(from) =>
          if (tableList.size == 0) {
            from ! IsInvitableResponse(User(ClientMem.ownName, context.self), result = true)
          } else {
            from ! IsInvitableResponse(User(ClientMem.ownName, context.self), result = false)
          }
          Behaviors.same
        case IsInvitableResponse(from, result) =>
          Platform.runLater {
            Lobby.control.playerInviteResult(from, result)
          }
          Behaviors.same
        case SendInvitation(target) =>
          target ! ReceiveInvitation(User(ClientMem.ownName, context.self))
          Behaviors.same
        case ReceiveInvitation(from) =>
          Platform.runLater {
            Lobby.control.playerInvited(from)
          }
          Behaviors.same
        case AcceptInvitation(sender) =>
          tableList += User(ClientMem.ownName, context.self)
          ClientMem.hostRef = Option(sender.ref)
          sender.ref ! InvitationResponse(choice = true, User(ClientMem.ownName, context.self))
          Behaviors.same
        case RejectInvitation(sender) =>
          sender.ref ! InvitationResponse(choice = false, User(ClientMem.ownName, context.self))
          Behaviors.same
        case InvitationResponse(choice, from) =>
          if (choice) { //If accepted invitation
            tableList += from
            for (outerUser <- tableList) { //For each players in the room
              if (outerUser.ref != MainApp.ownRef) { //Except itself (the host)
                for (user <- tableList) { //Send each player info about all players in the room
                  outerUser.ref ! GameClient.AddToRoomList(user)
                }
              }
            }
          }
          Platform.runLater {
            Lobby.control.showInvitationResponse(choice, from)
          }
          Behaviors.same
        case AddToRoomList(from) =>
          tableList += from
          Behaviors.same
        case RemoveFromRoomList(from) =>
          tableList -= from
          Behaviors.same
        case ResetRoomList =>
          tableList.clear()
          ClientMem.hostRef = None
          Behaviors.same
        case HostLeaveRoom(target) =>
          target.ref ! GameClient.HostLeaveRoomReceive
          Behaviors.same
        case HostLeaveRoomReceive =>
          Platform.runLater {
            Lobby.control.hostLeft()
          }
          Behaviors.same
        case ClientLeaveRoom(target) =>
          target.ref ! GameClient.ClientLeaveRoomReceive(User(ClientMem.ownName, context.self)) //Announce leaving
          Behaviors.same
        case ClientLeaveRoomReceive(from) =>
          ClientMem.ownRef.foreach(_ ! GameClient.RemoveFromRoomList(from))
          Platform.runLater {
            if (ClientMem.isPlaying)
              Board.control.clientSuddenLeft(from)
          }
          Behaviors.same

        case GameStart(target) =>
          target ! GameClient.GameStartReceive
          Behaviors.same
        case GameStartReceive =>
          Platform.runLater {
            Lobby.control.gameLoad()
          }
          Behaviors.same

        case RoundStart(target) =>
          target.ref ! GameClient.RoundStartReceive
          Behaviors.same
        case RoundStartReceive =>
          Platform.runLater {
            Board.control.roundStartReceive()
          }
          Behaviors.same

        case GiveCard(cardMeta, target) =>
          target.ref ! GameClient.ReceiveCard(cardMeta)
          Behaviors.same
        case ReceiveCard(cardMeta) =>
          Platform.runLater {
            Board.control.updateCard(cardMeta)
          }
          Behaviors.same

        case AnnounceTurn(currentUser, target) =>
          target.ref ! GameClient.AnnounceTurnReceive(currentUser)
          Behaviors.same
        case AnnounceTurnReceive(currentUser) =>
          Platform.runLater {
            Board.control.announceTurnReceive(currentUser)
          }
          Behaviors.same
        case PlayerHit(currentUser) =>
          Platform.runLater {
            Board.control.playerHitReceive(currentUser)
          }
          Behaviors.same
        case PlayerStand(currentUser) =>
          Platform.runLater {
            Board.control.playerStandReceive(currentUser)
          }
          Behaviors.same

        case IncreaseBetSend(hostRef) =>
          hostRef ! GameClient.IncreaseBetReceive(User(ClientMem.ownName, context.self))
          Behaviors.same
        case IncreaseBetReceive(from) =>
          Platform.runLater {
            Board.control.increaseBetReceive(from)
          }
          Behaviors.same

        case DecreaseBetSend(hostRef) =>
          hostRef ! GameClient.DecreaseBetReceive(User(ClientMem.ownName, context.self))
          Behaviors.same
        case DecreaseBetReceive(from) =>
          Platform.runLater {
            Board.control.decreaseBetReceive(from)
          }
          Behaviors.same

        case UpdateBet(target, user, amt) =>
          target.ref ! GameClient.UpdateBetReceive(user, amt)
          Behaviors.same
        case UpdateBetReceive(user, amt) =>
          Platform.runLater {
            Board.control.updateBetReceive(user, amt)
          }
          Behaviors.same

        case ConfirmBetSend(hostRef) =>
          hostRef ! GameClient.ConfirmBetReceive(User(ClientMem.ownName, context.self))
          Behaviors.same
        case ConfirmBetReceive(from) =>
          Platform.runLater {
            Board.control.confirmBetReceive(from)
          }
          from.ref ! ConfirmBetReceiveAck
          Behaviors.same
        case ConfirmBetReceiveAck =>
          Platform.runLater {
            Board.control.confirmBetReceiveAck()
          }
          Behaviors.same

        case UpdateBalAndBet(target, player, bal, bet) =>
          target.ref ! GameClient.UpdateBalAndBetReceive(player, bal, bet)
          Behaviors.same
        case UpdateBalAndBetReceive(player, bal, bet) =>
          Platform.runLater {
            Board.control.updateBalAndBetReceive(player, bal, bet)
          }
          Behaviors.same

        case AnnounceHouseWin(target) =>
          target.ref ! GameClient.AnnounceHouseWinReceive
          Behaviors.same
        case AnnounceHouseWinReceive =>
          Platform.runLater {
          }
          Behaviors.same

        case AnnounceWinResult(target, playerResult) =>
          target.ref ! GameClient.AnnounceWinResultReceive(playerResult)
          Behaviors.same
        case AnnounceWinResultReceive(playerResult) =>
          Platform.runLater {
            Board.control.announceWinResult(playerResult)
          }
          Behaviors.same
        case _ =>
          Behaviors.unhandled
      }
    }.receiveSignal {
      case (context, PostStop) =>
        for (name <- nameOpt; remote <- remoteOpt) {
          remote ! Server.Leave(name, context.self)
        }
        defaultBehavior.getOrElse(Behaviors.same)
    }

  def apply(): Behavior[GameClient.Command] =
    Behaviors.setup { context =>
      var counter = 0
      // (1) a ServiceKey is a unique identifier for this actor
      Upnp.bindPort(context)

      val reachabilityAdapter = context.messageAdapter(ReachabilityChange)
      Cluster(context.system).subscriptions ! Subscribe(reachabilityAdapter, classOf[ReachabilityEvent])

      // (2) Create an ActorRef, AKA Receptionist Listing “adapter.”
      val listingAdapter: ActorRef[Receptionist.Listing] = context.messageAdapter { listing =>
        println(s"listingAdapter:listing: ${listing.toString}")
        GameClient.ListingResponse(listing) //To tell receptionist how to reply us after contacting it in step 4
      }
      // (3) Tell Receptionist we want to subscribe to events related to Server.ServerKey (Server actor)
      context.system.receptionist ! Receptionist.Subscribe(Server.ServerKey, listingAdapter)

      defaultBehavior = Some(Behaviors.receiveMessage { message =>
        message match {
          case GameClient.start =>
            context.self ! FindServer
            Behaviors.same
          // (4) Ask Receptionist to find listings of Server.ServerKey (Server actor)
          case FindServer =>
            println(s"Client received FindTheServer message")
            context.system.receptionist ! Receptionist.Find(Server.ServerKey, listingAdapter)
            Behaviors.same
          // (5) Receptionist sends us set of ActorRef[Server.Command], only 1 for now
          case ListingResponse(Server.ServerKey.Listing(listings)) =>
            val xs: Set[ActorRef[Server.Command]] = listings
            for (x <- xs) {
              remoteOpt = Some(x)
            }
            Behaviors.same
          case StartJoin(name) =>
            nameOpt = Option(name)
            remoteOpt.foreach(_ ! Server.JoinChat(name, context.self))
            Behaviors.same
          case GameClient.Joined(x) =>
            Platform.runLater {
              Lobby.control.playerJoined()
              MainApp.stage.setTitle(s"Blackjack | ${nameOpt.get}")
            }
            users.clear()
            users ++= x
            messageStarted()
          case _ =>
            Behaviors.unhandled
        }
      })
      defaultBehavior.get
    }
}