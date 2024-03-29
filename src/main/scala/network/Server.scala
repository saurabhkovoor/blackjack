package network

import akka.actor.Address
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.adapter._
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.cluster.ClusterEvent.{ReachabilityEvent, ReachableMember, UnreachableMember}
import akka.cluster.typed._
import akka.management.cluster.bootstrap.ClusterBootstrap
import akka.management.scaladsl.AkkaManagement
import model.User
import protocol.JsonSerializable
import com.typesafe.config.ConfigFactory
import scalafx.collections.ObservableHashSet
object Server {
  sealed trait Command extends JsonSerializable
  case class JoinChat(name: String, from: ActorRef[GameClient.Command]) extends Command
  case class Leave(name: String, from: ActorRef[GameClient.Command]) extends Command
  private final case class ReachabilityChange(reachabilityEvent: ReachabilityEvent) extends Command

  val ServerKey: ServiceKey[Server.Command] = ServiceKey("ChatServer")
  val members = new ObservableHashSet[User]()
  val unreachables = new ObservableHashSet[Address]()

  members.onChange{(ns, _) =>
    for(member <- ns){
      member.ref ! GameClient.MemberList(ns.toList.filter(y => ! unreachables.contains(y.ref.path.address)))
    }
  }

  unreachables.onChange{(ns, _) =>
    for(member <- members){
      member.ref ! GameClient.MemberList(members.toList.filter(y => ! unreachables.contains(y.ref.path.address)))
    }
  }

  def apply(): Behavior[Server.Command] = Behaviors.setup { context =>
    context.system.receptionist ! Receptionist.Register(ServerKey, context.self)

    val reachabilityAdapter = context.messageAdapter(ReachabilityChange)
    Cluster(context.system).subscriptions ! Subscribe(reachabilityAdapter, classOf[ReachabilityEvent])

    Upnp.bindPort(context)

    Behaviors.receiveMessage {
      // Keep track of unreachable members
      case ReachabilityChange(reachabilityEvent) =>
        reachabilityEvent match {
          case UnreachableMember(member) =>
            unreachables += member.address
            Behaviors.same
          case ReachableMember(member) =>
            unreachables -= member.address
            Behaviors.same
        }

      // Client join server
      case JoinChat(name, from) =>
        members += User(name, from)
        from ! GameClient.Joined(members.toList.filter(y => !unreachables.contains(y.ref.path.address)))
        Behaviors.same

      // Client left server
      case Leave(name, from) =>
        members -= User(name, from)
        Behaviors.same
    }
  }
}

object ServerApp extends App {
  try{
    val config = ConfigFactory.load()
    val mainSystem = akka.actor.ActorSystem("Blackjack", Configuration.askServerConfig(2222).withFallback(config)) //classic
    val typedSystem: ActorSystem[Nothing] = mainSystem.toTyped
    val cluster = Cluster(typedSystem)
    cluster.manager ! Join(cluster.selfMember.address)
    AkkaManagement(mainSystem).start()
    //val serviceDiscovery = Discovery(mainSystem).discovery
    ClusterBootstrap(mainSystem).start()
    //val greeterMain: ActorSystem[ChatServer.Command] = ActorSystem(ChatServer(), "HelloSystem")
    mainSystem.spawn(Server(), "BlackjackServer")
  }catch {
    case e :Exception => e.printStackTrace()
  }

}
