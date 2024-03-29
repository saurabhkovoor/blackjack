package network

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.adapter._
import akka.cluster.typed._
import akka.discovery.{Discovery, ServiceDiscovery}
import com.typesafe.config.ConfigFactory
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.image.Image
import scalafxml.core.{FXMLLoader, NoDependencyResolver}
import view.{BlackjackTableController, MenuController}

import java.net.URL

object MainApp extends JFXApp {
  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
  val config = ConfigFactory.load()
  var mainSystem = akka.actor.ActorSystem("HelloSystem", Configuration.askClientConfig().withFallback(config))
  var greeterMain: ActorSystem[Nothing] = mainSystem.toTyped
  var cluster = Cluster(greeterMain)
  var discovery: ServiceDiscovery = Discovery(mainSystem).discovery
  var ownRef = mainSystem.spawn(GameClient(), "ChatClient")

  def joinSeedNode(serverIP: String, serverPort: Int): Unit = {
    val address = akka.actor.Address("akka", "HelloSystem", serverIP, serverPort)
    cluster.manager ! JoinSeedNodes(List(address))
  }
  //joinSeedNode("bjgame.ddns.net", 2222)
  joinSeedNode("127.0.0.1", 2222)

  //Initialise empty JFXApp
  val rootResource: URL = getClass.getResource("view/RootLayout.fxml")
  val loader = new FXMLLoader(rootResource, NoDependencyResolver)
  loader.load()
  val roots = loader.getRoot[javafx.scene.layout.BorderPane]
  stage = new PrimaryStage {
    title = "Blackjack"
    icons += new Image(getClass.getResource("img/icon.png").toURI.toString)
    scene = new Scene {
      root = roots
    }
  }

  stage.setResizable(false)
  stage.onCloseRequest = handle( {
    mainSystem
      .terminate
  })
  Lobby.load()
}

//Objects to load the various fxml
object Lobby {
  val resource = getClass.getResource("view/Lobby.fxml")
  val loader = new FXMLLoader(resource, NoDependencyResolver)
  loader.load()
  val roots = loader.getRoot[javafx.scene.layout.AnchorPane]
  var control = loader.getController[MenuController]()

  def load(): Unit = {
    ClientMem.ownRef = Option(MainApp.ownRef)
    MainApp.roots.setCenter(roots)
    control.muteUpdate()
  }
}

object Board {
  val resource = getClass.getResource("view/Board.fxml")
  val loader = new FXMLLoader(resource, NoDependencyResolver)
  loader.load()
  val roots = loader.getRoot[javafx.scene.layout.AnchorPane]
  var control = loader.getController[BlackjackTableController]()
  MainApp.roots.setCenter(roots)

  def load(): Unit = {
    val resource = getClass.getResource("view/Board.fxml")
    val loader = new FXMLLoader(resource, NoDependencyResolver)
    loader.load()
    val roots = loader.getRoot[javafx.scene.layout.AnchorPane]
    MainApp.roots.setCenter(roots)
    this.control = loader.getController[BlackjackTableController]()
  }
}
