package code.snippet

import java.util.concurrent.atomic.AtomicReference
import java.util.function.UnaryOperator

import net.liftweb.actor.LiftActor
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.SetValById
import net.liftweb.http.{ListenerManager, S, SHtml, SessionVar, UpdateDOM}
import net.liftweb.util.ClearClearable
import net.liftweb.util.Helpers._

object User extends SessionVar[Option[String]](None)

case class Message(user:String, text:String)

object Chat {
  private[this] val ms = new AtomicReference(List(Message("proftom", "I still think I'm cool"), Message("Elsa", "Beware of the frozen heart")))

  private[this] def append(msg: Message): Unit = {
    ms.updateAndGet(new UnaryOperator[List[Message]] {
      override def apply(m: List[Message]) = (m :+ msg).takeRight(5)
    })

    ChatActor ! ""
  }


  private [this] def doLogin: Unit = for {
    r <- S.request if r.post_?
    name <- S.param("name")
  } yield {
    User(Some(name))
    S.redirectTo("/")
  }

  private [this] def doChat: Unit = for {
    r <- S.request if r.post_?
    chat <- S.param("text")
    user <- User.get
  } yield {
    println("YO??")
    append(Message(user, chat))
    S.redirectTo("/")
  }

  def render = {
    S.session.foreach(_.plumbUpdateDOM(listenTo = List(ChatActor)))

    doLogin
    doChat

    var msg = ""

    def onAjax(): JsCmd = {
      User.get.foreach(u => append(Message(u, msg)))
      SetValById("text", "")
    }


    (if(User.get.isDefined) "#login [class+]" #> "hide"
    else "#chat [class+]" #> "hide") &
      ClearClearable &
      ".message *" #> ms.get.map(msg =>
        ".sender-name *" #> msg.user &
        ".message-content *" #> msg.text
      ) & "#text" #> (SHtml.text(msg, msg = _, "id" -> "text") ++ SHtml.hidden(onAjax)) andThen
      "#email-form" #> SHtml.makeFormsAjax

  }

}

object ChatActor extends LiftActor with ListenerManager {
  override def createUpdate = ""

  override def lowPriority = {
    case _ => sendListenersMessage(UpdateDOM())
  }
}
