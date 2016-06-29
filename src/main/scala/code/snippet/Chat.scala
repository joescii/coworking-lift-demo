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
  private [this] val ms = new AtomicReference(List(Message("proftom", "I'm awesome"), Message("Elsa", "Beware of the frozen heart")))

  private [this] def append(msg:Message):Unit =
    ms.updateAndGet(new UnaryOperator[List[Message]] {
      override def apply(m: List[Message]) = (m :+ msg).takeRight(5)
    })

  private [this] def doLogin:Unit = for {
    r <- S.request if r.post_?
    name <- S.param("name")
  } yield {
    User(Some(name))
    S.redirectTo("/")
  }

  private [this] def doChat:Unit = for {
    r <- S.request if r.post_?
    message <- S.param("Compose")
    user <- User.get
  } yield {
    append(Message(user, message))
    S.redirectTo("/")
  }

  def render = {
    S.session.foreach(_.plumbUpdateDOM(listenTo = List(ChatActor)))
    doLogin
    doChat

    var msg = ""

    def onAjax():JsCmd = {
      User.get.foreach(u => append(Message(u, msg)))
      ChatActor ! ""
      SetValById("Compose", "")
    }

    (if (User.get.isDefined) ".login [class+]" #> "hide"
    else ".chat [class+]" #> "hide") &
      ClearClearable &
      ".message *" #> ms.get().map { msg =>
        ".sender-name *" #> msg.user &
        ".message-content *" #> msg.text
      } &
      "#Compose" #> (SHtml.text(msg, msg = _, "id" -> "Compose") ++ SHtml.hidden(onAjax)) andThen
      "#wf-form-Compose" #> SHtml.makeFormsAjax 
  }

}

object ChatActor extends LiftActor with ListenerManager {
  override def createUpdate = ""

  override def lowPriority = {
    case _ => sendListenersMessage(UpdateDOM())
  }
}
