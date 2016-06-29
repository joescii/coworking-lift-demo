package code.snippet

import java.util.concurrent.atomic.AtomicReference

import net.liftweb.http.{S, SessionVar}
import net.liftweb.util.ClearClearable
import net.liftweb.util.Helpers._

object User extends SessionVar[Option[String]](None)

case class Message(user:String, text:String)

object Chat {
  private [this] val ms = new AtomicReference(List(Message("proftom", "I'm awesome"), Message("Elsa", "Beware of the frozen heart")))

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
  } yield {
    println(message)
    S.redirectTo("/")
  }

  def render = {
    doLogin
    doChat

    (if (User.get.isDefined) ".login [class+]" #> "hide"
    else ".chat [class+]" #> "hide") &
      ClearClearable &
      ".message *" #> ms.get().map(_.text)
  }

}
