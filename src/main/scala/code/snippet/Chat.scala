package code.snippet

import net.liftweb.http.{S, SessionVar}
import net.liftweb.util.Helpers._

object User extends SessionVar[Option[String]](None)

object Chat {
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

    if (User.get.isDefined) ".login [class+]" #> "hide"
    else ".chat [class+]" #> "hide"
  }

}
