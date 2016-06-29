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

  def render = {
    doLogin

    if (User.get.isDefined) ".login [class+]" #> "hide"
    else ".chat [class+]" #> "hide"
  }

}
