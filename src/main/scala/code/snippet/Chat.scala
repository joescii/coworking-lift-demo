package code.snippet

import net.liftweb.http.{S, SessionVar}
import net.liftweb.util.Helpers._

object User extends SessionVar[Option[String]](None)

object Chat {
  private [this] def doPost:Unit = for {
    r <- S.request if r.post_?
    name <- S.param("name")
  } yield {
    println(name)
    S.redirectTo("/")
  }

  def render = {
    doPost

    if (User.get.isDefined) ".login [class+]" #> "hide"
    else ".chat [class+]" #> "hide"
  }

}
