package code.snippet

import net.liftweb.http.{S, SessionVar}
import net.liftweb.util.Helpers._

object User extends SessionVar[Option[String]](None)

object Chat {
  def render =
    if(User.get.isDefined) ".login [class+]" #> "hide"
    else ".chat [class+]" #> "hide"
}
