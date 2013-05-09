package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.libs.ws._
import play.api.libs.concurrent.Execution.Implicits._

import views._

case class LoginData(
  username: String,
  password: String,
  host: String
)

object Application extends Controller with Secured {
  
  def index = IsAuthenticated { username => implicit request =>
    (for {
      password <- request.session.get("password")
      host <- request.session.get("host")
    } yield {
      val feedUrl = host + ":8091/pools/default/buckets/default/nodes"
      Async {
        WS.url(feedUrl)
        .get().map { response =>
          println(response.json \ "servers")
          Ok(response.json \ "servers")
        }
      }
    }) getOrElse NotFound
  }

  def login = Action {
    Ok(html.index(loginForm))
  }

  def logout = Action {
    Redirect(routes.Application.index).withNewSession
  }

  def authenticate = Action { implicit request =>
    loginForm.bindFromRequest.fold(
      errors => {
        BadRequest(html.index(errors))
      },
      myForm => {
        println(response.json \ "servers")
        Redirect(routes.Application.index).withSession(
          "username" -> myForm.username,
          "password" -> myForm.password,
          "host" -> myForm.host
        )
      }
    )
  }

  def loginForm = Form(
    mapping(
      "username" -> nonEmptyText,
      "password" -> nonEmptyText,
      "host" -> nonEmptyText
    )(LoginData.apply)(LoginData.unapply)
  )
  
}

/**
 * Provide security features
 */
trait Secured {
  
  /**
   * Retrieve the connected user email.
   */
  private def username(request: RequestHeader) = request.session.get("username")

  /**
   * Redirect to login if the user in not authorized.
   */
  private def onUnauthorized(request: RequestHeader) = Results.Redirect(routes.Application.login
  )

  // --

  /**
   * Action for authenticated users.
   */
  def IsAuthenticated(f: => String => Request[AnyContent] => Result) = Security.Authenticated(username, onUnauthorized) { user =>
    Action(request => f(user)(request))
  }

}