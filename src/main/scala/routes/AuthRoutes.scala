package routes

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import services.AuthService
import models.{SignUpRequest, UserCredentials}
import spray.json._

object AuthJsonProtocol extends DefaultJsonProtocol {
  implicit val signUpRequestFormat: RootJsonFormat[SignUpRequest] = jsonFormat3(SignUpRequest)
  implicit val userCredentialsFormat: RootJsonFormat[UserCredentials] = jsonFormat2(UserCredentials)
}

object AuthRoutes {
  import AuthJsonProtocol._

  def apply(authService: AuthService): Route = {
    pathPrefix("auth") {
      concat(
        path("signup") {
          post {
            entity(as[SignUpRequest]) { request =>
              authService.signUp(request) match {
                case scala.util.Success(user) =>
                  complete(StatusCodes.Created, s"User created successfully with ID: ${user.id}")
                case scala.util.Failure(ex) =>
                  complete(StatusCodes.BadRequest, ex.getMessage)
              }
            }
          }
        },
        path("login") {
          post {
            entity(as[UserCredentials]) { credentials =>
              authService.login(credentials) match {
                case scala.util.Success(user) =>
                  complete(StatusCodes.OK, s"Login successful for user: ${user.username}")
                case scala.util.Failure(ex) =>
                  complete(StatusCodes.Unauthorized, ex.getMessage)
              }
            }
          }
        }
      )
    }
  }
} 