package services

import models.{User, UserCredentials, SignUpRequest}
import java.security.MessageDigest
import java.util.Base64
import scala.collection.mutable.Map
import scala.util.{Success, Failure, Try}
import java.io.{File, PrintWriter, FileWriter, BufferedWriter}
import scala.io.Source
import spray.json._
import DefaultJsonProtocol._

object AuthJsonProtocol extends DefaultJsonProtocol {
  implicit val userFormat: RootJsonFormat[User] = jsonFormat5(User.apply)
}

class AuthService {
  private val USERS_FILE = "users.json"
  private var users: Map[String, User] = loadUsers()
  
  private def hashPassword(password: String): String = {
    val digest = MessageDigest.getInstance("SHA-256")
    val hash = digest.digest(password.getBytes("UTF-8"))
    Base64.getEncoder.encodeToString(hash)
  }
  
  private def loadUsers(): Map[String, User] = {
    val userMap = Map[String, User]()
    try {
      if (new File(USERS_FILE).exists()) {
        val source = Source.fromFile(USERS_FILE)
        val json = source.mkString
        source.close()
        
        import AuthJsonProtocol._
        val userList = json.parseJson.convertTo[List[User]]
        userList.foreach(user => userMap.put(user.id, user))
      }
    } catch {
      case e: Exception =>
        println(s"Error loading users: ${e.getMessage}")
    }
    userMap
  }
  
  private def saveUsers(): Unit = {
    try {
      import AuthJsonProtocol._
      val json = users.values.toList.toJson.prettyPrint
      val writer = new BufferedWriter(new FileWriter(USERS_FILE))
      writer.write(json)
      writer.close()
    } catch {
      case e: Exception =>
        println(s"Error saving users: ${e.getMessage}")
    }
  }
  
  def signUp(request: SignUpRequest): Try[User] = {
    if (users.values.exists(_.email == request.email)) {
      Failure(new Exception("Email already registered"))
    } else {
      val user = User(
        id = java.util.UUID.randomUUID().toString,
        username = request.username,
        email = request.email,
        passwordHash = hashPassword(request.password),
        createdAt = System.currentTimeMillis()
      )
      users.put(user.id, user)
      saveUsers() // Save to file after adding new user
      Success(user)
    }
  }
  
  def login(credentials: UserCredentials): Try[User] = {
    users.values.find(_.email == credentials.email) match {
      case Some(user) if user.passwordHash == hashPassword(credentials.password) =>
        Success(user)
      case Some(_) =>
        Failure(new Exception("Invalid password"))
      case None =>
        Failure(new Exception("User not found"))
    }
  }
  
  def getUserById(id: String): Option[User] = users.get(id)
} 