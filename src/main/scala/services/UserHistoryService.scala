package services

import models.{UserHistory, UserInteraction, QuizResult, QuizQuestion}
import spray.json._
import DefaultJsonProtocol._
import java.io.{File, PrintWriter, FileWriter, BufferedWriter}
import scala.io.Source
import scala.collection.mutable.Map

object UserHistoryJsonProtocol extends DefaultJsonProtocol {
  implicit val quizQuestionFormat: RootJsonFormat[QuizQuestion] = jsonFormat4(QuizQuestion)
  implicit val quizResultFormat: RootJsonFormat[QuizResult] = jsonFormat9(QuizResult.apply)
  implicit val userInteractionFormat: RootJsonFormat[UserInteraction] = jsonFormat6(UserInteraction)
  implicit val userHistoryFormat: RootJsonFormat[UserHistory] = jsonFormat5(UserHistory.apply)
}

class UserHistoryService {
  private val HISTORY_FILE = "user_history.json"
  private var userHistories: Map[String, UserHistory] = Map.empty
  
  // Initialize by loading existing histories
  initializeHistoryFile()
  userHistories = loadHistories()
  
  // Initialize the history file if it doesn't exist
  private def initializeHistoryFile(): Unit = {
    val file = new File(HISTORY_FILE)
    if (!file.exists() || file.length() == 0) {
      try {
        val writer = new BufferedWriter(new FileWriter(file))
        try {
          writer.write("[]")
        } finally {
          writer.close()
        }
      } catch {
        case e: Exception =>
          // Silently handle error
      }
    }
  }
  
  private def loadHistories(): Map[String, UserHistory] = {
    val historyMap = Map[String, UserHistory]()
    try {
      initializeHistoryFile()
      val file = new File(HISTORY_FILE)
      if (file.exists() && file.length() > 0) {
        val source = Source.fromFile(file)
        val json = source.mkString
        source.close()
        
        import UserHistoryJsonProtocol._
        val historyList = json.parseJson.convertTo[List[UserHistory]]
        historyList.foreach(history => historyMap.put(history.userId, history))
      }
    } catch {
      case e: Exception =>
        // Silently handle error
    }
    historyMap
  }
  
  private def saveHistories(): Unit = {
    try {
      import UserHistoryJsonProtocol._
      val json = userHistories.values.toList.toJson.prettyPrint
      val writer = new BufferedWriter(new FileWriter(HISTORY_FILE))
      try {
        writer.write(json)
      } finally {
        writer.close()
      }
    } catch {
      case e: Exception =>
        // Silently handle error
    }
  }
  
  def addInteraction(userId: String, username: String, email: String, question: String, response: String, category: String): Unit = {
    val interaction = UserInteraction(
      userId = userId,
      question = question,
      response = response,
      category = category
    )
    
    val currentHistory = userHistories.getOrElse(userId, UserHistory(userId, username, email))
    val updatedHistory = currentHistory.copy(
      interactions = interaction :: currentHistory.interactions
    )
    
    userHistories.put(userId, updatedHistory)
    saveHistories()
  }
  
  def addQuizResult(
    userId: String,
    username: String,
    email: String,
    quizType: String,
    score: Double,
    totalQuestions: Int,
    correctAnswers: Int,
    questions: List[QuizQuestion],
    userAnswers: List[String]
  ): Unit = {
    val quizResult = QuizResult(
      userId = userId,
      quizType = quizType,
      score = score,
      totalQuestions = totalQuestions,
      correctAnswers = correctAnswers,
      questions = questions,
      userAnswers = userAnswers
    )
    
    val currentHistory = userHistories.getOrElse(userId, UserHistory(userId, username, email))
    val updatedHistory = currentHistory.copy(
      quizResults = quizResult :: currentHistory.quizResults
    )
    
    userHistories.put(userId, updatedHistory)
    saveHistories()
  }
  
  def getUserHistory(userId: String): Option[UserHistory] = userHistories.get(userId)
  
  def getRecentInteractions(userId: String, limit: Int = 10): List[UserInteraction] = {
    userHistories.get(userId) match {
      case Some(history) => history.interactions.take(limit)
      case None => List.empty
    }
  }
  
  def getQuizResults(userId: String): List[QuizResult] = {
    userHistories.get(userId) match {
      case Some(history) => history.quizResults
      case None => List.empty
    }
  }
  
  def getAverageQuizScore(userId: String): Option[Double] = {
    userHistories.get(userId) match {
      case Some(history) if history.quizResults.nonEmpty =>
        val scores = history.quizResults.map(_.score)
        Some(scores.sum / scores.length)
      case _ => None
    }
  }
  
  def displayUserHistory(userId: String): String = {
    userHistories.get(userId) match {
      case Some(history) =>
        val interactions = history.interactions.map { interaction =>
          s"""
          |Time: ${new java.util.Date(interaction.timestamp)}
          |Question: ${interaction.question}
          |Response: ${interaction.response}
          |Category: ${interaction.category}
          |""".stripMargin
        }.mkString("\n")
        
        val quizResults = history.quizResults.map { result =>
          s"""
          |Quiz Type: ${result.quizType}
          |Score: ${result.score}
          |Total Questions: ${result.totalQuestions}
          |Correct Answers: ${result.correctAnswers}
          |Time: ${new java.util.Date(result.timestamp)}
          |""".stripMargin
        }.mkString("\n")
        
        s"""
        |User Information:
        |Username: ${history.username}
        |Email: ${history.email}
        |
        |Recent Interactions:
        |$interactions
        |
        |Quiz Results:
        |$quizResults
        |""".stripMargin
        
      case None => "No history found for this user."
    }
  }
  
  def getTotalQuestions(userId: String): Int = {
    userHistories.get(userId) match {
      case Some(history) =>
        history.quizResults.foldLeft(0) { (total, result) =>
          total + result.totalQuestions
        }
      case None => 0
    }
  }

  def getTotalCorrectAnswers(userId: String): Int = {
    userHistories.get(userId) match {
      case Some(history) =>
        history.quizResults.foldLeft(0) { (total, result) =>
          total + result.correctAnswers
        }
      case None => 0
    }
  }
} 