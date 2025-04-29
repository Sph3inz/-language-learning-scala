package languagelearningbot

import java.time.{Instant, LocalDateTime, ZoneId}
import java.time.format.DateTimeFormatter

object AnalyticsDashboard {
  // Required analytics functions as specified in the requirements
  
  // Variable to store interaction logs immutably
  private var interactionLogs: List[InteractionLog] = List.empty
  private var quizLogs: List[QuizLog] = List.empty
  
  /**
   * Log an interaction between user and chatbot
   */
  def logInteraction(userInput: String, chatbotResponse: String): Unit = {
    val newId = interactionLogs.size + 1
    val timestamp = System.currentTimeMillis()
    val newLog = InteractionLog(newId, userInput, chatbotResponse, timestamp)
    
    // Update logs immutably (create a new list)
    interactionLogs = interactionLogs :+ newLog
  }
  
  /**
   * Log a quiz interaction with details
   */
  def logQuizInteraction(quizSession: QuizSession): Unit = {
    val newId = quizLogs.size + 1
    val timestamp = System.currentTimeMillis()
    val score = quizSession.calculateScore
    
    val newLog = QuizLog(
      newId,
      quizSession.quizType,
      quizSession.questions,
      quizSession.userAnswers,
      score,
      timestamp
    )
    
    // Update logs immutably (create a new list)
    quizLogs = quizLogs :+ newLog
  }
  
  /**
   * Retrieve the full log of interactions
   */
  def getInteractionLog(): List[(Int, String, String)] = {
    interactionLogs.map(log => (log.id, log.userInput, log.chatbotResponse))
  }
  
  /**
   * Process the log using higher-order functions to generate insights
   */
  def analyzeInteractions(log: List[InteractionLog]): Map[String, Any] = {
    if (log.isEmpty) {
      Map("total_interactions" -> 0)
    } else {
      // Use higher-order functions to analyze the log
      val totalInteractions = log.size
      
      // Calculate interactions per day using groupBy
      val interactionsByDay = log.groupBy(log => 
        LocalDateTime.ofInstant(
          Instant.ofEpochMilli(log.timestamp), 
          ZoneId.systemDefault()
        ).toLocalDate.toString
      ).map { case (day, logs) => 
        (day, logs.size) 
      }
      
      // Find common user inputs using pattern matching and groupBy
      val commonInputs = log
        .map(_.userInput.toLowerCase)
        .groupBy(identity)
        .map { case (input, occurrences) => (input, occurrences.size) }
        .toList
        .sortBy(-_._2)
        .take(5)
        .toMap
      
      // Collect the analytics
      Map(
        "total_interactions" -> totalInteractions,
        "interactions_by_day" -> interactionsByDay,
        "common_inputs" -> commonInputs,
        "latest_interaction_time" -> formatTimestamp(log.maxBy(_.timestamp).timestamp)
      )
    }
  }
  
  /**
   * Extract quiz-specific analytics
   */
  def analyzeQuizPerformance(log: List[QuizLog]): Map[String, Any] = {
    if (log.isEmpty) {
      Map("total_quizzes" -> 0)
    } else {
      // Use higher-order functions to analyze quiz performance
      val totalQuizzes = log.size
      
      // Calculate average score using higher-order function
      val averageScore = log.map(_.score).sum.toDouble / totalQuizzes
      
      // Calculate success rate by quiz type
      val quizzesByType = log.groupBy(_.quizType)
      val successRateByType = quizzesByType.map { 
        case (quizType, quizzes) =>
          val typeTotal = quizzes.size
          // Calculate correct answers for each quiz and sum them
          val correctAnswers = quizzes.foldLeft(0) { (total, quiz) =>
            // For each quiz, count correct answers and add to running total
            total + quiz.questions.zip(quiz.userAnswers).count { case (question, answer) => 
              question.correctAnswer.toLowerCase == answer.toLowerCase
            }
          }
          val totalQuestions = quizzes.flatMap(_.questions).size
          val successRate = if (totalQuestions > 0) {
            correctAnswers.toDouble / totalQuestions * 100
          } else {
            0.0
          }
          (quizType.toString, Map(
            "count" -> typeTotal,
            "success_rate" -> f"$successRate%.1f%%"
          ))
      }
      
      // Most challenging questions (most frequently missed)
      val challengingQuestions = log.flatMap { quiz =>
        quiz.questions.zip(quiz.userAnswers).filter {
          case (question, answer) => 
            question.correctAnswer.toLowerCase != answer.toLowerCase
        }.map(_._1.prompt)
      }.groupBy(identity)
       .map { case (prompt, occurrences) => (prompt, occurrences.size) }
       .toList
       .sortBy(-_._2)
       .take(3)
       .toMap
      
      // Collect the analytics
      Map(
        "total_quizzes" -> totalQuizzes,
        "average_score" -> f"$averageScore%.1f",
        "success_rate_by_type" -> successRateByType,
        "challenging_questions" -> challengingQuestions,
        "latest_quiz_time" -> formatTimestamp(log.maxBy(_.timestamp).timestamp)
      )
    }
  }
  
  /**
   * Generate a language proficiency report
   */
  def generateLanguageProficiencyReport(quizLogs: List[QuizLog], targetLanguage: Language): Map[String, Any] = {
    if (quizLogs.isEmpty) {
      Map("proficiency_level" -> "Not enough data")
    } else {
      // Filter logs for the target language
      val languageQuizzes = quizLogs // In a real app, we'd filter by language
      
      // Calculate overall success rate
      val totalQuestions = languageQuizzes.flatMap(_.questions).size
      val correctAnswers = languageQuizzes.map(_.score).sum
      val overallSuccessRate = if (totalQuestions > 0) 
                                correctAnswers.toDouble / totalQuestions * 100 
                              else 0.0
      
      // Determine proficiency level based on success rate
      val proficiencyLevel = overallSuccessRate match {
        case rate if rate >= 90 => "Advanced"
        case rate if rate >= 75 => "Intermediate"
        case rate if rate >= 60 => "Pre-Intermediate"
        case rate if rate >= 40 => "Elementary"
        case _ => "Beginner"
      }
      
      // Calculate strengths and weaknesses by quiz type
      val performanceByType = languageQuizzes.groupBy(_.quizType).map { 
        case (quizType, quizzes) =>
          val typeTotal = quizzes.flatMap(_.questions).size
          val typeCorrect = quizzes.map(_.score).sum
          val typeRate = if (typeTotal > 0) typeCorrect.toDouble / typeTotal * 100 else 0.0
          
          (quizType.toString, f"$typeRate%.1f%%")
      }
      
      // Identify strengths and weaknesses
      val strengths = performanceByType.toList.sortBy(-_._2.dropRight(1).toDouble).take(2).map(_._1)
      val weaknesses = performanceByType.toList.sortBy(_._2.dropRight(1).toDouble).take(2).map(_._1)
      
      // Determine learning trend (improving, steady, declining)
      val trend = if (languageQuizzes.size >= 3) {
        val recentQuizzes = languageQuizzes.sortBy(-_.timestamp).take(3)
        val recentScores = recentQuizzes.map { quiz => 
          quiz.score.toDouble / quiz.questions.size * 100
        }
        
        if (recentScores.head > recentScores.last + 5) "Improving"
        else if (recentScores.head < recentScores.last - 5) "Declining"
        else "Steady"
      } else "Not enough data"
      
      // Collect the report data
      Map(
        "proficiency_level" -> proficiencyLevel,
        "overall_success_rate" -> f"$overallSuccessRate%.1f%%",
        "strengths" -> strengths,
        "areas_for_improvement" -> weaknesses,
        "learning_trend" -> trend,
        "total_quizzes_taken" -> languageQuizzes.size
      )
    }
  }
  
  /**
   * Format a timestamp for display
   */
  private def formatTimestamp(timestamp: Long): String = {
    val instant = Instant.ofEpochMilli(timestamp)
    val dateTime = LocalDateTime.ofInstant(instant, ZoneId.systemDefault())
    dateTime.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
  }
}
