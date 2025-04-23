package languagelearningbot

import scala.io.StdIn.readLine
import scala.util.{Try, Success, Failure}

object Main extends App {
  println("Starting Language Learning Bot...")
  
  // State variables (kept immutable by always reassigning)
  var userPreferences: Option[UserPreferences] = None
  var currentQuizSession: Option[QuizSession] = None
  var inQuizMode: Boolean = false
  var currentQuestionIndex: Int = 0
  
  // Display welcome message
  println(ChatbotCore.greetUser(userPreferences))
  println("Type 'help' for available commands.")
  
  // Main interaction loop
  var running = true
  while (running) {
    print("\nYou: ")
    val userInput = readLine().trim
    
    // Check for exit command
    if (userInput.toLowerCase == "exit") {
      println("\nBot: Thank you for using the Language Learning Bot. Goodbye!")
      running = false
    } else if (inQuizMode) {
      // Handle quiz mode interaction
      handleQuizMode(userInput)
    } else {
      // Regular chatbot interaction
      handleRegularMode(userInput)
    }
  }
  
  /**
   * Handles user input in regular chatbot mode
   */
  def handleRegularMode(input: String): Unit = {
    input.toLowerCase match {
      case "quiz" | "start quiz" =>
        startQuiz()
        
      case "settings" | "preferences" =>
        showSettings()
        
      case s if s.startsWith("set mother language ") =>
        val lang = s.substring("set mother language ".length).trim
        setMotherLanguage(lang)
        
      case s if s.startsWith("set target language ") =>
        val lang = s.substring("set target language ".length).trim
        setTargetLanguage(lang)
        
      case s if s.startsWith("set difficulty ") =>
        val diff = s.substring("set difficulty ".length).trim
        setDifficulty(diff)
        
      case "analytics" | "stats" | "progress" =>
        showAnalytics()
        
      case "help" =>
        showHelp()
        
      case _ =>
        // Process general input
        val response = ChatbotCore.handleUserInput(input, userPreferences)
        println(s"\nBot: $response")
        AnalyticsDashboard.logInteraction(input, response)
    }
  }
  
  /**
   * Starts a new quiz session
   */
  def startQuiz(): Unit = {
    userPreferences match {
      case Some(prefs) =>
        println("\nBot: What type of quiz would you like to take?")
        println("Available quiz types: vocabulary, grammar, translation, mcq, correction")
        print("\nYou: ")
        val quizTypeInput = readLine().trim.toLowerCase
        
        // Parse quiz type using pattern matching
        val quizType = quizTypeInput match {
          case "vocabulary" | "vocab" => Some(Vocabulary)
          case "grammar" => Some(Grammar)
          case "translation" | "translate" => Some(Translation)
          case "mcq" | "multiple choice" => Some(MCQ)
          case "correction" | "correct" => Some(Correction)
          case _ => None
        }
        
        quizType match {
          case Some(qt) =>
            // Generate quiz questions
            val questions = QuizGenerator.selectQuizQuestions(
              prefs.targetLanguage, 
              prefs.difficulty, 
              qt, 
              5 // 5 questions per quiz
            )
            
            currentQuizSession = Some(QuizSession(questions, List.empty, qt))
            inQuizMode = true
            currentQuestionIndex = 0
            
            // Display first question
            displayCurrentQuestion()
            
          case None =>
            println("\nBot: Sorry, I don't recognize that quiz type. Please try again.")
        }
        
      case None =>
        println("\nBot: Please set your language preferences before starting a quiz.")
        showSettings()
    }
  }
  
  /**
   * Handles user input during quiz mode
   */
  def handleQuizMode(input: String): Unit = {
    input.toLowerCase match {
      case "exit quiz" | "quit quiz" =>
        println("\nBot: Exiting quiz mode.")
        inQuizMode = false
        
      case _ =>
        currentQuizSession match {
          case Some(session) =>
            // Record the answer
            val updatedAnswers = session.userAnswers :+ input
            val currentQuestion = session.questions(currentQuestionIndex)
            
            // Evaluate the answer
            val (isCorrect, feedback) = QuizGenerator.evaluateQuizAnswer(input, currentQuestion.correctAnswer)
            println(s"\nBot: $feedback")
            
            // Update the quiz session immutably
            currentQuizSession = Some(QuizSession(
              session.questions,
              updatedAnswers,
              session.quizType
            ))
            
            // Move to next question or end quiz
            currentQuestionIndex += 1
            if (currentQuestionIndex < session.questions.length) {
              displayCurrentQuestion()
            } else {
              // Quiz completed
              val completedSession = currentQuizSession.get
              println(s"\nBot: ${QuizGenerator.summarizeQuizResults(completedSession)}")
              
              // Log the completed quiz
              AnalyticsDashboard.logQuizInteraction(completedSession)
              
              // Reset quiz mode
              inQuizMode = false
              currentQuizSession = None
              currentQuestionIndex = 0
            }
            
          case None =>
            println("\nBot: There was an error with the quiz. Exiting quiz mode.")
            inQuizMode = false
        }
    }
  }
  
  /**
   * Display the current question in the quiz
   */
  def displayCurrentQuestion(): Unit = {
    currentQuizSession match {
      case Some(session) if currentQuestionIndex < session.questions.length =>
        val question = session.questions(currentQuestionIndex)
        val questionText = QuizGenerator.presentQuizQuestion(question, currentQuestionIndex + 1)
        println(s"\nBot: $questionText")
        
      case _ =>
        println("\nBot: No questions available.")
    }
  }
  
  /**
   * Show and modify user settings
   */
  def showSettings(): Unit = {
    println("\nBot: ===== User Settings =====")
    println("Available languages: English, Spanish, French, German, Arabic")
    println("Available difficulty levels: Easy, Medium, Hard, Impossible")
    
    val currentSettings = userPreferences match {
      case Some(prefs) =>
        s"Mother Language: ${prefs.motherLanguage}\n" +
        s"Target Language: ${prefs.targetLanguage}\n" +
        s"Difficulty: ${prefs.difficulty}"
      case None =>
        "No preferences set yet."
    }
    
    println(s"\nCurrent settings:\n$currentSettings")
    println("\nTo update settings, use commands like:")
    println("- set mother language English")
    println("- set target language Spanish")
    println("- set difficulty Medium")
  }
  
  /**
   * Set mother language
   */
  def setMotherLanguage(langStr: String): Unit = {
    val language = parseLanguage(langStr)
    language match {
      case Some(lang) =>
        userPreferences = ChatbotCore.storeUserPreferences(
          motherLanguage = Some(lang),
          targetLanguage = userPreferences.map(_.targetLanguage),
          difficulty = userPreferences.map(_.difficulty),
          currentPreferences = userPreferences
        )
        println(s"\nBot: Mother language set to $lang")
        
      case None =>
        println("\nBot: Sorry, I don't recognize that language. Available options: English, Spanish, French, German, Arabic")
    }
  }
  
  /**
   * Set target language
   */
  def setTargetLanguage(langStr: String): Unit = {
    val language = parseLanguage(langStr)
    language match {
      case Some(lang) =>
        userPreferences = ChatbotCore.storeUserPreferences(
          motherLanguage = userPreferences.map(_.motherLanguage),
          targetLanguage = Some(lang),
          difficulty = userPreferences.map(_.difficulty),
          currentPreferences = userPreferences
        )
        println(s"\nBot: Target language set to $lang")
        
      case None =>
        println("\nBot: Sorry, I don't recognize that language. Available options: English, Spanish, French, German, Arabic")
    }
  }
  
  /**
   * Set difficulty level
   */
  def setDifficulty(diffStr: String): Unit = {
    val difficulty = parseDifficulty(diffStr)
    difficulty match {
      case Some(diff) =>
        userPreferences = ChatbotCore.storeUserPreferences(
          motherLanguage = userPreferences.map(_.motherLanguage),
          targetLanguage = userPreferences.map(_.targetLanguage),
          difficulty = Some(diff),
          currentPreferences = userPreferences
        )
        println(s"\nBot: Difficulty set to $diff")
        
      case None =>
        println("\nBot: Sorry, I don't recognize that difficulty level. Available options: Easy, Medium, Hard, Impossible")
    }
  }
  
  /**
   * Show analytics dashboard
   */
  def showAnalytics(): Unit = {
    println("\nBot: ===== Analytics Dashboard =====")
    
    // Get interaction analytics
    val interactionLog = AnalyticsDashboard.getInteractionLog()
    val interactionStats = AnalyticsDashboard.analyzeInteractions(
      interactionLog.map { case (id, input, response) => 
        InteractionLog(id, input, response, System.currentTimeMillis()) 
      }
    )
    
    println("\n== Interaction Statistics ==")
    println(s"Total Interactions: ${interactionStats("total_interactions")}")
    
    // Display language proficiency report if preferences are set
    userPreferences.foreach { prefs =>
      println(s"\n== Language Proficiency Report for ${prefs.targetLanguage} ==")
      
      val quizLogs = currentQuizSession.toList.map { session =>
        QuizLog(
          1, 
          session.quizType, 
          session.questions, 
          session.userAnswers, 
          session.calculateScore, 
          System.currentTimeMillis()
        )
      }
      
      val proficiencyReport = AnalyticsDashboard.generateLanguageProficiencyReport(
        quizLogs, 
        prefs.targetLanguage
      )
      
      println(s"Proficiency Level: ${proficiencyReport("proficiency_level")}")
      println(s"Overall Success Rate: ${proficiencyReport("overall_success_rate")}")
      
      if (proficiencyReport.contains("strengths")) {
        println(s"Strengths: ${proficiencyReport("strengths").asInstanceOf[List[String]].mkString(", ")}")
      }
      
      if (proficiencyReport.contains("areas_for_improvement")) {
        println(s"Areas for Improvement: ${proficiencyReport("areas_for_improvement").asInstanceOf[List[String]].mkString(", ")}")
      }
      
      println(s"Learning Trend: ${proficiencyReport("learning_trend")}")
      println(s"Total Quizzes Taken: ${proficiencyReport("total_quizzes_taken")}")
    }
  }
  
  /**
   * Show help information
   */
  def showHelp(): Unit = {
    println("\nBot: ===== Language Learning Bot Help =====")
    println("Available commands:")
    println("- quiz: Start a new quiz session")
    println("- settings: View and change your language preferences")
    println("- set mother language [language]: Set your native language")
    println("- set target language [language]: Set the language you want to learn")
    println("- set difficulty [level]: Set the difficulty level")
    println("- analytics: View your learning progress and statistics")
    println("- help: Show this help message")
    println("- exit: Close the application")
    println("\nDuring a quiz:")
    println("- Type your answer to respond to questions")
    println("- exit quiz: End the current quiz session")
  }
  
  /**
   * Parse language string to Language type using pattern matching
   */
  def parseLanguage(lang: String): Option[Language] = {
    lang.toLowerCase match {
      case "english" => Some(English)
      case "spanish" => Some(Spanish)
      case "french" => Some(French)
      case "german" => Some(German)
      case "arabic" => Some(Arabic)
      case _ => None
    }
  }
  
  /**
   * Parse difficulty string to Difficulty type using pattern matching
   */
  def parseDifficulty(diff: String): Option[Difficulty] = {
    diff.toLowerCase match {
      case "easy" => Some(Easy)
      case "medium" => Some(Medium)
      case "hard" => Some(Hard)
      case "impossible" => Some(Impossible)
      case _ => None
    }
  }
}
