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
  
  // Flags to track which preferences have been explicitly set
  var motherLanguageExplicitlySet: Boolean = false
  var targetLanguageExplicitlySet: Boolean = false
  var difficultyExplicitlySet: Boolean = false
  
  // Display welcome message
  val welcomeMessage = ChatbotCore.greetUser(userPreferences)
  println(formatDualLanguageResponse(welcomeMessage))
  println(formatDualLanguageResponse("Type 'help' for available commands."))
  
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
        val formattedResponse = formatDualLanguageResponse(response)
        println(s"\nBot: $formattedResponse")
        AnalyticsDashboard.logInteraction(input, response)
    }
  }
  
  /**
   * Starts a new quiz session
   */
  def startQuiz(): Unit = {
    userPreferences match {
      case Some(prefs) =>
        println(s"\nBot: ${formatDualLanguageResponse("What type of quiz would you like to take?")}")
        println(formatDualLanguageResponse("Available quiz types: vocabulary, grammar, translation, mcq, correction"))
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
            println(s"\nBot: ${formatDualLanguageResponse("Sorry, I don't recognize that quiz type. Please try again.")}")
        }
        
      case None =>
        println(s"\nBot: ${formatDualLanguageResponse("Please set your language preferences before starting a quiz.")}")
        showSettings()
    }
  }
  
  /**
   * Handles user input during quiz mode
   */
  def handleQuizMode(input: String): Unit = {
    input.toLowerCase match {
      case "exit quiz" | "quit quiz" =>
        println(s"\nBot: ${formatDualLanguageResponse("Exiting quiz mode.")}")
        inQuizMode = false
        
      case _ =>
        currentQuizSession match {
          case Some(session) =>
            // Record the answer
            val updatedAnswers = session.userAnswers :+ input
            val currentQuestion = session.questions(currentQuestionIndex)
            
            // Evaluate the answer
            val (isCorrect, feedback) = QuizGenerator.evaluateQuizAnswer(input, currentQuestion.correctAnswer)
            println(s"\nBot: ${formatDualLanguageResponse(feedback)}")
            
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
              val summary = QuizGenerator.summarizeQuizResults(completedSession)
              println(s"\nBot: ${formatDualLanguageResponse(summary)}")
              
              // Log the completed quiz
              AnalyticsDashboard.logQuizInteraction(completedSession)
              
              // Reset quiz mode
              inQuizMode = false
              currentQuizSession = None
              currentQuestionIndex = 0
            }
            
          case None =>
            println(s"\nBot: ${formatDualLanguageResponse("There was an error with the quiz. Exiting quiz mode.")}")
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
        println(s"\nBot: ${formatDualLanguageResponse(questionText)}")
        
      case _ =>
        println(s"\nBot: ${formatDualLanguageResponse("No questions available.")}")
    }
  }
  
  /**
   * Show and modify user settings
   */
  def showSettings(): Unit = {
    println(s"\nBot: ${formatDualLanguageResponse("===== User Settings =====")}")
    println(formatDualLanguageResponse("Available languages: English, Spanish, French, German, Arabic"))
    println(formatDualLanguageResponse("Available difficulty levels: Easy, Medium, Hard, Impossible"))
    
    val currentSettings = userPreferences match {
      case Some(prefs) =>
        val motherLangString = if (prefs.motherLanguage == English && !motherLanguageExplicitlySet) {
          formatDualLanguageResponse("Mother Language: Not set yet\n")
        } else {
          formatDualLanguageResponse(s"Mother Language: ${languageToString(prefs.motherLanguage)}\n")
        }
        
        val targetLangString = if (prefs.targetLanguage == English && !targetLanguageExplicitlySet) {
          formatDualLanguageResponse("Target Language: Not set yet\n")
        } else {
          formatDualLanguageResponse(s"Target Language: ${languageToString(prefs.targetLanguage)}\n")
        }
        
        val difficultyString = if (prefs.difficulty == Easy && !difficultyExplicitlySet) {
          formatDualLanguageResponse("Difficulty: Not set yet")
        } else {
          formatDualLanguageResponse(s"Difficulty: ${difficultyToString(prefs.difficulty)}")
        }
        
        motherLangString + targetLangString + difficultyString
      case None =>
        formatDualLanguageResponse("No preferences set yet.")
    }
    
    println(s"\n${formatDualLanguageResponse("Current settings:")}\n$currentSettings")
    println(formatDualLanguageResponse("\nTo update settings, use commands like:"))
    println(formatDualLanguageResponse("- set mother language English"))
    println(formatDualLanguageResponse("- set target language Spanish"))
    println(formatDualLanguageResponse("- set difficulty Medium"))
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
          targetLanguage = if (userPreferences.isDefined) Some(userPreferences.get.targetLanguage) else None,
          difficulty = if (userPreferences.isDefined) Some(userPreferences.get.difficulty) else None,
          currentPreferences = userPreferences
        )
        // Mark mother language as explicitly set
        motherLanguageExplicitlySet = true
        val response = s"Mother language set to ${languageToString(lang)}"
        println(s"\nBot: ${formatDualLanguageResponse(response)}")
        
      case None =>
        println(s"\nBot: ${formatDualLanguageResponse("Sorry, I don't recognize that language. Available options: English, Spanish, French, German, Arabic")}")
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
          motherLanguage = if (userPreferences.isDefined) Some(userPreferences.get.motherLanguage) else None,
          targetLanguage = Some(lang),
          difficulty = if (userPreferences.isDefined) Some(userPreferences.get.difficulty) else None,
          currentPreferences = userPreferences
        )
        // Mark target language as explicitly set
        targetLanguageExplicitlySet = true
        val response = s"Target language set to ${languageToString(lang)}"
        println(s"\nBot: ${formatDualLanguageResponse(response)}")
        
      case None =>
        println(s"\nBot: ${formatDualLanguageResponse("Sorry, I don't recognize that language. Available options: English, Spanish, French, German, Arabic")}")
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
          motherLanguage = if (userPreferences.isDefined) Some(userPreferences.get.motherLanguage) else None,
          targetLanguage = if (userPreferences.isDefined) Some(userPreferences.get.targetLanguage) else None,
          difficulty = Some(diff),
          currentPreferences = userPreferences
        )
        // Mark difficulty as explicitly set
        difficultyExplicitlySet = true
        val response = s"Difficulty set to ${difficultyToString(diff)}"
        println(s"\nBot: ${formatDualLanguageResponse(response)}")
        
      case None =>
        println(s"\nBot: ${formatDualLanguageResponse("Sorry, I don't recognize that difficulty level. Available options: Easy, Medium, Hard, Impossible")}")
    }
  }
  
  /**
   * Show analytics dashboard
   */
  def showAnalytics(): Unit = {
    println(s"\nBot: ${formatDualLanguageResponse("===== Analytics Dashboard =====")}")
    
    // Get interaction analytics
    val interactionLog = AnalyticsDashboard.getInteractionLog()
    val interactionStats = AnalyticsDashboard.analyzeInteractions(
      interactionLog.map { case (id, input, response) => 
        InteractionLog(id, input, response, System.currentTimeMillis()) 
      }
    )
    
    println(formatDualLanguageResponse("\n== Interaction Statistics =="))
    println(formatDualLanguageResponse(s"Total Interactions: ${interactionStats("total_interactions")}"))
    
    // Display language proficiency report if preferences are set
    userPreferences.foreach { prefs =>
      println(formatDualLanguageResponse(s"\n== Language Proficiency Report for ${languageToString(prefs.targetLanguage)} =="))
      
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
      
      println(formatDualLanguageResponse(s"Proficiency Level: ${proficiencyReport("proficiency_level")}"))
      println(formatDualLanguageResponse(s"Overall Success Rate: ${proficiencyReport("overall_success_rate")}"))
      
      if (proficiencyReport.contains("strengths")) {
        println(formatDualLanguageResponse(s"Strengths: ${proficiencyReport("strengths").asInstanceOf[List[String]].mkString(", ")}"))
      }
      
      if (proficiencyReport.contains("areas_for_improvement")) {
        println(formatDualLanguageResponse(s"Areas for Improvement: ${proficiencyReport("areas_for_improvement").asInstanceOf[List[String]].mkString(", ")}"))
      }
      
      println(formatDualLanguageResponse(s"Learning Trend: ${proficiencyReport("learning_trend")}"))
      println(formatDualLanguageResponse(s"Total Quizzes Taken: ${proficiencyReport("total_quizzes_taken")}"))
    }
  }
  
  /**
   * Show help information
   */
  def showHelp(): Unit = {
    println(s"\nBot: ${formatDualLanguageResponse("===== Language Learning Bot Help =====")}")
    println(formatDualLanguageResponse("Available commands:"))
    println(formatDualLanguageResponse("- quiz: Start a new quiz session"))
    println(formatDualLanguageResponse("- settings: View and change your language preferences"))
    println(formatDualLanguageResponse("- set mother language [language]: Set your native language"))
    println(formatDualLanguageResponse("- set target language [language]: Set the language you want to learn"))
    println(formatDualLanguageResponse("- set difficulty [level]: Set the difficulty level"))
    println(formatDualLanguageResponse("- analytics: View your learning progress and statistics"))
    println(formatDualLanguageResponse("- help: Show this help message"))
    println(formatDualLanguageResponse("- exit: Close the application"))
    println("\n" + formatDualLanguageResponse("During a quiz:"))
    println(formatDualLanguageResponse("- Type your answer to respond to questions"))
    println(formatDualLanguageResponse("- exit quiz: End the current quiz session"))
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
  
  /**
   * Helper method to convert Language to a string representation
   */
  def languageToString(language: Language): String = {
    language match {
      case English => "English"
      case Spanish => "Spanish"
      case French => "French"
      case German => "German"
      case Arabic => "Arabic"
      case _ => "Unknown"
    }
  }
  
  /**
   * Helper method to convert Difficulty to a string representation
   */
  def difficultyToString(difficulty: Difficulty): String = {
    difficulty match {
      case Easy => "Easy"
      case Medium => "Medium"
      case Hard => "Hard"
      case Impossible => "Impossible"
      case _ => "Unknown"
    }
  }
  
  /**
   * Format a response in both target and mother languages
   */
  def formatDualLanguageResponse(response: String): String = {
    userPreferences match {
      case Some(prefs) if motherLanguageExplicitlySet && targetLanguageExplicitlySet => 
        // If both languages are set, translate to both
        val targetLangResponse = ChatbotCore.translateText(response, prefs.targetLanguage)
        val motherLangResponse = ChatbotCore.translateText(response, prefs.motherLanguage)
        
        // If the languages are the same, don't duplicate
        if (prefs.targetLanguage == prefs.motherLanguage) {
          targetLangResponse
        } else {
          s"$targetLangResponse\n(${languageToString(prefs.motherLanguage)}): $motherLangResponse"
        }
      
      case Some(prefs) if targetLanguageExplicitlySet => 
        // If only target language is set
        ChatbotCore.translateText(response, prefs.targetLanguage)
        
      case Some(prefs) if motherLanguageExplicitlySet => 
        // If only mother language is set
        ChatbotCore.translateText(response, prefs.motherLanguage)
        
      case _ => 
        // If no languages are set, default to English
        response
    }
  }
}
