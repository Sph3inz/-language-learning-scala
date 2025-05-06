package languagelearningbot

import scala.io.StdIn.readLine
import scala.util.{Try, Success, Failure}
import java.io.{File, PrintWriter, FileWriter, BufferedWriter}
import java.nio.file.{Files, Paths}
import scala.io.Source

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
  
  // Settings file constants
  val SETTINGS_FILE = "user_preferences.txt"
  
  // Try to load saved settings at startup
  try {
    loadPreferences() match {
      case Success(Some(loadedPrefs)) => 
        userPreferences = Some(loadedPrefs)
        // Set all flags to true since these were explicitly saved
        motherLanguageExplicitlySet = true
        targetLanguageExplicitlySet = true
        difficultyExplicitlySet = true
        println("Found saved settings. Loaded your previously saved preferences.")
      case _ => 
        // No saved settings or error loading - start fresh
    }
  } catch {
    case _: Throwable => 
      // Handle any errors gracefully
      println("Could not load settings. Starting with default configuration.")
  }
  
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
        
      case "save settings" | "save preferences" =>
        saveSettings()
        
      case "load settings" | "load preferences" =>
        loadSettings()
        
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
        println(formatDualLanguageResponse("Available quiz types: vocabulary, grammar, translation, mcq"))
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
            val currentQuestion = session.questions(currentQuestionIndex)
            
            // Evaluate the answer - now supporting both the answer text and letter choices
            // This returns (isCorrect, feedback, processedAnswer)
            val (isCorrect, feedback, processedAnswer) = QuizGenerator.evaluateQuizAnswer(input, currentQuestion.correctAnswer, Some(currentQuestion))
            
            // Record the processed answer instead of the raw input
            // This ensures letter answers (a,b,c,d) are converted to the actual option text
            val updatedAnswers = session.userAnswers :+ processedAnswer
            
            // For quiz feedback, use only the target language if set
            val translatedFeedback = userPreferences match {
              case Some(prefs) if targetLanguageExplicitlySet =>
                // Quiz feedback is only shown in target language
                ChatbotCore.translateText(feedback, prefs.targetLanguage)
              case Some(prefs) if motherLanguageExplicitlySet =>
                // If only mother language is set, use that
                ChatbotCore.translateText(feedback, prefs.motherLanguage)
              case _ => feedback
            }
            
            println(s"\nBot: $translatedFeedback")
            
            // Update the quiz session immutably with the processed answers
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
              
              // For quiz summary, use only the target language if set
              val translatedSummary = userPreferences match {
                case Some(prefs) if targetLanguageExplicitlySet =>
                  // Quiz summary is only shown in target language
                  ChatbotCore.translateText(summary, prefs.targetLanguage)
                case Some(prefs) if motherLanguageExplicitlySet =>
                  // If only mother language is set, use that
                  ChatbotCore.translateText(summary, prefs.motherLanguage)
                case _ => summary
              }
              
              println(s"\nBot: $translatedSummary")
              
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
        
        // For quiz questions, always use the target language if set
        val translatedText = userPreferences match {
          case Some(prefs) if targetLanguageExplicitlySet =>
            // Quiz content is only shown in target language
            ChatbotCore.translateText(questionText, prefs.targetLanguage)
          case Some(prefs) if motherLanguageExplicitlySet =>
            // If only mother language is set, use that
            ChatbotCore.translateText(questionText, prefs.motherLanguage)
          case _ => questionText
        }
        
        println(s"\nBot: $translatedText")
        
      case _ =>
        println(s"\nBot: ${formatDualLanguageResponse("No questions available.")}")
    }
  }
  
  /**
   * Show and modify user settings
   */
  def showSettings(): Unit = {
    // Display settings only in mother language (with English as fallback)
    val displayLanguage = userPreferences.map(_.motherLanguage).getOrElse(English)
    
    // Helper function to translate text to only mother language
    def translateToMotherLang(text: String): String = {
      ChatbotCore.translateText(text, displayLanguage)
    }
    
    println(s"\nBot: ===== ${translateToMotherLang("User Settings")} =====")
    println(s"${translateToMotherLang("Available languages")}: English, Spanish, French, German, Arabic")
    println(s"${translateToMotherLang("Available difficulty levels")}: Easy, Medium, Hard, Impossible")
    
    println(s"\n${translateToMotherLang("Current settings")}:")
    
    userPreferences match {
      case Some(prefs) =>
        // Mother language display
        if (prefs.motherLanguage == English && !motherLanguageExplicitlySet) {
          println(s"${translateToMotherLang("Mother Language")}: ${translateToMotherLang("Not set yet")}")
        } else {
          println(s"${translateToMotherLang("Mother Language")}: ${languageToString(prefs.motherLanguage)}")
        }
        
        // Target language display
        if (prefs.targetLanguage == English && !targetLanguageExplicitlySet) {
          println(s"${translateToMotherLang("Target Language")}: ${translateToMotherLang("Not set yet")}")
        } else {
          println(s"${translateToMotherLang("Target Language")}: ${languageToString(prefs.targetLanguage)}")
        }
        
        // Difficulty display
        if (prefs.difficulty == Easy && !difficultyExplicitlySet) {
          println(s"${translateToMotherLang("Difficulty")}: ${translateToMotherLang("Not set yet")}")
        } else {
          println(s"${translateToMotherLang("Difficulty")}: ${difficultyToString(prefs.difficulty)}")
        }
        
      case None =>
        println(translateToMotherLang("No preferences set yet."))
    }
    
    println(s"\n${translateToMotherLang("To update settings, use commands like")}:")
    println(s"- ${translateToMotherLang("set mother language")} English")
    println(s"- ${translateToMotherLang("set target language")} Spanish")
    println(s"- ${translateToMotherLang("set difficulty")} Medium")
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
    // Display analytics only in mother language (with English as fallback)
    val displayLanguage = userPreferences.map(_.motherLanguage).getOrElse(English)
    
    // Helper function to translate text to only mother language
    def translateToMotherLang(text: String): String = {
      ChatbotCore.translateText(text, displayLanguage)
    }
    
    println(s"\nBot: ===== ${translateToMotherLang("Analytics Dashboard")} =====")
    
    // Get interaction analytics
    val interactionLog = AnalyticsDashboard.getInteractionLog()
    val interactionStats = AnalyticsDashboard.analyzeInteractions(
      interactionLog.map { case (id, input, response) => 
        InteractionLog(id, input, response, System.currentTimeMillis()) 
      }
    )
    
    println(s"\n== ${translateToMotherLang("Interaction Statistics")} ==")
    println(s"${translateToMotherLang("Total Interactions")}: ${interactionStats("total_interactions")}")
    
    // Display language proficiency report if preferences are set
    userPreferences.foreach { prefs =>
      println(s"\n== ${translateToMotherLang("Language Proficiency Report for")} ${languageToString(prefs.targetLanguage)} ==")
      
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
      
      println(s"${translateToMotherLang("Proficiency Level")}: ${proficiencyReport("proficiency_level")}")
      println(s"${translateToMotherLang("Overall Success Rate")}: ${proficiencyReport("overall_success_rate")}")
      
      if (proficiencyReport.contains("strengths")) {
        println(s"${translateToMotherLang("Strengths")}: ${proficiencyReport("strengths").asInstanceOf[List[String]].mkString(", ")}")
      }
      
      if (proficiencyReport.contains("areas_for_improvement")) {
        println(s"${translateToMotherLang("Areas for Improvement")}: ${proficiencyReport("areas_for_improvement").asInstanceOf[List[String]].mkString(", ")}")
      }
      
      println(s"${translateToMotherLang("Learning Trend")}: ${proficiencyReport("learning_trend")}")
      println(s"${translateToMotherLang("Total Quizzes Taken")}: ${proficiencyReport("total_quizzes_taken")}")
    }
  }
  
  /**
   * Show help information
   */
  def showHelp(): Unit = {
    // Display help only in mother language (with English as fallback)
    val displayLanguage = userPreferences.map(_.motherLanguage).getOrElse(English)
    
    // Helper function to translate text to only mother language
    def translateToMotherLang(text: String): String = {
      ChatbotCore.translateText(text, displayLanguage)
    }
    
    println(s"\nBot: ===== ${translateToMotherLang("Language Learning Bot Help")} =====")
    println(s"${translateToMotherLang("Available commands")}:")
    println(s"- ${translateToMotherLang("quiz")}: ${translateToMotherLang("Start a new quiz session")}")
    println(s"- ${translateToMotherLang("settings")}: ${translateToMotherLang("View and change your language preferences")}")
    println(s"- ${translateToMotherLang("save settings")}: ${translateToMotherLang("Save your current preferences to file")}")
    println(s"- ${translateToMotherLang("load settings")}: ${translateToMotherLang("Load your saved preferences from file")}")
    println(s"- ${translateToMotherLang("set mother language")} [${translateToMotherLang("language")}]: ${translateToMotherLang("Set your native language")}")
    println(s"- ${translateToMotherLang("set target language")} [${translateToMotherLang("language")}]: ${translateToMotherLang("Set the language you want to learn")}")
    println(s"- ${translateToMotherLang("set difficulty")} [${translateToMotherLang("level")}]: ${translateToMotherLang("Set the difficulty level")}")
    println(s"- ${translateToMotherLang("analytics")}: ${translateToMotherLang("View your learning progress and statistics")}")
    println(s"- ${translateToMotherLang("help")}: ${translateToMotherLang("Show this help message")}")
    println(s"- ${translateToMotherLang("exit")}: ${translateToMotherLang("Close the application")}")
    
    println(s"\n${translateToMotherLang("During a quiz")}:")
    println(s"- ${translateToMotherLang("Type your answer to respond to questions")}")
    println(s"- ${translateToMotherLang("exit quiz")}: ${translateToMotherLang("End the current quiz session")}")
  }
  
  /**
   * Save user preferences to file
   */
  def saveSettings(): Unit = {
    userPreferences match {
      case Some(prefsToSave) => 
        println(s"\nBot: ${formatDualLanguageResponse("Do you want to save your current settings to file? (yes/no)")}")
        print("\nYou: ")
        
        val response = readLine().trim.toLowerCase
        if (response == "yes" || response == "y") {
          try {
            savePreferences(prefsToSave) match {
              case Success(_) => 
                println(s"\nBot: ${formatDualLanguageResponse("Settings saved successfully!")}")
              case Failure(error) => 
                println(s"\nBot: ${formatDualLanguageResponse(s"Failed to save settings: ${error.getMessage}")}")
            }
          } catch {
            case ex: Throwable =>
              println(s"\nBot: ${formatDualLanguageResponse(s"Error saving settings: ${ex.getMessage}")}")
          }
        } else {
          println(s"\nBot: ${formatDualLanguageResponse("Settings were not saved.")}")
        }
        
      case None => 
        println(s"\nBot: ${formatDualLanguageResponse("No preferences to save. Please set your preferences first.")}")
    }
  }
  
  /**
   * Save user preferences to file
   */
  def savePreferences(preferences: UserPreferences): Try[Unit] = {
    Try {
      // Convert preferences to a string representation
      val prefsString = serializePreferences(preferences)
      
      // Write to file
      val writer = new BufferedWriter(new FileWriter(SETTINGS_FILE))
      writer.write(prefsString)
      writer.close()
    }
  }
  
  /**
   * Convert preferences to string format for file storage
   */
  private def serializePreferences(prefs: UserPreferences): String = {
    // Simple format: motherLanguage|targetLanguage|difficulty|name
    val motherLang = languageToString(prefs.motherLanguage)
    val targetLang = languageToString(prefs.targetLanguage)
    val diff = difficultyToString(prefs.difficulty)
    val name = prefs.name.getOrElse("")
    
    s"$motherLang|$targetLang|$diff|$name"
  }
  
  /**
   * Load user preferences from file
   */
  def loadSettings(): Unit = {
    println(s"\nBot: ${formatDualLanguageResponse("Do you want to load your saved settings? This will override your current settings. (yes/no)")}")
    print("\nYou: ")
    
    val response = readLine().trim.toLowerCase
    if (response == "yes" || response == "y") {
      try {
        loadPreferences() match {
          case Success(Some(loadedPrefs)) => 
            userPreferences = Some(loadedPrefs)
            // Set flags based on loaded preferences
            motherLanguageExplicitlySet = true
            targetLanguageExplicitlySet = true
            difficultyExplicitlySet = true
            
            println(s"\nBot: ${formatDualLanguageResponse("Settings loaded successfully!")}")
            showSettings() // Display the loaded settings
            
          case Success(None) => 
            println(s"\nBot: ${formatDualLanguageResponse("No saved settings found.")}")
            
          case Failure(error) => 
            println(s"\nBot: ${formatDualLanguageResponse(s"Failed to load settings: ${error.getMessage}")}")
        }
      } catch {
        case ex: Throwable =>
          println(s"\nBot: ${formatDualLanguageResponse(s"Error loading settings: ${ex.getMessage}")}")
      }
    } else {
      println(s"\nBot: ${formatDualLanguageResponse("Settings were not loaded.")}")
    }
  }
  
  /**
   * Load user preferences from file
   */
  def loadPreferences(): Try[Option[UserPreferences]] = {
    if (Files.exists(Paths.get(SETTINGS_FILE))) {
      Try {
        val source = Source.fromFile(SETTINGS_FILE)
        val content = source.getLines().mkString("\n")
        source.close()
        
        deserializePreferences(content)
      }
    } else {
      Success(None) // File doesn't exist yet
    }
  }
  
  /**
   * Parse preferences string back into UserPreferences object
   */
  private def deserializePreferences(prefsString: String): Option[UserPreferences] = {
    val parts = prefsString.split("\\|")
    
    if (parts.length >= 3) {
      val motherLang = parseLanguage(parts(0))
      val targetLang = parseLanguage(parts(1))
      val difficulty = parseDifficulty(parts(2))
      
      val name = if (parts.length > 3 && parts(3).nonEmpty) Some(parts(3)) else None
      
      for {
        ml <- motherLang
        tl <- targetLang
        d <- difficulty
      } yield UserPreferences(ml, tl, d, name)
    } else {
      None
    }
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
    // Check if this is quiz-related content
    val isQuizInstruction = response.contains("quiz type") || 
                          response.contains("Available quiz types") ||
                          response.contains("starting a quiz") ||
                          response.contains("Exiting quiz mode") ||
                          response.contains("error with the quiz")
    
    // Check if this is quiz content (choices, corrections, etc.)
    val isQuizContent = response.contains("vocabulary") ||
                       response.contains("grammar") ||
                       response.contains("translation") ||
                       response.contains("mcq") ||
                       response.contains("correction")
    
    userPreferences match {
      case Some(prefs) if motherLanguageExplicitlySet && targetLanguageExplicitlySet => 
        if (isQuizInstruction) {
          // For quiz instructions, show only in mother language
          ChatbotCore.translateText(response, prefs.motherLanguage)
        } else if (isQuizContent) {
          // For quiz content, show only in target language
          ChatbotCore.translateText(response, prefs.targetLanguage)
        } else {
          // For regular content, show in both languages
          val targetLangResponse = ChatbotCore.translateText(response, prefs.targetLanguage)
          val motherLangResponse = ChatbotCore.translateText(response, prefs.motherLanguage)
          
          // If the languages are the same, don't duplicate
          if (prefs.targetLanguage == prefs.motherLanguage) {
            targetLangResponse
          } else {
            s"$targetLangResponse\n(${languageToString(prefs.motherLanguage)}): $motherLangResponse"
          }
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
