package languagelearningbot

import scala.io.StdIn.readLine
import scala.util.{Try, Success, Failure}
import java.io.{File, PrintWriter, FileWriter, BufferedWriter}
import java.nio.file.{Files, Paths}
import scala.io.Source
import services.AuthService
import models.{SignUpRequest, UserCredentials, GuestUserRequest}
import services.UserHistoryService
import models.QuizQuestion

object Main extends App {
  println("Starting Language Learning Bot...")
  
  // Initialize AuthService
  val authService = new AuthService()
  val userHistoryService = new UserHistoryService()
  
  // State variables (kept immutable by always reassigning)
  var userPreferences: Option[UserPreferences] = None
  var currentQuizSession: Option[QuizSession] = None
  var inQuizMode: Boolean = false
  var currentQuestionIndex: Int = 0
  var currentUser: Option[models.User] = None
  var pendingQuizType: Option[QuizType] = None  // New variable to store pending quiz type
  
  // Flags to track which preferences have been explicitly set
  var motherLanguageExplicitlySet: Boolean = false
  var targetLanguageExplicitlySet: Boolean = false
  var difficultyExplicitlySet: Boolean = false
  
  // Settings file constants
  val SETTINGS_FILE = "user_preferences.txt"
  
  // Authentication menu
  def showAuthMenu(): Unit = {
    println("\n=== Authentication Menu ===")
    println("1. Sign Up")
    println("2. Login")
    println("3. Exit")
    print("\nChoose an option: ")
    
    val input = readLine().trim.toLowerCase
    
    input match {
      // Number choices
      case "1" => handleSignUp()
      case "2" => handleLogin()
      case "3" => System.exit(0)
      
      // Text commands
      case "signup" | "sign up" | "register" | "registration" => handleSignUp()
      case "login" | "signin" | "sign in" => handleLogin()
      case "exit" | "quit" | "close" => System.exit(0)
      
      case _ => 
        println("Invalid option. Please try again.")
        println("You can use numbers (1-3) or type commands like 'signup', 'login', or 'exit'")
        showAuthMenu()
    }
  }
  
  def handleSignUp(): Unit = {
    println("\n=== Sign Up ===")
    print("Username: ")
    val username = readLine().trim
    print("Email: ")
    val email = readLine().trim
    print("Password: ")
    val password = readLine().trim
    
    val signUpRequest = SignUpRequest(username, email, password)
    authService.signUp(signUpRequest) match {
      case Success(user) =>
        currentUser = Some(user)
        println(s"Welcome, ${user.username}!")
        loadUserPreferences()
      case Failure(ex) =>
        println(s"Sign up failed: ${ex.getMessage}")
        println("Would you like to try again? (yes/no)")
        readLine().trim.toLowerCase match {
          case "yes" | "y" => handleSignUp()
          case _ => showAuthMenu()
        }
    }
  }
  
  def handleLogin(): Unit = {
    println("\n=== Login ===")
    print("Email: ")
    val email = readLine().trim
    print("Password: ")
    val password = readLine().trim
    
    val credentials = UserCredentials(email, password)
    authService.login(credentials) match {
      case Success(user) =>
        // Store the user with their original ID
        currentUser = Some(user)
        println(s"Welcome back, ${user.username}!")
        loadUserPreferences()
      case Failure(ex) =>
        println(s"Login failed: ${ex.getMessage}")
        println("Would you like to try again? (yes/no)")
        readLine().trim.toLowerCase match {
          case "yes" | "y" => handleLogin()
          case _ => showAuthMenu()
        }
    }
  }
  
  def loadUserPreferences(): Unit = {
    currentUser match {
      case Some(user) =>
        // Try to load saved settings for registered users
        loadPreferences() match {
          case Success(Some(loadedPrefs)) => 
            userPreferences = Some(loadedPrefs)
            motherLanguageExplicitlySet = true
            targetLanguageExplicitlySet = true
            difficultyExplicitlySet = true
            println("Loaded your previously saved preferences.")
          case _ => 
            println("No saved preferences found. Let's set up your preferences.")
            showSettings()
        }
      case None =>
        println("Please sign in.")
        showAuthMenu()
    }
  }
  
  // Start with authentication
  showAuthMenu()
  
  // Display welcome message
  val welcomeMessage = ChatbotCore.greetUser(userPreferences, currentUser)
  println(formatDualLanguageResponse(welcomeMessage))
  println(formatDualLanguageResponse("Type 'help' for available commands."))
  
  // Main interaction loop
  var running = true
  while (running) {
    print("\nYou: ")
    val userInput = readLine().trim
    
    // Check for exit command
    if (userInput.toLowerCase match {
      case "exit" | "eit" | "exif" | "exi" | "exitt" | "exxit" | "exiit" |
           "quit" | "qit" | "quitt" | "qutt" |
           "end" | "stp" | "stopp" | "stop" => true
      case _ => false
    }) {
      println("\nBot: Thank you for using the Language Learning Bot. Goodbye!")
      running = false
    } else if (userInput.toLowerCase == "logout") {
      currentUser = None
      println("\nBot: You have been logged out.")
      showAuthMenu()
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
    // First, check if the input is a thank you message
    val thanksWords = Set("thanks", "thank", "thx", "ty", "thankyou", "thank you", "gracias", "merci", "danke")
    if (thanksWords.exists(word => input.toLowerCase.contains(word))) {
      // For thank you messages, just respond with a simple acknowledgment and don't start a new quiz
      val response = ChatbotCore.handleUserInput(input, userPreferences, currentUser)
      val formattedResponse = formatDualLanguageResponse(response)
      println(s"\nBot: $formattedResponse")
      
      // Log the interaction if user is logged in
      currentUser.foreach { user =>
        userHistoryService.addInteraction(
          userId = user.id,
          username = user.username,
          email = user.email,
          question = input,
          response = response,
          category = "thanks"
        )
      }
      return
    }
    
        // Removed the difficulty regex pattern matching since we now handle it explicitly in the case statement
    
    // First, check for direct quiz type requests using ChatbotCore's detectQuizType
    val tokens = ChatbotCore.parseInput(input)
    
    // Special case for generic quiz requests without a specific quiz type
    if (input.trim.toLowerCase == "quiz" || 
        input.trim.toLowerCase == "give me quiz" || 
        input.trim.toLowerCase == "start quiz" || 
        input.trim.toLowerCase == "take quiz") {
      // Ask what type of quiz they want
      startQuiz()
      return
    }
    
    val detectedQuizType = ChatbotCore.detectQuizType(tokens)
    
    if (detectedQuizType.isDefined) {
      val quizTypeValue = detectedQuizType.get match {
        case "vocabulary" => Vocabulary
        case "grammar" => Grammar
        case "translation" => Translation
        case "mcq" => MCQ
        case _ => Vocabulary // fallback to vocabulary
      }
      
      userPreferences match {
        case Some(prefs) =>
          // Start quiz with the specified type
          val questions = QuizGenerator.selectQuizQuestions(
            prefs.targetLanguage,
            prefs.difficulty,
            quizTypeValue,
            5
          )
          currentQuizSession = Some(QuizSession(questions, List.empty, quizTypeValue))
          inQuizMode = true
          currentQuestionIndex = 0
          displayCurrentQuestion()
          
          // Save quiz start interaction
          currentUser.foreach { user =>
            userHistoryService.addInteraction(
              userId = user.id,
              username = user.username,
              email = user.email,
              question = input,
              response = s"Started a ${quizTypeValue} quiz",
              category = "quiz"
            )
          }
          return
        case None =>
          println(s"\nBot: ${formatDualLanguageResponse("Please set your language preferences before starting a quiz.")}")
          showSettings()
          return
      }
    }
    
    // Check language learning intent patterns
    val learnWords = Set("learn", "study", "practice", "master", "speak", "teach", "understand", "know", "use")
    val languageWords = Set("english", "spanish", "french", "german")
    
    // Check if input contains both a learning verb and a language name anywhere
    val containsLearnVerb = learnWords.exists(word => input.toLowerCase.contains(word))
    val containedLanguages = languageWords.filter(lang => input.toLowerCase.contains(lang))
    
    if (containsLearnVerb && containedLanguages.nonEmpty) {
      // If multiple languages are mentioned, try to determine the most likely target language
      // Default to the first one if we can't tell
      val language = if (containedLanguages.size > 1) {
        // Look for explicit indicators like "to" or "in" followed by a language
        val toPattern = containedLanguages.find(lang => 
          input.toLowerCase.contains(s"to $lang") || 
          input.toLowerCase.contains(s"in $lang") ||
          input.toLowerCase.contains(s"target $lang"))
        
        toPattern.getOrElse(containedLanguages.head)
      } else {
        containedLanguages.head
      }
      
      // Set it as target language
      setTargetLanguage(language)
      // Prompt for quiz - using two separate translations for better localization
      val part1 = ChatbotCore.translateText("Great! I'll help you learn ", userPreferences.map(_.motherLanguage).getOrElse(English))
      val part2 = ChatbotCore.translateText(". Would you like to take a quiz to practice?", userPreferences.map(_.motherLanguage).getOrElse(English))
      println(s"\nBot: $part1$language$part2")
      println(formatDualLanguageResponse("Available quiz types: vocabulary, grammar, translation, mcq"))
      // Save interaction
      currentUser.foreach { user =>
        userHistoryService.addInteraction(
          userId = user.id,
          username = user.username,
          email = user.email,
          question = input,
          response = s"Great! I'll help you learn $language. Would you like to take a quiz to practice?",
          category = "language_selection"
        )
      }
      return
    } else if (containsLearnVerb && containedLanguages.isEmpty) {
      // If user mentions learning but no specific language
      val msg = ChatbotCore.translateText("Great news! Which language would you like to study?", userPreferences.map(_.motherLanguage).getOrElse(English))
      println(s"\nBot: $msg")
      // Wait for the next input and handle language selection
      print("\nYou: ")
      val nextInput = readLine().trim.toLowerCase
      
      // See if the input contains any language name
      val detectedLanguage = languageWords.find(lang => nextInput.contains(lang))
      
      // Try to parse language directly if it's just a language name
      val parsedLang = parseLanguage(nextInput)
      
      if (parsedLang.isDefined) {
        // If it's a direct language name like "French"
        setTargetLanguage(languageToString(parsedLang.get).toLowerCase)
        startQuiz()
        return
      } else if (detectedLanguage.isDefined) {
        // If a language is mentioned as part of a sentence
        setTargetLanguage(detectedLanguage.get)
        startQuiz()
        return
      } else {
        // Try one more method - check for partial matches
        val partialMatches = languageWords.filter(lang => 
          lang.startsWith(nextInput) || nextInput.startsWith(lang.substring(0, 3)))
          
        if (partialMatches.nonEmpty) {
          setTargetLanguage(partialMatches.head)
          startQuiz()
          return
        } else {
          // If not a valid language, fallback to normal handling
          println(s"\nBot: ${formatDualLanguageResponse("I didn't recognize that language. Please specify one of: English, Spanish, French, or German.")}")
          handleRegularMode(nextInput)
          return
        }
      }
    }
    
    // Check if input is a direct quiz type request
    val directQuizType = input.trim.toLowerCase match {
      case "vocabulary" | "vocab" | "vocabulario" | "vocabulaire" | "vokabular" | "المفردات" | "1" => Some(Vocabulary)
      case "grammar" | "gramática" | "grammaire" | "grammatik" | "القواعد" | "2" => Some(Grammar)
      case "translation" | "translate" | "traducción" | "traduction" | "übersetzung" | "الترجمة" | "3" => Some(Translation)
      case "mcq" | "multiple choice" | "opción múltiple" | "choix multiple" | "multiple choice" | "الاختيار من متعدد" | "4" => Some(MCQ)
      case _ => None
    }
    
    if (directQuizType.isDefined) {
      userPreferences match {
        case Some(prefs) =>
          // Start quiz with the specified type
          val questions = QuizGenerator.selectQuizQuestions(
            prefs.targetLanguage,
            prefs.difficulty,
            directQuizType.get,
            5
          )
          currentQuizSession = Some(QuizSession(questions, List.empty, directQuizType.get))
          inQuizMode = true
          currentQuestionIndex = 0
          displayCurrentQuestion()
          
          // Save quiz start interaction
          currentUser.foreach { user =>
            userHistoryService.addInteraction(
              userId = user.id,
              username = user.username,
              email = user.email,
              question = input,
              response = s"Started a ${directQuizType.get} quiz",
              category = "quiz"
            )
          }
          return
        case None =>
          println(s"\nBot: ${formatDualLanguageResponse("Please set your language preferences before starting a quiz.")}")
          showSettings()
          return
      }
    }
    
    // Allow starting a quiz directly by typing the quiz type
    val quizTypeMap = Map(
      "vocabulary" -> Vocabulary,
      "vocab" -> Vocabulary,
      "grammar" -> Grammar,
      "translation" -> Translation,
      "translate" -> Translation,
      "mcq" -> MCQ,
      "mcq quiz" -> MCQ,
      "multiple choice quiz" -> MCQ,
      "mcw" -> MCQ,
      "mcc" -> MCQ,
      "multiple choice" -> MCQ,
      "multichoice" -> MCQ,
      "multi-choice" -> MCQ,
      "multi choice" -> MCQ,
      "multiplechoice" -> MCQ,
      "multiple-choice" -> MCQ
    )
    if (quizTypeMap.contains(input.trim.toLowerCase)) {
      // Store the quiz type and ask for confirmation
      pendingQuizType = Some(quizTypeMap(input.trim.toLowerCase))
      println(s"\nBot: ${formatDualLanguageResponse("Would you like to start a quiz? If yes, please type 'start quiz' to begin.")}")
      return
    }
    
    // If the input contains 'quiz' or 'quizzes', start the quiz process
    if (input.toLowerCase.contains("quizzes")) {
      startQuiz()
      return
    }
    
    // Proceed with regular command matching
    // Check first for exit quiz commands
    val exitPhrases = Set("exit quiz", "quit quiz", "end quiz", "stop quiz", "cancel quiz")
    val containsExitPhrase = exitPhrases.exists(phrase => input.toLowerCase.contains(phrase))
    
    if (containsExitPhrase) {
      // Handle users trying to exit quiz when not in quiz mode
      println(s"\nBot: ${formatDualLanguageResponse("You're not currently in a quiz.")}")
      return
    }
    
    input.toLowerCase match {
            // Removed regex pattern matching for difficulty since we handle it explicitly via the direct command
        
      case "quiz" | "start quiz" =>
        pendingQuizType match {
          case Some(qt) =>
            // Start quiz with the pending type
            userPreferences match {
              case Some(prefs) =>
                val questions = QuizGenerator.selectQuizQuestions(
                  prefs.targetLanguage,
                  prefs.difficulty,
                  qt,
                  5
                )
                currentQuizSession = Some(QuizSession(questions, List.empty, qt))
                inQuizMode = true
                currentQuestionIndex = 0
                pendingQuizType = None  // Clear the pending type
                displayCurrentQuestion()
              case None =>
                println(s"\nBot: ${formatDualLanguageResponse("Please set your language preferences before starting a quiz.")}")
                showSettings()
            }
          case None =>
            startQuiz()  // If no pending type, show quiz type selection
        }
        // Save quiz start interaction
        currentUser.foreach { user =>
          userHistoryService.addInteraction(
            userId = user.id,
            username = user.username,
            email = user.email,
            question = "quiz",
            response = "Started a new quiz session",
            category = "quiz"
          )
        }
        
      case "settings" | "preferences" | "show settings" | "show preferences" =>
        showSettings()
        // Save settings interaction to history
        currentUser.foreach { user =>
          userHistoryService.addInteraction(
            userId = user.id,
            username = user.username,
            email = user.email,
            question = "settings",
            response = "Displayed user settings",
            category = "system"
          )
        }
        
      case "save settings" | "save preferences" =>
        saveSettings()
        // Save save settings interaction to history
        currentUser.foreach { user =>
          userHistoryService.addInteraction(
            userId = user.id,
            username = user.username,
            email = user.email,
            question = "save settings",
            response = "Attempted to save user settings",
            category = "system"
          )
        }
        
      case "load settings" | "load preferences" =>
        loadSettings()
        // Save load settings interaction to history
        currentUser.foreach { user =>
          userHistoryService.addInteraction(
            userId = user.id,
            username = user.username,
            email = user.email,
            question = "load settings",
            response = "Attempted to load user settings",
            category = "system"
          )
        }
        
      case s if s.startsWith("set mother language ") || s.startsWith("set mother langauge ") =>
        val lang = s.substring("set mother language ".length).trim
        setMotherLanguage(lang)
        
      case s if s.startsWith("set target language ") || s.startsWith("set target langauge ") =>
        val lang = s.substring("set target language ".length).trim
        setTargetLanguage(lang)
        
      case s if s.startsWith("set difficulty ") =>
        val diff = s.substring("set difficulty ".length).trim
        setDifficulty(diff)
        
      case "analytics" | "stats" | "progress" =>
        showAnalytics()
        // Save analytics interaction to history
        currentUser.foreach { user =>
          userHistoryService.addInteraction(
            userId = user.id,
            username = user.username,
            email = user.email,
            question = "analytics",
            response = "Displayed user analytics",
            category = "system"
          )
        }
        
      case "history" | "my history" =>
        currentUser match {
          case Some(user) =>
            val history = userHistoryService.displayUserHistory(user.id)
            println(s"\nBot: ${formatDualLanguageResponse(history)}")
          case None =>
            println(s"\nBot: ${formatDualLanguageResponse("Please sign in to view your history.")}")
        }
        
      case "help" =>
        val helpMessage = """
          |Available commands:
          |1. chat - Start a conversation
          |2. quiz - Take a quiz
          |3. history - View your learning history
          |4. preferences - Set your language preferences
          |5. help - Show this help message
          |6. logout - Log out
          |7. exit - Exit the application
          |""".stripMargin
        println(helpMessage)
        // Save help interaction to history
        userHistoryService.addInteraction(
          userId = currentUser.get.id,
          username = currentUser.get.username,
          email = currentUser.get.email,
          question = "help",
          response = helpMessage,
          category = "system"
        )
        
      case _ =>
        // Process general input using ChatbotCore which will return the "unknown" response for unrecognized input
        val response = ChatbotCore.handleUserInput(input, userPreferences, currentUser)
        val formattedResponse = formatDualLanguageResponse(response)
        println(s"\nBot: $formattedResponse")
        
        // Log the interaction if user is logged in
        currentUser.foreach { user =>
          userHistoryService.addInteraction(
            userId = user.id,
            username = user.username,
            email = user.email,
            question = input,
            response = response,
            category = ChatbotCore.categorizeInput(input)
          )
        }
    }
  }
  
  /**
   * Starts a new quiz session
   */
  def startQuiz(): Unit = {
    userPreferences match {
      case Some(prefs) =>
        // Create a recursive function to keep asking for quiz type until valid
        def askForQuizType(): Unit = {
          // Get quiz selection message directly in mother language - English
          val quizMessage = """What type of quiz would you like to take? We offer:
1. Vocabulary - Test your knowledge of words
2. Grammar - Practice language rules
3. Translation - Translate between languages
4. Multiple Choice (MCQ) - General language questions

Please type the number (1-4) or name of the quiz you'd like to take."""
          
          // Always use the mother language for quiz instructions
          val translatedMessage = if (prefs.motherLanguage != English && motherLanguageExplicitlySet) {
            // Only translate if mother language is not English and is explicitly set
            ChatbotCore.translateText(quizMessage, prefs.motherLanguage)
          } else {
            // Default to English
            quizMessage
          }
          
          println(s"\nBot: $translatedMessage")
          print("\nYou: ")
          val quizTypeInput = readLine().trim.toLowerCase
          
          // Save the quiz type response
          currentUser.foreach { user =>
            userHistoryService.addInteraction(
              userId = user.id,
              username = user.username,
              email = user.email,
              question = "What type of quiz would you like to take?",
              response = quizTypeInput,
              category = "quiz"
            )
          }
          
          // Parse quiz type using pattern matching
          val quizType = quizTypeInput match {
            case "exit quiz" | "quit quiz" | "end quiz" | "stop quiz" | "cancel" | "back" =>
              println(s"\nBot: ${formatDualLanguageResponse("Cancelling quiz setup.")}")
              
              // Save quiz exit interaction
              currentUser.foreach { user =>
                userHistoryService.addInteraction(
                  userId = user.id,
                  username = user.username,
                  email = user.email,
                  question = quizTypeInput,
                  response = "Cancelled quiz setup",
                  category = "quiz"
                )
              }
              
              return // Exit the askForQuizType function early
              
            case "vocabulary" | "vocab" | "1" | "vocabulario" | "vocabulaire" | "vokabular" | "المفردات" => Some(Vocabulary)
            case "grammar" | "2" | "gramática" | "grammaire" | "grammatik" | "القواعد" => Some(Grammar)
            case "translation" | "translate" | "3" | "traducción" | "traduction" | "übersetzung" | "الترجمة" => Some(Translation)
            case "mcq" | "multiple choice" | "4" | "opción múltiple" | "choix multiple" | "multiple choice" | "الاختيار من متعدد" => Some(MCQ)
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
              // Instead of returning to regular mode, inform the user and ask again
              println(s"\nBot: ${formatDualLanguageResponse("Sorry, I don't recognize that quiz type. Please try again.")}")
              askForQuizType() // Recursively ask again
          }
        }
        
        // Start the recursive function
        askForQuizType()
        
      case None =>
        println(s"\nBot: ${formatDualLanguageResponse("Please set your language preferences before starting a quiz.")}")
        showSettings()
    }
  }
  
  /**
   * Handles user input during quiz mode
   */
  def handleQuizMode(input: String): Unit = {
    // Check if the input contains any exit phrases regardless of position
    val exitPhrases = Set("exit quiz", "quit quiz", "end quiz", "stop quiz", "cancel quiz")
    val containsExitPhrase = exitPhrases.exists(phrase => input.toLowerCase.contains(phrase))
    
    if (containsExitPhrase) {
      println(s"\nBot: ${formatDualLanguageResponse("Exiting quiz mode.")}")
      inQuizMode = false
      currentQuizSession = None
      currentQuestionIndex = 0
      // Save quiz exit interaction
      currentUser.foreach { user =>
        userHistoryService.addInteraction(
          userId = user.id,
          username = user.username,
          email = user.email,
          question = "exit quiz",
          response = "Exited quiz mode",
          category = "quiz"
        )
      }
    } else if (input.toLowerCase == "exit" || input.toLowerCase == "quit") {
      println(s"\nBot: ${formatDualLanguageResponse("To exit the quiz, please type 'exit quiz' or 'quit quiz'.")}")
    } else {
      currentQuizSession match {
        case Some(session) =>
          val currentQuestion = session.questions(currentQuestionIndex)
          
          // Evaluate the answer
          val (isCorrect, feedback, processedAnswer) = QuizGenerator.evaluateQuizAnswer(input, currentQuestion.correctAnswer, Some(currentQuestion))
          
          // Save the quiz answer interaction
          currentUser.foreach { user =>
            userHistoryService.addInteraction(
              userId = user.id,
              username = user.username,
              email = user.email,
              question = s"Quiz Question ${currentQuestionIndex + 1}: ${currentQuestion.prompt}",
              response = s"Answer: $input, Feedback: $feedback",
              category = "quiz"
            )
          }
          
          // Record the processed answer and whether it was correct
          val updatedAnswers = session.userAnswers :+ (if (isCorrect) currentQuestion.correctAnswer else processedAnswer)
          
          // For quiz feedback, use only the target language if set
          val translatedFeedback = userPreferences match {
            case Some(prefs) if targetLanguageExplicitlySet =>
              ChatbotCore.translateText(feedback, prefs.targetLanguage)
            case Some(prefs) if motherLanguageExplicitlySet =>
              ChatbotCore.translateText(feedback, prefs.motherLanguage)
            case _ => feedback
          }
          
          println(s"\nBot: $translatedFeedback")
          
          // Update the quiz session
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
            
            // Save quiz results if user is logged in
            currentUser.foreach { user =>
              val quizQuestions = completedSession.questions.zip(completedSession.userAnswers).map {
                case (question, answer) =>
                  QuizQuestion(
                    question = question.prompt,
                    correctAnswer = question.correctAnswer,
                    userAnswer = answer,
                    isCorrect = answer == question.correctAnswer
                  )
              }
              
              // Calculate correct answers for this quiz
              val correctAnswers = quizQuestions.count(_.isCorrect)
              
              // Get total correct answers across all quizzes
              val totalCorrectAnswers = userHistoryService.getTotalCorrectAnswers(user.id)
              val totalQuestions = userHistoryService.getTotalQuestions(user.id)
              
              userHistoryService.addQuizResult(
                userId = user.id,
                username = user.username,
                email = user.email,
                quizType = completedSession.quizType.toString,
                score = completedSession.calculateScore,
                totalQuestions = completedSession.questions.length,
                correctAnswers = correctAnswers,  // Use the actual count of correct answers
                questions = quizQuestions,
                userAnswers = completedSession.userAnswers
              )
              
              // Add total stats to summary
              val totalStats = s"\nTotal Questions Answered: $totalQuestions\nTotal Correct Answers: $totalCorrectAnswers"
              val finalSummary = summary + totalStats
              
              // For quiz summary, use only the target language if set
              val translatedSummary = userPreferences match {
                case Some(prefs) if targetLanguageExplicitlySet =>
                  ChatbotCore.translateText(finalSummary, prefs.targetLanguage)
                case Some(prefs) if motherLanguageExplicitlySet =>
                  ChatbotCore.translateText(finalSummary, prefs.motherLanguage)
                case _ => finalSummary
              }
              
              println(s"\nBot: $translatedSummary")
            }
            
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
    println(s"${translateToMotherLang("Available languages")}: English, Spanish, French, German")
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
        
        // Automatically save settings to file
        userPreferences.foreach(prefs => {
          try {
            savePreferences(prefs) match {
              case Success(_) => 
                // Settings saved silently, no need to notify user
              case Failure(error) => 
                println(s"\nBot: ${formatDualLanguageResponse(s"Note: Could not save settings to file: ${error.getMessage}")}")
            }
          } catch {
            case ex: Throwable =>
              println(s"\nBot: ${formatDualLanguageResponse(s"Note: Error saving settings: ${ex.getMessage}")}")
          }
        })
        
        // Log the setting change if user is logged in
        currentUser.foreach { user =>
          userHistoryService.addInteraction(
            userId = user.id,
            username = user.username,
            email = user.email,
            question = s"set mother language ${langStr}",
            response = response,
            category = "settings"
          )
        }
        
      case None =>
        println(s"\nBot: ${formatDualLanguageResponse("Sorry, I don't recognize that language. Available options: English, Spanish, French, German")}")
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
        
        // Automatically save settings to file
        userPreferences.foreach(prefs => {
          try {
            savePreferences(prefs) match {
              case Success(_) => 
                // Settings saved silently, no need to notify user
              case Failure(error) => 
                println(s"\nBot: ${formatDualLanguageResponse(s"Note: Could not save settings to file: ${error.getMessage}")}")
            }
          } catch {
            case ex: Throwable =>
              println(s"\nBot: ${formatDualLanguageResponse(s"Note: Error saving settings: ${ex.getMessage}")}")
          }
        })
        
        // Log the setting change if user is logged in
        currentUser.foreach { user =>
          userHistoryService.addInteraction(
            userId = user.id,
            username = user.username,
            email = user.email,
            question = s"set target language ${langStr}",
            response = response,
            category = "settings"
          )
        }
        
      case None =>
        println(s"\nBot: ${formatDualLanguageResponse("Sorry, I don't recognize that language. Available options: English, Spanish, French, German")}")
    }
  }
  
  /**
   * Set difficulty level
   */
  def setDifficulty(diffStr: String): Unit = {
    val difficulty = parseDifficulty(diffStr)
    difficulty match {
      case Some(diff) =>
        // Create new preferences with the updated difficulty
        userPreferences = userPreferences match {
          case Some(prefs) =>
            // Preserve existing preferences, only update difficulty
            ChatbotCore.storeUserPreferences(
              motherLanguage = Some(prefs.motherLanguage),
              targetLanguage = Some(prefs.targetLanguage),
              difficulty = Some(diff),
              currentPreferences = Some(prefs)
            )
          case None =>
            // If no preferences exist yet, create with defaults and new difficulty
            ChatbotCore.storeUserPreferences(
              motherLanguage = Some(English), // Default to English
              targetLanguage = Some(English), // Default to English
              difficulty = Some(diff),
              currentPreferences = None
            )
        }
        
        // Mark difficulty as explicitly set
        difficultyExplicitlySet = true
        
        // Simple direct confirmation message
        println(s"\nBot: ✅ Difficulty level set to ${difficultyToString(diff)}.")
        
        // Automatically save settings to file
        userPreferences.foreach(prefs => {
          try {
            savePreferences(prefs) match {
              case Success(_) => 
                // Settings saved silently, no need to notify user
              case Failure(error) => 
                println(s"\nBot: ${formatDualLanguageResponse(s"Note: Could not save settings to file: ${error.getMessage}")}")
            }
          } catch {
            case ex: Throwable =>
              println(s"\nBot: ${formatDualLanguageResponse(s"Note: Error saving settings: ${ex.getMessage}")}")
          }
        })
        
        // Log the setting change if user is logged in
        currentUser.foreach { user =>
          userHistoryService.addInteraction(
            userId = user.id,
            username = user.username,
            email = user.email,
            question = s"set difficulty ${diffStr}",
            response = s"Difficulty set to ${difficultyToString(diff)}",
            category = "settings"
          )
        }
        
      case None =>
        // Error message for invalid difficulty
        println(s"\nBot: ❌ Invalid difficulty level. Available options are: Easy, Medium, Hard, Impossible")
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
    
    // === Top Users ===
    println(s"\n== ${translateToMotherLang("Top Users")}")
    val topUsers = userHistoryService.getTopUsers(5) // Assume returns List[(String, Int)] (username, interactionCount)
    topUsers.foreach { case (username, count) =>
      println(s"- $username: $count ${translateToMotherLang("interactions")}")
    }
    
    // === Number of Interactions ===
    println(s"\n== ${translateToMotherLang("Number of Interactions")}")
    val totalInteractions = userHistoryService.getTotalInteractions
    println(s"${translateToMotherLang("Total interactions")}: $totalInteractions")
    
    // === Performance Summaries ===
    println(s"\n== ${translateToMotherLang("Performance Summaries")}")
    val avgScore = userHistoryService.getAverageQuizScore(currentUser.map(_.id).getOrElse(""))
    val totalQuizzes = userHistoryService.getTotalQuizzes(Some(currentUser.map(_.id).getOrElse("")))
    println(s"${translateToMotherLang("Average quiz score")}: $avgScore")
    println(s"${translateToMotherLang("Total quizzes taken")}: $totalQuizzes")
    
    // === User Breakdown ===
    println(s"\n== ${translateToMotherLang("User Breakdown")}")
    val allUsers = userHistoryService.getAllUserHistories
    if (allUsers.isEmpty) {
      println(translateToMotherLang("No users found."))
    } else {
      allUsers.foreach { user =>
        val quizzes = user.quizResults.length
        val correct = user.quizResults.map(_.correctAnswers).sum
        println(s"${translateToMotherLang("Username")}: ${user.username}")
        println(s"${translateToMotherLang("Interactions")}: ${user.interactions.length}")
        println(s"${translateToMotherLang("Quizzes Taken")}: $quizzes")
        println(s"${translateToMotherLang("Correct Answers")}: $correct")
        println("------------------------------")
      }
    }
    
    // Existing proficiency report, if any
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
    println(s"- ${translateToMotherLang("history")}: ${translateToMotherLang("View your interaction history")}")
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
    
    // Check if this is quiz content (choices, , etc.)
    val isQuizContent = response.contains("vocabulary") ||
                       response.contains("grammar") ||
                       response.contains("translation") ||
                       response.contains("mcq")
    
    // Check if this is a greeting response
    val isGreeting = response.startsWith("Hello") || 
                    response.contains("Language Learning Bot") ||
                    response.contains("I'm your Language Learning Bot")
    
    userPreferences match {
      case Some(prefs) if motherLanguageExplicitlySet && targetLanguageExplicitlySet => 
        if (isQuizInstruction) {
          // For quiz instructions, show only in mother language
          ChatbotCore.translateText(response, prefs.motherLanguage)
        } else if (isQuizContent) {
          // For quiz content, show only in target language
          ChatbotCore.translateText(response, prefs.targetLanguage)
        } else if (isGreeting) {
          // For greetings, show only in mother language
          ChatbotCore.translateText(response, prefs.motherLanguage)
        } else {
          // Only show English translation as requested
          ChatbotCore.translateText(response, English)
        }
      
      case Some(prefs) if targetLanguageExplicitlySet => 
        // If only target language is set
        ChatbotCore.translateText(response, prefs.targetLanguage)
        
      case Some(prefs) if motherLanguageExplicitlySet => 
        // If only mother language is set
        ChatbotCore.translateText(response, prefs.motherLanguage)
        
      case _ => 
        // If no languages are set, return original response
        response
    }
  }
}
