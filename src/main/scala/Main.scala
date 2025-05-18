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
  
  // Settings manager instance
  val settingsManager = new SettingsManager()
  
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
  
  def isValidEmail(email: String): Boolean = {
    val emailRegex = """^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$""".r
    emailRegex.findFirstMatchIn(email).isDefined
  }
  
  def handleSignUp(): Unit = {
    println("\n=== Sign Up ===")
    print("Username: ")
    val username = readLine().trim
    print("Email: ")
    val email = readLine().trim
    
    if (!isValidEmail(email)) {
      println("Invalid email format. Please enter a valid email address (e.g., user@example.com)")
      println("Would you like to try again? (yes/no)")
      readLine().trim.toLowerCase match {
        case "yes" | "y" => handleSignUp()
        case _ => showAuthMenu()
      }
      return
    }
    
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
    
    if (!isValidEmail(email)) {
      println("Invalid email format. Please enter a valid email address (e.g., user@example.com)")
      println("Would you like to try again? (yes/no)")
      readLine().trim.toLowerCase match {
        case "yes" | "y" => handleLogin()
        case _ => showAuthMenu()
      }
      return
    }
    
    print("Password: ")
    val password = readLine().trim
    
    val credentials = UserCredentials(email, password)
    authService.login(credentials) match {
      case Success(user) =>
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

    // Check early for preferences/settings request with possible misspellings
    if (containsPreferencesCommand(input)) {
      showSettings()
      // Save settings interaction to history
      currentUser.foreach { user =>
        userHistoryService.addInteraction(
          userId = user.id,
          username = user.username,
          email = user.email,
          question = input,
          response = "Displayed user settings",
          category = "system"
        )
      }
      return
    }
    
    // First, check for direct settings commands (case insensitive)
    val lowerInput = input.toLowerCase()
    
    // Standard command processing for settings
    if (lowerInput.startsWith("set mother language ") || lowerInput.startsWith("set mother langauge ")) {
      val lang = input.substring(lowerInput.indexOf("language") + 9).trim
      setMotherLanguage(lang)
      return
    } else if (lowerInput.startsWith("set target language ") || lowerInput.startsWith("set target langauge ")) {
      val lang = input.substring(lowerInput.indexOf("language") + 9).trim
      setTargetLanguage(lang)
      return
    }
    
    // Continue with normal flow - check for direct quiz type requests
    val tokens = ChatbotCore.parseInput(input)
    
    // Special case for generic quiz requests without a specific quiz type
    val quizStartPhrases = Set(
      "quiz", 
      "give me quiz", 
      "start quiz", 
      "take quiz",
      "i want quiz",
      "i want start quiz",
      "i want to start quiz",
      "i want a quiz",
      "i'd like a quiz",
      "i would like a quiz",
      "i want to take a quiz",
      "can i take a quiz",
      "can i start a quiz"
    )
    
    if (quizStartPhrases.exists(phrase => input.trim.toLowerCase.contains(phrase)) || 
        (input.toLowerCase.contains("want") && input.toLowerCase.contains("quiz")) ||
        (input.toLowerCase.contains("start") && input.toLowerCase.contains("quiz"))) {
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
      case "mcq" | "multiple choice" | "opción múltiple" | "choix multiple" | "الاختيار من متعدد" | "4" => Some(MCQ)
      case _ => None
    }
    
    if (directQuizType.isDefined) {
      userPreferences match {
        case Some(prefs) =>
          // Start quiz with the specified type
          val questions = QuizGenerator.selectQuizQuestions(
            prefs.targetLanguage,
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
      case "quiz" | "start quiz" =>
        pendingQuizType match {
          case Some(qt) =>
            // Start quiz with the pending type
            userPreferences match {
              case Some(prefs) =>
                val questions = QuizGenerator.selectQuizQuestions(
                  prefs.targetLanguage,
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
        
      case "save settings" | "save preferences" =>
        userPreferences match {
          case Some(prefsToSave) => 
            // Directly save settings without confirmation
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
            
            // Save to user_preferences.txt file as well
            try {
              val motherLang = languageToString(prefsToSave.motherLanguage)
              val targetLang = languageToString(prefsToSave.targetLanguage)
              val name = prefsToSave.name.getOrElse("")
              
              val content = s"$motherLang|$targetLang|$name"
              val file = new File("user_preferences.txt")
              val writer = new PrintWriter(file)
              writer.write(content)
              writer.close()
            } catch {
              case ex: Throwable =>
                println(s"\nBot: ${formatDualLanguageResponse(s"Error saving to user_preferences.txt: ${ex.getMessage()}")}")
            }
            
          case None => 
            println(s"\nBot: ${formatDualLanguageResponse("No preferences to save. Please set your preferences first.")}")
        }
        
        // Save save settings interaction to history
        currentUser.foreach { user =>
          userHistoryService.addInteraction(
            userId = user.id,
            username = user.username,
            email = user.email,
            question = "save settings",
            response = "Saved user settings",
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
        
      case "chat" =>
        // Start a weather-based conversation in target language with translation
        startWeatherChat()
        
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
      
      case _ if ChatbotCore.categorizeInput(input) == "weather" =>
        // Handle weather-related queries
        val weatherResponse = ChatbotCore.handleWeatherQuery(input)
        val formattedResponse = formatDualLanguageResponse(weatherResponse)
        println(s"\nBot: $formattedResponse")
        
        // Log the weather interaction if user is logged in
        currentUser.foreach { user =>
          userHistoryService.addInteraction(
            userId = user.id,
            username = user.username,
            email = user.email,
            question = input,
            response = weatherResponse,
            category = "weather"
          )
        }
      
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
   * Checks if input contains variations of "preferences" or "settings"
   */
  def containsPreferencesCommand(input: String): Boolean = {
    val inputLower = input.toLowerCase
    
    // Common variations and misspellings of preferences/settings
    val prefVariations = Set(
      "preferences", "preference", "preferances", "preferance", "prefrences", "prefrence",
      "prefs", "pref", "preffs", "prefernces", "prefrances", "preferenses", "prefrences",
      "settings", "setting", "setings", "seting", "setings", "settigns", "options", "option"
    )
    
    // Check if any variation is contained in the input
    prefVariations.exists(variation => inputLower.contains(variation))
  }
  
  /**
   * Starts a new quiz session
   */
  def startQuiz(): Unit = {
    userPreferences match {
      case Some(prefs) =>
        // Create a recursive function to keep asking for quiz type until valid
        def askForQuizType(): Unit = {
          // Get quiz selection message directly in mother language
          val quizMessage = """What type of quiz would you like to take? We offer:
1. Vocabulary - Test your knowledge of words
2. Grammar - Practice language rules
3. Translation - Translate between languages
4. Multiple Choice (MCQ) - General language questions

Please type the number (1-4) or name of the quiz you'd like to take."""
          
          // Always use the mother language for quiz instructions
          val translatedMessage = userPreferences match {
            case Some(p) if motherLanguageExplicitlySet =>
              // Use mother language for quiz instructions
              ChatbotCore.translateText(quizMessage, p.motherLanguage)
            case _ =>
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
            case "mcq" | "multiple choice" | "4" | "opción múltiple" | "choix multiple" | "الاختيار من متعدد" => Some(MCQ)
            case _ => None
          }
          
          quizType match {
            case Some(qt) =>
              // Generate quiz questions
              val questions = QuizGenerator.selectQuizQuestions(
                prefs.targetLanguage, 
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
            handleQuizCompletion()
          }
          
        case None =>
          println(s"\nBot: ${formatDualLanguageResponse("There was an error with the quiz. Exiting quiz mode.")}")
          inQuizMode = false
      }
    }
  }
  
  /**
   * Handles quiz completion
   */
  def handleQuizCompletion(): Unit = {
    currentQuizSession match {
      case Some(session) =>
        val score = session.calculateScore
        val accuracy = session.calculateAccuracy.toInt
        
        // First save the quiz result
        currentUser.foreach { user =>
          userHistoryService.addQuizResult(
            userId = user.id,
            username = user.username,
            email = user.email,
            quizType = session.quizType.toString,
            score = accuracy,
            totalQuestions = session.questions.length,
            correctAnswers = score,
            questions = session.questions.zip(session.userAnswers).map { case (q, ua) =>
              QuizQuestion(
                question = q.prompt,
                correctAnswer = q.correctAnswer,
                userAnswer = ua,
                isCorrect = q.correctAnswer.toLowerCase == ua.toLowerCase
              )
            },
            userAnswers = session.userAnswers
          )
        }

        // Then get the updated totals
        val (totalQuestions, totalCorrectAnswers) = currentUser.map { user =>
          (userHistoryService.getTotalQuestions(user.id), userHistoryService.getTotalCorrectAnswers(user.id))
        }.getOrElse((0, 0))

        // Format and display results
        val performanceFeedback = accuracy match {
          case a if a >= 90 => "Excellent work! You've mastered this material!"
          case a if a >= 80 => "Great job! You have a strong grasp of this content."
          case a if a >= 70 => "Good work! You're on the right track."
          case a if a >= 60 => "Not bad, but there's room for improvement."
          case _ => "You might want to review this material more thoroughly."
        }

        val results = s"""Quiz Complete!
                       |Score: $score out of ${session.questions.length} correct
                       |Accuracy: $accuracy%
                       |$performanceFeedback
                       |Total Questions Answered: $totalQuestions
                       |Total Correct Answers: $totalCorrectAnswers""".stripMargin

        println(s"\nBot: ${formatDualLanguageResponse(results)}")
        
        // Reset quiz state
        currentQuizSession = None
        inQuizMode = false
        currentQuestionIndex = 0
        pendingQuizType = None

      case None =>
        println("\nBot: No active quiz session found.")
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
    println(s"\nBot: ${formatDualLanguageResponse("You can change your settings for:")}")
    println(s"- ${formatDualLanguageResponse("Mother language")}")
    println(s"- ${formatDualLanguageResponse("Target language")}")
    
    // Display current settings if they exist
    userPreferences match {
      case Some(prefs) =>
        println(s"\n${formatDualLanguageResponse("Your current settings:")}")
        println(s"${formatDualLanguageResponse("Mother language")}: ${languageToString(prefs.motherLanguage)}")
        println(s"${formatDualLanguageResponse("Target language")}: ${languageToString(prefs.targetLanguage)}")
      case None =>
        println(s"\n${formatDualLanguageResponse("You haven't set any preferences yet.")}")
    }
    
    println(s"\n${formatDualLanguageResponse("What would you like to update?")}")
    
    // Save this interaction
    currentUser.foreach { user =>
      userHistoryService.addInteraction(
        userId = user.id,
        username = user.username,
        email = user.email,
        question = "settings", 
        response = "You can change your settings for:\n- Mother language\n- Target language\nWhat would you like to update?",
        category = "settings"
      )
    }
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
          currentPreferences = userPreferences
        )
        // Mark mother language as explicitly set
        motherLanguageExplicitlySet = true
        val response = s"Mother language set to ${languageToString(lang)}"
        println(s"\nBot: ${formatDualLanguageResponse(response)}")
        
        // Automatically save mother language to file using the settings manager
        try {
          settingsManager.saveMotherLanguage(lang) match {
            case Success(_) => 
              // Settings saved silently, no need to notify user
            case Failure(error) => 
              println(s"\nBot: ${formatDualLanguageResponse(s"Note: Could not save mother language setting: ${error.getMessage}")}")
          }
        } catch {
          case ex: Throwable =>
            println(s"\nBot: ${formatDualLanguageResponse(s"Note: Error saving mother language setting: ${ex.getMessage}")}")
        }
        
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
          currentPreferences = userPreferences
        )
        // Mark target language as explicitly set
        targetLanguageExplicitlySet = true
        val response = s"Target language set to ${languageToString(lang)}"
        println(s"\nBot: ${formatDualLanguageResponse(response)}")
        
        // Automatically save target language to file using the settings manager
        try {
          settingsManager.saveTargetLanguage(lang) match {
            case Success(_) => 
              // Settings saved silently, no need to notify user
            case Failure(error) => 
              println(s"\nBot: ${formatDualLanguageResponse(s"Note: Could not save target language setting: ${error.getMessage}")}")
          }
        } catch {
          case ex: Throwable =>
            println(s"\nBot: ${formatDualLanguageResponse(s"Note: Error saving target language setting: ${ex.getMessage}")}")
        }
        
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
   * Save user preferences using the settings manager
   */
  def savePreferences(preferences: UserPreferences): Try[Unit] = {
    // Use the settings manager to save all preferences
    settingsManager.saveAllPreferences(preferences)
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
   * Load user preferences using the settings manager
   */
  def loadPreferences(): Try[Option[UserPreferences]] = {
    // Use the settings manager to load all preferences
    settingsManager.loadAllPreferences()
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
   * Format a response in both target and mother languages
   */
  def formatDualLanguageResponse(response: String): String = {
    // Check if this is quiz-related content
    val isQuizInstruction = response.contains("quiz type") || 
                          response.contains("Available quiz types") ||
                          response.contains("starting a quiz") ||
                          response.contains("Exiting quiz mode") ||
                          response.contains("error with the quiz") ||
                          response.contains("What type of quiz would you like to take?") ||
                          response.contains("Would you like to take a quiz") ||
                          response.contains("Please type the number") ||
                          response.contains("Vocabulary -") ||
                          response.contains("Grammar -") ||
                          response.contains("Translation -") ||
                          response.contains("MCQ -") ||
                          response.contains("quiz") ||
                          response.contains("Quiz")
    
    // If this is quiz instruction, don't translate it
    if (isQuizInstruction) {
      return response
    }
    
    // Otherwise, apply standard translation - don't show multiple language tags
    userPreferences match {
      case Some(prefs) if prefs.motherLanguage != prefs.targetLanguage =>
        // Only translate to mother language if it's not English
        if (prefs.motherLanguage != English) {
          val motherLangTranslation = ChatbotCore.translateText(response, prefs.motherLanguage)
          s"$motherLangTranslation\n(in English: $response)"
        } else {
          // If mother language is English, just return response in English
          response
        }
        
      case _ => 
        // If no preferences or same language, just return the original
        response
    }
  }
  
  /**
   * Starts a weather-based conversation in the target language
   * with translations to practice language skills
   */
  def startWeatherChat(): Unit = {
    userPreferences match {
      case Some(prefs) if targetLanguageExplicitlySet && motherLanguageExplicitlySet => 
        val weatherQuestions = Map(
          "How is the weather today?" -> Map(
            "Spanish" -> "¿Cómo está el clima hoy?",
            "French" -> "Comment est le temps aujourd'hui?",
            "German" -> "Wie ist das Wetter heute?"
          ),
          "Is it hot or cold outside?" -> Map(
            "Spanish" -> "¿Hace calor o frío afuera?",
            "French" -> "Fait-il chaud ou froid dehors?",
            "German" -> "Ist es draußen heiß oder kalt?"
          ),
          "Do you like this weather?" -> Map(
            "Spanish" -> "¿Te gusta este clima?",
            "French" -> "Est-ce que tu aimes ce temps?",
            "German" -> "Magst du dieses Wetter?"
          ),
          "What's your favorite season?" -> Map(
            "Spanish" -> "¿Cuál es tu estación favorita?",
            "French" -> "Quelle est ta saison préférée?",
            "German" -> "Was ist deine Lieblingsjahreszeit?"
          ),
          "Is it raining today?" -> Map(
            "Spanish" -> "¿Está lloviendo hoy?",
            "French" -> "Pleut-il aujourd'hui?",
            "German" -> "Regnet es heute?"
          )
        )
        
        val followupQuestions = Map(
          "Do you prefer hot or cold weather?" -> Map(
            "Spanish" -> "¿Prefieres el clima caliente o frío?",
            "French" -> "Préférez-vous le temps chaud ou froid?",
            "German" -> "Bevorzugen Sie heißes oder kaltes Wetter?"
          ),
          "What do you usually wear in this weather?" -> Map(
            "Spanish" -> "¿Qué sueles usar con este clima?",
            "French" -> "Que portez-vous habituellement par ce temps?",
            "German" -> "Was tragen Sie normalerweise bei diesem Wetter?"
          ),
          "What activities do you enjoy in this weather?" -> Map(
            "Spanish" -> "¿Qué actividades disfrutas con este clima?",
            "French" -> "Quelles activités aimez-vous faire par ce temps?",
            "German" -> "Welche Aktivitäten machen Sie gerne bei diesem Wetter?"
          ),
          "How does the weather affect your mood?" -> Map(
            "Spanish" -> "¿Cómo afecta el clima tu estado de ánimo?",
            "French" -> "Comment la météo affecte-t-elle votre humeur?",
            "German" -> "Wie beeinflusst das Wetter Ihre Stimmung?"
          ),
          "What's the weather like in your hometown?" -> Map(
            "Spanish" -> "¿Cómo es el clima en tu ciudad natal?",
            "French" -> "Quel temps fait-il dans votre ville natale?",
            "German" -> "Wie ist das Wetter in Ihrer Heimatstadt?"
          )
        )
        
        // Select a random question
        val questionKeys = weatherQuestions.keys.toArray
        val questionEn = questionKeys(scala.util.Random.nextInt(questionKeys.length))
        
        // Get translations
        val targetLangName = languageToString(prefs.targetLanguage)
        val motherLangName = languageToString(prefs.motherLanguage)
        
        // Get actual translations using our predefined maps
        val targetLangQuestion = if (prefs.targetLanguage == English || !weatherQuestions(questionEn).contains(targetLangName)) {
          questionEn // Default to English if translation not available
        } else {
          weatherQuestions(questionEn)(targetLangName)
        }
        
        val motherLangQuestion = if (prefs.motherLanguage == English || !weatherQuestions(questionEn).contains(motherLangName)) {
          questionEn // Default to English if translation not available
        } else {
          weatherQuestions(questionEn)(motherLangName)
        }
        
        // Display the question with translation
        println(s"\nBot: $targetLangQuestion")
        if (prefs.targetLanguage != prefs.motherLanguage) {
          println(s"(${languageToString(prefs.motherLanguage)}: $motherLangQuestion)")
        }
        
        // Start conversation loop
        var chatting = true
        var questionsAsked = 1
        
        while (chatting && questionsAsked < 5) {
          print("\nYou: ")
          val userResponse = readLine().trim
          
          // Check for exit conditions
          if (userResponse.toLowerCase == "exit" || 
              userResponse.toLowerCase == "quit" || 
              userResponse.toLowerCase == "end chat" || 
              userResponse.toLowerCase == "stop" ||
              userResponse.toLowerCase == "exit chat" ||
              userResponse.toLowerCase == "stop chat") {
            
            val endMessage = "Hope you have an amazing day! Thanks for chatting. Feel free to come back anytime to practice more."
            val endMessageTranslated = if (prefs.targetLanguage == English) {
              endMessage
            } else if (prefs.targetLanguage == Spanish) {
              "¡Espero que tengas un día increíble! Gracias por charlar. No dudes en volver cuando quieras para practicar más."
            } else if (prefs.targetLanguage == French) {
              "J'espère que vous passerez une excellente journée ! Merci pour cette conversation. N'hésitez pas à revenir quand vous voulez pour vous entraîner davantage."
            } else if (prefs.targetLanguage == German) {
              "Ich wünsche Ihnen einen wunderschönen Tag! Danke für das Gespräch. Kommen Sie jederzeit wieder, um mehr zu üben."
            } else {
              endMessage
            }
            
            println(s"\nBot: $endMessageTranslated")
            if (prefs.targetLanguage != prefs.motherLanguage && prefs.motherLanguage != English) {
              println(s"(${languageToString(prefs.motherLanguage)}: $endMessage)")
            }
            chatting = false
          } else {
            // Log conversation in history
            currentUser.foreach { user =>
              userHistoryService.addInteraction(
                userId = user.id,
                username = user.username,
                email = user.email,
                question = questionEn, // Store the English version for logging
                response = userResponse,
                category = "chat"
              )
            }
            
            // Track if we've found weather or clothing in the response
            var responseSent = false
            
            // First, check for clothing items
            val clothingDetected = ChatbotCore.detectClothing(userResponse)
            
            // Also check if response mentions weather
            val isWeatherInResponse = ChatbotCore.isWeatherQuery(ChatbotCore.parseInput(userResponse))
            
            // Variables to track what type of weather was last discussed
            var lastWeatherWasHot = false
            var lastWeatherWasCold = false
            
            if (userResponse.toLowerCase.contains("hot") || 
                userResponse.toLowerCase.contains("warm") || 
                userResponse.toLowerCase.contains("heat")) {
              lastWeatherWasHot = true
              lastWeatherWasCold = false
            } else if (userResponse.toLowerCase.contains("cold") || 
                      userResponse.toLowerCase.contains("cool") || 
                      userResponse.toLowerCase.contains("chill") || 
                      userResponse.toLowerCase.contains("freez")) {
              lastWeatherWasCold = true
              lastWeatherWasHot = false
            }
            
            // Handle clothing feedback if detected
            if (clothingDetected.isDefined) {
              val (clothingItem, isHotWeatherClothing) = clothingDetected.get
              
              // Evaluate if clothing is appropriate for mentioned weather
              val clothingFeedback = ChatbotCore.evaluateClothingChoice(
                clothingItem, 
                isHotWeatherClothing,
                lastWeatherWasHot
              )
              
              // Translate clothing feedback
              val translatedFeedback = if (prefs.targetLanguage == Spanish) {
                if (isHotWeatherClothing && lastWeatherWasHot) {
                  s"¡Excelente elección con la/el $clothingItem! Es perfecto para el clima cálido. Te mantendrá fresco y cómodo."
                } else if (!isHotWeatherClothing && lastWeatherWasCold) {
                  s"¡Excelente elección con la/el $clothingItem! Es perfecto para el clima frío. Te mantendrá abrigado y cómodo."
                } else if (isHotWeatherClothing && lastWeatherWasCold) {
                  s"Una/Un $clothingItem podría no ser suficientemente abrigado para el clima frío. Considera añadir una chaqueta, suéter o abrigo para mantenerte caliente."
                } else {
                  s"Una/Un $clothingItem podría ser demasiado abrigado para el clima cálido. Algo más ligero como una camiseta sería más cómodo."
                }
              } else if (prefs.targetLanguage == French) {
                if (isHotWeatherClothing && lastWeatherWasHot) {
                  s"Excellent choix avec le $clothingItem ! C'est parfait pour un temps chaud. Cela vous gardera frais et à l'aise."
                } else if (!isHotWeatherClothing && lastWeatherWasCold) {
                  s"Excellent choix avec le $clothingItem ! C'est parfait pour un temps froid. Cela vous gardera au chaud et confortable."
                } else if (isHotWeatherClothing && lastWeatherWasCold) {
                  s"Un $clothingItem pourrait ne pas être assez chaud pour un temps froid. Pensez à ajouter une veste, un pull ou un manteau pour rester au chaud."
                } else {
                  s"Un $clothingItem pourrait être trop chaud pour un temps chaud. Quelque chose de plus léger comme un t-shirt serait plus confortable."
                }
              } else if (prefs.targetLanguage == German) {
                if (isHotWeatherClothing && lastWeatherWasHot) {
                  s"Tolle Wahl mit dem $clothingItem! Das ist perfekt für heißes Wetter. Es wird Sie kühl und bequem halten."
                } else if (!isHotWeatherClothing && lastWeatherWasCold) {
                  s"Ausgezeichnete Wahl mit dem $clothingItem! Das ist perfekt für kaltes Wetter. Es wird Sie warm und gemütlich halten."
                } else if (isHotWeatherClothing && lastWeatherWasCold) {
                  s"Ein $clothingItem ist vielleicht nicht warm genug für kaltes Wetter. Erwägen Sie, eine Jacke, einen Pullover oder einen Mantel hinzuzufügen, um warm zu bleiben."
                } else {
                  s"Ein $clothingItem ist vielleicht zu warm für heißes Wetter. Etwas Leichteres wie ein T-Shirt wäre bequemer."
                }
              } else {
                clothingFeedback // Default to English
              }
              
              // Display clothing feedback with translation
              println(s"\nBot: $translatedFeedback")
              if (prefs.targetLanguage != prefs.motherLanguage && prefs.motherLanguage != English) {
                println(s"(${languageToString(prefs.motherLanguage)}: $clothingFeedback)")
              } else if (prefs.motherLanguage == English && prefs.targetLanguage != English) {
                println(s"(English: $clothingFeedback)")
              }
              
              responseSent = true
            }
            
            // Based on response, provide a weather-related follow-up
            if (isWeatherInResponse && !responseSent) {
              // If user mentioned weather, give specific response
              val weatherReply = ChatbotCore.handleWeatherQuery(userResponse)
              
              // Translate weather reply (simple translations for common phrases)
              val targetLangReply = if (prefs.targetLanguage == Spanish) {
                if (weatherReply.contains("It sounds hot")) {
                  "¡Parece que hace calor! Te recomiendo usar ropa ligera y transpirable como camisetas y ropa de algodón. Mantente hidratado, usa protector solar y trata de estar en la sombra cuando sea posible."
                  lastWeatherWasHot = true
                  lastWeatherWasCold = false
                } else if (weatherReply.contains("It sounds cold")) {
                  "¡Parece que hace frío! Asegúrate de usar capas abrigadas - un suéter o forro polar debajo de una chaqueta. No olvides una bufanda para proteger tu cuello del viento frío."
                  lastWeatherWasHot = false
                  lastWeatherWasCold = true
                } else {
                  "Cuando hablamos del clima, ¡puedo darte sugerencias de ropa! Solo dime si hace calor o frío afuera, y te recomendaré qué usar."
                }
              } else if (prefs.targetLanguage == French) {
                if (weatherReply.contains("It sounds hot")) {
                  "Il fait chaud! Je vous recommande de porter des vêtements légers et respirants comme des shorts et un t-shirt. Restez hydraté, utilisez de la crème solaire et essayez de rester à l'ombre si possible."
                  lastWeatherWasHot = true
                  lastWeatherWasCold = false
                } else if (weatherReply.contains("It sounds cold")) {
                  "Il fait froid! Assurez-vous de porter des couches chaudes - un pull ou une polaire sous une veste, plus un chapeau et des gants s'il fait très froid."
                  lastWeatherWasHot = false
                  lastWeatherWasCold = true
                } else {
                  "En parlant de la météo, je peux vous donner des suggestions vestimentaires! Dites-moi simplement s'il fait chaud ou froid dehors, et je vous recommanderai quoi porter."
                }
              } else if (prefs.targetLanguage == German) {
                if (weatherReply.contains("It sounds hot")) {
                  "Es klingt heiß! Ich empfehle leichte, atmungsaktive Kleidung wie Shorts und ein T-Shirt. Bleiben Sie hydratisiert, verwenden Sie Sonnenschutz und versuchen Sie, wenn möglich im Schatten zu bleiben."
                  lastWeatherWasHot = true
                  lastWeatherWasCold = false
                } else if (weatherReply.contains("It sounds cold")) {
                  "Es klingt kalt! Achten Sie auf warme Schichten - einen Pullover oder Fleece unter einer Jacke, plus eine Mütze und Handschuhe, wenn es sehr kalt ist."
                  lastWeatherWasHot = false
                  lastWeatherWasCold = true
                } else {
                  "Wenn wir über das Wetter sprechen, kann ich Ihnen Kleidungsvorschläge machen! Sagen Sie mir einfach, ob es draußen heiß oder kalt ist, und ich empfehle Ihnen, was Sie tragen sollten."
                }
              } else {
                weatherReply // Default to English
              }
              
              // Display response with translation
              println(s"\nBot: $targetLangReply")
              if (prefs.targetLanguage != prefs.motherLanguage && prefs.motherLanguage != English) {
                println(s"(${languageToString(prefs.motherLanguage)}: $weatherReply)")
              } else if (prefs.motherLanguage == English && prefs.targetLanguage != English) {
                println(s"(English: $weatherReply)")
              }
              
              responseSent = true
            }
            
            // ALWAYS follow up with a new question
            // Wait a moment before asking the next question for better UX
            Thread.sleep(1000) 
            
            // Add clothing-specific questions to follow-up options
            val clothingQuestions = Map(
              "What do you usually wear in this kind of weather?" -> Map(
                "Spanish" -> "¿Qué sueles usar en este tipo de clima?",
                "French" -> "Que portez-vous habituellement par ce temps?",
                "German" -> "Was tragen Sie normalerweise bei diesem Wetter?"
              ),
              "Do you prefer light or heavy clothing?" -> Map(
                "Spanish" -> "¿Prefieres ropa ligera o pesada?",
                "French" -> "Préférez-vous des vêtements légers ou lourds?",
                "German" -> "Bevorzugen Sie leichte oder schwere Kleidung?"
              ),
              "What color clothes do you like to wear when it's hot?" -> Map(
                "Spanish" -> "¿Qué color de ropa te gusta usar cuando hace calor?",
                "French" -> "De quelle couleur aimez-vous porter des vêtements quand il fait chaud?",
                "German" -> "Welche Farbe Kleidung tragen Sie gerne, wenn es heiß ist?"
              ),
              "What's your favorite jacket for cold days?" -> Map(
                "Spanish" -> "¿Cuál es tu chaqueta favorita para días fríos?",
                "French" -> "Quelle est votre veste préférée pour les jours froids?",
                "German" -> "Was ist Ihre Lieblingsjacke für kalte Tage?"
              )
            )
            
            // Combine the clothing questions with our follow-up questions for more variety
            val allQuestions = followupQuestions ++ clothingQuestions
            
            // Select a follow-up question
            val allKeys = allQuestions.keys.toArray
            val newQuestionEn = allKeys(scala.util.Random.nextInt(allKeys.length))
            
            // Get translated follow-up question
            val targetLangNewQ = if (prefs.targetLanguage == English || !allQuestions(newQuestionEn).contains(targetLangName)) {
              newQuestionEn // Default to English if translation not available
            } else {
              allQuestions(newQuestionEn)(targetLangName)
            }
            
            val motherLangNewQ = if (prefs.motherLanguage == English || !allQuestions(newQuestionEn).contains(motherLangName)) {
              newQuestionEn // Default to English if translation not available
            } else {
              allQuestions(newQuestionEn)(motherLangName)
            }
            
            // Display the question with translation
            println(s"\nBot: $targetLangNewQ")
            if (prefs.targetLanguage != prefs.motherLanguage) {
              println(s"(${languageToString(prefs.motherLanguage)}: $motherLangNewQ)")
            }
            
            questionsAsked += 1
          }
        }
        
        // End conversation after 5 exchanges
        if (questionsAsked >= 5 && chatting) {
          val endMessage = "That was a great chat! Hope you have an amazing day. Feel free to come back anytime to practice more."
          val endMessageTranslated = if (prefs.targetLanguage == English) {
            endMessage
          } else if (prefs.targetLanguage == Spanish) {
            "¡Fue una gran conversación! Espero que tengas un día increíble. Puedes volver cuando quieras para practicar más."
          } else if (prefs.targetLanguage == French) {
            "C'était une excellente conversation! J'espère que vous passerez une journée incroyable. N'hésitez pas à revenir quand vous voulez pour pratiquer davantage."
          } else if (prefs.targetLanguage == German) {
            "Das war ein tolles Gespräch! Ich hoffe, Sie haben einen wunderbaren Tag. Sie können jederzeit wiederkommen, um mehr zu üben."
          } else {
            endMessage
          }
          
          println(s"\nBot: $endMessageTranslated")
          if (prefs.targetLanguage != prefs.motherLanguage && prefs.motherLanguage != English) {
            println(s"(${languageToString(prefs.motherLanguage)}: $endMessage)")
          }
        }
        
      case Some(_) => 
        // Not all preferences are set
        println(s"\nBot: ${formatDualLanguageResponse("Please make sure both your mother language and target language are set to start a chat.")}")
        showSettings()
        
      case None =>
        println(s"\nBot: ${formatDualLanguageResponse("Please set your language preferences before starting a chat.")}")
        showSettings()
    }
  }
}
