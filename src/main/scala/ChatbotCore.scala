package languagelearningbot

import java.util.regex.Pattern
import scala.util.Random
import scala.util.matching.Regex
import models.User

object ChatbotCore {
  // Required core chatbot functions as specified in the requirements
  
  // Standard translation dictionary for common UI phrases
  private val translationDict: Map[(String, String), String] = Map(
    // Settings-related translations
    ("Your current settings are:", "Spanish") -> "Tus configuraciones actuales son:",
    ("Your current settings are:", "French") -> "Vos paramètres actuels sont :",
    ("Your current settings are:", "German") -> "Ihre aktuellen Einstellungen sind:",
    
    ("Mother language:", "Spanish") -> "Idioma materno:",
    ("Mother language:", "French") -> "Langue maternelle :",
    ("Mother language:", "German") -> "Muttersprache:",
    
    ("Target language:", "Spanish") -> "Idioma objetivo:",
    ("Target language:", "French") -> "Langue cible :",
    ("Target language:", "German") -> "Zielsprache:",
    
    ("You can change these with commands like:", "Spanish") -> "Puedes cambiar estas con comandos como:",
    ("You can change these with commands like:", "French") -> "Vous pouvez modifier ces paramètres avec des commandes comme :",
    ("You can change these with commands like:", "German") -> "Sie können diese mit Befehlen wie den folgenden ändern:",
    
    // Settings menu translations
    ("You can change your settings for:", "Spanish") -> "Puedes cambiar tus configuraciones para:",
    ("You can change your settings for:", "French") -> "Vous pouvez modifier vos paramètres pour :",
    ("You can change your settings for:", "German") -> "Sie können Ihre Einstellungen ändern für:",
    
    ("Mother language", "Spanish") -> "Idioma materno",
    ("Mother language", "French") -> "Langue maternelle",
    ("Mother language", "German") -> "Muttersprache",
    
    ("Target language", "Spanish") -> "Idioma objetivo",
    ("Target language", "French") -> "Langue cible",
    ("Target language", "German") -> "Zielsprache",
    
    // Language selection confirmations
    ("Mother language set to", "Spanish") -> "Idioma materno establecido a",
    ("Mother language set to", "French") -> "Langue maternelle définie sur",
    ("Mother language set to", "German") -> "Muttersprache eingestellt auf",
    
    ("Target language set to", "Spanish") -> "Idioma objetivo establecido a",
    ("Target language set to", "French") -> "Langue cible définie sur",
    ("Target language set to", "German") -> "Zielsprache eingestellt auf",
    
    // Quiz-related translations
    ("Starting a", "Spanish") -> "Iniciando un",
    ("Starting a", "French") -> "Démarrage d'un",
    ("Starting a", "German") -> "Starten eines",
    
    ("quiz in", "Spanish") -> "cuestionario en",
    ("quiz in", "French") -> "quiz en",
    ("quiz in", "German") -> "Quiz in",
    
    // Quiz types
    ("Vocabulary", "Spanish") -> "Vocabulario",
    ("Vocabulary", "French") -> "Vocabulaire",
    ("Vocabulary", "German") -> "Wortschatz",
    
    ("Grammar", "Spanish") -> "Gramática",
    ("Grammar", "French") -> "Grammaire",
    ("Grammar", "German") -> "Grammatik",
    
    ("Translation", "Spanish") -> "Traducción",
    ("Translation", "French") -> "Traduction",
    ("Translation", "German") -> "Übersetzung",
    
    ("Multiple Choice", "Spanish") -> "Opción múltiple",
    ("Multiple Choice", "French") -> "Choix multiple",
    ("Multiple Choice", "German") -> "Multiple Choice"
  )
  
  /**
   * Returns an initial greeting based on user preferences if any
   */
  def greetUser(preferences: Option[UserPreferences] = None, currentUser: Option[User] = None): String = {
    val userGreeting = currentUser match {
      case Some(user) => s"Hello, ${user.username}!"
      case None => "Hello!"
    }
    
    preferences match {
      case Some(prefs) => 
        val welcomeMessage = "Welcome to the Language Learning Bot. How can I help you today?"
        s"$userGreeting ${translateText(welcomeMessage, prefs.targetLanguage)}"
      case None => s"$userGreeting Welcome to the Language Learning Bot! Please set your preferred language to get started."
    }
  }
  
  /**
   * Compute the Levenshtein distance between two strings
   */
  private def levenshtein(s1: String, s2: String): Int = {
    val memo = Array.ofDim[Int](s1.length + 1, s2.length + 1)
    for (i <- 0 to s1.length) memo(i)(0) = i
    for (j <- 0 to s2.length) memo(0)(j) = j
    for (i <- 1 to s1.length; j <- 1 to s2.length) {
      val cost = if (s1(i - 1) == s2(j - 1)) 0 else 1
      memo(i)(j) = List(
        memo(i - 1)(j) + 1,    // deletion
        memo(i)(j - 1) + 1,    // insertion
        memo(i - 1)(j - 1) + cost // substitution
      ).min
    }
    memo(s1.length)(s2.length)
  }

  /**
   * Corrects a token if it is a misspelling of a known keyword (edit distance <= 2)
   */
  private def correctSpelling(token: String, keywords: Set[String]): String = {
    val lowerToken = token.toLowerCase
    
    // If the token is already a keyword, return it as is
    if (keywords.contains(lowerToken)) return token
    
    // Get all candidates with their distances
    val candidates = keywords.map(w => (w, levenshtein(lowerToken, w.toLowerCase)))
    
    // Find the closest match with distance <= 2
    val closestMatch = candidates
      .filter(_._2 <= 2)  // Only consider matches with distance <= 2
      .minByOption(_._2)  // Get the closest match if any exist
    
    closestMatch match {
      case Some((word, distance)) =>
        // If we found a close match, use it
        word
      case None =>
        // If no close match found, return original token
        token
    }
  }

  /**
   * Parses user input into a list of tokens for easier processing, with spell  for known keywords
   */
  def parseInput(input: String): List[String] = {
    // Ensure input is properly encoded in UTF-8
    val encodedInput = new String(input.getBytes("UTF-8"), "UTF-8")
    val rawTokens = encodedInput.toLowerCase
      .replaceAll("[^a-zA-Z0-9áéíóúüñçßÄÖÜäöü\\s]", "")
      .split("\\s+")
      .toList
    val keywords = Set(
      // Command related
      "set", "sets", "setting", "settings", "change", "changes", "changed", "changing",
      "target", "targets", "mother", "mothers", "native", "first", "primary", "source",
      "language", "langauge", "langage", "lang", "languages", "lingua", "idioma", "sprache",
      "difficulty", "difficulties", "level", "levels", "easy", "medium", "hard", "advanced",
      
      // Exit related
      "exit", "exits", "exited", "exiting", "quit", "quits", "quit", "quitting",
      "end", "ends", "ended", "ending", "stop", "stops", "stopped", "stopping",
      "eit", "exif", "exi", "exitt", "exxit", "exiit", "exitt", "exxit",
      "qit", "quitt", "quitt", "quitt", "quitt", "quitt", "quitt", "quitt",
      "stp", "stopp", "stopp", "stopp", "stopp", "stopp", "stopp", "stopp",
      
      // Language related
      "english", "spanish", "french", "german", "chinese", "japanese", "russian",
      
      // Weather related
      "weather", "temperature", "forecast", "rain", "sunny", "cloudy", "hot", "cold",
      "warm", "cool", "freezing", "heat", "chilly", "frigid", "degrees", "celsius",
      "fahrenheit", "climate", "storm", "snowing", "raining", "sun", "snow", "wind",
      "windy", "humidity", "thunder", "lightning", "hail", "fog", "foggy", "mist",
      "misty", "drizzle", "shower", "breeze", "gust", "blizzard", "tropical", 
      
      // Quiz related
      "quiz", "quizzes", "test", "tests", "testing", "exam", "exams", "examination",
      "practice", "practise", "practicing", "practising", "exercise", "exercises",
      "question", "questions", "challenge", "challenges", "assessment", "assessments",
      "vocabulary", "vocab", "words", "word", "terms", "term",
      "grammar", "grammatical", "syntax", "syntactical",
      "translation", "translate", "translates", "translated", "translating",
      "multiple", "choice", "mcq", "mcw", "mcc", "options", "option", "select", "selection",
      "multichoice", "multi-choice", "multi choice", "multiplechoice", "multiple-choice",
      
      // Settings related
      "preferences", "preference", "config", "configuration",
      "configure", "configures", "configured", "configuring", "options", "option",
      "beginner", "intermediate", "expert",
      
      // Help related
      "help", "helps", "helped", "helping", "assist", "assists", "assisted",
      "assisting", "guide", "guides", "guided", "guiding", "support", "supports",
      "supported", "supporting", "instruction", "instructions", "instruct",
      "instructs", "instructed", "instructing", "info", "information",
      "explain", "explains", "explained", "explaining", "show", "shows",
      "showed", "showing", "tell", "tells", "told", "telling",
      
      // Question words
      "how", "what", "why", "when", "where", "which", "who", "whom", "whose",
      
      // Language learning specific
      "mean", "means", "meant", "meaning", "say", "says", "said", "saying", "spell", "spells",
      "spelled", "spelling", "pronounce", "pronounces", "pronounced",
      "pronouncing", "speak", "speaks", "spoke", "speaking", "write", "writes",
      "wrote", "writing", "read", "reads", "reading", "listen", "listens",
      "listened", "listening", "understand", "understands", "understood",
      "understanding", "comprehend", "comprehends", "comprehended",
      "comprehending",
      
      // Navigation and control
      "back", "return", "returns",
      "returned", "returning", "next", "previous", "continue", "continues",
      "continued", "continuing", "start", "starts", "started", "starting",
      "begin", "begins", "began", "beginning",
      
      // Common actions
      "save", "saves", "saved", "saving", "load", "loads", "loaded", "loading",
      "update", "updates", "updated", "updating", "delete", "deletes", "deleted",
      "deleting", "remove", "removes", "removed", "removing", "add", "adds",
      "added", "adding", "create", "creates", "created", "creating", "new",
      "edit", "edits", "edited", "editing", "modify", "modifies", "modified",
      "modifying",
      
      // Common responses
      "yes", "yeah", "yep", "sure", "ok", "okay", "fine", "good", "great",
      "no", "nope", "nah", "not", "never", "none", "nothing",
      "maybe", "perhaps", "possibly", "probably", "might", "could",
      "please", "thanks", "thank", "thank you", "sorry", "excuse",
      
      // Time related
      "now", "today", "tomorrow", "yesterday", "later", "soon", "before",
      "after", "during", "while", "when", "time", "times", "schedule",
      "scheduled", "scheduling",
      
      // Progress related
      "progress", "advance", "advances", "advanced", "advancing", "improve",
      "improves", "improved", "improving", "better", "best", "worst", "worse",
      "complete", "completes", "completed", "completing", "finish", "finishes",
      "finished", "finishing", "done", "complete", "completes", "completed",
      "completing"
    )
    rawTokens.map(token => correctSpelling(token, keywords))
  }
  
  /**
   * Categorizes and processes the user query via pattern matching
   */
  def handleUserInput(input: String, preferences: Option[UserPreferences] = None, currentUser: Option[User] = None): String = {
    val tokens = parseInput(input)
    
    // Special case for basic greetings
    if (input.trim.toLowerCase == "hi" || input.trim.toLowerCase == "hello" || input.trim.toLowerCase == "hey") {
      return generateResponse("greeting", preferences, currentUser)
    }
    
    // First, check for specific intents that should be prioritized
    if (isGreeting(tokens)) {
      return generateResponse("greeting", preferences, currentUser)
    }
    
    // Check for thanks/gratitude
    if (isThanks(tokens)) {
      return generateResponse("thanks", preferences, currentUser)
    }
    
    // Check for weather-related queries
    if (isWeatherQuery(tokens)) {
      return handleWeatherQuery(input)
    }
    
    // Check for explicit quiz type requests (like "grammar", "vocabulary", etc.)
    detectQuizType(tokens) match {
      case Some(quizType) =>
        // Just return quiz_request - the actual quiz will be handled by Main
        return generateResponse("quiz_request", preferences, currentUser)
      case None => 
        // Continue with other intent checks
    }
    
    // Then check other intents
    if (isQuizRequest(tokens)) {
      return generateResponse("quiz_request", preferences, currentUser)
    }
    
    if (isSettingsChange(tokens)) {
      return generateResponse("settings", preferences, currentUser) 
    }
    
    if (isHelp(tokens)) {
      return generateResponse("help", preferences, currentUser)
    }
    
    if (isLanguageQuestion(tokens)) {
      return generateResponse("language_question", preferences, currentUser)
    }
    
    // Default case - if none of the specific patterns matched, return "unknown" response
    generateResponse("unknown", preferences, currentUser)
  }
  
  /**
   * Crafts a well-structured response based on the query category
   */
  def generateResponse(category: String, preferences: Option[UserPreferences] = None, currentUser: Option[User] = None): String = {
    // Get base response in English
    val baseResponse = category match {
      case "greeting" => 
        currentUser match {
          case Some(user) =>
            s"Hello, ${user.username}! I'm your Language Learning Bot. Would you like to practice vocabulary, grammar, or start a quiz?"
          case None =>
            "Hello! I'm your Language Learning Bot. Would you like to practice vocabulary, grammar, or start a quiz?"
        }
      
      case "thanks" =>
        "You're welcome! Would you like to try a quiz or Changing Settings?"
      
      case "quiz_request" => 
        currentUser match {
          case Some(_) =>
            // Use the appropriate quiz selection message based on target language
            preferences match {
              case Some(prefs) => getQuizSelectionMessage(prefs.targetLanguage)
              case None => quizSelectionEnglish
            }
          case None =>
            "Please sign in or continue as guest to start a quiz."
        }
      
      case "settings" => 
        currentUser match {
          case Some(user) =>
            "You can change your settings for:\n" +
            "- Mother language\n- Target language\n" +
            "What would you like to update?"
          case None =>
            "Please sign in or continue as guest to access settings."
        }
      
      case "help" => 
        currentUser match {
          case Some(_) =>
            "I'm here to help you learn languages! You can:\n" +
            "- Start a quiz by typing 'quiz'\n" +
            "- Change settings with 'settings'\n" +
            "- View your progress with 'analytics'\n" +
            "- Ask language questions directly\n" +
            "- Type 'logout' to sign out"
          case None =>
            "Please sign in or continue as guest to access the language learning features."
        }
        
      case "language_question" =>
        currentUser match {
          case Some(_) =>
            "What would you like to know about language learning? I can help with vocabulary, grammar, and more."
          case None =>
            "Please sign in or continue as guest to ask language questions."
        }
      
      case "unknown" => 
        currentUser match {
          case Some(_) =>
            "I'm not sure I understand. Try asking about quizzes, changing settings."
          case None =>
            "Please sign in or continue as guest to use the language learning features."
        }
    }
    
    // Return the base response (will be translated later in formatDualLanguageResponse)
    baseResponse
  }
  
  // Helper functions for intent detection using pattern matching
  private def isGreeting(tokens: List[String]): Boolean = {
    val greetings = Set("hello", "hi", "hey", "hola", "bonjour", "hallo", "ciao", "howdy", "greetings", 
                       "morning", "afternoon", "evening", "night", "yo", "sup", "salut", "ola", "gday", "heya")
    
    // Check if any token is a greeting (not the whole input)
    tokens.exists(token => greetings.contains(token.toLowerCase))
  }
  
  private def isThanks(tokens: List[String]): Boolean = {
    val thanksWords = Set("thanks", "thank", "thx", "ty", "thankyou", "thank you", "gracias", "merci", "danke", "appreciated", "grateful")
    tokens.exists(thanksWords.contains)
  }
  
  private def isQuizRequest(tokens: List[String]): Boolean = {
    val quizWords = Set("quiz", "test", "practice", "exercise", "question", "challenge", "learn", "study")
    val quizTypes = Set("vocabulary", "vocab", "grammar", "translation", "mcq", "multiple", "choice")
    val actionWords = Set("give", "start", "begin", "want", "show", "get", "take")
    
    // Check if the input contains words like "exit quiz" or "quit quiz"
    // Return false for these phrases to avoid triggering quiz mode
    val input = tokens.mkString(" ")
    if (input.contains("exit quiz") || 
        input.contains("quit quiz") || 
        input.contains("end quiz") || 
        input.contains("stop quiz")) {
      return false
    }
    
    // Check for quiz request patterns:
    // 1. Contains quiz words directly
    // 2. Contains action words followed by quiz-related words
    // 3. Contains quiz type words
    tokens.exists(quizWords.contains) || 
    tokens.exists(quizTypes.contains) ||
    (tokens.exists(actionWords.contains) && tokens.mkString(" ").contains("quiz")) ||
    (tokens.sliding(2).exists(pair => 
      actionWords.contains(pair.head) && (quizWords.contains(pair.last) || quizTypes.contains(pair.last))
    ))
  }
  
  /**
   * Detects if the input contains a specific quiz type request
   */
  def detectQuizType(tokens: List[String]): Option[String] = {
    val input = tokens.mkString(" ").toLowerCase
    
    // Skip generic quiz requests without specific type - use a regex pattern
    val genericQuizPattern = """^(?:give|start|take|begin|i want|show me|let's|do)?\s*(?:a|me|us|some|the)?\s*(?:quiz|test|exam|questions?)$""".r
    if (genericQuizPattern.findFirstIn(input).isDefined) {
      return None
    }
    
    // Map of quiz type variations to standardized types, including multilingual options
    val quizTypeMap = Map(
      Set("vocabulary", "vocab", "words", "word", "terms", "vocabulario", "vocabulaire", "vokabular", "المفردات", "1", "vocabulary quiz", "vocab quiz") -> "vocabulary",
      Set("grammar", "grammatical", "syntax", "gramática", "grammaire", "grammatik", "القواعد", "2", "grammar quiz") -> "grammar",
      Set("translation", "translate", "translating", "traducción", "traduction", "übersetzung", "الترجمة", "3", "translation quiz", "translate quiz") -> "translation",
      Set("mcq", "multiple choice", "multiplechoice", "multiple-choice", "choice", "opción múltiple", "choix multiple", "الاختيار من متعدد", "4", "mcq quiz", "multiple choice quiz", "mcw", "mcc") -> "mcq"
    )
    
    // Check for patterns like "give me grammar quiz"
    val giveQuizPattern = """(?i)(?:give|start|begin|do|make|try|want|i want|let's)(?:\s+\w+){0,3}\s+(vocabulary|vocab|grammar|translation|translate|mcq|multiple choice)(?:\s+quiz)?""".r
    giveQuizPattern.findFirstMatchIn(input) match {
      case Some(m) => 
        val quizWord = m.group(1).toLowerCase
        if (quizWord == "vocabulary" || quizWord == "vocab") return Some("vocabulary")
        if (quizWord == "grammar") return Some("grammar")
        if (quizWord == "translation" || quizWord == "translate") return Some("translation")
        if (quizWord == "mcq" || quizWord == "multiple choice") return Some("mcq")
      case None => // Continue with other checks
    }
    
    // Check direct matches for common quiz type patterns
    if (input == "mcq" || input == "mcq quiz" || input.contains("multiple choice quiz") || input == "4") {
      return Some("mcq")
    } else if (input == "vocabulary" || input == "vocab" || input == "vocabulary quiz" || input == "vocab quiz" || input == "1") {
      return Some("vocabulary")
    } else if (input == "grammar" || input == "grammar quiz" || input == "2") {
      return Some("grammar")
    } else if (input == "translation" || input == "translate" || input == "translation quiz" || input == "3") {
      return Some("translation")
    }
    
    // Check for mixed language patterns with the word "quiz"
    if (input.contains("mcq") && input.contains("quiz")) {
      return Some("mcq")
    } else if (input.contains("vocab") && input.contains("quiz")) {
      return Some("vocabulary")
    } else if (input.contains("grammar") && input.contains("quiz")) {
      return Some("grammar")
    } else if (input.contains("translat") && input.contains("quiz")) {
      return Some("translation")
    }
    
    // Check for partial matches if no direct match
    quizTypeMap.find { case (variations, _) => 
      variations.exists(input.contains)
    }.map(_._2)
  }
  
  // Various word sets for detecting query intents
  private val settingsWords = Set("settings", "preferences", "change", "configure", "options")
  
  /**
   * Determine if the input is asking about changing settings
   */
  private def isSettingsChange(tokens: List[String]): Boolean = {
    // Get full input as string
    val input = tokens.mkString(" ")
    
    // First check for specific setting commands that should be handled elsewhere
    if (input.contains("set") && (
        input.contains("mother language") ||
        input.contains("mother langauge") ||
        input.contains("target language") ||
        input.contains("target langauge"))) {
      return false
    }
    
    // Only match general settings-related words
    tokens.exists(settingsWords.contains)
  }
  
  /**
   * Handle queries about settings
   */
  private def handleSettingsQuery(tokens: List[String], userPreferences: Option[UserPreferences]): String = {
    userPreferences match {
      case Some(prefs) =>
        s"""Your current settings are:
           |Mother language: ${languageToString(prefs.motherLanguage)}
           |Target language: ${languageToString(prefs.targetLanguage)}
           |
           |You can change these with commands like:
           |'set mother language Spanish'
           |'set target language French'""".stripMargin
           
      case None =>
        """You haven't set any preferences yet. You can set them with:
          |'set mother language [your language]' 
          |'set target language [language you want to learn]'""".stripMargin
    }
  }
  
  /**
   * Helper method to convert Language to string representation
   */
  private def languageToString(language: Language): String = {
    language match {
      case English => "English"
      case Spanish => "Spanish"
      case French => "French"
      case German => "German"
      case _ => "Unknown"
    }
  }
  
  private def isHelp(tokens: List[String]): Boolean = {
    val helpWords = Set("help", "assist", "guide", "support", "instruction", "info", "information")
    tokens.exists(helpWords.contains)
  }
  
  private def isLanguageQuestion(tokens: List[String]): Boolean = {
    val questionIndicators = Set("why", "when", "translate", "mean", "say", "spell", "pronounce")
    // Add more specific language learning words that won't be confused with greeting inquiries
    val languageSpecificWords = Set("verb", "noun", "adjective", "grammar", "vocabulary", "phrase", "idiom", "word", "sentence")
    tokens.exists(questionIndicators.contains) || tokens.exists(languageSpecificWords.contains)
  }
  
  /**
   * Store user preferences immutably
   * Returns Option[UserPreferences]
   */
  def storeUserPreferences(motherLanguage: Option[Language] = None, 
                         targetLanguage: Option[Language] = None,
                         name: Option[String] = None,
                         currentPreferences: Option[UserPreferences] = None): Option[UserPreferences] = {
    
    // If we have current preferences, update only the fields that are provided
    // If not, create new preferences only with the provided fields
    currentPreferences match {
      case Some(prefs) => 
        Some(UserPreferences(
          motherLanguage.getOrElse(prefs.motherLanguage),
          targetLanguage.getOrElse(prefs.targetLanguage),
          name.orElse(prefs.name)
        ))
        
      case None =>
        if (motherLanguage.isDefined) {
          Some(UserPreferences(
            motherLanguage.get,
            targetLanguage.getOrElse(English),  // Use English as a placeholder for incomplete preferences
            name
          ))
        } else if (targetLanguage.isDefined) {
          Some(UserPreferences(
            English,                              // Use English as a placeholder for incomplete preferences
            targetLanguage.get,
            name
          ))
        } else {
          None
        }
    }
  }
  
  /**
   * Retrieve saved preferences safely using Option
   */
  def getUserPreferences(userPreferences: Option[UserPreferences]): Option[UserPreferences] = {
    userPreferences
  }
  
  /**
   * Translate text to the specified language
   * This is a simplified implementation. In a real system, you would use a translation API.
   */
  def translateText(text: String, targetLanguage: Language): String = {
    // Check if this is quiz instructions - never append language tags to quiz instructions
    val isQuizInstruction = text.contains("quiz type") || 
                          text.contains("What type of quiz would you like to take?") ||
                          text.contains("We offer:") ||
                          text.contains("Please type the number") ||
                          text.contains("Vocabulary -") ||
                          text.contains("Grammar -") ||
                          text.contains("Translation -") ||
                          text.contains("Multiple Choice") ||
                          text.contains("MCQ") ||
                          text.contains("I'd be happy to start a quiz")
    
    // Helper function to get translation based on language type
    def getTranslation(text: String, lang: Language): String = {
      // Convert language to string for lookup
      val langStr = languageToString(lang)
      
      // Check if we have a translation for the exact text
      if (translationDict.contains((text, langStr))) {
        translationDict((text, langStr))
      } else {
        // For simplicity, if no exact match, look for partial matches at the beginning
        val matchingPrefix = translationDict.keys
          .filter(_._2 == langStr)
          .map(_._1)
          .find(phrase => text.startsWith(phrase))
        
        matchingPrefix match {
          case Some(prefix) => 
            // If we find a prefix match, translate that part and keep the rest
            val translated = translationDict((prefix, langStr))
            val remainder = text.substring(prefix.length)
            translated + remainder
          case None => 
            // For quiz instructions, never append language markers
            if (isQuizInstruction) {
              lang match {
                case English => text
                case Spanish => text  // In a real system we'd apply Spanish translation
                case French => text   // In a real system we'd apply French translation
                case German => text   // In a real system we'd apply German translation
                case _ => text
              }
            } else {
              // Fall back to appending a language marker for unknown translations
              lang match {
                case English => text + " (in English)"
                case Spanish => text + " (en Español)"
                case French => text + " (en Français)"
                case German => text + " (auf Deutsch)"
                case _ => text
              }
            }
        }
      }
    }
    
    // Get translation using the helper function
    getTranslation(text, targetLanguage)
  }

  /**
   * Categorizes user input into a specific category
   */
  def categorizeInput(input: String): String = {
    val tokens = parseInput(input)
    
    if (isGreeting(tokens)) "greeting"
    else if (isThanks(tokens)) "thanks"
    else if (isWeatherQuery(tokens)) "weather"
    else if (isQuizRequest(tokens)) "quiz_request"
    else if (isSettingsChange(tokens)) "settings"
    else if (isHelp(tokens)) "help"
    else if (isLanguageQuestion(tokens)) "language_question"
    else "unknown"
  }
  
  /**
   * Checks if input is weather-related
   */
  def isWeatherQuery(tokens: List[String]): Boolean = {
    val weatherWords = Set("weather", "temperature", "forecast", "rain", "sunny", "cloudy", 
                          "hot", "cold", "warm", "cool", "freezing", "heat", "chilly", "frigid",
                          "degrees", "celsius", "fahrenheit", "climate", "storm", "snowing", "raining")
    tokens.exists(weatherWords.contains)
  }
  
  /**
   * Checks if input contains clothing items
   */
  def detectClothing(input: String): Option[(String, Boolean)] = {
    val hotWeatherClothes = Set(
      "t-shirt", "t shirt", "tshirt", "tank top", "short sleeve shirt", 
      "light shirt", "cotton shirt", "polo shirt", "summer top", "blouse", 
      "sleeveless top", "linen shirt", "breathable shirt", "jersey", 
      "light fabric", "thin clothes", "summer clothes"
    )
    
    val coldWeatherClothes = Set(
      "coat", "jacket", "sweater", "sweatshirt", "hoodie", "scarf",
      "winter coat", "wool coat", "parka", "pullover", "cardigan", "fleece",
      "turtleneck", "long sleeve", "thick shirt", "thermal shirt", "windbreaker",
      "down jacket", "heavy jacket", "puffer jacket", "winter clothes", "warm top"
    )
    
    val lowerInput = input.toLowerCase
    
    // First check for hot weather clothes
    val hotClothingMatch = hotWeatherClothes.find(item => lowerInput.contains(item))
    if (hotClothingMatch.isDefined) {
      return Some((hotClothingMatch.get, true)) // true means hot weather appropriate
    }
    
    // Then check for cold weather clothes
    val coldClothingMatch = coldWeatherClothes.find(item => lowerInput.contains(item))
    if (coldClothingMatch.isDefined) {
      return Some((coldClothingMatch.get, false)) // false means cold weather appropriate
    }
    
    None // No clothing detected
  }
  
  /**
   * Evaluates if clothing choice is appropriate for the weather and returns feedback
   */
  def evaluateClothingChoice(clothingItem: String, isHotWeatherClothing: Boolean, isHotWeather: Boolean): String = {
    if (isHotWeatherClothing && isHotWeather) {
      s"Great choice with the $clothingItem! That's perfect for hot weather. It will keep you cool and comfortable."
    } else if (!isHotWeatherClothing && !isHotWeather) {
      s"Excellent choice with the $clothingItem! That's perfect for cold weather. It will keep you warm and cozy."
    } else if (isHotWeatherClothing && !isHotWeather) {
      s"A $clothingItem might not be warm enough for cold weather. Consider adding a jacket, sweater, or coat to stay warm."
    } else {
      s"A $clothingItem might be too warm for hot weather. Something lighter like a t-shirt or shorts would be more comfortable."
    }
  }

  /**
   * Handle weather-related queries and provide appropriate responses
   */
  def handleWeatherQuery(input: String): String = {
    val tokens = parseInput(input)
    val lowerInput = input.toLowerCase
    
    // Check if the input mentions hot/warm conditions
    val isHot = lowerInput.contains("hot") || lowerInput.contains("warm") || 
                lowerInput.contains("heat") || lowerInput.contains("burning") ||
                lowerInput.contains("boiling") || lowerInput.contains("tropical") ||
                (lowerInput.contains("degrees") && 
                 lowerInput.replaceAll("[^0-9]", " ").trim.split(" ").filter(_.nonEmpty)
                   .headOption.map(_.toIntOption).flatten.getOrElse(0) > 30)
    
    // Check if the input mentions cold conditions
    val isCold = lowerInput.contains("cold") || lowerInput.contains("cool") || 
                 lowerInput.contains("chilly") || lowerInput.contains("freezing") ||
                 lowerInput.contains("frigid") || lowerInput.contains("snow") ||
                 lowerInput.contains("frost") || lowerInput.contains("icy") ||
                 (lowerInput.contains("degrees") && 
                  lowerInput.replaceAll("[^0-9]", " ").trim.split(" ").filter(_.nonEmpty)
                    .headOption.map(_.toIntOption).flatten.getOrElse(20) < 15)
    
    // Determine appropriate response
    if (isHot) {
      "It sounds hot! I recommend wearing light, breathable clothes like shorts and a t-shirt. " +
      "Stay hydrated, use sunscreen, and try to stay in the shade when possible. A hat can also help protect you from the sun."
    } else if (isCold) {
      "It sounds cold! Make sure to wear warm layers - a sweater or fleece under a jacket, plus a hat and gloves if it's very cold. " +
      "Don't forget a scarf to protect your neck from the cold wind. Warm socks and waterproof shoes are also important."
    } else {
      "When talking about the weather, I can give you clothing suggestions! " +
      "Just tell me if it's hot or cold outside, and I'll recommend what to wear."
    }
  }

  // --- Quiz selection messages in different languages ---
  /**
   * Returns quiz selection message in the appropriate language
   */
  def getQuizSelectionMessage(language: Language): String = {
    language match {
      case Spanish => quizSelectionSpanish
      case French => quizSelectionFrench
      case German => quizSelectionGerman
      case _ => quizSelectionEnglish
    }
  }
  
  // Quiz selection messages for each supported language
  val quizSelectionEnglish: String = """What type of quiz would you like to take? We offer:
    |1. Vocabulary - Test your word knowledge
    |2. Grammar - Practice language rules
    |3. Translation - Translate between languages
    |4. Multiple Choice (MCQ) - General language questions
    |
    |Please type the number (1-4) or name of the quiz you'd like to take.""".stripMargin
    
  val quizSelectionSpanish: String = """¿Qué tipo de cuestionario te gustaría hacer? Ofrecemos:
    |1. Vocabulario - Prueba tu conocimiento de palabras
    |2. Gramática - Practica reglas del lenguaje
    |3. Traducción - Traduce entre idiomas
    |4. Opción múltiple (MCQ) - Preguntas generales de idioma
    |
    |Por favor, escribe el número (1-4) o el nombre del cuestionario que te gustaría hacer.""".stripMargin
    
  val quizSelectionFrench: String = """Quel type de quiz souhaitez-vous passer? Nous proposons:
    |1. Vocabulaire - Testez votre connaissance des mots
    |2. Grammaire - Pratiquez les règles de langue
    |3. Traduction - Traduisez entre les langues
    |4. Choix multiple (QCM) - Questions générales de langue
    |
    |Veuillez taper le numéro (1-4) ou le nom du quiz que vous voulez passer.""".stripMargin
    
  val quizSelectionGerman: String = """Welche Art von Quiz möchten Sie machen? Wir bieten:
    |1. Wortschatz - Testen Sie Ihre Wortkenntnis
    |2. Grammatik - Üben Sie Sprachregeln
    |3. Übersetzung - Übersetzen Sie zwischen Sprachen
    |4. Multiple Choice - Allgemeine Sprachfragen
    |
    |Bitte geben Sie die Nummer (1-4) oder den Namen des Quiz ein, das Sie machen möchten.""".stripMargin
    
  
}
