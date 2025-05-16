package languagelearningbot

import scala.util.matching.Regex
import models.User

object ChatbotCore {
  // Required core chatbot functions as specified in the requirements
  
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
            "- Mother language\n- Target language\n- Difficulty level\n" +
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
  
  private def isSettingsChange(tokens: List[String]): Boolean = {
    val settingsWords = Set("settings", "preferences", "change", "set", "configure", "options")
    tokens.exists(settingsWords.contains)
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
                         difficulty: Option[Difficulty] = None,
                         name: Option[String] = None,
                         currentPreferences: Option[UserPreferences] = None): Option[UserPreferences] = {
    
    // If we have current preferences, update only the fields that are provided
    // If not, create new preferences only with the provided fields
    currentPreferences match {
      case Some(prefs) => 
        Some(UserPreferences(
          motherLanguage.getOrElse(prefs.motherLanguage),
          targetLanguage.getOrElse(prefs.targetLanguage),
          difficulty.getOrElse(prefs.difficulty),
          name.orElse(prefs.name)
        ))
        
      case None =>
        if (motherLanguage.isDefined) {
          Some(UserPreferences(
            motherLanguage.get,
            targetLanguage.getOrElse(English),  // Use English as a placeholder for incomplete preferences
            difficulty.getOrElse(Easy),         // Use Easy as a placeholder for incomplete preferences
            name
          ))
        } else if (targetLanguage.isDefined) {
          Some(UserPreferences(
            English,                              // Use English as a placeholder for incomplete preferences
            targetLanguage.get,
            difficulty.getOrElse(Easy),          // Use Easy as a placeholder for incomplete preferences
            name
          ))
        } else if (difficulty.isDefined) {
          Some(UserPreferences(
            English,                              // Use English as a placeholder for incomplete preferences
            English,                              // Use English as a placeholder for incomplete preferences
            difficulty.get,
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
    // Helper function to get translation based on language type
    def getTranslation(text: String, lang: Language): String = {
      // Simple translations for demonstration
      val translations = Map(
        // Greetings
        ("Hello", "English") -> "Hello",
        ("Hello", "Spanish") -> "¡Hola!",
        ("Hello", "French") -> "Bonjour !",
        ("Hello", "German") -> "Hallo!",
        
        // Thanks response
        ("You're welcome! Would you like to try a quiz or Changing Settings?", "English") -> 
          "You're welcome! Would you like to try a quiz or Changing Settings?",
        ("You're welcome! Would you like to try a quiz or Changing Settings?", "Spanish") -> 
          "¡De nada! ¿Te gustaría hacer un cuestionario o cambiar la configuración?",
        ("You're welcome! Would you like to try a quiz or Changing Settings?", "French") -> 
          "De rien ! Souhaitez-vous essayer un quiz ou modifier les paramètres ?",
        ("You're welcome! Would you like to try a quiz or Changing Settings?", "German") -> 
          "Gern geschehen! Möchten Sie ein Quiz versuchen oder die Einstellungen ändern?",
       
          
        // Language learning confirmation
        ("Great! I'll help you learn ", "English") -> "Great! I'll help you learn ",
        ("Great! I'll help you learn ", "Spanish") -> "¡Genial! Te ayudaré a aprender ",
        ("Great! I'll help you learn ", "French") -> "Parfait ! Je vais vous aider à apprendre le ",
        ("Great! I'll help you learn ", "German") -> "Großartig! Ich helfe Ihnen beim Erlernen von ",
        
        
        (". Would you like to take a quiz to practice?", "English") -> ". Would you like to take a quiz to practice?",
        (". Would you like to take a quiz to practice?", "Spanish") -> ". ¿Te gustaría hacer un cuestionario para practicar?",
        (". Would you like to take a quiz to practice?", "French") -> ". Souhaitez-vous faire un quiz pour vous entraîner ?",
        (". Would you like to take a quiz to practice?", "German") -> ". Möchten Sie ein Quiz machen, um zu üben?",
       
        
        // Exit quiz message when not in quiz mode
        ("You're not currently in a quiz.", "English") -> "You're not currently in a quiz.",
        ("You're not currently in a quiz.", "Spanish") -> "No estás en un cuestionario en este momento.",
        ("You're not currently in a quiz.", "French") -> "Vous n'êtes pas actuellement dans un quiz.",
        ("You're not currently in a quiz.", "German") -> "Sie befinden sich derzeit nicht in einem Quiz.",
       
        
        // Settings save messages
        ("Do you want to save your current settings to file? (yes/no)", "English") -> "Do you want to save your current settings to file? (yes/no)",
        ("Do you want to save your current settings to file? (yes/no)", "Spanish") -> "¿Desea guardar su configuración actual en el archivo? (sí/no)",
        ("Do you want to save your current settings to file? (yes/no)", "French") -> "Voulez-vous enregistrer vos paramètres actuels dans le fichier ? (oui/non)",
        ("Do you want to save your current settings to file? (yes/no)", "German") -> "Möchten Sie Ihre aktuellen Einstellungen in der Datei speichern? (ja/nein)",
        
        
        ("Settings saved successfully!", "English") -> "Settings saved successfully!",
        ("Settings saved successfully!", "Spanish") -> "¡Configuración guardada con éxito!",
        ("Settings saved successfully!", "French") -> "Paramètres enregistrés avec succès !",
        ("Settings saved successfully!", "German") -> "Einstellungen erfolgreich gespeichert!",
        
        
        // Common phrases
        ("Type 'help' for available commands.", "English") -> "Type 'help' for available commands.",
        ("Type 'help' for available commands.", "Spanish") -> "Escribe 'help' para ver los comandos disponibles.",
        ("Type 'help' for available commands.", "French") -> "Tapez 'help' pour voir les commandes disponibles.",
        ("Type 'help' for available commands.", "German") -> "Geben Sie 'help' ein, um verfügbare Befehle zu sehen.",
        
        
        ("I'm here to help you learn languages", "English") -> "I'm here to help you learn languages",
        ("I'm here to help you learn languages", "Spanish") -> "Estoy aquí para ayudarte a aprender idiomas",
        ("I'm here to help you learn languages", "French") -> "Je suis là pour vous aider à apprendre des langues",
        ("I'm here to help you learn languages", "German") -> "Ich bin hier, um Ihnen beim Sprachenlernen zu helfen",
        
        
        ("What would you like to do today?", "English") -> "What would you like to do today?",
        ("What would you like to do today?", "Spanish") -> "¿Qué te gustaría hacer hoy?",
        ("What would you like to do today?", "French") -> "Que souhaitez-vous faire aujourd'hui ?",
        ("What would you like to do today?", "German") -> "Was möchten Sie heute machen?",
        
        
        // Greeting responses
        ("Hello! I'm your Language Learning Bot. Would you like to practice vocabulary, grammar, or start a quiz?", "English") -> 
          "Hello! I'm your Language Learning Bot. Would you like to practice vocabulary, grammar, or start a quiz?",
        ("Hello! I'm your Language Learning Bot. Would you like to practice vocabulary, grammar, or start a quiz?", "Spanish") -> 
          "¡Hola! Soy tu Bot de Aprendizaje de Idiomas. ¿Te gustaría practicar vocabulario, gramática o comenzar un cuestionario?",
        ("Hello! I'm your Language Learning Bot. Would you like to practice vocabulary, grammar, or start a quiz?", "French") -> 
          "Bonjour ! Je suis votre Bot d'Apprentissage des Langues. Souhaitez-vous pratiquer le vocabulaire, la grammaire ou commencer un quiz ?",
        ("Hello! I'm your Language Learning Bot. Would you like to practice vocabulary, grammar, or start a quiz?", "German") -> 
          "Hallo! Ich bin Ihr Sprachlern-Bot. Möchten Sie Vokabeln üben, Grammatik lernen oder ein Quiz starten?",
        
        
        // Other common responses
        ("I'd be happy to help with your language question. Could you be more specific?", "English") -> 
          "I'd be happy to help with your language question. Could you be more specific?",
        ("I'd be happy to help with your language question. Could you be more specific?", "Spanish") -> 
          "Con gusto te ayudo con tu pregunta sobre idiomas. ¿Podrías ser más específico?",
        ("I'd be happy to help with your language question. Could you be more specific?", "French") -> 
          "Je serais ravi de vous aider avec votre question linguistique. Pourriez-vous être plus précis ?",
        ("I'd be happy to help with your language question. Could you be more specific?", "German") -> 
          "Ich helfe Ihnen gerne bei Ihrer Sprachfrage. Könnten Sie das genauer erläutern?",
        
        
        ("I'd be happy to start a quiz! What type would you prefer? We offer:\n1. Vocabulary\n2. Grammar\n3. Translation\n4. Multiple Choice", "English") -> 
          "I'd be happy to start a quiz! What type would you prefer? We offer:\n1. Vocabulary\n2. Grammar\n3. Translation\n4. Multiple Choice",
        ("I'd be happy to start a quiz! What type would you prefer? We offer:\n1. Vocabulary\n2. Grammar\n3. Translation\n4. Multiple Choice", "Spanish") -> 
          "¡Con gusto comenzamos un cuestionario! ¿Qué tipo prefieres? Ofrecemos:\n1. Vocabulario\n2. Gramática\n3. Traducción\n4. Opción Múltiple",
        ("I'd be happy to start a quiz! What type would you prefer? We offer:\n1. Vocabulary\n2. Grammar\n3. Translation\n4. Multiple Choice", "French") -> 
          "Je serais ravi de commencer un quiz ! Quel type préférez-vous ? Nous proposons :\n1. Vocabulaire\n2. Grammaire\n3. Traduction\n4. Choix Multiple",
        ("I'd be happy to start a quiz! What type would you prefer? We offer:\n1. Vocabulary\n2. Grammar\n3. Translation\n4. Multiple Choice", "German") -> 
          "Ich starte gerne ein Quiz! Welchen Typ bevorzugen Sie? Wir bieten:\n1. Vokabeln\n2. Grammatik\n3. Übersetzung\n4. Multiple-Choice",
        
        
        ("You can change your settings for:\n- Mother language\n- Target language\n- Difficulty level\nWhat would you like to update?", "English") -> 
          "You can change your settings for:\n- Mother language\n- Target language\n- Difficulty level\nWhat would you like to update?",
        ("You can change your settings for:\n- Mother language\n- Target language\n- Difficulty level\nWhat would you like to update?", "Spanish") -> 
          "Puedes modificar tu configuración para:\n- Idioma materno\n- Idioma objetivo\n- Nivel de dificultad\n¿Qué te gustaría actualizar?",
        ("You can change your settings for:\n- Mother language\n- Target language\n- Difficulty level\nWhat would you like to update?", "French") -> 
          "Vous pouvez modifier vos paramètres pour :\n- Langue maternelle\n- Langue cible\n- Niveau de difficulté\nQue souhaitez-vous mettre à jour ?",
        ("You can change your settings for:\n- Mother language\n- Target language\n- Difficulty level\nWhat would you like to update?", "German") -> 
          "Sie können Ihre Einstellungen ändern für:\n- Muttersprache\n- Zielsprache\n- Schwierigkeitsgrad\nWas möchten Sie aktualisieren?",
        

        // Quiz-related translations
        ("What type of quiz would you like to take?", "English") -> 
          "What type of quiz would you like to take?",
        ("What type of quiz would you like to take?", "Spanish") -> 
          "¿Qué tipo de cuestionario te gustaría realizar?",
        ("What type of quiz would you like to take?", "French") -> 
          "Quel type de quiz souhaitez-vous passer ?",
        ("What type of quiz would you like to take?", "German") -> 
          "Welche Art von Quiz möchten Sie machen?",
        
          
        ("Available quiz types: vocabulary, grammar, translation, mcq", "English") -> 
          "Available quiz types: vocabulary, grammar, translation, mcq",
        ("Available quiz types: vocabulary, grammar, translation, mcq", "Spanish") -> 
          "Tipos de cuestionarios disponibles: vocabulario, gramática, traducción, opción múltiple",
        ("Available quiz types: vocabulary, grammar, translation, mcq", "French") -> 
          "Types de quiz disponibles : vocabulaire, grammaire, traduction, choix multiple",
        ("Available quiz types: vocabulary, grammar, translation, mcq", "German") -> 
          "Verfügbare Quiz-Typen: Vokabeln, Grammatik, Übersetzung, Multiple-Choice",
        

        ("Cancelling quiz setup.", "English") -> 
          "Cancelling quiz setup.",
        ("Cancelling quiz setup.", "Spanish") -> 
          "Cancelando la configuración del cuestionario.",
        ("Cancelling quiz setup.", "French") -> 
          "Annulation de la configuration du quiz.",
        ("Cancelling quiz setup.", "German") -> 
          "Quiz-Einrichtung wird abgebrochen.",
        

        ("Exiting quiz mode.", "English") -> 
          "Exiting quiz mode.",
        ("Exiting quiz mode.", "Spanish") -> 
          "Saliendo del modo cuestionario.",
        ("Exiting quiz mode.", "French") -> 
          "Sortie du mode quiz.",
        ("Exiting quiz mode.", "German") -> 
          "Quiz-Modus wird beendet.",
        

        ("There was an error with the quiz. Exiting quiz mode.", "English") -> 
          "There was an error with the quiz. Exiting quiz mode.",
        ("There was an error with the quiz. Exiting quiz mode.", "Spanish") -> 
          "Hubo un error con el cuestionario. Saliendo del modo cuestionario.",
        ("There was an error with the quiz. Exiting quiz mode.", "French") -> 
          "Une erreur s'est produite avec le quiz. Sortie du mode quiz.",
        ("There was an error with the quiz. Exiting quiz mode.", "German") -> 
          "Es gab einen Fehler beim Quiz. Quiz-Modus wird beendet.",
        
        ("No questions available.", "English") -> 
          "No questions available.",
        ("No questions available.", "Spanish") -> 
          "No hay preguntas disponibles.",
        ("No questions available.", "French") -> 
          "Aucune question disponible.",
        ("No questions available.", "German") -> 
          "Keine Fragen verfügbar.",
        

        ("Sorry, I don't recognize that quiz type. Please try again.", "English") -> 
          "Sorry, I don't recognize that quiz type. Please try again.",
        ("Sorry, I don't recognize that quiz type. Please try again.", "Spanish") -> 
          "Lo siento, no reconozco ese tipo de cuestionario. Por favor, inténtalo de nuevo.",
        ("Sorry, I don't recognize that quiz type. Please try again.", "French") -> 
          "Désolé, je ne reconnais pas ce type de quiz. Veuillez réessayer.",
        ("Sorry, I don't recognize that quiz type. Please try again.", "German") -> 
          "Entschuldigung, ich erkenne diesen Quiz-Typ nicht. Bitte versuchen Sie es erneut.",
        
        ("Please set your language preferences before starting a quiz.", "English") -> 
          "Please set your language preferences before starting a quiz.",
        ("Please set your language preferences before starting a quiz.", "Spanish") -> 
          "Por favor, configura tus preferencias de idioma antes de comenzar un cuestionario.",
        ("Please set your language preferences before starting a quiz.", "French") -> 
          "Veuillez définir vos préférences linguistiques avant de commencer un quiz.",
        ("Please set your language preferences before starting a quiz.", "German") -> 
          "Bitte legen Sie Ihre Spracheinstellungen fest, bevor Sie ein Quiz starten.",
        
        
        ("I'm not sure I understand. Try asking about quizzes, changing settings.", "English") -> 
          "I'm not sure I understand. Try asking about quizzes, changing settings.",
        ("I'm not sure I understand. Try asking about quizzes, changing settings.", "French") -> 
          "Je ne suis pas sûr de comprendre. Essayez de poser des questions sur les quiz ou de modifier les paramètres.",
        ("I'm not sure I understand. Try asking about quizzes, changing settings.", "Spanish") -> 
          "No estoy seguro de entender. Intenta preguntar sobre cuestionarios o cambiar la configuración.",
        ("I'm not sure I understand. Try asking about quizzes, changing settings.", "German") -> 
          "Ich bin mir nicht sicher, ob ich das verstehe. Versuchen Sie, nach Quiz zu fragen oder die Einstellungen zu ändern.",
        
        // Settings messages
        ("Mother language set to", "English") -> "Mother language set to",
        ("Mother language set to", "Spanish") -> "Idioma materno establecido en",
        ("Mother language set to", "French") -> "Langue maternelle définie sur",
        ("Mother language set to", "German") -> "Muttersprache festgelegt auf",
        
        
        ("Target language set to", "English") -> "Target language set to",
        ("Target language set to", "Spanish") -> "Idioma objetivo establecido en",
        ("Target language set to", "French") -> "Langue cible définie sur",
        ("Target language set to", "German") -> "Zielsprache festgelegt auf",
        
        
        ("Difficulty set to", "English") -> "Difficulty set to",
        ("Difficulty set to", "Spanish") -> "Nivel de dificultad establecido en",
        ("Difficulty set to", "French") -> "Niveau de difficulté défini sur",
        ("Difficulty set to", "German") -> "Schwierigkeitsgrad festgelegt auf",
        

        ("Welcome to the Language Learning Bot. How can I help you today?", "Spanish") -> "¡Bienvenido al Bot de Aprendizaje de Idiomas. ¿Cómo puedo ayudarte hoy?",
        ("Welcome to the Language Learning Bot. How can I help you today?", "French") -> "Bienvenue sur le Bot d'Apprentissage des Langues. Comment puis-je vous aider aujourd'hui ?",
        ("Welcome to the Language Learning Bot. How can I help you today?", "German") -> "Willkommen beim Sprachlern-Bot. Wie kann ich Ihnen heute helfen?",
      
        ("Welcome to the Language Learning Bot. How can I help you today?", "English") -> "Welcome to the Language Learning Bot. How can I help you today?"
      )
      
      // Convert language to string for lookup
      val langStr = languageToString(lang)
      
      // Check if we have a translation for the exact text
      if (translations.contains((text, langStr))) {
        translations((text, langStr))
      } else {
        // For simplicity, if no exact match, look for partial matches at the beginning
        val matchingPrefix = translations.keys
          .filter(_._2 == langStr)
          .map(_._1)
          .find(phrase => text.startsWith(phrase))
        
        matchingPrefix match {
          case Some(prefix) => 
            // If we find a prefix match, translate that part and keep the rest
            val translated = translations((prefix, langStr))
            val remainder = text.substring(prefix.length)
            translated + remainder
          case None => 
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
    
    // Helper method to convert Language to string
    def languageToString(language: Language): String = {
      language match {
        case English => "English"
        case Spanish => "Spanish"
        case French => "French"
        case German => "German"
        
        case _ => "Unknown"
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
    else if (isQuizRequest(tokens)) "quiz_request"
    else if (isSettingsChange(tokens)) "settings"
    else if (isHelp(tokens)) "help"
    else if (isLanguageQuestion(tokens)) "language_question"
    else "unknown"
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
    
  val quizSelectionSpanish: String = """¿Qué tipo de cuestionario te gustaría realizar? Ofrecemos:
    |1. Vocabulario - Pon a prueba tu conocimiento de palabras
    |2. Gramática - Practica reglas del lenguaje
    |3. Traducción - Traduce entre idiomas
    |4. Opción múltiple (MCQ) - Preguntas generales de idioma
    |
    |Por favor, escribe el número (1-4) o el nombre del cuestionario que deseas realizar.""".stripMargin
    
  val quizSelectionFrench: String = """Quel type de quiz souhaitez-vous passer ? Nous proposons :
    |1. Vocabulaire - Testez votre connaissance des mots
    |2. Grammaire - Pratiquez les règles de langue
    |3. Traduction - Traduisez entre les langues
    |4. Choix multiple (MCQ) - Questions générales sur la langue
    |
    |Veuillez taper le numéro (1-4) ou le nom du quiz que vous souhaitez passer.""".stripMargin
    
  val quizSelectionGerman: String = """Welche Art von Quiz möchten Sie machen? Wir bieten:
    |1. Vokabular - Testen Sie Ihre Wortkenntnisse
    |2. Grammatik - Üben Sie Sprachregeln
    |3. Übersetzung - Übersetzen Sie zwischen Sprachen
    |4. Multiple Choice (MCQ) - Allgemeine Sprachfragen
    |
    |Bitte geben Sie die Nummer (1-4) oder den Namen des Quiz ein, das Sie machen möchten.""".stripMargin
    
  
}
