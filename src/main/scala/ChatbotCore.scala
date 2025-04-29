package languagelearningbot

import scala.util.matching.Regex

object ChatbotCore {
  // Required core chatbot functions as specified in the requirements
  
  /**
   * Returns an initial greeting based on user preferences if any
   */
  def greetUser(preferences: Option[UserPreferences] = None): String = {
    preferences match {
      case Some(prefs) => prefs.targetLanguage match {
        case English => "Hello! Welcome to the Language Learning Bot. How can I help you today?"
        case Spanish => "¡Hola! Bienvenido al Bot de Aprendizaje de Idiomas. ¿Cómo puedo ayudarte hoy?"
        case French => "Bonjour! Bienvenue sur le Bot d'Apprentissage des Langues. Comment puis-je vous aider aujourd'hui?"
        case German => "Hallo! Willkommen beim Sprachlern-Bot. Wie kann ich Ihnen heute helfen?"
        case Arabic => "مرحبًا! مرحبًا بك في روبوت تعلم اللغة. كيف يمكنني مساعدتك اليوم؟"
      }
      case None => "Welcome to the Language Learning Bot! Please set your preferred language to get started."
    }
  }
  
  /**
   * Parses user input into a list of tokens for easier processing
   */
  def parseInput(input: String): List[String] = {
    input.toLowerCase.replaceAll("[^a-zA-Z0-9áéíóúüñçßÄÖÜäöü\\s]", "").split("\\s+").toList
  }
  
  /**
   * Categorizes and processes the user query via pattern matching
   */
  def handleUserInput(input: String, preferences: Option[UserPreferences] = None): String = {
    val tokens = parseInput(input)
    
    // Pattern matching on input tokens to determine intent
    tokens match {
      case x if isGreeting(x) => 
        generateResponse("greeting", preferences)
      
      case x if isQuizRequest(x) =>
        generateResponse("quiz_request", preferences)
      
      case x if isSettingsChange(x) =>
        generateResponse("settings", preferences)
      
      case x if isHelp(x) =>
        generateResponse("help", preferences)
        
      case x if isLanguageQuestion(x) =>
        generateResponse("language_question", preferences)
        
      case _ => 
        generateResponse("unknown", preferences)
    }
  }
  
  /**
   * Crafts a well-structured response based on the query category
   */
  def generateResponse(category: String, preferences: Option[UserPreferences] = None): String = {
    // Get base response in English
    val baseResponse = category match {
      case "greeting" => 
        "Hello! I'm your Language Learning Bot. Would you like to practice vocabulary, grammar, or start a quiz?"
      
      case "quiz_request" => 
        "I'd be happy to start a quiz! What type would you prefer? We offer:\n" +
        "1. Vocabulary\n2. Grammar\n3. Translation\n4. Multiple Choice\n5. Correction"
      
      case "settings" => 
        "You can change your settings for:\n" +
        "- Mother language\n- Target language\n- Difficulty level\n" +
        "What would you like to update?"
      
      case "help" => 
        "I'm here to help you learn languages! You can:\n" +
        "- Start a quiz by typing 'quiz'\n" +
        "- Change settings with 'settings'\n" +
        "- View your progress with 'analytics'\n" +
        "- Ask language questions directly"
        
      case "language_question" =>
        "I'd be happy to help with your language question. Could you be more specific?"
      
      case "unknown" => 
        "I'm not sure I understand. Try asking about quizzes, changing settings, or asking a specific language question."
    }
    
    // Return the base response (will be translated later in formatDualLanguageResponse)
    baseResponse
  }
  
  // Helper functions for intent detection using pattern matching
  private def isGreeting(tokens: List[String]): Boolean = {
    val greetings = Set("hello", "hi", "hey", "greetings", "howdy", "hola", "bonjour", "hallo")
    tokens.exists(greetings.contains)
  }
  
  private def isQuizRequest(tokens: List[String]): Boolean = {
    val quizWords = Set("quiz", "test", "practice", "exercise", "question", "challenge")
    tokens.exists(quizWords.contains)
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
    val questionIndicators = Set("how", "what", "why", "when", "translate", "mean", "say", "spell", "pronounce")
    tokens.exists(questionIndicators.contains)
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
        ("Hello", "French") -> "Bonjour!",
        ("Hello", "German") -> "Hallo!",
        ("Hello", "Arabic") -> "مرحبا!",
        
        // Common phrases
        ("I'm here to help you learn languages", "English") -> "I'm here to help you learn languages",
        ("I'm here to help you learn languages", "Spanish") -> "Estoy aquí para ayudarte a aprender idiomas",
        ("I'm here to help you learn languages", "French") -> "Je suis là pour vous aider à apprendre des langues",
        ("I'm here to help you learn languages", "German") -> "Ich bin hier, um Ihnen beim Sprachenlernen zu helfen",
        ("I'm here to help you learn languages", "Arabic") -> "أنا هنا لمساعدتك في تعلم اللغات",
        
        ("What would you like to do today?", "English") -> "What would you like to do today?",
        ("What would you like to do today?", "Spanish") -> "¿Qué te gustaría hacer hoy?",
        ("What would you like to do today?", "French") -> "Que voulez-vous faire aujourd'hui?",
        ("What would you like to do today?", "German") -> "Was möchten Sie heute tun?",
        ("What would you like to do today?", "Arabic") -> "ماذا تريد أن تفعل اليوم؟",
        
        // Greeting responses
        ("Hello! I'm your Language Learning Bot. Would you like to practice vocabulary, grammar, or start a quiz?", "English") -> 
          "Hello! I'm your Language Learning Bot. Would you like to practice vocabulary, grammar, or start a quiz?",
        ("Hello! I'm your Language Learning Bot. Would you like to practice vocabulary, grammar, or start a quiz?", "Spanish") -> 
          "¡Hola! Soy tu Bot de Aprendizaje de Idiomas. ¿Te gustaría practicar vocabulario, gramática o comenzar un cuestionario?",
        ("Hello! I'm your Language Learning Bot. Would you like to practice vocabulary, grammar, or start a quiz?", "French") -> 
          "Bonjour! Je suis votre Bot d'Apprentissage des Langues. Voulez-vous pratiquer le vocabulaire, la grammaire ou commencer un quiz?",
        ("Hello! I'm your Language Learning Bot. Would you like to practice vocabulary, grammar, or start a quiz?", "German") -> 
          "Hallo! Ich bin Ihr Sprachlern-Bot. Möchten Sie Vokabeln üben, Grammatik lernen oder ein Quiz starten?",
        ("Hello! I'm your Language Learning Bot. Would you like to practice vocabulary, grammar, or start a quiz?", "Arabic") -> 
          "مرحبًا! أنا روبوت تعلم اللغة الخاص بك. هل ترغب في ممارسة المفردات أو القواعد أو بدء اختبار؟",
        
        // Other common responses
        ("I'd be happy to help with your language question. Could you be more specific?", "English") -> 
          "I'd be happy to help with your language question. Could you be more specific?",
        ("I'd be happy to help with your language question. Could you be more specific?", "Spanish") -> 
          "Estaría encantado de ayudarte con tu pregunta sobre idiomas. ¿Podrías ser más específico?",
        ("I'd be happy to help with your language question. Could you be more specific?", "French") -> 
          "Je serais ravi de vous aider avec votre question linguistique. Pourriez-vous être plus précis?",
        ("I'd be happy to help with your language question. Could you be more specific?", "German") -> 
          "Ich helfe Ihnen gerne bei Ihrer Sprachfrage. Könnten Sie etwas genauer sein?",
        ("I'd be happy to help with your language question. Could you be more specific?", "Arabic") -> 
          "سأكون سعيدًا بمساعدتك في سؤالك عن اللغة. هل يمكن أن تكون أكثر تحديدًا؟",
        
        ("I'd be happy to start a quiz! What type would you prefer? We offer:\n1. Vocabulary\n2. Grammar\n3. Translation\n4. Multiple Choice\n5. Correction", "English") -> 
          "I'd be happy to start a quiz! What type would you prefer? We offer:\n1. Vocabulary\n2. Grammar\n3. Translation\n4. Multiple Choice\n5. Correction",
        ("I'd be happy to start a quiz! What type would you prefer? We offer:\n1. Vocabulary\n2. Grammar\n3. Translation\n4. Multiple Choice\n5. Correction", "Spanish") -> 
          "¡Me encantaría comenzar un cuestionario! ¿Qué tipo prefieres? Ofrecemos:\n1. Vocabulario\n2. Gramática\n3. Traducción\n4. Selección Múltiple\n5. Corrección",
        ("I'd be happy to start a quiz! What type would you prefer? We offer:\n1. Vocabulary\n2. Grammar\n3. Translation\n4. Multiple Choice\n5. Correction", "French") -> 
          "Je serais ravi de commencer un quiz! Quel type préférez-vous? Nous proposons:\n1. Vocabulaire\n2. Grammaire\n3. Traduction\n4. Choix Multiple\n5. Correction",
        ("I'd be happy to start a quiz! What type would you prefer? We offer:\n1. Vocabulary\n2. Grammar\n3. Translation\n4. Multiple Choice\n5. Correction", "German") -> 
          "Ich würde gerne ein Quiz starten! Welchen Typ bevorzugen Sie? Wir bieten:\n1. Vokabeln\n2. Grammatik\n3. Übersetzung\n4. Multiple-Choice\n5. Korrektur",
        ("I'd be happy to start a quiz! What type would you prefer? We offer:\n1. Vocabulary\n2. Grammar\n3. Translation\n4. Multiple Choice\n5. Correction", "Arabic") -> 
          "سأكون سعيدًا ببدء اختبار! ما هو النوع الذي تفضله؟ نحن نقدم:\n1. المفردات\n2. القواعد\n3. الترجمة\n4. الاختيار من متعدد\n5. التصحيح",
        
        ("You can change your settings for:\n- Mother language\n- Target language\n- Difficulty level\nWhat would you like to update?", "English") -> 
          "You can change your settings for:\n- Mother language\n- Target language\n- Difficulty level\nWhat would you like to update?",
        ("You can change your settings for:\n- Mother language\n- Target language\n- Difficulty level\nWhat would you like to update?", "Spanish") -> 
          "Puedes cambiar tu configuración para:\n- Idioma materno\n- Idioma objetivo\n- Nivel de dificultad\n¿Qué te gustaría actualizar?",
        ("You can change your settings for:\n- Mother language\n- Target language\n- Difficulty level\nWhat would you like to update?", "French") -> 
          "Vous pouvez modifier vos paramètres pour:\n- Langue maternelle\n- Langue cible\n- Niveau de difficulté\nQue souhaitez-vous mettre à jour?",
        ("You can change your settings for:\n- Mother language\n- Target language\n- Difficulty level\nWhat would you like to update?", "German") -> 
          "Sie können Ihre Einstellungen ändern für:\n- Muttersprache\n- Zielsprache\n- Schwierigkeitsgrad\nWas möchten Sie aktualisieren?",
        ("You can change your settings for:\n- Mother language\n- Target language\n- Difficulty level\nWhat would you like to update?", "Arabic") -> 
          "يمكنك تغيير إعداداتك لـ:\n- اللغة الأم\n- اللغة الهدف\n- مستوى الصعوبة\nماذا تريد أن تحدث؟",
        
        ("I'm not sure I understand. Try asking about quizzes, changing settings, or asking a specific language question.", "English") -> 
          "I'm not sure I understand. Try asking about quizzes, changing settings, or asking a specific language question.",
        ("I'm not sure I understand. Try asking about quizzes, changing settings, or asking a specific language question.", "Spanish") -> 
          "No estoy seguro de entender. Intenta preguntar sobre cuestionarios, cambiar configuraciones o hacer una pregunta específica sobre idiomas.",
        ("I'm not sure I understand. Try asking about quizzes, changing settings, or asking a specific language question.", "French") -> 
          "Je ne suis pas sûr de comprendre. Essayez de poser des questions sur les quiz, de modifier les paramètres ou de poser une question linguistique spécifique.",
        ("I'm not sure I understand. Try asking about quizzes, changing settings, or asking a specific language question.", "German") -> 
          "Ich bin mir nicht sicher, ob ich verstehe. Versuchen Sie, nach Quiz zu fragen, Einstellungen zu ändern oder eine bestimmte Sprachfrage zu stellen.",
        ("I'm not sure I understand. Try asking about quizzes, changing settings, or asking a specific language question.", "Arabic") -> 
          "لست متأكدًا من فهمي. حاول السؤال عن الاختبارات أو تغيير الإعدادات أو طرح سؤال محدد حول اللغة.",
        
        // Settings messages
        ("Mother language set to", "English") -> "Mother language set to",
        ("Mother language set to", "Spanish") -> "Idioma materno establecido a",
        ("Mother language set to", "French") -> "Langue maternelle définie sur",
        ("Mother language set to", "German") -> "Muttersprache festgelegt auf",
        ("Mother language set to", "Arabic") -> "تم تعيين اللغة الأم إلى",
        
        ("Target language set to", "English") -> "Target language set to",
        ("Target language set to", "Spanish") -> "Idioma objetivo establecido a",
        ("Target language set to", "French") -> "Langue cible définie sur",
        ("Target language set to", "German") -> "Zielsprache festgelegt auf",
        ("Target language set to", "Arabic") -> "تم تعيين اللغة الهدف إلى",
        
        ("Difficulty set to", "English") -> "Difficulty set to",
        ("Difficulty set to", "Spanish") -> "Dificultad establecida en",
        ("Difficulty set to", "French") -> "Difficulté définie sur",
        ("Difficulty set to", "German") -> "Schwierigkeitsgrad festgelegt auf",
        ("Difficulty set to", "Arabic") -> "تم تعيين مستوى الصعوبة إلى"
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
              case Arabic => text + " (بالعربية)"
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
        case Arabic => "Arabic"
        case _ => "Unknown"
      }
    }
    
    // Get translation using the helper function
    getTranslation(text, targetLanguage)
  }
}
