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
   * Parses user input into a list of tokens for easier processing
   */
  def parseInput(input: String): List[String] = {
    input.toLowerCase.replaceAll("[^a-zA-Z0-9áéíóúüñçßÄÖÜäöü\\s]", "").split("\\s+").toList
  }
  
  /**
   * Categorizes and processes the user query via pattern matching
   */
  def handleUserInput(input: String, preferences: Option[UserPreferences] = None, currentUser: Option[User] = None): String = {
    val tokens = parseInput(input)
    
    // Pattern matching on input tokens to determine intent
    tokens match {
      case x if isGreeting(x) => 
        generateResponse("greeting", preferences, currentUser)
      
      case x if isQuizRequest(x) =>
        generateResponse("quiz_request", preferences, currentUser)
      
      case x if isSettingsChange(x) =>
        generateResponse("settings", preferences, currentUser)
      
      case x if isHelp(x) =>
        generateResponse("help", preferences, currentUser)
        
      case x if isLanguageQuestion(x) =>
        generateResponse("language_question", preferences, currentUser)
        
      case _ => 
        generateResponse("unknown", preferences, currentUser)
    }
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
            "Hello! I'm your Language Learning Bot. Please sign in or continue as guest to get started."
        }
      
      case "quiz_request" => 
        currentUser match {
          case Some(_) =>
            "I'd be happy to start a quiz! What type would you prefer? We offer:\n" +
            "1. Vocabulary\n2. Grammar\n3. Translation\n4. Multiple Choice\n5. Correction"
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
            "I'd be happy to help with your language question. Could you be more specific?"
          case None =>
            "Please sign in or continue as guest to ask language questions."
        }
      
      case "unknown" => 
        currentUser match {
          case Some(_) =>
            "I'm not sure I understand. Try asking about quizzes, changing settings, or asking a specific language question."
          case None =>
            "Please sign in or continue as guest to use the language learning features."
        }
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
        ("Hello", "French") -> "Bonjour !",
        ("Hello", "German") -> "Hallo!",
        ("Hello", "Arabic") -> "مرحباً!",
        
        // Common phrases
        ("Type 'help' for available commands.", "English") -> "Type 'help' for available commands.",
        ("Type 'help' for available commands.", "Spanish") -> "Escribe 'help' para ver los comandos disponibles.",
        ("Type 'help' for available commands.", "French") -> "Tapez 'help' pour voir les commandes disponibles.",
        ("Type 'help' for available commands.", "German") -> "Geben Sie 'help' ein, um verfügbare Befehle zu sehen.",
        ("Type 'help' for available commands.", "Arabic") -> "اكتب 'help' لرؤية الأوامر المتاحة.",
        
        ("I'm here to help you learn languages", "English") -> "I'm here to help you learn languages",
        ("I'm here to help you learn languages", "Spanish") -> "Estoy aquí para ayudarte a aprender idiomas",
        ("I'm here to help you learn languages", "French") -> "Je suis là pour vous aider à apprendre des langues",
        ("I'm here to help you learn languages", "German") -> "Ich bin hier, um Ihnen beim Sprachenlernen zu helfen",
        ("I'm here to help you learn languages", "Arabic") -> "أنا هنا لمساعدتك في تعلم اللغات",
        
        ("What would you like to do today?", "English") -> "What would you like to do today?",
        ("What would you like to do today?", "Spanish") -> "¿Qué te gustaría hacer hoy?",
        ("What would you like to do today?", "French") -> "Que souhaitez-vous faire aujourd'hui ?",
        ("What would you like to do today?", "German") -> "Was möchten Sie heute machen?",
        ("What would you like to do today?", "Arabic") -> "ماذا تريد أن تفعل اليوم؟",
        
        // Greeting responses
        ("Hello! I'm your Language Learning Bot. Would you like to practice vocabulary, grammar, or start a quiz?", "English") -> 
          "Hello! I'm your Language Learning Bot. Would you like to practice vocabulary, grammar, or start a quiz?",
        ("Hello! I'm your Language Learning Bot. Would you like to practice vocabulary, grammar, or start a quiz?", "Spanish") -> 
          "¡Hola! Soy tu Bot de Aprendizaje de Idiomas. ¿Te gustaría practicar vocabulario, gramática o comenzar un cuestionario?",
        ("Hello! I'm your Language Learning Bot. Would you like to practice vocabulary, grammar, or start a quiz?", "French") -> 
          "Bonjour ! Je suis votre Bot d'Apprentissage des Langues. Souhaitez-vous pratiquer le vocabulaire, la grammaire ou commencer un quiz ?",
        ("Hello! I'm your Language Learning Bot. Would you like to practice vocabulary, grammar, or start a quiz?", "German") -> 
          "Hallo! Ich bin Ihr Sprachlern-Bot. Möchten Sie Vokabeln üben, Grammatik lernen oder ein Quiz starten?",
        ("Hello! I'm your Language Learning Bot. Would you like to practice vocabulary, grammar, or start a quiz?", "Arabic") -> 
          "مرحباً! أنا روبوت تعلم اللغات الخاص بك. هل ترغب في ممارسة المفردات أو القواعد أو بدء اختبار؟",
        
        // Other common responses
        ("I'd be happy to help with your language question. Could you be more specific?", "English") -> 
          "I'd be happy to help with your language question. Could you be more specific?",
        ("I'd be happy to help with your language question. Could you be more specific?", "Spanish") -> 
          "Con gusto te ayudo con tu pregunta sobre idiomas. ¿Podrías ser más específico?",
        ("I'd be happy to help with your language question. Could you be more specific?", "French") -> 
          "Je serais ravi de vous aider avec votre question linguistique. Pourriez-vous être plus précis ?",
        ("I'd be happy to help with your language question. Could you be more specific?", "German") -> 
          "Ich helfe Ihnen gerne bei Ihrer Sprachfrage. Könnten Sie das genauer erläutern?",
        ("I'd be happy to help with your language question. Could you be more specific?", "Arabic") -> 
          "سأكون سعيداً بمساعدتك في سؤالك اللغوي. هل يمكنك أن تكون أكثر تحديداً؟",
        
        ("I'd be happy to start a quiz! What type would you prefer? We offer:\n1. Vocabulary\n2. Grammar\n3. Translation\n4. Multiple Choice\n5. Correction", "English") -> 
          "I'd be happy to start a quiz! What type would you prefer? We offer:\n1. Vocabulary\n2. Grammar\n3. Translation\n4. Multiple Choice\n5. Correction",
        ("I'd be happy to start a quiz! What type would you prefer? We offer:\n1. Vocabulary\n2. Grammar\n3. Translation\n4. Multiple Choice\n5. Correction", "Spanish") -> 
          "¡Con gusto comenzamos un cuestionario! ¿Qué tipo prefieres? Ofrecemos:\n1. Vocabulario\n2. Gramática\n3. Traducción\n4. Opción Múltiple\n5. Corrección",
        ("I'd be happy to start a quiz! What type would you prefer? We offer:\n1. Vocabulary\n2. Grammar\n3. Translation\n4. Multiple Choice\n5. Correction", "French") -> 
          "Je serais ravi de commencer un quiz ! Quel type préférez-vous ? Nous proposons :\n1. Vocabulaire\n2. Grammaire\n3. Traduction\n4. Choix Multiple\n5. Correction",
        ("I'd be happy to start a quiz! What type would you prefer? We offer:\n1. Vocabulary\n2. Grammar\n3. Translation\n4. Multiple Choice\n5. Correction", "German") -> 
          "Ich starte gerne ein Quiz! Welchen Typ bevorzugen Sie? Wir bieten:\n1. Vokabeln\n2. Grammatik\n3. Übersetzung\n4. Multiple-Choice\n5. Korrektur",
        ("I'd be happy to start a quiz! What type would you prefer? We offer:\n1. Vocabulary\n2. Grammar\n3. Translation\n4. Multiple Choice\n5. Correction", "Arabic") -> 
          "سأكون سعيداً ببدء اختبار! ما هو النوع الذي تفضله؟ نقدم:\n1. المفردات\n2. القواعد\n3. الترجمة\n4. اختيار من متعدد\n5. التصحيح",
        
        ("You can change your settings for:\n- Mother language\n- Target language\n- Difficulty level\nWhat would you like to update?", "English") -> 
          "You can change your settings for:\n- Mother language\n- Target language\n- Difficulty level\nWhat would you like to update?",
        ("You can change your settings for:\n- Mother language\n- Target language\n- Difficulty level\nWhat would you like to update?", "Spanish") -> 
          "Puedes modificar tu configuración para:\n- Idioma materno\n- Idioma objetivo\n- Nivel de dificultad\n¿Qué te gustaría actualizar?",
        ("You can change your settings for:\n- Mother language\n- Target language\n- Difficulty level\nWhat would you like to update?", "French") -> 
          "Vous pouvez modifier vos paramètres pour :\n- Langue maternelle\n- Langue cible\n- Niveau de difficulté\nQue souhaitez-vous mettre à jour ?",
        ("You can change your settings for:\n- Mother language\n- Target language\n- Difficulty level\nWhat would you like to update?", "German") -> 
          "Sie können Ihre Einstellungen ändern für:\n- Muttersprache\n- Zielsprache\n- Schwierigkeitsgrad\nWas möchten Sie aktualisieren?",
        ("You can change your settings for:\n- Mother language\n- Target language\n- Difficulty level\nWhat would you like to update?", "Arabic") -> 
          "يمكنك تغيير إعداداتك لـ:\n- اللغة الأم\n- اللغة الهدف\n- مستوى الصعوبة\nماذا تريد تحديثه؟",

        // Quiz-related translations
        ("What type of quiz would you like to take?", "English") -> 
          "What type of quiz would you like to take?",
        ("What type of quiz would you like to take?", "Spanish") -> 
          "¿Qué tipo de cuestionario te gustaría realizar?",
        ("What type of quiz would you like to take?", "French") -> 
          "Quel type de quiz souhaitez-vous passer ?",
        ("What type of quiz would you like to take?", "German") -> 
          "Welche Art von Quiz möchten Sie machen?",
        ("What type of quiz would you like to take?", "Arabic") -> 
          "ما نوع الاختبار الذي ترغب في إجرائه؟",
          
        ("Available quiz types: vocabulary, grammar, translation, mcq", "English") -> 
          "Available quiz types: vocabulary, grammar, translation, mcq",
        ("Available quiz types: vocabulary, grammar, translation, mcq", "Spanish") -> 
          "Tipos de cuestionarios disponibles: vocabulario, gramática, traducción, opción múltiple",
        ("Available quiz types: vocabulary, grammar, translation, mcq", "French") -> 
          "Types de quiz disponibles : vocabulaire, grammaire, traduction, choix multiple",
        ("Available quiz types: vocabulary, grammar, translation, mcq", "German") -> 
          "Verfügbare Quiz-Typen: Vokabeln, Grammatik, Übersetzung, Multiple-Choice",
        ("Available quiz types: vocabulary, grammar, translation, mcq", "Arabic") -> 
          "أنواع الاختبارات المتاحة: المفردات، القواعد، الترجمة، الاختيار من متعدد",

        ("Exiting quiz mode.", "English") -> 
          "Exiting quiz mode.",
        ("Exiting quiz mode.", "Spanish") -> 
          "Saliendo del modo cuestionario.",
        ("Exiting quiz mode.", "French") -> 
          "Sortie du mode quiz.",
        ("Exiting quiz mode.", "German") -> 
          "Quiz-Modus wird beendet.",
        ("Exiting quiz mode.", "Arabic") -> 
          "الخروج من وضع الاختبار.",

        ("There was an error with the quiz. Exiting quiz mode.", "English") -> 
          "There was an error with the quiz. Exiting quiz mode.",
        ("There was an error with the quiz. Exiting quiz mode.", "Spanish") -> 
          "Hubo un error con el cuestionario. Saliendo del modo cuestionario.",
        ("There was an error with the quiz. Exiting quiz mode.", "French") -> 
          "Une erreur s'est produite avec le quiz. Sortie du mode quiz.",
        ("There was an error with the quiz. Exiting quiz mode.", "German") -> 
          "Es gab einen Fehler beim Quiz. Quiz-Modus wird beendet.",
        ("There was an error with the quiz. Exiting quiz mode.", "Arabic") -> 
          "حدث خطأ في الاختبار. الخروج من وضع الاختبار.",

        ("No questions available.", "English") -> 
          "No questions available.",
        ("No questions available.", "Spanish") -> 
          "No hay preguntas disponibles.",
        ("No questions available.", "French") -> 
          "Aucune question disponible.",
        ("No questions available.", "German") -> 
          "Keine Fragen verfügbar.",
        ("No questions available.", "Arabic") -> 
          "لا توجد أسئلة متاحة.",

        ("Sorry, I don't recognize that quiz type. Please try again.", "English") -> 
          "Sorry, I don't recognize that quiz type. Please try again.",
        ("Sorry, I don't recognize that quiz type. Please try again.", "Spanish") -> 
          "Lo siento, no reconozco ese tipo de cuestionario. Por favor, inténtalo de nuevo.",
        ("Sorry, I don't recognize that quiz type. Please try again.", "French") -> 
          "Désolé, je ne reconnais pas ce type de quiz. Veuillez réessayer.",
        ("Sorry, I don't recognize that quiz type. Please try again.", "German") -> 
          "Entschuldigung, ich erkenne diesen Quiz-Typ nicht. Bitte versuchen Sie es erneut.",
        ("Sorry, I don't recognize that quiz type. Please try again.", "Arabic") -> 
          "عذراً، لا أتعرف على نوع الاختبار هذا. يرجى المحاولة مرة أخرى.",

        ("Please set your language preferences before starting a quiz.", "English") -> 
          "Please set your language preferences before starting a quiz.",
        ("Please set your language preferences before starting a quiz.", "Spanish") -> 
          "Por favor, configura tus preferencias de idioma antes de comenzar un cuestionario.",
        ("Please set your language preferences before starting a quiz.", "French") -> 
          "Veuillez définir vos préférences linguistiques avant de commencer un quiz.",
        ("Please set your language preferences before starting a quiz.", "German") -> 
          "Bitte legen Sie Ihre Spracheinstellungen fest, bevor Sie ein Quiz starten.",
        ("Please set your language preferences before starting a quiz.", "Arabic") -> 
          "يرجى تحديد تفضيلات اللغة قبل بدء الاختبار.",
        
        ("I'm not sure I understand. Try asking about quizzes, changing settings, or asking a specific language question.", "English") -> 
          "I'm not sure I understand. Try asking about quizzes, changing settings, or asking a specific language question.",
        ("I'm not sure I understand. Try asking about quizzes, changing settings, or asking a specific language question.", "Spanish") -> 
          "No estoy seguro de entender. Intenta preguntar sobre cuestionarios, cambiar configuraciones o hacer una pregunta específica sobre idiomas.",
        ("I'm not sure I understand. Try asking about quizzes, changing settings, or asking a specific language question.", "French") -> 
          "Je ne suis pas sûr de comprendre. Essayez de poser des questions sur les quiz, de modifier les paramètres ou de poser une question linguistique spécifique.",
        ("I'm not sure I understand. Try asking about quizzes, changing settings, or asking a specific language question.", "German") -> 
          "Ich bin mir nicht sicher, ob ich verstehe. Versuchen Sie, nach Quiz zu fragen, Einstellungen zu ändern oder eine bestimmte Sprachfrage zu stellen.",
        ("I'm not sure I understand. Try asking about quizzes, changing settings, or asking a specific language question.", "Arabic") -> 
          "لست متأكداً من فهمي. حاول السؤال عن الاختبارات أو تغيير الإعدادات أو طرح سؤال محدد حول اللغة.",
        
        // Settings messages
        ("Mother language set to", "English") -> "Mother language set to",
        ("Mother language set to", "Spanish") -> "Idioma materno establecido en",
        ("Mother language set to", "French") -> "Langue maternelle définie sur",
        ("Mother language set to", "German") -> "Muttersprache festgelegt auf",
        ("Mother language set to", "Arabic") -> "تم تعيين اللغة الأم إلى",
        
        ("Target language set to", "English") -> "Target language set to",
        ("Target language set to", "Spanish") -> "Idioma objetivo establecido en",
        ("Target language set to", "French") -> "Langue cible définie sur",
        ("Target language set to", "German") -> "Zielsprache festgelegt auf",
        ("Target language set to", "Arabic") -> "تم تعيين اللغة الهدف إلى",
        
        ("Difficulty set to", "English") -> "Difficulty set to",
        ("Difficulty set to", "Spanish") -> "Nivel de dificultad establecido en",
        ("Difficulty set to", "French") -> "Niveau de difficulté défini sur",
        ("Difficulty set to", "German") -> "Schwierigkeitsgrad festgelegt auf",
        ("Difficulty set to", "Arabic") -> "تم تعيين مستوى الصعوبة إلى",

        ("Welcome to the Language Learning Bot. How can I help you today?", "Spanish") -> "¡Bienvenido al Bot de Aprendizaje de Idiomas. ¿Cómo puedo ayudarte hoy?",
        ("Welcome to the Language Learning Bot. How can I help you today?", "French") -> "Bienvenue sur le Bot d'Apprentissage des Langues. Comment puis-je vous aider aujourd'hui ?",
        ("Welcome to the Language Learning Bot. How can I help you today?", "German") -> "Willkommen beim Sprachlern-Bot. Wie kann ich Ihnen heute helfen?",
        ("Welcome to the Language Learning Bot. How can I help you today?", "Arabic") -> "مرحباً بك في روبوت تعلم اللغات. كيف يمكنني مساعدتك اليوم؟"
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

  /**
   * Categorizes user input into a specific category
   */
  def categorizeInput(input: String): String = {
    val tokens = parseInput(input)
    
    if (isGreeting(tokens)) "greeting"
    else if (isQuizRequest(tokens)) "quiz_request"
    else if (isSettingsChange(tokens)) "settings"
    else if (isHelp(tokens)) "help"
    else if (isLanguageQuestion(tokens)) "language_question"
    else "unknown"
  }
}
