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
    category match {
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
    // If not, create new preferences only if all required fields are provided
    currentPreferences match {
      case Some(prefs) => 
        Some(UserPreferences(
          motherLanguage.getOrElse(prefs.motherLanguage),
          targetLanguage.getOrElse(prefs.targetLanguage),
          difficulty.getOrElse(prefs.difficulty),
          name.orElse(prefs.name)
        ))
        
      case None =>
        for {
          ml <- motherLanguage
          tl <- targetLanguage
          d <- difficulty
        } yield UserPreferences(ml, tl, d, name)
    }
  }
  
  /**
   * Retrieve saved preferences safely using Option
   */
  def getUserPreferences(userPreferences: Option[UserPreferences]): Option[UserPreferences] = {
    userPreferences
  }
}
