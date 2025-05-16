package languagelearningbot

import scala.util.Random

object QuizGenerator {
  // Required quiz generator functions as specified in the requirements

  /**
   * Returns a list of questions based on the chosen language, difficulty, and quiz type
   */
  def selectQuizQuestions(language: Language, difficulty: Difficulty, quizType: QuizType, count: Int = 5): List[Question] = {
    // Get multiple versions of question banks for the given parameters
    val questionBanks = getQuestionBanks(language, difficulty, quizType)

    // Randomly select one version of the question bank
    val selectedBank = if (questionBanks.nonEmpty) {
      // Pick a random version from the available banks
      questionBanks(Random.nextInt(questionBanks.length))
    } else {
      // Default to an empty list if no banks are available
      List()
    }

    // Randomly select questions from the chosen bank
    Random.shuffle(selectedBank).take(count)
  }

  /**
   * Formats and displays the question with its options
   */
  def presentQuizQuestion(question: Question, questionNumber: Int): String = {
    val optionsFormatted = question.options.zipWithIndex.map {
      case (option, idx) => s"${('a' + idx).toChar}) $option"
    }.mkString("\n")

    s"Question $questionNumber: ${question.prompt}\n$optionsFormatted"
  }

  /**
   * Uses pattern matching to classify the answer and provides feedback
   * Accepts either the full answer text or just the letter choice (a, b, c, d)
   * Returns (isCorrect, feedback, processedAnswer) where processedAnswer is the actual answer after processing
   */
  def evaluateQuizAnswer(userAnswer: String, correctAnswer: String, question: Option[Question] = None): (Boolean, String, String) = {
    // First check if user provided a letter answer (a, b, c, d)
    val letterAnswer = userAnswer.toLowerCase.trim match {
      case letter if letter.length == 1 && letter >= "a" && letter <= "d" => 
        // Convert letter to index (a->0, b->1, etc.)
        val index = letter.charAt(0) - 'a'
        
        // Get the corresponding option if question is provided
        question match {
          case Some(q) if index >= 0 && index < q.options.length => 
            Some(q.options(index))
          case _ => None
        }
      case _ => None
    }
    
    // Process the answer (either the original text or the mapped letter option)
    val processedAnswer = letterAnswer.getOrElse(userAnswer)
    
    // This handles the case where user types the full answer exactly as is
    // Compare directly without case conversion to handle special characters properly
    if (processedAnswer == correctAnswer) {
      return (true, "Correct! Well done!", processedAnswer)
    }
    
    // This is for when the user types in the full answer with different capitalization
    if (processedAnswer.toLowerCase.trim == correctAnswer.toLowerCase.trim) {
      return (true, "Correct! Well done!", processedAnswer)
    }

    // If the answer is one of the predefined choices but not the correct one
    val isOption = question match {
      case Some(q) => q.options.map(_.toLowerCase.trim).contains(processedAnswer.toLowerCase.trim)
      case None => false
    }
    
    // Handle normal answer evaluation cases
    val (isCorrect, feedback) = (processedAnswer.toLowerCase.trim, correctAnswer.toLowerCase.trim) match {
      case (u, c) if u.nonEmpty && c.startsWith(u) && !isOption =>
        (false, s"Close, but not quite. The correct answer is: $correctAnswer")

      case ("", _) =>
        (false, "You didn't provide an answer. The correct answer is: " + correctAnswer)

      case _ =>
        (false, s"That's not correct. The correct answer is: $correctAnswer")
    }
    
    // Return the processed answer along with the evaluation results
    (isCorrect, feedback, processedAnswer)
  }

  /**
   * Summarizes quiz performance
   */
  def summarizeQuizResults(quizSession: QuizSession): String = {
    if (quizSession.userAnswers.isEmpty) {
      "You haven't completed any questions yet."
    } else {
      val score = quizSession.calculateScore
      val totalQuestions = quizSession.questions.size
      val accuracy = quizSession.calculateAccuracy

      s"Quiz Complete!\n" +
        s"Score: $score out of $totalQuestions correct\n" +
        s"Accuracy: ${accuracy.toInt}%\n" +
        provideFeedback(score, totalQuestions)
    }
  }

  // Helper function for contextual feedback
  private def provideFeedback(score: Int, total: Int): String = {
    val percentage = (score.toDouble / total) * 100

    percentage match {
      case p if p >= 90 => "Excellent! You're mastering this language!"
      case p if p >= 70 => "Good job! Keep practicing to improve further."
      case p if p >= 50 => "Not bad, but there's room for improvement."
      case _ => "You should review this topic more. Keep practicing!"
    }
  }

  // Sample question banks for demonstration
  /**
   * Get all question banks for the given parameters
   * Returns a list of question banks, where each bank is a list of questions
   */
  private def getQuestionBanks(language: Language, difficulty: Difficulty, quizType: QuizType): List[List[Question]] = {
    // For vocabulary quizzes, we have multiple versions for each difficulty level
    (language, difficulty, quizType) match {
      // Spanish vocabulary quizzes - Easy difficulty (3 versions)
      case (Spanish, Easy, Vocabulary) => List(
        // Version 1 - Fruits and Food
        List(
          Question("What is 'apple' in Spanish?", List("manzana", "plátano", "naranja", "uva"), "manzana", Vocabulary),
          Question("What is 'banana' in Spanish?", List("plátano", "manzana", "pera", "sandía"), "plátano", Vocabulary),
          Question("What is 'bread' in Spanish?", List("pan", "queso", "leche", "agua"), "pan", Vocabulary),
          Question("What is 'water' in Spanish?", List("agua", "vino", "cerveza", "jugo"), "agua", Vocabulary),
          Question("What is 'cheese' in Spanish?", List("queso", "pan", "mantequilla", "huevo"), "queso", Vocabulary)
        ),

        // Version 2 - Household items
        List(
          Question("What is 'house' in Spanish?", List("casa", "apartamento", "edificio", "oficina"), "casa", Vocabulary),
          Question("What is 'table' in Spanish?", List("mesa", "silla", "cama", "sofá"), "mesa", Vocabulary),
          Question("What is 'door' in Spanish?", List("puerta", "ventana", "pared", "techo"), "puerta", Vocabulary),
          Question("What is 'cshair' in Spanish?", List("silla", "mesa", "sofá", "cama"), "silla", Vocabulary),
          Question("What is 'bed' in Spanish?", List("cama", "silla", "mesa", "sofá"), "cama", Vocabulary)
        ),

        // Version 3 - Basic verbs
        List(
          Question("What is 'to eat' in Spanish?", List("comer", "beber", "dormir", "hablar"), "comer", Vocabulary),
          Question("What is 'to sleep' in Spanish?", List("dormir", "comer", "beber", "hablar"), "dormir", Vocabulary),
          Question("What is 'to speak' in Spanish?", List("hablar", "escuchar", "leer", "escribir"), "hablar", Vocabulary),
          Question("What is 'to read' in Spanish?", List("leer", "escribir", "hablar", "escuchar"), "leer", Vocabulary),
          Question("What is 'to write' in Spanish?", List("escribir", "leer", "hablar", "escuchar"), "escribir", Vocabulary)
        )
      )

      // Spanish vocabulary quizzes - Medium difficulty (3 versions)
      case (Spanish, Medium, Vocabulary) => List(
        // Version 1 - Professions
        List(
          Question("What is 'doctor' in Spanish?", List("médico", "profesor", "ingeniero", "abogado"), "médico", Vocabulary),
          Question("What is 'teacher' in Spanish?", List("profesor", "estudiante", "médico", "abogado"), "profesor", Vocabulary),
          Question("What is 'engineer' in Spanish?", List("ingeniero", "médico", "profesor", "enfermero"), "ingeniero", Vocabulary),
          Question("What is 'lawyer' in Spanish?", List("abogado", "juez", "policía", "bombero"), "abogado", Vocabulary),
          Question("What is 'nurse' in Spanish?", List("enfermero", "médico", "farmacéutico", "dentista"), "enfermero", Vocabulary)
        ),

        // Version 2 - Traveling
        List(
          Question("What is 'passport' in Spanish?", List("pasaporte", "boleto", "maleta", "hotel"), "pasaporte", Vocabulary),
          Question("What is 'airport' in Spanish?", List("aeropuerto", "estación", "puerto", "terminal"), "aeropuerto", Vocabulary),
          Question("What is 'hotel' in Spanish?", List("hotel", "hostal", "apartamento", "casa"), "hotel", Vocabulary),
          Question("What is 'ticket' in Spanish?", List("boleto", "pasaporte", "equipaje", "reservación"), "boleto", Vocabulary),
          Question("What is 'suitcase' in Spanish?", List("maleta", "mochila", "bolsa", "equipaje"), "maleta", Vocabulary)
        ),

        // Version 3 - Technology
        List(
          Question("What is 'computer' in Spanish?", List("computadora", "teléfono", "tableta", "televisión"), "computadora", Vocabulary),
          Question("What is 'cell phone' in Spanish?", List("teléfono móvil", "computadora", "tableta", "televisión"), "teléfono móvil", Vocabulary),
          Question("What is 'internet' in Spanish?", List("internet", "web", "red", "wifi"), "internet", Vocabulary),
          Question("What is 'printer' in Spanish?", List("impresora", "escáner", "computadora", "monitor"), "impresora", Vocabulary),
          Question("What is 'keyboard' in Spanish?", List("teclado", "ratón", "pantalla", "impresora"), "teclado", Vocabulary)
        )
      )

      // Spanish vocabulary quizzes - Hard difficulty (3 versions)
      case (Spanish, Hard, Vocabulary) => List(
        // Version 1 - Business terminology
        List(
          Question("What is 'meeting' in Spanish?", List("reunión", "presentación", "negocio", "conferencia"), "reunión", Vocabulary),
          Question("What is 'contract' in Spanish?", List("contrato", "acuerdo", "documento", "proyecto"), "contrato", Vocabulary),
          Question("What is 'investment' in Spanish?", List("inversión", "ganancia", "finanzas", "presupuesto"), "inversión", Vocabulary),
          Question("What is 'profit' in Spanish?", List("beneficio", "ganancia", "ingreso", "sueldo"), "beneficio", Vocabulary),
          Question("What is 'management' in Spanish?", List("gestión", "administración", "dirección", "coordinación"), "gestión", Vocabulary)
        ),

        // Version 2 - Medical terminology
        List(
          Question("What is 'diagnosis' in Spanish?", List("diagnóstico", "tratamiento", "síntoma", "enfermedad"), "diagnóstico", Vocabulary),
          Question("What is 'prescription' in Spanish?", List("receta médica", "medicamento", "tratamiento", "farmacia"), "receta médica", Vocabulary),
          Question("What is 'examination' in Spanish?", List("examen", "consulta", "revisión", "diagnóstico"), "examen", Vocabulary),
          Question("What is 'symptom' in Spanish?", List("síntoma", "enfermedad", "dolor", "diagnóstico"), "síntoma", Vocabulary),
          Question("What is 'treatment' in Spanish?", List("tratamiento", "medicamento", "terapia", "cuidado"), "tratamiento", Vocabulary)
        ),

        // Version 3 - Legal terminology
        List(
          Question("What is 'law' in Spanish?", List("ley", "derecho", "justicia", "código"), "ley", Vocabulary),
          Question("What is 'justice' in Spanish?", List("justicia", "ley", "tribunal", "juez"), "justicia", Vocabulary),
          Question("What is 'lawsuit' in Spanish?", List("demanda", "juicio", "pleito", "litigio"), "demanda", Vocabulary),
          Question("What is 'evidence' in Spanish?", List("evidencia", "prueba", "testimonio", "declaración"), "evidencia", Vocabulary),
          Question("What is 'verdict' in Spanish?", List("veredicto", "sentencia", "decisión", "juicio"), "veredicto", Vocabulary)
        )
      )

      // Spanish vocabulary quizzes - Impossible difficulty cases removed

      // French vocabulary quizzes - For each difficulty level (only one version for each as example)
      case (French, Easy, Vocabulary) => List(
        List(
          Question("What is 'hello' in French?", List("bonjour", "au revoir", "merci", "s'il vous plaît"), "bonjour", Vocabulary),
          Question("What is 'thank you' in French?", List("merci", "bonjour", "au revoir", "s'il vous plaît"), "merci", Vocabulary),
          Question("What is 'yes' in French?", List("oui", "non", "peut-être", "bonjour"), "oui", Vocabulary),
          Question("What is 'no' in French?", List("non", "oui", "peut-être", "au revoir"), "non", Vocabulary),
          Question("What is 'please' in French?", List("s'il vous plaît", "merci", "bonjour", "au revoir"), "s'il vous plaît", Vocabulary)
        )
      )

      // Spanish grammar quizzes - Easy difficulty (3 versions)
      case (Spanish, Easy, Grammar) => List(
        // Version 1 - Present tense conjugation
        List(
          Question("What is the correct present tense conjugation for 'hablar' (yo)?", List("hablo", "hablas", "habla", "hablamos"), "hablo", Grammar),
          Question("What is the correct present tense conjugation for 'comer' (tú)?", List("comes", "como", "come", "comemos"), "comes", Grammar),
          Question("What is the correct present tense conjugation for 'vivir' (él)?", List("vive", "vivo", "vives", "vivimos"), "vive", Grammar),
          Question("What is the correct present tense for 'ser' (nosotros)?", List("somos", "son", "sois", "eres"), "somos", Grammar),
          Question("What is the correct present tense for 'ir' (ellos)?", List("van", "vas", "vamos", "voy"), "van", Grammar)
        ),
        
        // Version 2 - Articles and gender
        List(
          Question("Which article is correct: '__ casa'?", List("la", "el", "los", "las"), "la", Grammar),
          Question("Which article is correct: '__ libro'?", List("el", "la", "los", "las"), "el", Grammar),
          Question("Which form is correct?", List("El chica alta", "La chica alta", "El chica alto", "La chica alto"), "La chica alta", Grammar),
          Question("Which form is correct?", List("Los libros rojos", "Las libros rojas", "Los libros rojas", "Las libros rojos"), "Los libros rojos", Grammar),
          Question("Which noun is feminine?", List("mano", "día", "libro", "coche"), "mano", Grammar)
        ),
        
        // Version 3 - Simple prepositions
        List(
          Question("Which preposition completes: 'Voy ___ Madrid'?", List("a", "en", "de", "con"), "a", Grammar),
          Question("Which preposition completes: 'El libro es ___ Juan'?", List("de", "a", "en", "por"), "de", Grammar),
          Question("Which preposition completes: 'Viajo ___ tren'?", List("en", "a", "de", "con"), "en", Grammar),
          Question("Which preposition completes: 'Hablo ___ mi amigo'?", List("con", "a", "de", "en"), "con", Grammar),
          Question("Which preposition completes: 'Vivo ___ España'?", List("en", "a", "de", "por"), "en", Grammar)
        )
      );
      
      // Spanish grammar quizzes - Medium difficulty (3 versions)
      case (Spanish, Medium, Grammar) => List(
        // Version 1 - Past tense conjugation
        List(
          Question("What is the correct preterite tense for 'hablar' (yo)?", List("hablé", "hablaste", "habló", "hablamos"), "hablé", Grammar),
          Question("What is the correct imperfect tense for 'comer' (tú)?", List("comías", "comiste", "comías", "comerás"), "comías", Grammar),
          Question("What is the correct past tense form: 'Ayer yo ___ al parque'?", List("fui", "fue", "iba", "voy"), "fui", Grammar),
          Question("Which sentence uses the imperfect tense correctly?", List("Cuando era niño, jugaba al fútbol", "Ayer jugué al fútbol", "Mañana jugaré al fútbol", "Acabo de jugar al fútbol"), "Cuando era niño, jugaba al fútbol", Grammar),
          Question("Which past tense describes a completed action?", List("Pretérito indefinido", "Pretérito imperfecto", "Pretérito perfecto", "Pretérito pluscuamperfecto"), "Pretérito indefinido", Grammar)
        ),
        
        // Version 2 - Direct and indirect objects
        List(
          Question("Identify the direct object: 'Juan compró un libro'.", List("un libro", "Juan", "compró", "None of these"), "un libro", Grammar),
          Question("Identify the indirect object: 'María le dio un regalo a Pedro'.", List("a Pedro", "María", "un regalo", "le dio"), "a Pedro", Grammar),
          Question("Which sentence correctly uses the indirect object pronoun?", List("Le di el libro a María", "La di el libro a María", "Lo di el libro a María", "Las di el libro a María"), "Le di el libro a María", Grammar),
          Question("Replace the direct object with a pronoun: 'Compro las manzanas'.", List("Las compro", "Les compro", "La compro", "Los compro"), "Las compro", Grammar),
          Question("Which sentence is correct?", List("Se lo di", "Le lo di", "Lo le di", "Le la di"), "Se lo di", Grammar)
        ),
        
        // Version 3 - Subjunctive mood basics
        List(
          Question("When is the subjunctive mood used in Spanish?", List("To express doubt or uncertainty", "To state facts", "To talk about the past", "To give commands"), "To express doubt or uncertainty", Grammar),
          Question("Which phrase typically introduces a subjunctive verb?", List("Espero que", "Creo que", "Sé que", "Es cierto que"), "Espero que", Grammar),
          Question("Complete: 'Quiero que tú ___ (venir) mañana'.", List("vengas", "vienes", "vendrás", "viniste"), "vengas", Grammar),
          Question("Complete: 'No creo que él ___ (estar) en casa'.", List("esté", "está", "estará", "estaba"), "esté", Grammar),
          Question("Which sentence uses the subjunctive correctly?", List("Ojalá llueva mañana", "Ojalá llueve mañana", "Ojalá lloverá mañana", "Ojalá llovió mañana"), "Ojalá llueva mañana", Grammar)
        )
      )

      // Spanish grammar quizzes - Hard difficulty (3 versions)
      case (Spanish, Hard, Grammar) => List(
        // Version 1 - Complex verb tenses
        List(
          Question("What is the pluperfect subjunctive of 'hablar' (yo)?", List("hubiera hablado", "haya hablado", "hablaría", "hubiese hablado"), "hubiera hablado", Grammar),
          Question("What tense is used in: 'Si tuviera dinero, viajaría a España'?", List("Imperfect subjunctive + conditional", "Present subjunctive + future", "Preterite + imperfect", "Present + present"), "Imperfect subjunctive + conditional", Grammar),
          Question("Complete: 'Para mañana, ya ___ (terminar) el trabajo'.", List("habré terminado", "he terminado", "había terminado", "terminé"), "habré terminado", Grammar),
          Question("What tense is 'hubiera cantado'?", List("Pluperfect subjunctive", "Perfect subjunctive", "Future perfect", "Conditional perfect"), "Pluperfect subjunctive", Grammar),
          Question("Complete: 'Si ___ (tener) tiempo ayer, te habría visitado'.", List("hubiera tenido", "tendría", "tenía", "tuviera"), "hubiera tenido", Grammar)
        ),
        
        // Version 2 - Advanced subjunctive usages
        List(
          Question("Which does NOT typically use the subjunctive?", List("Creo que", "Es posible que", "Ojalá que", "A menos que"), "Creo que", Grammar),
          Question("Complete: '¡Qué ___ (tener) buena suerte!'", List("tengas", "tienes", "tendrás", "tenías"), "tengas", Grammar),
          Question("Complete: 'Te llamaré tan pronto como ___ (llegar) a casa'.", List("llegue", "llegaré", "llego", "llegaría"), "llegue", Grammar),
          Question("Complete: 'Busco un restaurante que ___ (servir) comida vegetariana'.", List("sirva", "sirve", "servirá", "sirvió"), "sirva", Grammar),
          Question("Which phrase introduces a subjunctive when used with doubt?", List("No pienso que", "Pienso que", "Es obvio que", "Estoy seguro de que"), "No pienso que", Grammar)
        ),
        
        // Version 3 - Conditional constructions
        List(
          Question("Complete: 'Si ___ (ganar) la lotería, compraría una casa'.", List("ganara", "ganaré", "gano", "ganaría"), "ganara", Grammar),
          Question("Complete: 'Si hubieras estudiado, ___ (aprobar) el examen'.", List("habrías aprobado", "aprobarías", "apruebes", "aprobarás"), "habrías aprobado", Grammar),
          Question("Which type of conditional is: 'Si tengo tiempo, iré a la fiesta'?", List("First conditional (real)", "Second conditional (unlikely)", "Third conditional (impossible)", "Zero conditional (always true)"), "First conditional (real)", Grammar),
          Question("Complete: 'De haber sabido, no ___ (venir)'.", List("habría venido", "vendría", "viniera", "vendré"), "habría venido", Grammar),
          Question("What type of conditional is: 'Si hubiera sabido, te habría dicho'?", List("Third conditional (impossible)", "Second conditional (unlikely)", "First conditional (real)", "Zero conditional (always true)"), "Third conditional (impossible)", Grammar)
        )
      )

      // Spanish grammar quizzes - Impossible difficulty (3 versions)
      

      // French grammar quizzes - Easy difficulty (3 versions)
      case (French, Easy, Grammar) => List(
        // Version 1 - Articles and gender
        List(
          Question("Which article is correct: ___ livre", List("le", "la", "les", "l'"), "le", Grammar),
          Question("Which article is correct: ___ voiture", List("la", "le", "les", "l'"), "la", Grammar),
          Question("Which article is correct: ___ ami", List("l'", "le", "la", "les"), "l'", Grammar),
          Question("Which is the correct plural form of 'un animal'?", List("des animaux", "des animals", "les animal", "les animaux"), "des animaux", Grammar),
          Question("Which is the correct feminine form of 'un ami'?", List("une amie", "la amie", "une ami", "la ami"), "une amie", Grammar)
        ),
        
        // Version 2 - Basic verb conjugation
        List(
          Question("What is the correct conjugation of 'être' for 'je'?", List("suis", "es", "est", "sommes"), "suis", Grammar),
          Question("What is the correct conjugation of 'avoir' for 'tu'?", List("as", "a", "ai", "avons"), "as", Grammar),
          Question("What is the correct conjugation of 'faire' for 'il'?", List("fait", "fais", "faisons", "font"), "fait", Grammar),
          Question("What is the correct conjugation of 'aller' for 'nous'?", List("allons", "allions", "irons", "va"), "allons", Grammar),
          Question("What is the correct conjugation of 'parler' for 'vous'?", List("parlez", "parlons", "parlent", "parle"), "parlez", Grammar)
        ),
        
        // Version 3 - Simple prepositions
        List(
          Question("Which preposition completes: 'Je vais ___ Paris'?", List("à", "en", "au", "aux"), "à", Grammar),
          Question("Which preposition completes: 'Je vais ___ France'?", List("en", "à", "au", "aux"), "en", Grammar),
          Question("Which preposition completes: 'Je viens ___ Canada'?", List("du", "de", "des", "d'"), "du", Grammar),
          Question("Which preposition completes: 'Je voyage ___ avion'?", List("en", "à", "par", "avec"), "en", Grammar),
          Question("Which preposition completes: 'Le livre est ___ la table'?", List("sur", "dans", "sous", "à"), "sur", Grammar)
        )
      )
      
      // French grammar quizzes - Medium difficulty (3 versions)
      case (French, Medium, Grammar) => List(
        // Version 1 - Past tenses
        List(
          Question("Which past tense is used for completed actions?", List("Passé composé", "Imparfait", "Plus-que-parfait", "Présent"), "Passé composé", Grammar),
          Question("Which past tense is used for habitual or ongoing actions in the past?", List("Imparfait", "Passé composé", "Plus-que-parfait", "Passé simple"), "Imparfait", Grammar),
          Question("What is the correct passé composé form of 'parler' with 'je'?", List("j'ai parlé", "je parlais", "je parle", "j'avais parlé"), "j'ai parlé", Grammar),
          Question("What is the correct imparfait form of 'faire' with 'nous'?", List("nous faisions", "nous avons fait", "nous faisons", "nous fîmes"), "nous faisions", Grammar),
          Question("Which sentence uses the imparfait correctly?", List("Quand j'étais petit, j'allais souvent au parc", "Hier, j'ai visité mes grands-parents", "L'année dernière, nous sommes allés en France", "Ce matin, j'ai mangé du pain"), "Quand j'étais petit, j'allais souvent au parc", Grammar)
        ),
        
        // Version 2 - Pronouns and objects
        List(
          Question("Replace the direct object: 'Je mange la pomme' → 'Je ___ mange'", List("la", "le", "lui", "les"), "la", Grammar),
          Question("Replace the indirect object: 'Je parle à Marie' → 'Je ___ parle'", List("lui", "la", "le", "leur"), "lui", Grammar),
          Question("Which pronoun replaces 'les livres' in 'Je donne les livres à Paul'?", List("les", "leur", "eux", "ils"), "les", Grammar),
          Question("What is the correct order in: 'Je ___ ___ donne' (I give it to him)", List("le lui", "lui le", "lui en", "en lui"), "le lui", Grammar),
          Question("What is the correct pronoun in: 'J'ai besoin ___ livre' (of the book)", List("du", "de", "au", "des"), "du", Grammar)
        ),
        
        // Version 3 - Comparative and superlative
        List(
          Question("How do you form the comparative of superiority for adjectives in French?", List("plus + adjective + que", "moins + adjective + que", "aussi + adjective + que", "adjective + er + que"), "plus + adjective + que", Grammar),
          Question("What is the comparative form of: 'Marie est intelligente'?", List("Marie est plus intelligente que Pierre", "Marie est plus intelligente comme Pierre", "Marie est autant intelligente que Pierre", "Marie est très intelligente que Pierre"), "Marie est plus intelligente que Pierre", Grammar),
          Question("What is the superlative of 'bon'?", List("le meilleur", "le plus bon", "le très bon", "le bien"), "le meilleur", Grammar),
          Question("What is the correct superlative: 'C'est ___ film que j'ai jamais vu'?", List("le meilleur", "le plus bon", "le plus meilleur", "le mieux"), "le meilleur", Grammar),
          Question("What is the correct comparative: 'Cette voiture est ___ que l'autre'?", List("moins chère", "plus chère que", "autant chère", "aussi chère comme"), "moins chère", Grammar)
        )
      )
      
      // French grammar quizzes - Hard difficulty (3 versions)
      case (French, Hard, Grammar) => List(
        // Version 1 - Subjunctive mood
        List(
          Question("When is the subjunctive mood typically used in French?", List("After expressions of doubt, desire, or emotion", "To describe facts or certainty", "For future actions", "For past completed actions"), "After expressions of doubt, desire, or emotion", Grammar),
          Question("Which expression introduces the subjunctive?", List("Il faut que", "Je pense que", "Je sais que", "Il est certain que"), "Il faut que", Grammar),
          Question("Complete: 'Je veux que tu ___ (venir) demain'", List("viennes", "viens", "viendrais", "venais"), "viennes", Grammar),
          Question("Complete: 'Bien qu'il ___ (être) riche, il vit simplement'", List("soit", "est", "sera", "était"), "soit", Grammar),
          Question("Which verb is NOT typically followed by the subjunctive?", List("croire (when affirmative)", "vouloir", "souhaiter", "douter"), "croire (when affirmative)", Grammar)
        ),
        
        // Version 2 - Complex tenses
        List(
          Question("What is the correct plus-que-parfait of 'partir' with 'elle'?", List("elle était partie", "elle a été partie", "elle est partie", "elle avait parti"), "elle était partie", Grammar),
          Question("What is the correct futur antérieur of 'finir' with 'nous'?", List("nous aurons fini", "nous avons fini", "nous finîrons", "nous avions fini"), "nous aurons fini", Grammar),
          Question("Complete: 'Si j'avais su, je t'___ (dire)'", List("aurais dit", "avais dit", "ai dit", "dirais"), "aurais dit", Grammar),
          Question("What is the conditionnel passé of 'faire' with 'ils'?", List("ils auraient fait", "ils feraient", "ils avaient fait", "ils ont fait"), "ils auraient fait", Grammar),
          Question("Which tense is used in 'Il m'avait promis qu'il viendrait'?", List("Conditionnel présent", "Futur simple", "Subjonctif présent", "Imparfait"), "Conditionnel présent", Grammar)
        ),
        
        // Version 3 - Passive voice and reported speech
        List(
          Question("Convert to passive voice: 'Le professeur explique la leçon'", List("La leçon est expliquée par le professeur", "La leçon était expliquée par le professeur", "Le professeur est expliqué par la leçon", "La leçon a expliqué le professeur"), "La leçon est expliquée par le professeur", Grammar),
          Question("Convert to indirect speech: 'Il a dit: «Je suis fatigué»'", List("Il a dit qu'il était fatigué", "Il a dit qu'il est fatigué", "Il dit qu'il est fatigué", "Il dit qu'il était fatigué"), "Il a dit qu'il était fatigué", Grammar),
          Question("What happens to 'demain' in reported speech when the reporting verb is in the past?", List("It becomes 'le lendemain'", "It remains 'demain'", "It becomes 'hier'", "It becomes 'aujourd'hui'"), "It becomes 'le lendemain'", Grammar),
          Question("Convert: 'Elle demande: «Où habites-tu?»'", List("Elle demande où tu habites", "Elle demande où habites-tu", "Elle demande où j'habite", "Elle demande: 'Où habites-tu?'"), "Elle demande où tu habites", Grammar),
          Question("Which is NOT a correct transformation in reported speech?", List("'Je' → 'tu' (always)", "'Maintenant' → 'alors'", "'Ici' → 'là'", "'Ce matin' → 'ce matin-là'"), "'Je' → 'tu' (always)", Grammar)
        )
      )

      

      
      
      // English grammar quizzes - Easy difficulty (3 versions)
      case (English, Easy, Grammar) => List(
        // Version 1 - Present tense and basic sentence structure
        List(
          Question("Which sentence is grammatically correct?", List("She works at a hospital", "She work at a hospital", "She working at a hospital", "She be working at a hospital"), "She works at a hospital", Grammar),
          Question("Choose the correct verb form: 'They ___ to the store every day.'", List("go", "goes", "going", "went"), "go", Grammar),
          Question("Which is the correct sentence structure?", List("I always eat breakfast", "I eat always breakfast", "Always I eat breakfast", "I eat breakfast always"), "I always eat breakfast", Grammar),
          Question("Which sentence uses the present continuous correctly?", List("He is playing tennis now", "He playing tennis now", "He play tennis now", "He does playing tennis now"), "He is playing tennis now", Grammar),
          Question("What is the correct article usage?", List("I saw an elephant at the zoo", "I saw a elephant at the zoo", "I saw the elephant at a zoo", "I saw elephant at the zoo"), "I saw an elephant at the zoo", Grammar)
        ),
        
        // Version 2 - Plural forms and basic prepositions
        List(
          Question("What is the correct plural form of 'child'?", List("children", "childs", "childes", "childeren"), "children", Grammar),
          Question("Choose the correct preposition: 'She arrived ___ noon.'", List("at", "in", "on", "to"), "at", Grammar),
          Question("What is the correct plural form of 'sheep'?", List("sheep", "sheeps", "sheepes", "sheeppen"), "sheep", Grammar),
          Question("Choose the correct preposition: 'The book is ___ the table.'", List("on", "in", "at", "by"), "on", Grammar),
          Question("What is the correct plural form of 'tomato'?", List("tomatoes", "tomatos", "tomatoes's", "tomatoen"), "tomatoes", Grammar)
        ),
        
        // Version 3 - Basic verb forms and possessives
        List(
          Question("Which is the correct past tense of 'swim'?", List("swam", "swum", "swimmed", "swimming"), "swam", Grammar),
          Question("Choose the correct possessive form: 'This is ___ house.'", List("John's", "Johns", "John", "Johns'"), "John's", Grammar),
          Question("Which is the correct past tense of 'go'?", List("went", "goed", "gone", "going"), "went", Grammar),
          Question("Choose the correct possessive form for a plural noun: 'The ___ toys are in the box.'", List("children's", "childrens", "childrens'", "children"), "children's", Grammar),
          Question("Which is the correct past tense of 'eat'?", List("ate", "eaten", "eated", "eating"), "ate", Grammar)
        )
      )
      
      // English grammar quizzes - Medium difficulty (3 versions)
      case (English, Medium, Grammar) => List(
        // Version 1 - Tense consistency and comparatives
        List(
          Question("Which sentence has correct tense consistency?", List("He said that he was tired", "He said that he is tired", "He says that he was tired", "He saying that he was tired"), "He said that he was tired", Grammar),
          Question("Choose the correct comparative form: 'This book is ___ than that one.'", List("more interesting", "interestinger", "more interestinger", "most interesting"), "more interesting", Grammar),
          Question("Identify the sentence with correct tense usage:", List("I have been waiting for an hour", "I have wait for an hour", "I am waiting for an hour", "I waiting for an hour"), "I have been waiting for an hour", Grammar),
          Question("Choose the correct comparative: 'She is ___ than her sister.'", List("taller", "more tall", "tallest", "more taller"), "taller", Grammar),
          Question("Which sentence correctly uses the present perfect?", List("I have lived here since 2010", "I lived here since 2010", "I am living here since 2010", "I had lived here since 2010"), "I have lived here since 2010", Grammar)
        ),
        
        // Version 2 - Modal verbs and conditionals
        List(
          Question("Choose the correct modal verb: 'You ___ wear a seatbelt while driving.'", List("must", "might", "would", "could"), "must", Grammar),
          Question("Which sentence is a correct first conditional?", List("If it rains, I will stay home", "If it rains, I would stay home", "If it rained, I will stay home", "If it will rain, I stay home"), "If it rains, I will stay home", Grammar),
          Question("Choose the correct modal verb: 'She ___ be at home now, but I'm not sure.'", List("might", "must", "can", "shall"), "might", Grammar),
          Question("Which is a correct second conditional sentence?", List("If I had more money, I would travel more", "If I have more money, I will travel more", "If I had more money, I will travel more", "If I have more money, I would travel more"), "If I had more money, I would travel more", Grammar),
          Question("Choose the correct modal verb for ability: 'She ___ speak three languages.'", List("can", "must", "should", "would"), "can", Grammar)
        ),
        
        // Version 3 - Passive voice and reported speech
        List(
          Question("Convert to passive voice: 'They build houses.'", List("Houses are built by them", "Houses built by them", "Houses were built by them", "Houses are build by them"), "Houses are built by them", Grammar),
          Question("Change to reported speech: 'I am tired,' she said.", List("She said that she was tired", "She said that I am tired", "She said that she is tired", "She said that I was tired"), "She said that she was tired", Grammar),
          Question("Which sentence is in the passive voice?", List("The letter was written by John", "John wrote the letter", "The letter is writing by John", "John has written the letter"), "The letter was written by John", Grammar),
          Question("Change to reported speech: 'I will call you tomorrow,' he promised.", List("He promised that he would call me the next day", "He promised that he will call me tomorrow", "He promised that I will call you tomorrow", "He promised that he will call me the next day"), "He promised that he would call me the next day", Grammar),
          Question("Convert to passive voice: 'Someone has stolen my wallet.'", List("My wallet has been stolen", "My wallet has stolen", "My wallet was stolen", "My wallet is stolen"), "My wallet has been stolen", Grammar)
        )
      )
      
      // English grammar quizzes - Hard difficulty (3 versions)
      case (English, Hard, Grammar) => List(
        // Version 1 - Perfect and progressive tenses
        List(
          Question("Which sentence correctly uses the past perfect?", List("I had finished my homework before she called", "I have finished my homework before she called", "I finished my homework before she had called", "I was finishing my homework before she called"), "I had finished my homework before she called", Grammar),
          Question("Identify the correct future perfect progressive form:", List("By next year, I will have been studying English for 10 years", "By next year, I will be studying English for 10 years", "By next year, I have been studying English for 10 years", "By next year, I will have studied English for 10 years"), "By next year, I will have been studying English for 10 years", Grammar),
          Question("Which sentence correctly uses the present perfect progressive?", List("She has been living in Paris for three years", "She is living in Paris for three years", "She lives in Paris for three years", "She had been living in Paris for three years"), "She has been living in Paris for three years", Grammar),
          Question("Choose the sentence with the correct past perfect progressive:", List("I had been waiting for an hour when she finally arrived", "I have been waiting for an hour when she finally arrived", "I was waiting for an hour when she finally arrived", "I would have been waiting for an hour when she finally arrived"), "I had been waiting for an hour when she finally arrived", Grammar),
          Question("Which sentence correctly uses the future perfect tense?", List("By the time we arrive, the movie will have started", "By the time we arrive, the movie will start", "By the time we arrive, the movie has started", "By the time we arrive, the movie starts"), "By the time we arrive, the movie will have started", Grammar)
        ),
        
        // Version 2 - Advanced conditionals and subjunctive
        List(
          Question("Which is a correct third conditional sentence?", List("If I had studied harder, I would have passed the exam", "If I studied harder, I would have passed the exam", "If I had studied harder, I would pass the exam", "If I study harder, I would have passed the exam"), "If I had studied harder, I would have passed the exam", Grammar),
          Question("Identify the correct use of the subjunctive mood:", List("I suggest that he be more careful", "I suggest that he is more careful", "I suggest that he was more careful", "I suggest that he being more careful"), "I suggest that he be more careful", Grammar),
          Question("Which is a correct mixed conditional?", List("If I had taken the job, I would be rich now", "If I took the job, I would be rich now", "If I had taken the job, I would have been rich now", "If I would take the job, I would be rich now"), "If I had taken the job, I would be rich now", Grammar),
          Question("Choose the correct subjunctive in this formal expression:", List("Be that as it may, we must continue", "Is that as it may, we must continue", "Being that as it may, we must continue", "Been that as it may, we must continue"), "Be that as it may, we must continue", Grammar),
          Question("Which is a correct inverted conditional form?", List("Had I known earlier, I would have told you", "If I had known earlier, I had told you", "Would I have known earlier, I would have told you", "Have I known earlier, I would have told you"), "Had I known earlier, I would have told you", Grammar)
        ),
        
        // Version 3 - Complex sentence structures and clauses
        List(
          Question("Identify the sentence with a correct relative clause:", List("The woman whose car was stolen called the police", "The woman which car was stolen called the police", "The woman who car was stolen called the police", "The woman whom car was stolen called the police"), "The woman whose car was stolen called the police", Grammar),
          Question("Which sentence correctly uses a participle phrase?", List("Walking to the store, he met his friend", "He met his friend, walked to the store", "While walk to the store, he met his friend", "To walking to the store, he met his friend"), "Walking to the store, he met his friend", Grammar),
          Question("Identify the correct use of a noun clause:", List("What she said surprised everyone", "That she said surprised everyone", "Which she said surprised everyone", "When she said surprised everyone"), "What she said surprised everyone", Grammar),
          Question("Choose the sentence with correct parallel structure:", List("She enjoys swimming, hiking, and to ride bikes", "She enjoys swimming, hiking, and riding bikes", "She enjoys to swim, to hike, and to ride bikes", "She enjoys swim, hike, and ride bikes"), "She enjoys swimming, hiking, and riding bikes", Grammar),
          Question("Which sentence contains a correct adverbial clause?", List("When it rains, the plants grow faster", "When it rains the plants growing faster", "When raining, the plants grow faster", "When it is rain, the plants grow faster"), "When it rains, the plants grow faster", Grammar)
        )
      )
      
     
      
      
      // German grammar quizzes - Easy difficulty (3 versions)
      case (German, Easy, Grammar) => List(
        // Version 1 - Articles and gender
        List(
          Question("What is the definite article for 'Buch' (book)?", List("das", "der", "die", "den"), "das", Grammar),
          Question("What is the definite article for 'Frau' (woman)?", List("die", "der", "das", "den"), "die", Grammar),
          Question("What is the definite article for 'Mann' (man)?", List("der", "die", "das", "den"), "der", Grammar),
          Question("Complete: '__ Kinder spielen im Garten' (The children play in the garden)", List("Die", "Der", "Das", "Den"), "Die", Grammar),
          Question("Which article is correct: '__ Auto ist neu' (The car is new)", List("Das", "Der", "Die", "Den"), "Das", Grammar)
        ),
        
        // Version 2 - Basic verb conjugation
        List(
          Question("What is the correct conjugation of 'sein' (to be) for 'ich'?", List("bin", "ist", "sind", "bist"), "bin", Grammar),
          Question("What is the correct conjugation of 'haben' (to have) for 'du'?", List("hast", "hat", "haben", "habe"), "hast", Grammar),
          Question("What is the correct conjugation of 'sprechen' (to speak) for 'er'?", List("spricht", "spreche", "sprichst", "sprechen"), "spricht", Grammar),
          Question("Complete: 'Wir ___ nach Berlin' (We travel to Berlin)", List("reisen", "reise", "reist", "reise"), "reisen", Grammar),
          Question("Complete: 'Ihr ___ sehr gut Deutsch' (You speak German very well)", List("sprecht", "sprichst", "sprechen", "sprich"), "sprecht", Grammar)
        ),
        
        // Version 3 - Basic sentence structure
        List(
          Question("Which sentence has the correct word order?", List("Ich gehe heute ins Kino", "Ich heute gehe ins Kino", "Ich gehe ins Kino heute", "Heute ich gehe ins Kino"), "Ich gehe heute ins Kino", Grammar),
          Question("Which time expression is placed correctly?", List("Ich lerne jeden Tag Deutsch", "Ich jeden Tag lerne Deutsch", "Jeden Tag ich lerne Deutsch", "Ich lerne Deutsch jeden Tag"), "Ich lerne jeden Tag Deutsch", Grammar),
          Question("Which sentence has the correct verb position?", List("Morgen gehe ich zum Arzt", "Morgen ich gehe zum Arzt", "Ich morgen gehe zum Arzt", "Ich gehe morgen zum Arzt"), "Morgen gehe ich zum Arzt", Grammar),
          Question("Complete with the correct pronoun: '___ spiele Fußball' (I play soccer)", List("Ich", "Du", "Er", "Wir"), "Ich", Grammar),
          Question("Which question is correctly structured?", List("Wo wohnst du?", "Wo du wohnst?", "Du wohnst wo?", "Wohnst wo du?"), "Wo wohnst du?", Grammar)
        )
      )
      
      // German grammar quizzes - Medium difficulty (3 versions)
      case (German, Medium, Grammar) => List(
        // Version 1 - Cases
        List(
          Question("Which is the correct accusative form: 'Ich sehe ___ Mann'?", List("den", "der", "dem", "des"), "den", Grammar),
          Question("Which is the correct dative form: 'Ich gebe ___ Frau ein Buch'?", List("der", "die", "den", "das"), "der", Grammar),
          Question("Complete with the correct preposition and case: 'Ich gehe ___ Schule' (I go to school)", List("zur", "zu der", "zu die", "zu dem"), "zur", Grammar),
          Question("Which is the correct genitive form: '___ Hund meines Bruders' (my brother's dog)", List("Der", "Den", "Dem", "Des"), "Der", Grammar),
          Question("Complete with the correct case: 'Mit ___ Freunden' (with friends)", List("meinen", "meine", "meiner", "meinem"), "meinen", Grammar)
        ),
        
        // Version 2 - Past tenses
        List(
          Question("Which is the correct perfect tense of 'gehen' (to go)?", List("ist gegangen", "hat gegangen", "ist gehen", "hat gehen"), "ist gegangen", Grammar),
          Question("Complete: 'Gestern ___ ich im Park' (Yesterday I was in the park)", List("war", "bin", "habe", "hatte"), "war", Grammar),
          Question("Which verb uses 'haben' in the perfect tense?", List("spielen", "gehen", "fahren", "fliegen"), "spielen", Grammar),
          Question("Complete: 'Sie ___ viel gelernt' (She has learned a lot)", List("hat", "ist", "wird", "haben"), "hat", Grammar),
          Question("Which sentence is in the correct past tense?", List("Ich habe ein Buch gelesen", "Ich bin ein Buch gelesen", "Ich habe ein Buch lesen", "Ich lese ein Buch"), "Ich habe ein Buch gelesen", Grammar)
        ),
        
        // Version 3 - Modal verbs
        List(
          Question("Complete: 'Ich ___ Deutsch lernen' (I want to learn German)", List("will", "wille", "wolle", "wollen"), "will", Grammar),
          Question("Complete: 'Du ___ das nicht machen' (You shouldn't do that)", List("solltest", "sollst", "sollen", "sollte"), "solltest", Grammar),
          Question("Which sentence uses the modal verb correctly?", List("Er kann sehr gut schwimmen", "Er kann sehr gut zu schwimmen", "Er sehr gut kann schwimmen", "Er kann schwimmen sehr gut"), "Er kann sehr gut schwimmen", Grammar),
          Question("Complete: 'Wir ___ nach Hause gehen' (We must go home)", List("müssen", "müsst", "muss", "müsse"), "müssen", Grammar),
          Question("What is the correct position of the infinitive with modal verbs?", List("Ich will einen Kaffee trinken", "Ich will trinken einen Kaffee", "Ich trinken will einen Kaffee", "Ich einen Kaffee will trinken"), "Ich will einen Kaffee trinken", Grammar)
        )
      )
      
      // German grammar quizzes - Hard difficulty (3 versions)
      case (German, Hard, Grammar) => List(
        // Version 1 - Passive voice
        List(
          Question("Convert to passive voice: 'Der Lehrer korrigiert die Tests'", List("Die Tests werden vom Lehrer korrigiert", "Die Tests werden korrigiert vom Lehrer", "Die Tests sind vom Lehrer korrigiert", "Die Tests korrigieren vom Lehrer"), "Die Tests werden vom Lehrer korrigiert", Grammar),
          Question("Which is the correct past passive form: 'Das Haus ___ gebaut'", List("wurde", "wird", "war", "ist"), "wurde", Grammar),
          Question("Complete: 'Der Brief ___ geschrieben worden' (The letter has been written)", List("ist", "wird", "hat", "wurde"), "ist", Grammar),
          Question("Which sentence is in the passive voice?", List("Das Buch wird gelesen", "Das Buch liest", "Das Buch ist lesend", "Das Buch hat gelesen"), "Das Buch wird gelesen", Grammar),
          Question("Complete the future passive: 'Das Problem ___ gelöst werden' (The problem will be solved)", List("wird", "wurde", "ist", "würde"), "wird", Grammar)
        ),
        
        // Version 2 - Subjunctive mood
        List(
          Question("What is the Konjunktiv II form of 'sein' for 'ich'?", List("wäre", "sei", "würde sein", "war"), "wäre", Grammar),
          Question("Complete: 'Wenn ich Zeit ___, würde ich kommen' (If I had time, I would come)", List("hätte", "habe", "würde haben", "hatte"), "hätte", Grammar),
          Question("Which is the correct subjunctive form for reported speech: 'Er sagt, er ___ krank'", List("sei", "ist", "wäre", "würde sein"), "sei", Grammar),
          Question("Complete: 'Ich ___ gerne mehr Sprachen' (I would like to speak more languages)", List("würde sprechen", "werde sprechen", "spreche", "spräche"), "würde sprechen", Grammar),
          Question("Choose the correct subjunctive form: 'Wenn ich du ___, würde ich das nicht tun' (If I were you, I wouldn't do that)", List("wäre", "würde sein", "sei", "bin"), "wäre", Grammar)
        ),
        
        // Version 3 - Complex sentence structures
        List(
          Question("Which conjunction causes the verb to move to the end of the clause?", List("weil", "und", "aber", "oder"), "weil", Grammar),
          Question("Complete: 'Ich weiß nicht, ___ er kommt' (I don't know if/whether he's coming)", List("ob", "wenn", "als", "wann"), "ob", Grammar),
          Question("Which sentence uses the relative pronoun correctly?", List("Der Mann, der dort steht, ist mein Lehrer", "Der Mann, den dort steht, ist mein Lehrer", "Der Mann, dessen dort steht, ist mein Lehrer", "Der Mann, dem dort steht, ist mein Lehrer"), "Der Mann, der dort steht, ist mein Lehrer", Grammar),
          Question("Identify the correct order in a subordinate clause:", List("Ich glaube, dass er morgen kommen wird", "Ich glaube, dass er wird morgen kommen", "Ich glaube, dass wird er morgen kommen", "Ich glaube, dass morgen er kommen wird"), "Ich glaube, dass er morgen kommen wird", Grammar),
          Question("Which German conjunction expresses purpose ('in order to')?", List("damit", "obwohl", "während", "denn"), "damit", Grammar)
        )
      )
      
      // German grammar quizzes - Impossible difficulty (3 versions)
      
      
     
      
      
   

      // For other quiz types and language combinations, return an empty list
      // This will make the system fall back to the default question bank
      // Spanish translation quizzes - Easy difficulty (3 versions)
      case (Spanish, Easy, Translation) => List(
        // Version 1 - Basic greetings and phrases
        List(
          Question("How do you say 'Hello, how are you?' in Spanish?", List("Hola, ¿cómo estás?", "Adiós, ¿qué tal?", "Buenos días, ¿dónde estás?", "Hola, ¿quién eres?"), "Hola, ¿cómo estás?", Translation),
          Question("Translate 'My name is John' to Spanish", List("Me llamo John", "Mi nombre es John", "Soy John", "All of these are correct"), "All of these are correct", Translation),
          Question("Translate 'I would like a coffee, please' to Spanish", List("Quisiera un café, por favor", "Quiero un café, gracias", "Necesito un café, por favor", "Dame un café, por favor"), "Quisiera un café, por favor", Translation),
          Question("How do you say 'Where is the bathroom?' in Spanish?", List("¿Dónde está el baño?", "¿Dónde es el baño?", "¿Cuál es el baño?", "¿Qué es el baño?"), "¿Dónde está el baño?", Translation),
          Question("Translate 'Thank you very much' to Spanish", List("Muchas gracias", "Gracias mucho", "Muy gracias", "Gracias muy"), "Muchas gracias", Translation)
        ),
        
        // Version 2 - Food and restaurants
        List(
          Question("Translate 'I would like to order' to Spanish", List("Quisiera ordenar", "Quiero pedir", "Me gustaría pedir", "All of these are correct"), "All of these are correct", Translation),
          Question("How do you say 'The bill, please' in Spanish?", List("La cuenta, por favor", "El precio, por favor", "La factura, por favor", "El pago, por favor"), "La cuenta, por favor", Translation),
          Question("Translate 'Is there a vegetarian option?' to Spanish", List("¿Hay alguna opción vegetariana?", "¿Existe comida vegetariana?", "¿Tienen platos sin carne?", "¿Puedo comer vegetariano aquí?"), "¿Hay alguna opción vegetariana?", Translation),
          Question("How do you say 'This food is delicious' in Spanish?", List("Esta comida está deliciosa", "Esta comida es deliciosa", "Esta comida sabe deliciosa", "All of these are correct"), "Esta comida está deliciosa", Translation),
          Question("Translate 'I am allergic to nuts' to Spanish", List("Soy alérgico a los frutos secos", "Tengo alergia a las nueces", "No puedo comer nueces", "All of these are correct"), "Soy alérgico a los frutos secos", Translation)
        ),
        
        // Version 3 - Travel and directions
        List(
          Question("How do you say 'How much does this cost?' in Spanish?", List("¿Cuánto cuesta esto?", "¿Cuál es el precio?", "¿Cuánto es?", "None of these"), "¿Cuánto cuesta esto?", Translation),
          Question("Translate 'I need a hotel' to Spanish", List("Necesito un hotel", "Quiero un hotel", "Busco un hotel", "Me falta un hotel"), "Necesito un hotel", Translation),
          Question("How do you say 'Turn right at the corner' in Spanish?", List("Gire a la derecha en la esquina", "Doble a la derecha en la esquina", "Vaya a la derecha en la esquina", "Derecha en la esquina"), "Gire a la derecha en la esquina", Translation),
          Question("Translate 'Is it far from here?' to Spanish", List("¿Está lejos de aquí?", "¿Es lejos de aquí?", "¿Queda lejos de aquí?", "¿Distante de aquí?"), "¿Está lejos de aquí?", Translation),
          Question("How do you say 'I am lost' in Spanish?", List("Estoy perdido", "Me he perdido", "No sé dónde estoy", "Estoy confundido"), "Estoy perdido", Translation)
        )
      )

      // French translation quizzes - Easy difficulty (3 versions)
      case (French, Easy, Translation) => List(
        // Version 1 - Basic greetings and phrases
        List(
          Question("How do you say 'Hello, how are you?' in French?", List("Bonjour, comment allez-vous?", "Salut, comment vas-tu?", "Bonjour, comment ça va?", "All of these are correct"), "All of these are correct", Translation),
          Question("Translate 'My name is John' to French", List("Je m'appelle John", "Mon nom est John", "Je suis John", "All of these are acceptable"), "Je m'appelle John", Translation),
          Question("Translate 'I would like a coffee, please' to French", List("Je voudrais un café, s'il vous plaît", "Je veux un café, merci", "J'ai besoin d'un café, s'il vous plaît", "Donnez-moi un café, s'il vous plaît"), "Je voudrais un café, s'il vous plaît", Translation),
          Question("How do you say 'Where is the bathroom?' in French?", List("Où sont les toilettes?", "Où est la salle de bain?", "Où se trouve la toilette?", "All of these are acceptable"), "Où sont les toilettes?", Translation),
          Question("Translate 'Thank you very much' to French", List("Merci beaucoup", "Merci bien", "Je vous remercie", "All of these are correct"), "Merci beaucoup", Translation)
        ),
        
        // Version 2 - Food and restaurants
        List(
          Question("Translate 'I would like to order' to French", List("Je voudrais commander", "Je veux commander", "J'aimerais commander", "All of these are correct"), "All of these are correct", Translation),
          Question("How do you say 'The bill, please' in French?", List("L'addition, s'il vous plaît", "La facture, s'il vous plaît", "Le compte, s'il vous plaît", "La note, s'il vous plaît"), "L'addition, s'il vous plaît", Translation),
          Question("Translate 'Is there a vegetarian option?' to French", List("Y a-t-il une option végétarienne?", "Avez-vous des plats végétariens?", "Est-ce qu'il y a quelque chose pour les végétariens?", "All of these are acceptable"), "Y a-t-il une option végétarienne?", Translation),
          Question("How do you say 'This food is delicious' in French?", List("Cette nourriture est délicieuse", "Ce plat est délicieux", "C'est délicieux", "All of these are correct"), "All of these are correct", Translation),
          Question("Translate 'I am allergic to nuts' to French", List("Je suis allergique aux noix", "J'ai une allergie aux fruits à coque", "Je ne peux pas manger de noix", "All of these convey the same meaning"), "Je suis allergique aux noix", Translation)
        ),
        
        // Version 3 - Travel and directions
        List(
          Question("How do you say 'How much does this cost?' in French?", List("Combien ça coûte?", "Quel est le prix?", "C'est combien?", "All of these are correct"), "All of these are correct", Translation),
          Question("Translate 'I need a hotel' to French", List("J'ai besoin d'un hôtel", "Je cherche un hôtel", "Il me faut un hôtel", "All of these are correct"), "All of these are correct", Translation),
          Question("How do you say 'Turn right at the corner' in French?", List("Tournez à droite au coin", "Prenez à droite au coin", "À droite au coin", "All of these are acceptable"), "Tournez à droite au coin", Translation),
          Question("Translate 'Is it far from here?' to French", List("Est-ce loin d'ici?", "C'est loin d'ici?", "Est-ce que c'est loin?", "All of these are correct"), "All of these are correct", Translation),
          Question("How do you say 'I am lost' in French?", List("Je suis perdu(e)", "Je me suis égaré(e)", "Je ne sais pas où je suis", "All of these are correct"), "All of these are correct", Translation)
        )
      )
      
      // English translation quizzes - Easy difficulty (3 versions)
      case (English, Easy, Translation) => List(
        // Version 1 - Basic phrases from other languages
        List(
          Question("How do you say 'Hola, ¿cómo estás?' in English?", List("Hello, how are you?", "Hi, how are you doing?", "Hey, what's up?", "All of these are correct"), "Hello, how are you?", Translation),
          Question("Translate 'Je m'appelle John' from French to English", List("My name is John", "I call myself John", "I am named John", "They call me John"), "My name is John", Translation),
          Question("Translate 'Ich hätte gerne einen Kaffee, bitte' from German to English", List("I would like a coffee, please", "I want a coffee, please", "I'd like to have a coffee, please", "All of these are correct"), "I would like a coffee, please", Translation),
          Question("How do you say 'أين الحمام؟' from Arabic in English?", List("Where is the bathroom?", "Where is the toilet?", "Where is the restroom?", "All of these are correct"), "Where is the bathroom?", Translation),
          Question("Translate 'Grazie mille' from Italian to English", List("Thank you very much", "Thanks a lot", "Many thanks", "All of these are correct"), "Thank you very much", Translation)
        ),
        
        // Version 2 - Food and restaurants
        List(
          Question("Translate 'Quisiera ordenar' from Spanish to English", List("I would like to order", "I want to order", "I'd like to place an order", "All of these are correct"), "I would like to order", Translation),
          Question("How do you say 'L'addition, s'il vous plaît' from French in English?", List("The bill, please", "The check, please", "The tab, please", "All of these are correct"), "The bill, please", Translation),
          Question("Translate 'Gibt es vegetarische Optionen?' from German to English", List("Are there vegetarian options?", "Do you have vegetarian options?", "Is there anything vegetarian?", "All of these are correct"), "Are there vegetarian options?", Translation),
          Question("How do you say 'هذا الطعام لذيذ' from Arabic in English?", List("This food is delicious", "This meal is tasty", "This dish is very good", "All of these convey the same meaning"), "This food is delicious", Translation),
          Question("Translate 'Sono allergico alle noci' from Italian to English", List("I am allergic to nuts", "I have a nut allergy", "Nuts give me an allergic reaction", "All of these convey the same meaning"), "I am allergic to nuts", Translation)
        ),
        
        // Version 3 - Travel and directions
        List(
          Question("How do you say '¿Cuánto cuesta esto?' from Spanish in English?", List("How much does this cost?", "How much is this?", "What's the price of this?", "All of these are correct"), "How much does this cost?", Translation),
          Question("Translate 'J'ai besoin d'un hôtel' from French to English", List("I need a hotel", "I require a hotel", "I'm looking for a hotel", "All of these are correct"), "I need a hotel", Translation),
          Question("How do you say 'Biegen Sie rechts an der Ecke ab' from German in English?", List("Turn right at the corner", "Make a right at the corner", "Take a right at the corner", "All of these are correct"), "Turn right at the corner", Translation),
          Question("Translate 'هل هذا بعيد من هنا؟' from Arabic to English", List("Is this far from here?", "Is it far from here?", "How far is it from here?", "All of these convey the same meaning"), "Is this far from here?", Translation),
          Question("How do you say 'Mi sono perso' from Italian in English?", List("I am lost", "I've gotten lost", "I've lost my way", "All of these are correct"), "I am lost", Translation)
        )
      )
      
      // German translation quizzes - Easy difficulty (3 versions)
      case (German, Easy, Translation) => List(
        // Version 1 - Basic greetings and phrases
        List(
          Question("How do you say 'Hello, how are you?' in German?", List("Hallo, wie geht es dir?", "Guten Tag, wie geht's?", "Hallo, wie geht's?", "All of these are correct"), "All of these are correct", Translation),
          Question("Translate 'My name is John' to German", List("Ich heiße John", "Mein Name ist John", "Ich bin John", "All of these are acceptable"), "Ich heiße John", Translation),
          Question("Translate 'I would like a coffee, please' to German", List("Ich hätte gerne einen Kaffee, bitte", "Ich möchte einen Kaffee, bitte", "Ich würde gerne einen Kaffee haben, bitte", "All of these are correct"), "Ich hätte gerne einen Kaffee, bitte", Translation),
          Question("How do you say 'Where is the bathroom?' in German?", List("Wo ist die Toilette?", "Wo ist das Badezimmer?", "Wo ist das WC?", "All of these are acceptable"), "Wo ist die Toilette?", Translation),
          Question("Translate 'Thank you very much' to German", List("Vielen Dank", "Danke schön", "Danke sehr", "All of these are correct"), "Vielen Dank", Translation)
        ),
        
        // Version 2 - Food and restaurants
        List(
          Question("Translate 'I would like to order' to German", List("Ich möchte bestellen", "Ich würde gerne bestellen", "Ich hätte gerne bestellt", "All of these are correct"), "Ich möchte bestellen", Translation),
          Question("How do you say 'The bill, please' in German?", List("Die Rechnung, bitte", "Die Quittung, bitte", "Zahlen, bitte", "All of these are acceptable"), "Die Rechnung, bitte", Translation),
          Question("Translate 'Is there a vegetarian option?' to German", List("Gibt es vegetarische Optionen?", "Haben Sie vegetarische Gerichte?", "Gibt es etwas für Vegetarier?", "All of these are correct"), "Gibt es vegetarische Optionen?", Translation),
          Question("How do you say 'This food is delicious' in German?", List("Dieses Essen ist köstlich", "Das schmeckt sehr gut", "Das Essen ist lecker", "All of these are correct"), "Dieses Essen ist köstlich", Translation),
          Question("Translate 'I am allergic to nuts' to German", List("Ich bin allergisch gegen Nüsse", "Ich habe eine Nussallergie", "Ich vertrage keine Nüsse", "All of these convey the same meaning"), "Ich bin allergisch gegen Nüsse", Translation)
        ),
        
        // Version 3 - Travel and directions
        List(
          Question("How do you say 'How much does this cost?' in German?", List("Wie viel kostet das?", "Was kostet das?", "Wie teuer ist das?", "All of these are correct"), "All of these are correct", Translation),
          Question("Translate 'I need a hotel' to German", List("Ich brauche ein Hotel", "Ich benötige ein Hotel", "Ich suche ein Hotel", "All of these are correct"), "Ich brauche ein Hotel", Translation),
          Question("How do you say 'Turn right at the corner' in German?", List("Biegen Sie rechts an der Ecke ab", "Rechts abbiegen an der Ecke", "An der Ecke nach rechts", "All of these convey the instruction"), "Biegen Sie rechts an der Ecke ab", Translation),
          Question("Translate 'Is it far from here?' to German", List("Ist es weit von hier?", "Ist es weit entfernt?", "Ist es weit weg?", "All of these are correct"), "Ist es weit von hier?", Translation),
          Question("How do you say 'I am lost' in German?", List("Ich habe mich verirrt", "Ich bin verloren", "Ich weiß nicht, wo ich bin", "All of these convey the same meaning"), "Ich habe mich verirrt", Translation)
        )
      )
      
      // Arabic translation quizzes - Easy difficulty (3 versions)
      
      
      // French MCQ quizzes - Easy difficulty (3 versions)
      case (French, Easy, MCQ) => List(
        // Version 1 - Cultural knowledge
        List(
          Question("Which city is the capital of France?", List("Paris", "Lyon", "Marseille", "Nice"), "Paris", MCQ),
          Question("Which French dish consists of snails cooked with garlic and butter?", List("Escargots", "Ratatouille", "Bouillabaisse", "Quiche"), "Escargots", MCQ),
          Question("Which famous museum is located in Paris?", List("The Louvre", "The British Museum", "The Prado", "The Metropolitan"), "The Louvre", MCQ),
          Question("Which of these is a traditional French cheese?", List("Camembert", "Cheddar", "Gouda", "Feta"), "Camembert", MCQ),
          Question("What colors are on the French flag?", List("Blue, white, and red", "Red and white", "Blue and white", "Red, blue, and green"), "Blue, white, and red", MCQ)
        ),
        
        // Version 2 - Geography and nature
        List(
          Question("Which mountain range is found in France?", List("The Alps", "The Andes", "The Rockies", "The Himalayas"), "The Alps", MCQ),
          Question("Which French region is known for its wine production?", List("Bordeaux", "Normandy", "Brittany", "Picardy"), "Bordeaux", MCQ),
          Question("Which sea borders the south of France?", List("Mediterranean Sea", "Atlantic Ocean", "North Sea", "Baltic Sea"), "Mediterranean Sea", MCQ),
          Question("Which is the longest river in France?", List("Loire", "Seine", "Rhône", "Garonne"), "Loire", MCQ),
          Question("Which is NOT a neighboring country of France?", List("Portugal", "Spain", "Italy", "Belgium"), "Portugal", MCQ)
        ),
        
        // Version 3 - Everyday knowledge
        List(
          Question("What is the currency used in France?", List("Euro", "Franc", "Pound", "Dollar"), "Euro", MCQ),
          Question("What is the typical French greeting?", List("Bonjour", "Hola", "Ciao", "Hallo"), "Bonjour", MCQ),
          Question("Which of these is a famous French fashion brand?", List("Chanel", "Gucci", "Prada", "Zara"), "Chanel", MCQ),
          Question("What is a 'baguette'?", List("A type of bread", "A small bag", "A musical instrument", "A dance"), "A type of bread", MCQ),
          Question("What is the most widely spoken language in France?", List("French", "English", "German", "Spanish"), "French", MCQ)
        )
      )
      
      // French MCQ quizzes - Medium difficulty (3 versions)
      case (French, Medium, MCQ) => List(
        // Version 1 - History
        List(
          Question("Which king was known as the 'Sun King'?", List("Louis XIV", "Napoleon Bonaparte", "Louis XVI", "Charles de Gaulle"), "Louis XIV", MCQ),
          Question("In which year did the French Revolution begin?", List("1789", "1776", "1804", "1815"), "1789", MCQ),
          Question("Who was the first President of the Fifth French Republic?", List("Charles de Gaulle", "François Mitterrand", "Jacques Chirac", "Nicolas Sarkozy"), "Charles de Gaulle", MCQ),
          Question("Which treaty ended World War I?", List("Treaty of Versailles", "Treaty of Paris", "Treaty of London", "Treaty of Rome"), "Treaty of Versailles", MCQ),
          Question("Which French leader sold the Louisiana Territory to the United States?", List("Napoleon Bonaparte", "Louis XVI", "Robespierre", "Charles de Gaulle"), "Napoleon Bonaparte", MCQ)
        ),
        
        // Version 2 - Arts and literature
        List(
          Question("Who wrote 'Les Misérables'?", List("Victor Hugo", "Albert Camus", "Émile Zola", "Marcel Proust"), "Victor Hugo", MCQ),
          Question("Which French artist is known for his water lily paintings?", List("Claude Monet", "Paul Cézanne", "Edgar Degas", "Henri Matisse"), "Claude Monet", MCQ),
          Question("Which movement was founded by French writers and artists in the 1920s?", List("Surrealism", "Impressionism", "Renaissance", "Baroque"), "Surrealism", MCQ),
          Question("Which famous French actress won an Oscar for 'La Vie en Rose'?", List("Marion Cotillard", "Audrey Tautou", "Catherine Deneuve", "Juliette Binoche"), "Marion Cotillard", MCQ),
          Question("Which French composer wrote 'Clair de Lune'?", List("Claude Debussy", "Maurice Ravel", "Camille Saint-Saëns", "Hector Berlioz"), "Claude Debussy", MCQ)
        ),
        
        // Version 3 - Society and traditions
        List(
          Question("What is celebrated on July 14th in France?", List("Bastille Day", "Christmas", "New Year", "Labor Day"), "Bastille Day", MCQ),
          Question("What is a 'lycée' in France?", List("A high school", "A university", "A museum", "A government building"), "A high school", MCQ),
          Question("Which of these dishes is traditionally eaten at Christmas in France?", List("Bûche de Noël", "Coq au Vin", "Ratatouille", "Croque Monsieur"), "Bûche de Noël", MCQ),
          Question("What type of government does France have?", List("Semi-presidential republic", "Constitutional monarchy", "Federal republic", "Parliamentary democracy"), "Semi-presidential republic", MCQ),
          Question("What is the French education policy regarding religious symbols in public schools?", List("They are banned", "They are mandatory", "They are only allowed on special occasions", "There is no policy"), "They are banned", MCQ)
        )
      )
      
      // French MCQ quizzes - Hard difficulty (3 versions)
      case (French, Hard, MCQ) => List(
        // Version 1 - Politics and government
        List(
          Question("How many years is the French presidential term?", List("5 years", "4 years", "6 years", "7 years"), "5 years", MCQ),
          Question("What is the name of the lower house of the French Parliament?", List("National Assembly", "Senate", "House of Commons", "Chamber of Deputies"), "National Assembly", MCQ),
          Question("Which political movement did Emmanuel Macron create in 2016?", List("La République En Marche", "Parti Socialiste", "Les Républicains", "Front National"), "La République En Marche", MCQ),
          Question("How many administrative regions does metropolitan France have since 2016?", List("13", "18", "22", "27"), "13", MCQ),
          Question("What does the French motto 'Liberté, Égalité, Fraternité' mean?", List("Freedom, Equality, Brotherhood", "Liberty, Justice, Solidarity", "Peace, Progress, Prosperity", "Truth, Honor, Loyalty"), "Freedom, Equality, Brotherhood", MCQ)
        ),
        
        // Version 2 - Economy and industry
        List(
          Question("Which industry is France the world leader in?", List("Tourism", "Automobile manufacturing", "Pharmaceuticals", "Technology"), "Tourism", MCQ),
          Question("Which French company is one of the world's largest cosmetics companies?", List("L'Oréal", "Renault", "Total", "Carrefour"), "L'Oréal", MCQ),
          Question("Which economic sector employs the most people in France?", List("Services", "Agriculture", "Manufacturing", "Mining"), "Services", MCQ),
          Question("Which French region produces champagne?", List("Champagne-Ardenne", "Burgundy", "Provence", "Normandy"), "Champagne-Ardenne", MCQ),
          Question("What percentage of France's electricity comes from nuclear power?", List("About 70%", "About 30%", "About 50%", "About 10%"), "About 70%", MCQ)
        ),
        
        // Version 3 - Geography and climate
        List(
          Question("Which is France's highest mountain?", List("Mont Blanc", "Mont Ventoux", "Puy de Dôme", "Pic du Midi"), "Mont Blanc", MCQ),
          Question("Which climate type dominates most of northern France?", List("Oceanic", "Mediterranean", "Continental", "Alpine"), "Oceanic", MCQ),
          Question("Which French territory is located in the Caribbean?", List("Martinique", "Réunion", "French Polynesia", "New Caledonia"), "Martinique", MCQ),
          Question("Which French city has the largest population after Paris?", List("Marseille", "Lyon", "Toulouse", "Nice"), "Marseille", MCQ),
          Question("What is the total area of France in square kilometers?", List("About 550,000", "About 350,000", "About 750,000", "About 950,000"), "About 550,000", MCQ)
        )
      )
      
      
      
      
      // Spanish MCQ quizzes - Easy difficulty (3 versions)
      case (Spanish, Easy, MCQ) => List(
        // Version 1 - Cultural knowledge
        List(
          Question("Which city is the capital of Spain?", List("Madrid", "Barcelona", "Sevilla", "Valencia"), "Madrid", MCQ),
          Question("Which Spanish dish is made with rice and seafood?", List("Paella", "Gazpacho", "Tortilla", "Churros"), "Paella", MCQ),
          Question("Which Spanish artist painted 'Guernica'?", List("Pablo Picasso", "Salvador Dalí", "Francisco Goya", "Joan Miró"), "Pablo Picasso", MCQ),
          Question("Which Spanish football club has won the most UEFA Champions League titles?", List("Real Madrid", "Barcelona", "Atlético Madrid", "Valencia"), "Real Madrid", MCQ),
          Question("On which day do Spaniards traditionally eat twelve grapes at midnight?", List("New Year's Eve", "Christmas", "Three Kings Day", "Easter"), "New Year's Eve", MCQ)
        ),
        
        // Version 2 - Geography and nature
        List(
          Question("Which mountain range runs along the border between Spain and France?", List("Pyrenees", "Alps", "Sierra Nevada", "Picos de Europa"), "Pyrenees", MCQ),
          Question("Which of these Spanish islands is located in the Mediterranean Sea?", List("Mallorca", "Tenerife", "Gran Canaria", "La Palma"), "Mallorca", MCQ),
          Question("Which is the longest river in Spain?", List("Ebro", "Tajo", "Duero", "Guadalquivir"), "Ebro", MCQ),
          Question("Which Spanish city is famous for the 'Running of the Bulls'?", List("Pamplona", "Madrid", "Barcelona", "Valencia"), "Pamplona", MCQ),
          Question("Which animal is featured on the Spanish coat of arms?", List("Eagle", "Bull", "Lion", "Bear"), "Eagle", MCQ)
        ),
        
        // Version 3 - Everyday knowledge
        List(
          Question("What is the currency used in Spain?", List("Euro", "Peseta", "Dollar", "Pound"), "Euro", MCQ),
          Question("What time do Spaniards typically eat dinner?", List("9-10 PM", "6-7 PM", "7-8 PM", "5-6 PM"), "9-10 PM", MCQ),
          Question("What is the traditional Spanish midday rest called?", List("Siesta", "Fiesta", "Mañana", "Tarde"), "Siesta", MCQ),
          Question("Which of these is a traditional Spanish dance?", List("Flamenco", "Tango", "Salsa", "Samba"), "Flamenco", MCQ),
          Question("What color is the Spanish flag?", List("Red and yellow", "Blue and white", "Green and red", "Yellow and blue"), "Red and yellow", MCQ)
        )
      )
      
      // Spanish MCQ quizzes - Medium difficulty (3 versions)
      case (Spanish, Medium, MCQ) => List(
        // Version 1 - History
        List(
          Question("In which year did Spain join the European Union?", List("1986", "1975", "1992", "2002"), "1986", MCQ),
          Question("Which Spanish king ruled during the 'Golden Age' of Spanish culture?", List("Philip II", "Charles I", "Ferdinand VII", "Juan Carlos I"), "Philip II", MCQ),
          Question("Which Spanish explorer was the first to circumnavigate the globe (though he died before completing the journey)?", List("Ferdinand Magellan", "Christopher Columbus", "Hernán Cortés", "Francisco Pizarro"), "Ferdinand Magellan", MCQ),
          Question("Which of these regions has sought independence from Spain?", List("Catalonia", "Andalusia", "Extremadura", "Madrid"), "Catalonia", MCQ),
          Question("Which year marks the end of Francisco Franco's dictatorship in Spain?", List("1975", "1939", "1981", "1968"), "1975", MCQ)
        ),
        
        // Version 2 - Arts and literature
        List(
          Question("Which Spanish author wrote 'Don Quixote'?", List("Miguel de Cervantes", "Federico García Lorca", "Gabriel García Márquez", "Isabel Allende"), "Miguel de Cervantes", MCQ),
          Question("Which Spanish architect designed the Sagrada Familia in Barcelona?", List("Antoni Gaudí", "Santiago Calatrava", "Rafael Moneo", "Ricardo Bofill"), "Antoni Gaudí", MCQ),
          Question("Which Spanish painter created 'The Persistence of Memory'?", List("Salvador Dalí", "Pablo Picasso", "Francisco Goya", "Joan Miró"), "Salvador Dalí", MCQ),
          Question("Which Spanish film director won an Oscar for 'All About My Mother'?", List("Pedro Almodóvar", "Luis Buñuel", "Alejandro Amenábar", "Carlos Saura"), "Pedro Almodóvar", MCQ),
          Question("Which Spanish poet wrote 'Poet in New York'?", List("Federico García Lorca", "Antonio Machado", "Rafael Alberti", "Juan Ramón Jiménez"), "Federico García Lorca", MCQ)
        ),
        
        // Version 3 - Society and traditions
        List(
          Question("What is the name of Spain's national anthem?", List("Marcha Real", "La Marsellesa", "España Cañí", "El Concierto de Aranjuez"), "Marcha Real", MCQ),
          Question("Which Spanish festival features human towers called 'castells'?", List("La Mercè", "San Fermín", "La Tomatina", "Fallas"), "La Mercè", MCQ),
          Question("What is 'El Clásico' in Spain?", List("A football match between Real Madrid and Barcelona", "A classical music concert", "A traditional dance competition", "A religious procession"), "A football match between Real Madrid and Barcelona", MCQ),
          Question("Which Spanish city hosts the world's largest food fight?", List("Buñol", "Valencia", "Barcelona", "Sevilla"), "Buñol", MCQ),
          Question("What does the Spanish term 'sobremesa' refer to?", List("Time spent chatting at the table after a meal", "Dessert", "Table manners", "Midday siesta"), "Time spent chatting at the table after a meal", MCQ)
        )
      )
      
      // Spanish MCQ quizzes - Hard difficulty (3 versions)
      case (Spanish, Hard, MCQ) => List(
        // Version 1 - Politics and government
        List(
          Question("What type of government does Spain have?", List("Constitutional monarchy", "Republic", "Federal republic", "Direct democracy"), "Constitutional monarchy", MCQ),
          Question("Who was the first democratically elected Prime Minister of Spain after Franco's dictatorship?", List("Adolfo Suárez", "Felipe González", "Pedro Sánchez", "José María Aznar"), "Adolfo Suárez", MCQ),
          Question("Which Spanish political party uses the acronym PSOE?", List("Spanish Socialist Workers' Party", "Popular Party", "Citizens", "We Can"), "Spanish Socialist Workers' Party", MCQ),
          Question("How many autonomous communities does Spain have?", List("17", "15", "20", "12"), "17", MCQ),
          Question("Which of these is NOT an official language in Spain?", List("French", "Catalan", "Galician", "Basque"), "French", MCQ)
        ),
        
        // Version 2 - Economy and industry
        List(
          Question("What is Spain's main export?", List("Cars", "Olive oil", "Wine", "Electronics"), "Cars", MCQ),
          Question("Which Spanish company is the largest clothing retailer in the world?", List("Inditex (Zara)", "Mango", "Desigual", "El Corte Inglés"), "Inditex (Zara)", MCQ),
          Question("What percentage of Spain's GDP comes from tourism (pre-COVID)?", List("About 14%", "About 5%", "About 25%", "About 35%"), "About 14%", MCQ),
          Question("Which Spanish bank is one of the largest in Europe?", List("Santander", "La Caixa", "BBVA", "Sabadell"), "Santander", MCQ),
          Question("Which region produces most of Spain's wine?", List("La Rioja", "Andalucía", "Catalonia", "Galicia"), "La Rioja", MCQ)
        ),
        
        // Version 3 - Geography and climate
        List(
          Question("What percentage of European fruit and vegetables is produced in Spain?", List("About 30%", "About 10%", "About 50%", "About 70%"), "About 30%", MCQ),
          Question("Which Spanish climate zone is characterized by hot, dry summers and mild, rainy winters?", List("Mediterranean", "Oceanic", "Continental", "Mountain"), "Mediterranean", MCQ),
          Question("What is the highest mountain peak in Spain?", List("Teide", "Mulhacén", "Aneto", "Mont Blanc"), "Teide", MCQ),
          Question("Which Spanish city has the highest population density?", List("Barcelona", "Madrid", "Valencia", "Sevilla"), "Barcelona", MCQ),
          Question("Which of these is NOT a Spanish archipelago?", List("Azores", "Balearic Islands", "Canary Islands", "Columbretes Islands"), "Azores", MCQ)
        )
      )
      
     
      
      // English MCQ quizzes - Easy difficulty (3 versions)
      case (English, Easy, MCQ) => List(
        // Version 1 - Cultural knowledge
        List(
          Question("Which of these is a traditional English breakfast item?", List("Baked beans", "Croissant", "Churros", "Waffles"), "Baked beans", MCQ),
          Question("What is the national sport of England?", List("Cricket", "Rugby", "Football (Soccer)", "Tennis"), "Cricket", MCQ),
          Question("Which city is the capital of England?", List("London", "Manchester", "Liverpool", "Birmingham"), "London", MCQ),
          Question("What is the currency of the United Kingdom?", List("Pound Sterling", "Euro", "Dollar", "Yen"), "Pound Sterling", MCQ),
          Question("Which of these is a traditional English dessert?", List("Trifle", "Tiramisu", "Baklava", "Churros"), "Trifle", MCQ)
        ),
        
        // Version 2 - Geography and history
        List(
          Question("Which river runs through London?", List("Thames", "Seine", "Danube", "Rhine"), "Thames", MCQ),
          Question("Who was the first Queen Elizabeth of England?", List("Elizabeth I", "Elizabeth II", "Victoria", "Anne"), "Elizabeth I", MCQ),
          Question("Which of these countries is NOT part of the United Kingdom?", List("Ireland", "Scotland", "Wales", "Northern Ireland"), "Ireland", MCQ),
          Question("What is the highest mountain in England?", List("Scafell Pike", "Ben Nevis", "Snowdon", "Mont Blanc"), "Scafell Pike", MCQ),
          Question("Which of these is a famous English university?", List("Oxford", "Sorbonne", "Harvard", "Bologna"), "Oxford", MCQ)
        ),
        
        // Version 3 - Everyday knowledge
        List(
          Question("What side of the road do the British drive on?", List("Left", "Right", "Both", "It depends on the city"), "Left", MCQ),
          Question("What is a 'pub' in England?", List("A public house (bar)", "A public library", "A public park", "A public transport station"), "A public house (bar)", MCQ),
          Question("What is the traditional British meal eaten on Sunday?", List("Sunday roast", "Fish and chips", "Shepherd's pie", "Bangers and mash"), "Sunday roast", MCQ),
          Question("Which of these is a British car brand?", List("Aston Martin", "Mercedes", "Toyota", "Renault"), "Aston Martin", MCQ),
          Question("What is 'Cockney rhyming slang'?", List("A type of English dialect", "A poetic form", "A children's game", "A cooking technique"), "A type of English dialect", MCQ)
        )
      )
      
      // English MCQ quizzes - Medium difficulty (3 versions)
      case (English, Medium, MCQ) => List(
        // Version 1 - History
        List(
          Question("Which English monarch had six wives?", List("Henry VIII", "Edward VII", "George V", "William I"), "Henry VIII", MCQ),
          Question("In which year did the Battle of Hastings take place?", List("1066", "1215", "1485", "1588"), "1066", MCQ),
          Question("Who was the Prime Minister of the UK during most of World War II?", List("Winston Churchill", "Neville Chamberlain", "Clement Attlee", "Margaret Thatcher"), "Winston Churchill", MCQ),
          Question("Which historical period came first in England?", List("Tudor", "Victorian", "Georgian", "Edwardian"), "Tudor", MCQ),
          Question("Which English king signed the Magna Carta?", List("King John", "Richard the Lionheart", "Henry II", "Edward I"), "King John", MCQ)
        ),
        
        // Version 2 - Literature and arts
        List(
          Question("Who wrote 'Pride and Prejudice'?", List("Jane Austen", "Charlotte Brontë", "Virginia Woolf", "George Eliot"), "Jane Austen", MCQ),
          Question("Which of Shakespeare's plays features the character Hamlet?", List("Hamlet", "Macbeth", "Romeo and Juliet", "King Lear"), "Hamlet", MCQ),
          Question("Which English artist painted 'The Fighting Temeraire'?", List("J.M.W. Turner", "John Constable", "Thomas Gainsborough", "William Blake"), "J.M.W. Turner", MCQ),
          Question("Which band was NOT from England?", List("U2", "The Beatles", "The Rolling Stones", "Queen"), "U2", MCQ),
          Question("Which English author created the character Sherlock Holmes?", List("Arthur Conan Doyle", "Charles Dickens", "Agatha Christie", "H.G. Wells"), "Arthur Conan Doyle", MCQ)
        ),
        
        // Version 3 - Geography and society
        List(
          Question("Which of these is NOT a county in England?", List("Strathclyde", "Kent", "Essex", "Devon"), "Strathclyde", MCQ),
          Question("Which English city is known as the 'Steel City'?", List("Sheffield", "Manchester", "Birmingham", "Newcastle"), "Sheffield", MCQ),
          Question("What is the most widely practiced religion in England?", List("Christianity", "Islam", "Hinduism", "Judaism"), "Christianity", MCQ),
          Question("Which of these is a famous English music festival?", List("Glastonbury", "Tomorrowland", "Coachella", "Burning Man"), "Glastonbury", MCQ),
          Question("Which English institution is over 900 years old?", List("Tower of London", "British Museum", "London Underground", "Buckingham Palace"), "Tower of London", MCQ)
        )
      )
      
      // English MCQ quizzes - Hard difficulty (3 versions)
      case (English, Hard, MCQ) => List(
        // Version 1 - Perfect and progressive tenses
        List(
          Question("Which sentence correctly uses the past perfect?", List("I had finished my homework before she called", "I have finished my homework before she called", "I finished my homework before she had called", "I was finishing my homework before she called"), "I had finished my homework before she called", Grammar),
          Question("Identify the correct future perfect progressive form:", List("By next year, I will have been studying English for 10 years", "By next year, I will be studying English for 10 years", "By next year, I have been studying English for 10 years", "By next year, I will have studied English for 10 years"), "By next year, I will have been studying English for 10 years", Grammar),
          Question("Which sentence correctly uses the present perfect progressive?", List("She has been living in Paris for three years", "She is living in Paris for three years", "She lives in Paris for three years", "She had been living in Paris for three years"), "She has been living in Paris for three years", Grammar),
          Question("Choose the sentence with the correct past perfect progressive:", List("I had been waiting for an hour when she finally arrived", "I have been waiting for an hour when she finally arrived", "I was waiting for an hour when she finally arrived", "I would have been waiting for an hour when she finally arrived"), "I had been waiting for an hour when she finally arrived", Grammar),
          Question("Which sentence correctly uses the future perfect tense?", List("By the time we arrive, the movie will have started", "By the time we arrive, the movie will start", "By the time we arrive, the movie has started", "By the time we arrive, the movie starts"), "By the time we arrive, the movie will have started", Grammar)
        ),
        
        // Version 2 - Advanced conditionals and subjunctive
        List(
          Question("Which is a correct third conditional sentence?", List("If I had studied harder, I would have passed the exam", "If I studied harder, I would have passed the exam", "If I had studied harder, I would pass the exam", "If I study harder, I would have passed the exam"), "If I had studied harder, I would have passed the exam", Grammar),
          Question("Identify the correct use of the subjunctive mood:", List("I suggest that he be more careful", "I suggest that he is more careful", "I suggest that he was more careful", "I suggest that he being more careful"), "I suggest that he be more careful", Grammar),
          Question("Which is a correct mixed conditional?", List("If I had taken the job, I would be rich now", "If I took the job, I would be rich now", "If I had taken the job, I would have been rich now", "If I would take the job, I would be rich now"), "If I had taken the job, I would be rich now", Grammar),
          Question("Choose the correct subjunctive in this formal expression:", List("Be that as it may, we must continue", "Is that as it may, we must continue", "Being that as it may, we must continue", "Been that as it may, we must continue"), "Be that as it may, we must continue", Grammar),
          Question("Which is a correct inverted conditional form?", List("Had I known earlier, I would have told you", "If I had known earlier, I had told you", "Would I have known earlier, I would have told you", "Have I known earlier, I would have told you"), "Had I known earlier, I would have told you", Grammar)
        ),
        
        // Version 3 - Complex sentence structures and clauses
        List(
          Question("Identify the sentence with a correct relative clause:", List("The woman whose car was stolen called the police", "The woman which car was stolen called the police", "The woman who car was stolen called the police", "The woman whom car was stolen called the police"), "The woman whose car was stolen called the police", Grammar),
          Question("Which sentence correctly uses a participle phrase?", List("Walking to the store, he met his friend", "He met his friend, walked to the store", "While walk to the store, he met his friend", "To walking to the store, he met his friend"), "Walking to the store, he met his friend", Grammar),
          Question("Identify the correct use of a noun clause:", List("What she said surprised everyone", "That she said surprised everyone", "Which she said surprised everyone", "When she said surprised everyone"), "What she said surprised everyone", Grammar),
          Question("Choose the sentence with correct parallel structure:", List("She enjoys swimming, hiking, and to ride bikes", "She enjoys swimming, hiking, and riding bikes", "She enjoys to swim, to hike, and to ride bikes", "She enjoys swim, hike, and ride bikes"), "She enjoys swimming, hiking, and riding bikes", Grammar),
          Question("Which sentence contains a correct adverbial clause?", List("When it rains, the plants grow faster", "When it rains the plants growing faster", "When raining, the plants grow faster", "When it is rain, the plants grow faster"), "When it rains, the plants grow faster", Grammar)
        )
      )
      
      // English grammar quizzes - Impossible difficulty (3 versions)
      

      // German MCQ quizzes - Easy difficulty (3 versions)
      case (German, Easy, MCQ) => List(
        // Version 1 - Cultural knowledge
        List(
          Question("Which city is the capital of Germany?", List("Berlin", "Munich", "Hamburg", "Frankfurt"), "Berlin", MCQ),
          Question("Which German festival is celebrated in October?", List("Oktoberfest", "Christmas Market", "Carnival", "Berlinale"), "Oktoberfest", MCQ),
          Question("Which car manufacturer is German?", List("BMW", "Toyota", "Ford", "Fiat"), "BMW", MCQ),
          Question("What colors are on the German flag?", List("Black, red, and gold", "Red, white, and blue", "Green, white, and red", "Blue, white, and red"), "Black, red, and gold", MCQ),
          Question("Which German composer wrote the 9th Symphony?", List("Beethoven", "Bach", "Mozart", "Wagner"), "Beethoven", MCQ)
        ),
        
        // Version 2 - Geography and nature
        List(
          Question("Which river flows through Berlin?", List("Spree", "Rhine", "Elbe", "Danube"), "Spree", MCQ),
          Question("Which mountain range is found in Germany?", List("The Alps", "The Pyrenees", "The Carpathians", "The Andes"), "The Alps", MCQ),
          Question("Which of these countries does NOT border Germany?", List("Spain", "France", "Poland", "Denmark"), "Spain", MCQ),
          Question("What is the largest forest in Germany?", List("Black Forest", "Bavarian Forest", "Harz Forest", "Spessart Forest"), "Black Forest", MCQ),
          Question("Which sea lies to the north of Germany?", List("Baltic Sea", "Mediterranean Sea", "Black Sea", "Caspian Sea"), "Baltic Sea", MCQ)
        ),
        
        // Version 3 - Everyday knowledge
        List(
          Question("What is the currency used in Germany?", List("Euro", "Mark", "Franc", "Krone"), "Euro", MCQ),
          Question("What is a typical German breakfast food?", List("Brötchen (bread rolls)", "Eggs and bacon", "Cereal", "Crêpes"), "Brötchen (bread rolls)", MCQ),
          Question("What is the most common German greeting?", List("Hallo", "Guten Tag", "Grüß Gott", "All of these are common"), "All of these are common", MCQ),
          Question("What is 'Autobahn'?", List("Highway system", "Train system", "Subway", "Bicycle path"), "Highway system", MCQ),
          Question("Which is a traditional German food?", List("Sauerkraut", "Pizza", "Sushi", "Tacos"), "Sauerkraut", MCQ)
        )
      )
      
      // German MCQ quizzes - Medium difficulty (3 versions)
      case (German, Medium, MCQ) => List(
        // Version 1 - History
        List(
          Question("When did the Berlin Wall fall?", List("1989", "1991", "1945", "1961"), "1989", MCQ),
          Question("Who was the first chancellor of the Federal Republic of Germany?", List("Konrad Adenauer", "Helmut Kohl", "Willy Brandt", "Angela Merkel"), "Konrad Adenauer", MCQ),
          Question("When were East and West Germany reunified?", List("1990", "1989", "1991", "1995"), "1990", MCQ),
          Question("Which German city was divided into East and West during the Cold War?", List("Berlin", "Munich", "Hamburg", "Frankfurt"), "Berlin", MCQ),
          Question("Which German invention from 1440 revolutionized communication?", List("Printing press", "Telephone", "Radio", "Television"), "Printing press", MCQ)
        ),
        
        // Version 2 - Arts and literature
        List(
          Question("Which German brothers collected fairy tales?", List("The Brothers Grimm", "The Brothers Mann", "The Brothers Weber", "The Brothers Humboldt"), "The Brothers Grimm", MCQ),
          Question("Which German author wrote 'Faust'?", List("Goethe", "Schiller", "Kafka", "Mann"), "Goethe", MCQ),
          Question("Which art movement originated in Germany in the early 20th century?", List("Expressionism", "Impressionism", "Cubism", "Surrealism"), "Expressionism", MCQ),
          Question("Which German composer is known for 'The Magic Flute'?", List("Mozart", "Bach", "Beethoven", "Wagner"), "Mozart", MCQ),
          Question("Which film festival is held annually in Berlin?", List("Berlinale", "Cannes Film Festival", "Venice Film Festival", "Sundance Film Festival"), "Berlinale", MCQ)
        ),
        
        // Version 3 - Society and traditions
        List(
          Question("What is 'Karneval' in Germany?", List("A carnival season before Lent", "A summer festival", "A wine festival", "A Christmas tradition"), "A carnival season before Lent", MCQ),
          Question("What is a 'Biergarten'?", List("An outdoor beer garden", "A brewery", "A beer museum", "A type of pub"), "An outdoor beer garden", MCQ),
          Question("What does 'Kindergarten' literally mean in German?", List("Children's garden", "Children's play", "Children's school", "Children's home"), "Children's garden", MCQ),
          Question("What is the German political system?", List("Federal parliamentary republic", "Constitutional monarchy", "Direct democracy", "Presidential republic"), "Federal parliamentary republic", MCQ),
          Question("What is 'Mittelstand' in Germany?", List("Small and medium-sized enterprises", "Middle class", "Central government", "Middle management"), "Small and medium-sized enterprises", MCQ)
        )
      )

      case (German, Easy, Vocabulary) => List(
        // Version 1 - Basic nouns
        List(
          Question("Was ist 'apple' auf Deutsch?", List("Apfel", "Banane", "Orange", "Traube"), "Apfel", Vocabulary),
          Question("Was ist 'book' auf Deutsch?", List("Buch", "Heft", "Bleistift", "Kugelschreiber"), "Buch", Vocabulary),
          Question("Was ist 'dog' auf Deutsch?", List("Hund", "Katze", "Vogel", "Fisch"), "Hund", Vocabulary),
          Question("Was ist 'house' auf Deutsch?", List("Haus", "Wohnung", "Gebäude", "Hotel"), "Haus", Vocabulary),
          Question("Was ist 'car' auf Deutsch?", List("Auto", "Fahrrad", "Zug", "Flugzeug"), "Auto", Vocabulary)
        ),
        
        // Version 2 - Common verbs
        List(
          Question("Was ist 'to eat' auf Deutsch?", List("essen", "trinken", "schlafen", "laufen"), "essen", Vocabulary),
          Question("Was ist 'to drink' auf Deutsch?", List("trinken", "essen", "schlafen", "laufen"), "trinken", Vocabulary),
          Question("Was ist 'to sleep' auf Deutsch?", List("schlafen", "essen", "trinken", "laufen"), "schlafen", Vocabulary),
          Question("Was ist 'to walk' auf Deutsch?", List("laufen", "essen", "trinken", "schlafen"), "laufen", Vocabulary),
          Question("Was ist 'to write' auf Deutsch?", List("schreiben", "lesen", "sprechen", "hören"), "schreiben", Vocabulary)
        ),
        
        // Version 3 - Adjectives
        List(
          Question("Was ist 'big' auf Deutsch?", List("groß", "klein", "alt", "jung"), "groß", Vocabulary),
          Question("Was ist 'small' auf Deutsch?", List("klein", "groß", "alt", "jung"), "klein", Vocabulary),
          Question("Was ist 'old' auf Deutsch?", List("alt", "jung", "groß", "klein"), "alt", Vocabulary),
          Question("Was ist 'young' auf Deutsch?", List("jung", "alt", "groß", "klein"), "jung", Vocabulary),
          Question("Was ist 'beautiful' auf Deutsch?", List("schön", "hässlich", "gut", "schlecht"), "schön", Vocabulary)
        )
      )

      // German vocabulary quizzes - Medium difficulty (3 versions)
      case (German, Medium, Vocabulary) => List(
        // Version 1 - Professions
        List(
          Question("Was ist 'doctor' auf Deutsch?", List("Arzt", "Lehrer", "Ingenieur", "Anwalt"), "Arzt", Vocabulary),
          Question("Was ist 'teacher' auf Deutsch?", List("Lehrer", "Student", "Arzt", "Anwalt"), "Lehrer", Vocabulary),
          Question("Was ist 'engineer' auf Deutsch?", List("Ingenieur", "Arzt", "Lehrer", "Krankenschwester"), "Ingenieur", Vocabulary),
          Question("Was ist 'lawyer' auf Deutsch?", List("Anwalt", "Richter", "Polizist", "Feuerwehrmann"), "Anwalt", Vocabulary),
          Question("Was ist 'nurse' auf Deutsch?", List("Krankenschwester", "Arzt", "Apotheker", "Zahnarzt"), "Krankenschwester", Vocabulary)
        ),
        
        // Version 2 - Travel
        List(
          Question("Was ist 'passport' auf Deutsch?", List("Reisepass", "Ticket", "Koffer", "Hotel"), "Reisepass", Vocabulary),
          Question("Was ist 'airport' auf Deutsch?", List("Flughafen", "Bahnhof", "Hafen", "Terminal"), "Flughafen", Vocabulary),
          Question("Was ist 'hotel' auf Deutsch?", List("Hotel", "Pension", "Wohnung", "Haus"), "Hotel", Vocabulary),
          Question("Was ist 'ticket' auf Deutsch?", List("Fahrkarte", "Reisepass", "Gepäck", "Reservierung"), "Fahrkarte", Vocabulary),
          Question("Was ist 'suitcase' auf Deutsch?", List("Koffer", "Rucksack", "Tasche", "Gepäck"), "Koffer", Vocabulary)
        ),
        
        // Version 3 - Technology
        List(
          Question("Was ist 'computer' auf Deutsch?", List("Computer", "Telefon", "Tablet", "Fernseher"), "Computer", Vocabulary),
          Question("Was ist 'cell phone' auf Deutsch?", List("Handy", "Computer", "Tablet", "Fernseher"), "Handy", Vocabulary),
          Question("Was ist 'internet' auf Deutsch?", List("Internet", "Web", "Netz", "WLAN"), "Internet", Vocabulary),
          Question("Was ist 'printer' auf Deutsch?", List("Drucker", "Scanner", "Computer", "Monitor"), "Drucker", Vocabulary),
          Question("Was ist 'keyboard' auf Deutsch?", List("Tastatur", "Maus", "Bildschirm", "Drucker"), "Tastatur", Vocabulary)
        )
      )

      // German vocabulary quizzes - Hard difficulty (3 versions)
      case (German, Hard, Vocabulary) => List(
        // Version 1 - Business terminology
        List(
          Question("Was ist 'meeting' auf Deutsch?", List("Besprechung", "Präsentation", "Geschäft", "Konferenz"), "Besprechung", Vocabulary),
          Question("Was ist 'contract' auf Deutsch?", List("Vertrag", "Vereinbarung", "Dokument", "Projekt"), "Vertrag", Vocabulary),
          Question("Was ist 'investment' auf Deutsch?", List("Investition", "Gewinn", "Finanzen", "Budget"), "Investition", Vocabulary),
          Question("Was ist 'profit' auf Deutsch?", List("Gewinn", "Einnahme", "Einkommen", "Gehalt"), "Gewinn", Vocabulary),
          Question("Was ist 'management' auf Deutsch?", List("Management", "Verwaltung", "Leitung", "Koordination"), "Management", Vocabulary)
        ),
        
        // Version 2 - Medical terminology
        List(
          Question("Was ist 'diagnosis' auf Deutsch?", List("Diagnose", "Behandlung", "Symptom", "Krankheit"), "Diagnose", Vocabulary),
          Question("Was ist 'prescription' auf Deutsch?", List("Rezept", "Medikament", "Behandlung", "Apotheke"), "Rezept", Vocabulary),
          Question("Was ist 'examination' auf Deutsch?", List("Untersuchung", "Beratung", "Überprüfung", "Diagnose"), "Untersuchung", Vocabulary),
          Question("Was ist 'symptom' auf Deutsch?", List("Symptom", "Krankheit", "Schmerz", "Diagnose"), "Symptom", Vocabulary),
          Question("Was ist 'treatment' auf Deutsch?", List("Behandlung", "Medikament", "Therapie", "Pflege"), "Behandlung", Vocabulary)
        ),
        
        // Version 3 - Legal terminology
        List(
          Question("Was ist 'law' auf Deutsch?", List("Gesetz", "Recht", "Gerechtigkeit", "Kodex"), "Gesetz", Vocabulary),
          Question("Was ist 'justice' auf Deutsch?", List("Gerechtigkeit", "Gesetz", "Gericht", "Richter"), "Gerechtigkeit", Vocabulary),
          Question("Was ist 'lawsuit' auf Deutsch?", List("Klage", "Prozess", "Streit", "Rechtsstreit"), "Klage", Vocabulary),
          Question("Was ist 'evidence' auf Deutsch?", List("Beweis", "Zeugnis", "Aussage", "Erklärung"), "Beweis", Vocabulary),
          Question("Was ist 'verdict' auf Deutsch?", List("Urteil", "Entscheidung", "Beschluss", "Prozess"), "Urteil", Vocabulary)
        )
      )

      
      
      // German MCQ quizzes - Hard difficulty (3 versions)
      case (German, Hard, MCQ) => List(
        // Version 1 - Politics and government
        List(
          Question("How many states (Bundesländer) are there in Germany?", List("16", "12", "14", "18"), "16", MCQ),
          Question("What is the name of the German parliament?", List("Bundestag", "Bundesrat", "Reichstag", "Landtag"), "Bundestag", MCQ),
          Question("Who elects the German Federal President?", List("Federal Convention", "Direct popular vote", "Parliament", "Federal Council"), "Federal Convention", MCQ),
          Question("What is the 'Grundgesetz'?", List("The German constitution", "A basic tax law", "Civil code", "Criminal code"), "The German constitution", MCQ),
          Question("What is the term length for the German Chancellor?", List("No fixed term", "4 years", "5 years", "6 years"), "No fixed term", MCQ)
        ),
        
        // Version 2 - Economy and industry
        List(
          Question("Which German company is the world's largest chemical producer?", List("BASF", "Bayer", "Siemens", "ThyssenKrupp"), "BASF", MCQ),
          Question("What percentage of Germany's energy comes from renewable sources (as of 2020)?", List("About 45%", "About 25%", "About 65%", "About 10%"), "About 45%", MCQ),
          Question("Which German city hosts the world's largest computer expo (CeBIT)?", List("Hanover", "Berlin", "Munich", "Frankfurt"), "Hanover", MCQ),
          Question("Which economic system was practiced in East Germany?", List("Socialist planned economy", "Free market capitalism", "Social market economy", "Mixed economy"), "Socialist planned economy", MCQ),
          Question("What is 'Kurzarbeit' in the German labor system?", List("Reduced working hours during economic downturns", "Early retirement program", "Apprenticeship system", "Minimum wage standard"), "Reduced working hours during economic downturns", MCQ)
        ),
        
        // Version 3 - Language and education
        List(
          Question("What is 'Hochdeutsch'?", List("Standard German", "Swiss German", "Austrian German", "Medieval German"), "Standard German", MCQ),
          Question("What is the German university entrance qualification called?", List("Abitur", "Diplom", "Examen", "Zeugnis"), "Abitur", MCQ),
          Question("What is the German dual education system?", List("Combined vocational training and education", "Two separate university degrees", "Bilingual education", "Public and private schooling"), "Combined vocational training and education", MCQ),
          Question("Which grammatical feature exists in German but not in English?", List("Grammatical gender for nouns", "Verb conjugation", "Adjective order", "Plural forms"), "Grammatical gender for nouns", MCQ),
          Question("What is a 'Lehnwort' in German linguistics?", List("A loanword", "A neologism", "A compound word", "A pronoun"), "A loanword", MCQ)
        )
      )
      case _ => List()
    }
  }
  

  
  
  }

