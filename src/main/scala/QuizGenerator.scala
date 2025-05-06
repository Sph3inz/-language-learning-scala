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
      // Fallback to default questions if no specific banks exist for this combination
      getDefaultQuestionBank(language, quizType)
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
          Question("What is 'chair' in Spanish?", List("silla", "mesa", "sofá", "cama"), "silla", Vocabulary),
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

      // Spanish vocabulary quizzes - Impossible difficulty (3 versions)
      case (Spanish, Impossible, Vocabulary) => List(
        // Version 1 - Philosophical terms
        List(
          Question("What is 'epistemology' in Spanish?", List("epistemología", "ontología", "metafísica", "lógica"), "epistemología", Vocabulary),
          Question("What is 'existentialism' in Spanish?", List("existencialismo", "nihilismo", "empirismo", "racionalismo"), "existencialismo", Vocabulary),
          Question("What is 'dialectic' in Spanish?", List("dialéctica", "metafísica", "lógica", "retórica"), "dialéctica", Vocabulary),
          Question("What is 'hermeneutics' in Spanish?", List("hermenéutica", "semiótica", "fenomenología", "ontología"), "hermenéutica", Vocabulary),
          Question("What is 'phenomenology' in Spanish?", List("fenomenología", "epistemología", "ontología", "existencialismo"), "fenomenología", Vocabulary)
        ),

        // Version 2 - Rare scientific terms
        List(
          Question("What is 'bioluminescence' in Spanish?", List("bioluminiscencia", "fotosíntesis", "quimioluminiscencia", "biofísica"), "bioluminiscencia", Vocabulary),
          Question("What is 'quantum entanglement' in Spanish?", List("entrelazamiento cuántico", "dualidad onda-partícula", "superposición cuántica", "decoherencia cuántica"), "entrelazamiento cuántico", Vocabulary),
          Question("What is 'neurotransmitter' in Spanish?", List("neurotransmisor", "neuroreceptor", "sinapsis", "axón"), "neurotransmisor", Vocabulary),
          Question("What is 'nanotechnology' in Spanish?", List("nanotecnología", "biotecnología", "microtecnología", "femtotecnología"), "nanotecnología", Vocabulary),
          Question("What is 'thermodynamics' in Spanish?", List("termodinámica", "física cuántica", "mecánica de fluidos", "física nuclear"), "termodinámica", Vocabulary)
        ),

        // Version 3 - Obscure linguistic terms
        List(
          Question("What is 'subjunctive mood' in Spanish?", List("modo subjuntivo", "modo indicativo", "modo imperativo", "modo condicional"), "modo subjuntivo", Vocabulary),
          Question("What is 'lexicography' in Spanish?", List("lexicografía", "lexicología", "semántica", "etimología"), "lexicografía", Vocabulary),
          Question("What is 'morphology' in Spanish?", List("morfología", "fonología", "sintaxis", "pragmática"), "morfología", Vocabulary),
          Question("What is 'phonetics' in Spanish?", List("fonética", "fonología", "lingüística", "gramática"), "fonética", Vocabulary),
          Question("What is 'syntax' in Spanish?", List("sintaxis", "semántica", "morfología", "fonología"), "sintaxis", Vocabulary)
        )
      )

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
      case (Spanish, Impossible, Grammar) => List(
        // Version 1 - Exceptional cases and irregularities
        List(
          Question("Identify the correct statement about 'se' in Spanish.", List("Can be reflexive, reciprocal, impersonal, or passive marker", "Only used as a direct object pronoun", "Only used with reflexive verbs", "Only used in formal contexts"), "Can be reflexive, reciprocal, impersonal, or passive marker", Grammar),
          Question("Which is the correct archaic form of the second person plural imperative?", List("cantad", "cantáis", "cantéis", "cantarais"), "cantad", Grammar),
          Question("What is the correct usage of leísmo?", List("Using 'le' instead of 'lo' for male direct objects", "Using 'lo' instead of 'le' for indirect objects", "Using 'les' instead of 'los' for plural objects", "Using 'le' for formal situations"), "Using 'le' instead of 'lo' for male direct objects", Grammar),
          Question("What dialect feature is characterized by 'voseo'?", List("Using 'vos' instead of 'tú'", "Using 'ustedes' instead of 'vosotros'", "Using 'usted' instead of 'tú'", "Using 'vosotros' instead of 'ustedes'"), "Using 'vos' instead of 'tú'", Grammar),
          Question("In which case would 'sino' be used instead of 'pero'?", List("When contradicting a negative statement", "When introducing an exception", "When providing an alternative", "When comparing two things"), "When contradicting a negative statement", Grammar)
        ),
        
        // Version 2 - Literary and formal structures
        List(
          Question("What is the 'futuro de subjuntivo' used for in modern Spanish?", List("Legal or literary texts", "Everyday conversation", "Technical instructions", "Weather forecasts"), "Legal or literary texts", Grammar),
          Question("Complete this literary phrase: 'Aquellos que ___ (oír) oigan'", List("tengan oídos", "tienen oídos", "tendrán oídos", "tuvieran oídos"), "tengan oídos", Grammar),
          Question("Which literary tense is used in 'Cantara él con más ánimo'?", List("Imperfect subjunctive as a substitute for conditional", "Present subjunctive", "Future subjunctive", "Imperative"), "Imperfect subjunctive as a substitute for conditional", Grammar),
          Question("What is the correct form for 'deber' indicating probability in the past?", List("debió de + infinitive", "debió + infinitive", "debía de + infinitive", "debiera + infinitive"), "debió de + infinitive", Grammar),
          Question("Complete the academic phrase: 'Conforme a lo ___ (exponer) anteriormente'", List("expuesto", "exponido", "exponiendo", "exponido"), "expuesto", Grammar)
        ),
        
        // Version 3 - Dialectal variations
        List(
          Question("Which form of 'you' plural is used in most of Latin America?", List("ustedes", "vosotros", "vos", "tú"), "ustedes", Grammar),
          Question("How would 'tú tienes' be expressed using voseo?", List("vos tenés", "vos tienes", "tú tenés", "vos tenís"), "vos tenés", Grammar),
          Question("Which Spanish dialect uses 'ustedes' with third-person plural forms?", List("All Latin American dialects", "Only Peninsular Spanish", "Only Caribbean Spanish", "Only Andean Spanish"), "All Latin American dialects", Grammar),
          Question("In Rioplatense Spanish (Argentina), how would you say 'be quiet'?", List("callate", "cállate", "os calláis", "se calla"), "callate", Grammar),
          Question("Which dialectal feature is common in Andalusian Spanish?", List("Dropping final 's' sounds", "Adding extra 's' sounds", "Using 'vos' instead of 'tú'", "Using 'vosotros' instead of 'ustedes'"), "Dropping final 's' sounds", Grammar)
        )
      )

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

      // French grammar quizzes - Impossible difficulty (3 versions)
      case (French, Impossible, Grammar) => List(
        // Version 1 - Literary and formal French
        List(
          Question("What is the passé simple of 'prendre' for 'ils'?", List("ils prirent", "ils ont pris", "ils prenaient", "ils prendront"), "ils prirent", Grammar),
          Question("What is the correct usage of the 'ne explétif'?", List("After verbs of fear: 'Je crains qu'il ne soit trop tard'", "For negation: 'Je ne veux pas'", "For emphasis: 'N'est-ce pas vrai?'", "For formal commands: 'Ne venez pas'"), "After verbs of fear: 'Je crains qu'il ne soit trop tard'", Grammar),
          Question("Which is a correct example of the 'subjonctif imparfait'?", List("qu'il vînt", "qu'il vienne", "qu'il viendrait", "qu'il venait"), "qu'il vînt", Grammar),
          Question("What is the correct translation of the literary tense in: 'Eussé-je su la vérité'?", List("Had I known the truth", "I know the truth", "If I knew the truth", "I would know the truth"), "Had I known the truth", Grammar),
          Question("In literary French, what does 'que' replace in the inverted conditional?", List("'si' in a condition: 'Eût-il parlé' = 'S'il avait parlé'", "'quand' in time expressions", "'qui' in relative clauses", "'où' in place expressions"), "'si' in a condition: 'Eût-il parlé' = 'S'il avait parlé'", Grammar)
        ),
        
        // Version 2 - Archaic and exceptional forms
        List(
          Question("What is the correct archaic second person singular form of the imperative of 'avoir'?", List("aie", "as", "ai", "ais"), "aie", Grammar),
          Question("What is the 'subjonctif plus-que-parfait' of 'pouvoir' with 'vous'?", List("que vous eussiez pu", "que vous puissiez", "que vous avez pu", "que vous pouvez"), "que vous eussiez pu", Grammar),
          Question("Which is an example of a 'conditionnel surcomposé'?", List("il aurait eu terminé", "il aurait terminé", "il aura terminé", "il eût terminé"), "il aurait eu terminé", Grammar),
          Question("What is the archaic inversion form of 'Que Dieu vous protège'?", List("Que Dieu vous protège-t-il", "Dieu vous protège-t-il", "Vous protège Dieu", "Vous Dieu protège"), "Que Dieu vous protège-t-il", Grammar),
          Question("Which construction is an example of a 'proposition infinitive'?", List("Je les entends chanter", "Je veux chanter", "Je suis en train de chanter", "Je sais qu'ils chantent"), "Je les entends chanter", Grammar)
        ),
        
        // Version 3 - Dialectal and register variations
        List(
          Question("Which expression is typical of Quebec French?", List("C'est plate (It's boring)", "C'est génial", "C'est chouette", "C'est nul"), "C'est plate (It's boring)", Grammar),
          Question("Which negation structure is typical of informal spoken French?", List("'Je sais pas' (omitting 'ne')", "'Je ne sais pas'", "'Je ne sais point'", "'Je sais jamais'"), "'Je sais pas' (omitting 'ne')", Grammar),
          Question("In African French, what does 'Il a chicotté l'enfant' mean?", List("He whipped/beat the child", "He scolded the child", "He educated the child", "He punished the child"), "He whipped/beat the child", Grammar),
          Question("What is the meaning of the Belgian French term 'drache'?", List("Heavy rain", "Strong wind", "Snow", "Fog"), "Heavy rain", Grammar),
          Question("Which structure is common in Swiss French?", List("'Ça joue?' (Is it okay?)", "'Ça marche?'", "'Ça va?'", "'Ça roule?'"), "'Ça joue?' (Is it okay?)", Grammar)
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
      
      // English grammar quizzes - Impossible difficulty (3 versions)
      case (English, Impossible, Grammar) => List(
        // Version 1 - Complex grammatical structures and nuances
        List(
          Question("Identify the correct usage of the subjunctive in a contrary-to-fact statement:", List("If I were you, I would reconsider that decision", "If I was you, I would reconsider that decision", "If I am you, I would reconsider that decision", "If I be you, I would reconsider that decision"), "If I were you, I would reconsider that decision", Grammar),
          Question("Which sentence demonstrates correct subject-verb inversion for emphasis?", List("Never have I seen such beauty", "Never I have seen such beauty", "I have never seen such beauty", "I never have seen such beauty"), "Never have I seen such beauty", Grammar),
          Question("Identify the sentence with the correct use of a cleft structure:", List("It was in Paris that I met her for the first time", "In Paris is where I met her for the first time", "It is in Paris where I met her for the first time", "In Paris it was that I met her for the first time"), "It was in Paris that I met her for the first time", Grammar),
          Question("Which sentence correctly uses a double negative for rhetorical effect?", List("It is not uncommon to make such mistakes", "It is not unobvious to make such mistakes", "It is not unillegal to make such mistakes", "It is not uninformal to make such mistakes"), "It is not uncommon to make such mistakes", Grammar),
          Question("Choose the sentence with correct inversion in a hypothetical subjunctive construction:", List("Were she to arrive early, we would start the meeting", "Was she to arrive early, we would start the meeting", "Should she arrives early, we would start the meeting", "Would she arrive early, we would start the meeting"), "Were she to arrive early, we would start the meeting", Grammar)
        ),
        
        // Version 2 - Archaic and literary forms
        List(
          Question("Identify the archaic second-person singular verb form:", List("thou dost", "thou do", "thou does", "thou doing"), "thou dost", Grammar),
          Question("Which is the correct Shakespearean form of the negative imperative?", List("Fear not the challenge", "Do not fear the challenge", "Not fear the challenge", "Non-fear the challenge"), "Fear not the challenge", Grammar),
          Question("Identify the correct use of the historic present tense in narrative:", List("Caesar crosses the Rubicon and changes history forever", "Caesar crossing the Rubicon and changing history forever", "Caesar crossed the Rubicon and changed history forever", "Caesar had crossed the Rubicon and had changed history forever"), "Caesar crosses the Rubicon and changes history forever", Grammar),
          Question("Choose the correct archaic possessive form:", List("mine own house", "my own house", "the own house of mine", "the mine own house"), "mine own house", Grammar),
          Question("Identify the correct use of 'would' to express a habitual past action:", List("When I was a child, I would visit my grandmother every Sunday", "When I was a child, I had visited my grandmother every Sunday", "When I was a child, I will visit my grandmother every Sunday", "When I was a child, I visited my grandmother every Sunday"), "When I was a child, I would visit my grandmother every Sunday", Grammar)
        ),
        
        // Version 3 - Dialectal variations and register
        List(
          Question("Which sentence correctly uses the subjunctive in American English?", List("The committee recommends that he submit his report", "The committee recommends that he submits his report", "The committee recommends that he submitted his report", "The committee recommends that he would submit his report"), "The committee recommends that he submit his report", Grammar),
          Question("Identify the correct use of 'shall' in British formal English:", List("We shall require payment within 30 days", "We will require payment within 30 days", "We should require payment within 30 days", "We ought to require payment within 30 days"), "We shall require payment within 30 days", Grammar),
          Question("Which is an example of a correct complex preposition in formal writing?", List("The decision was made in accordance with the regulations", "The decision was made according with the regulations", "The decision was made accordant to the regulations", "The decision was made in according to the regulations"), "The decision was made in accordance with the regulations", Grammar),
          Question("Identify the sentence that correctly uses British English spelling and grammar:", List("The organisation specialises in analysing behavioural patterns", "The organization specializes in analyzing behavioral patterns", "The organisation specializes in analysing behavioural patterns", "The organization specialises in analyzing behavioral patterns"), "The organisation specialises in analysing behavioural patterns", Grammar),
          Question("Which sentence demonstrates correct use of the definite article in idiomatic English?", List("She plays the piano beautifully", "She plays piano beautifully", "She plays a piano beautifully", "She plays beautifully the piano"), "She plays the piano beautifully", Grammar)
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
      case (German, Impossible, Grammar) => List(
        // Version 1 - Advanced subjunctive and conditional forms
        List(
          Question("Which is the double infinitive construction: 'Er ___ besser machen können'", List("hätte", "würde", "hat", "wäre"), "hätte", Grammar),
          Question("Complete: 'Er tat so, als ___ er nichts gehört' (He acted as if he had heard nothing)", List("hätte", "habe", "würde haben", "hatte"), "hätte", Grammar),
          Question("Identify the correct subjunctive form in indirect speech: 'Sie sagte, sie ___ müde'", List("sei", "wäre", "ist", "würde sein"), "sei", Grammar),
          Question("What is the correct Konjunktiv II alternative form for 'käme'?", List("würde kommen", "komme", "kämen", "kommte"), "würde kommen", Grammar),
          Question("Which form is used for unreal past conditions in German?", List("Plusquamperfekt Konjunktiv", "Präteritum Konjunktiv", "Futur Konjunktiv", "Präsens Konjunktiv"), "Plusquamperfekt Konjunktiv", Grammar)
        ),
        
        // Version 2 - Advanced verbal structures
        List(
          Question("Which is the correct position for multiple verbal elements: 'Er ___ das Buch ___ ___'", List("hat lesen wollen", "hat wollen lesen", "wollen hat lesen", "lesen hat wollen"), "hat lesen wollen", Grammar),
          Question("Identify the correct extended infinitive structure: 'Sie geht in den Park, ___'", List("um zu spielen", "zu spielen", "für zu spielen", "damit zu spielen"), "um zu spielen", Grammar),
          Question("Which sentence correctly uses a modal particle?", List("Das ist ja interessant!", "Das ja ist interessant!", "Das ist interessant ja!", "Ja das ist interessant!"), "Das ist ja interessant!", Grammar),
          Question("Identify the correct use of a separable verb in the perfect tense:", List("Er hat den Vorschlag angenommen", "Er angenommen hat den Vorschlag", "Er hat angenommen den Vorschlag", "Er hat den Vorschlag genomannen"), "Er hat den Vorschlag angenommen", Grammar),
          Question("Which is the correct structure with two-way prepositions defining motion?", List("Ich gehe in die Schule", "Ich gehe in der Schule", "Ich gehe zu die Schule", "Ich gehe nach die Schule"), "Ich gehe in die Schule", Grammar)
        ),
        
        // Version 3 - Participles and advanced structures
        List(
          Question("Which is the correct present participle used as an adjective?", List("die lachenden Kinder", "die gelachten Kinder", "die lachten Kinder", "die zu lachenden Kinder"), "die lachenden Kinder", Grammar),
          Question("Identify the correct expanded participial construction:", List("In Berlin angekommen, ging er sofort zum Hotel", "Angekommen in Berlin, ging er sofort zum Hotel", "Er angekommen in Berlin, ging sofort zum Hotel", "Er ging sofort zum Hotel, in Berlin angekommen"), "In Berlin angekommen, ging er sofort zum Hotel", Grammar),
          Question("Which is the correct form of 'zu' with separable verbs?", List("anzufangen", "zu anfangen", "aufzuangen", "zu fangen an"), "anzufangen", Grammar),
          Question("Complete with the correct functional verb construction: 'Die Arbeit ___ zum Abschluss'", List("kommt", "macht", "gibt", "stellt"), "kommt", Grammar),
          Question("Identify the grammatically correct complex adjectival phrase:", List("der seit drei Jahren in Berlin lebende Mann", "der in Berlin seit drei Jahren lebend Mann", "der Mann seit drei Jahren in Berlin lebend", "der lebende Mann seit drei Jahren in Berlin"), "der seit drei Jahren in Berlin lebende Mann", Grammar)
        )
      )
      
      // Arabic grammar quizzes - Easy difficulty (3 versions)
      case (Arabic, Easy, Grammar) => List(
        // Version 1 - Basic sentence structure
        List(
          Question("What is the correct word order in Arabic sentences?", List("Verb-Subject-Object (VSO)", "Subject-Verb-Object (SVO)", "Object-Verb-Subject (OVS)", "Subject-Object-Verb (SOV)"), "Verb-Subject-Object (VSO)", Grammar),
          Question("Which is the correct definite article in Arabic?", List("ال (al)", "في (fi)", "من (min)", "على (ala)"), "ال (al)", Grammar),
          Question("How are adjectives positioned relative to nouns in Arabic?", List("After the noun", "Before the noun", "Either before or after", "No specific order"), "After the noun", Grammar),
          Question("What is the correct way to say 'a book' in Arabic (indefinite)?", List("كتاب (kitab)", "الكتاب (al-kitab)", "كتابٌ (kitabun)", "كتابي (kitabi)"), "كتابٌ (kitabun)", Grammar),
          Question("Which sentence has the correct word order in Arabic?", List("يأكل الولد التفاحة (The boy eats the apple)", "الولد يأكل التفاحة", "التفاحة الولد يأكل", "الولد التفاحة يأكل"), "يأكل الولد التفاحة (The boy eats the apple)", Grammar)
        ),
        
        // Version 2 - Gender and number agreement
        List(
          Question("How many grammatical genders are in Arabic?", List("Two (masculine and feminine)", "Three (masculine, feminine, and neuter)", "Four (masculine, feminine, neuter, and common)", "One (no gender)"), "Two (masculine and feminine)", Grammar),
          Question("Which is the correct feminine form of المعلم (the male teacher)?", List("المعلمة", "المعلمات", "المعلمتان", "المعلمون"), "المعلمة", Grammar),
          Question("How many number forms does Arabic have?", List("Three (singular, dual, and plural)", "Two (singular and plural)", "Four (singular, dual, plural, and collective)", "One (no number distinction)"), "Three (singular, dual, and plural)", Grammar),
          Question("Which is the correct dual form of كتاب (book)?", List("كتابان", "كتابين", "كتابات", "كتب"), "كتابان", Grammar),
          Question("What is the correct feminine plural form of طالب (male student)?", List("طالبات", "طالبان", "طالبين", "طلاب"), "طالبات", Grammar)
        ),
        
        // Version 3 - Basic verb conjugation
        List(
          Question("In Arabic, what is the base form of verbs called?", List("المصدر (masdar)", "الفعل (fi'l)", "الاسم (ism)", "الصفة (sifa)"), "المصدر (masdar)", Grammar),
          Question("What is the past tense form of 'to write' for 'he' in Arabic?", List("كتب (kataba)", "يكتب (yaktubu)", "اكتب (uktub)", "كاتب (katib)"), "كتب (kataba)", Grammar),
          Question("What is the present tense form of 'to read' for 'I' in Arabic?", List("أقرأ (aqra'u)", "قرأت (qara'tu)", "تقرأ (taqra'u)", "يقرأ (yaqra'u)"), "أقرأ (aqra'u)", Grammar),
          Question("Which sentence correctly uses the present tense in Arabic?", List("أنا أكتب رسالة (I write a letter)", "أنا كتبت رسالة", "أنا سوف أكتب رسالة", "أنا كاتب رسالة"), "أنا أكتب رسالة (I write a letter)", Grammar),
          Question("What is the imperative form of 'to open' for 'you' (masculine) in Arabic?", List("افتح (iftah)", "يفتح (yaftahu)", "فتح (fataha)", "مفتوح (maftooh)"), "افتح (iftah)", Grammar)
        )
      )
      
      // Arabic grammar quizzes - Medium difficulty (3 versions)
      case (Arabic, Medium, Grammar) => List(
        // Version 1 - Case endings
        List(
          Question("What are the three grammatical cases in Arabic?", List("Nominative, accusative, and genitive", "Subject, object, and possessive", "Agent, patient, and instrument", "Active, passive, and reflexive"), "Nominative, accusative, and genitive", Grammar),
          Question("What is the nominative case marker in Arabic?", List("الضمة (damma - ُ)", "الفتحة (fatha - َ)", "الكسرة (kasra - ِ)", "السكون (sukoon - ْ)"), "الضمة (damma - ُ)", Grammar),
          Question("Which case is used for the subject of a verbal sentence in Arabic?", List("Nominative", "Accusative", "Genitive", "Vocative"), "Nominative", Grammar),
          Question("Which case is used after most prepositions in Arabic?", List("Genitive", "Accusative", "Nominative", "Dative"), "Genitive", Grammar),
          Question("In the phrase 'كتاب الطالب' (the student's book), what case is 'الطالب' in?", List("Genitive", "Nominative", "Accusative", "Dative"), "Genitive", Grammar)
        ),
        
        // Version 2 - Verb forms
        List(
          Question("How many derived verb forms are there in Classical Arabic?", List("Ten", "Five", "Seven", "Three"), "Ten", Grammar),
          Question("Which form of the verb typically indicates reciprocal action?", List("Form VI (تفاعل)", "Form II (فعّل)", "Form IV (أفعل)", "Form VIII (افتعل)"), "Form VI (تفاعل)", Grammar),
          Question("What is the typical meaning added by Form II (فعّل) of Arabic verbs?", List("Causative or intensive", "Reciprocal", "Reflexive", "Passive"), "Causative or intensive", Grammar),
          Question("Which verb form is derived from كتب (to write) that means 'to correspond with each other'?", List("تكاتب (Form VI)", "كتّب (Form II)", "اكتتب (Form VIII)", "استكتب (Form X)"), "تكاتب (Form VI)", Grammar),
          Question("Which form is used for the verb عَلِمَ (to know) to make it mean 'to teach'?", List("Form II: عَلَّمَ", "Form IV: أَعْلَمَ", "Form III: عَالَمَ", "Form X: اِسْتَعْلَمَ"), "Form II: عَلَّمَ", Grammar)
        ),
        
        // Version 3 - Complex sentences
        List(
          Question("Which Arabic word is used to mean 'that' in a nominal sentence?", List("أنّ (anna)", "أن (an)", "لأن (li-anna)", "إذا (idha)"), "أنّ (anna)", Grammar),
          Question("Which particle is used to negate past tense verbs in Arabic?", List("لم (lam)", "لا (la)", "ما (ma)", "ليس (laysa)"), "لم (lam)", Grammar),
          Question("Which conditional particle means 'if' for possible conditions in Arabic?", List("إذا (idha)", "لو (law)", "إن (in)", "لولا (lawla)"), "إذا (idha)", Grammar),
          Question("What is the correct Arabic term for a verbal sentence?", List("جملة فعلية (jumla fi'liyya)", "جملة اسمية (jumla ismiyya)", "جملة ظرفية (jumla dharfiyya)", "جملة شرطية (jumla shartiyya)"), "جملة فعلية (jumla fi'liyya)", Grammar),
          Question("How do you say 'I want to go' in Arabic using the subjunctive?", List("أريد أن أذهب", "أريد أذهب", "أريد لأذهب", "أريد سوف أذهب"), "أريد أن أذهب", Grammar)
        )
      )
      
      // Arabic grammar quizzes - Hard difficulty (3 versions)
      case (Arabic, Hard, Grammar) => List(
        // Version 1 - Broken plurals
        List(
          Question("What is the term for irregular plurals in Arabic?", List("جمع التكسير (jam' at-taksir)", "جمع المؤنث السالم (jam' al-mu'annath as-salim)", "جمع المذكر السالم (jam' al-mudhakkar as-salim)", "المثنى (al-muthanna)"), "جمع التكسير (jam' at-taksir)", Grammar),
          Question("What is the plural form of كتاب (book)?", List("كتب (kutub)", "كتابون (kitaboon)", "كتابات (kitabat)", "أكتبة (aktiba)"), "كتب (kutub)", Grammar),
          Question("What is the plural form of قلب (heart)?", List("قلوب (quloob)", "قلبان (qalban)", "قلوبات (quloobat)", "أقلاب (aqlab)"), "قلوب (quloob)", Grammar),
          Question("What is the plural pattern used for مسجد (mosque)?", List("مساجد (masajid)", "مسجدون (masjidoon)", "مساجيد (masajeed)", "أمسجدة (amsjida)"), "مساجد (masajid)", Grammar),
          Question("Which word has a sound masculine plural?", List("مدرس (teacher) → مدرسون", "كتاب (book) → كتب", "بيت (house) → بيوت", "قلم (pen) → أقلام"), "مدرس (teacher) → مدرسون", Grammar)
        ),
        
        // Version 2 - Complex verb tenses
        List(
          Question("Which construction is used for the future perfect in Arabic?", List("سيكون قد + past verb", "سوف + present verb", "كان قد + past verb", "قد + present verb"), "سيكون قد + past verb", Grammar),
          Question("What is the correct way to express 'had been doing' in Arabic?", List("كان قد + past verb", "كان + present verb", "قد + past verb", "سيكون + past verb"), "كان قد + past verb", Grammar),
          Question("How do you express 'must have done' in Arabic?", List("لا بد أنه قد + past verb", "يجب أن + present verb", "من الضروري + present verb", "كان من المفروض أن + present verb"), "لا بد أنه قد + past verb", Grammar),
          Question("Which particle is used with the jussive mood to express prohibition?", List("لا (la) + jussive", "لن (lan) + subjunctive", "لم (lam) + jussive", "ما (ma) + perfect"), "لا (la) + jussive", Grammar),
          Question("How would you translate 'He has been writing' into Arabic?", List("كان يكتب", "قد كتب", "ما زال يكتب", "سوف يكتب"), "ما زال يكتب", Grammar)
        ),
        
        // Version 3 - Grammatical exceptions
        List(
          Question("Which of these nouns is diptote (doesn't take tanween)?", List("أحمد (Ahmad)", "كتاب (book)", "قلم (pen)", "رجل (man)"), "أحمد (Ahmad)", Grammar),
          Question("Which of these is a 'sister of كان' (kana)?", List("أصبح (asbaha - to become)", "قال (qala - to say)", "فتح (fataha - to open)", "ذهب (dhahaba - to go)"), "أصبح (asbaha - to become)", Grammar),
          Question("What is special about the 'Five Nouns' in Arabic grammar?", List("They have special case endings with و، ا، ي", "They only have singular forms", "They are always indefinite", "They never take the definite article"), "They have special case endings with و، ا، ي", Grammar),
          Question("Which of these is one of the 'Five Nouns'?", List("أب (father)", "كتاب (book)", "بيت (house)", "شجرة (tree)"), "أب (father)", Grammar),
          Question("Which type of noun doesn't accept nunation (tanween)?", List("Proper nouns", "Common nouns", "Abstract nouns", "Collective nouns"), "Proper nouns", Grammar)
        )
      )
      
      // Arabic grammar quizzes - Impossible difficulty (3 versions)
      case (Arabic, Impossible, Grammar) => List(
        // Version 1 - Classical Arabic poetry and rhetoric
        List(
          Question("What is the term for Arabic rhetoric?", List("البلاغة (al-balagha)", "النحو (an-nahw)", "الصرف (as-sarf)", "العروض (al-'arud)"), "البلاغة (al-balagha)", Grammar),
          Question("What is the branch of Arabic rhetoric that deals with syntax for effective expression?", List("علم المعاني (ilm al-ma'ani)", "علم البيان (ilm al-bayan)", "علم البديع (ilm al-badi')", "علم العروض (ilm al-'arud)"), "علم المعاني (ilm al-ma'ani)", Grammar),
          Question("What is the rhetoric term for personification in Arabic?", List("التشخيص (at-tashkhis)", "الاستعارة (al-isti'ara)", "التشبيه (at-tashbih)", "الكناية (al-kinaya)"), "التشخيص (at-tashkhis)", Grammar),
          Question("Which grammatical case is used for the circumstantial adverb (الحال) in Arabic?", List("النصب (accusative)", "الرفع (nominative)", "الجر (genitive)", "الجزم (jussive)"), "النصب (accusative)", Grammar),
          Question("In Arabic poetry meter, what is the smallest unit called?", List("السبب (as-sabab)", "البيت (al-bayt)", "البحر (al-bahr)", "القافية (al-qafiya)"), "السبب (as-sabab)", Grammar)
        ),
        
        // Version 2 - Dialectical variations
        List(
          Question("Which dialectical feature distinguishes Egyptian Arabic from Modern Standard Arabic?", List("Replacing ث with س or ت", "Dropping case endings", "Adding the prefix 'ب' to present tense verbs", "All of these"), "All of these", Grammar),
          Question("How does Levantine Arabic form the future tense, unlike MSA?", List("Using رح or ح before the verb", "Using سوف before the verb", "Using the prefix س", "Using the particle قد"), "Using رح or ح before the verb", Grammar),
          Question("In Moroccan Arabic (Darija), what often happens to short vowels?", List("They are frequently dropped", "They are lengthened", "They are replaced with dipthongs", "They are nasalized"), "They are frequently dropped", Grammar),
          Question("Which is a feature of Gulf Arabic not found in MSA?", List("The use of چ (ch) sound", "Lack of dual forms", "No gender distinction", "No definite article"), "The use of چ (ch) sound", Grammar),
          Question("What grammatical feature is preserved in some Yemeni dialects but lost in many other dialects?", List("Case endings", "Dual verbs", "The letter ث", "Passive voice"), "Case endings", Grammar)
        ),
        
        // Version 3 - Linguistic analysis
        List(
          Question("What is i'rab in Arabic grammar?", List("The system of case endings", "The plural formation", "The verbal paradigm", "The writing system"), "The system of case endings", Grammar),
          Question("What is the grammatical term for a verbal noun in Arabic?", List("مصدر (masdar)", "اسم فاعل (ism fa'il)", "اسم مفعول (ism maf'ul)", "اسم آلة (ism ala)"), "مصدر (masdar)", Grammar),
          Question("What is the linguistic term for the consonantal root system in Arabic?", List("الجذر (al-jidhr)", "الميزان الصرفي (al-mizan as-sarfi)", "الوزن (al-wazn)", "الصيغة (as-sigha)"), "الجذر (al-jidhr)", Grammar),
          Question("In Arabic linguistic theory, what does إعلال (i'lal) refer to?", List("Changes in weak letters in a word", "Addition of extra letters", "Shortening of vowels", "Dropping of case endings"), "Changes in weak letters in a word", Grammar),
          Question("What is the term for the phonological process where ت (ta) assimilates with certain consonants?", List("الإدغام (al-idgham)", "الإبدال (al-ibdal)", "الإعلال (al-i'lal)", "الإظهار (al-idhar)"), "الإدغام (al-idgham)", Grammar)
        )
      )

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
      case (Arabic, Easy, Translation) => List(
        // Version 1 - Basic greetings and phrases
        List(
          Question("How do you say 'Hello, how are you?' in Arabic?", List("مرحباً، كيف حالك؟", "السلام عليكم، كيف حالك؟", "أهلاً، كيف الحال؟", "All of these are correct"), "مرحباً، كيف حالك؟", Translation),
          Question("Translate 'My name is John' to Arabic", List("اسمي جون", "أنا جون", "يُدعى جون", "All of these are acceptable"), "اسمي جون", Translation),
          Question("Translate 'I would like a coffee, please' to Arabic", List("أريد قهوة من فضلك", "أود قهوة لو سمحت", "هل يمكنني الحصول على قهوة من فضلك", "All of these are correct"), "أريد قهوة من فضلك", Translation),
          Question("How do you say 'Where is the bathroom?' in Arabic?", List("أين الحمام؟", "أين المرحاض؟", "أين دورة المياه؟", "All of these are acceptable"), "أين الحمام؟", Translation),
          Question("Translate 'Thank you very much' to Arabic", List("شكراً جزيلاً", "شكراً كثيراً", "أشكرك بشدة", "All of these are correct"), "شكراً جزيلاً", Translation)
        ),
        
        // Version 2 - Food and restaurants
        List(
          Question("Translate 'I would like to order' to Arabic", List("أريد أن أطلب", "أود أن أطلب", "أرغب في الطلب", "All of these are correct"), "أريد أن أطلب", Translation),
          Question("How do you say 'The bill, please' in Arabic?", List("الحساب من فضلك", "الفاتورة لو سمحت", "أريد أن أدفع", "All of these are acceptable"), "الحساب من فضلك", Translation),
          Question("Translate 'Is there a vegetarian option?' to Arabic", List("هل هناك خيار نباتي؟", "هل توجد أطباق نباتية؟", "هل لديكم طعام للنباتيين؟", "All of these are correct"), "هل هناك خيار نباتي؟", Translation),
          Question("How do you say 'This food is delicious' in Arabic?", List("هذا الطعام لذيذ", "الطعام شهي", "وجبة لذيذة", "All of these convey the same meaning"), "هذا الطعام لذيذ", Translation),
          Question("Translate 'I am allergic to nuts' to Arabic", List("أنا أعاني من حساسية من المكسرات", "لدي حساسية من المكسرات", "أنا حساس للمكسرات", "All of these convey the meaning"), "لدي حساسية من المكسرات", Translation)
        ),
        
        // Version 3 - Travel and directions
        List(
          Question("How do you say 'How much does this cost?' in Arabic?", List("كم يكلف هذا؟", "كم سعر هذا؟", "بكم هذا؟", "All of these are correct"), "كم يكلف هذا؟", Translation),
          Question("Translate 'I need a hotel' to Arabic", List("أحتاج إلى فندق", "أريد فندقاً", "أبحث عن فندق", "All of these are correct"), "أحتاج إلى فندق", Translation),
          Question("How do you say 'Turn right at the corner' in Arabic?", List("انعطف يميناً عند الزاوية", "اتجه يميناً عند المنعطف", "خذ اليمين عند الزاوية", "All of these convey the direction"), "انعطف يميناً عند الزاوية", Translation),
          Question("Translate 'Is it far from here?' to Arabic", List("هل هو بعيد من هنا؟", "هل هذا بعيد؟", "هل المسافة طويلة من هنا؟", "All of these are correct"), "هل هو بعيد من هنا؟", Translation),
          Question("How do you say 'I am lost' in Arabic?", List("أنا تائه", "لقد ضللت الطريق", "أنا ضائع", "All of these convey being lost"), "أنا تائه", Translation)
        )
      )
      
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
      
      // French MCQ quizzes - Impossible difficulty (3 versions)
      case (French, Impossible, MCQ) => List(
        // Version 1 - Obscure history and politics
        List(
          Question("Who was the last king of France before the 1848 Revolution?", List("Louis-Philippe I", "Charles X", "Louis XVIII", "Napoleon III"), "Louis-Philippe I", MCQ),
          Question("Which French prime minister served the longest consecutive term?", List("François Fillon", "Georges Pompidou", "Pierre Mauroy", "Michel Rocard"), "François Fillon", MCQ),
          Question("Which constitutional article allows the French president to take emergency powers?", List("Article 16", "Article 49", "Article 89", "Article 35"), "Article 16", MCQ),
          Question("Which French overseas territory held an independence referendum in 2018?", List("New Caledonia", "French Guiana", "Réunion", "Mayotte"), "New Caledonia", MCQ),
          Question("What was the name of the French colonial policy of cultural assimilation?", List("Mission civilisatrice", "Pax Gallica", "Ancien Régime colonial", "Politique indigène"), "Mission civilisatrice", MCQ)
        ),
        
        // Version 2 - Specialized knowledge
        List(
          Question("What is the average annual wine production in France (in billions of liters)?", List("4.2", "2.8", "7.5", "10.1"), "4.2", MCQ),
          Question("Which French mathematician invented analytical geometry?", List("René Descartes", "Blaise Pascal", "Pierre de Fermat", "Joseph Fourier"), "René Descartes", MCQ),
          Question("What is 'Loi Toubon'?", List("Law protecting the French language", "Anti-terrorism legislation", "Agricultural subsidy regulation", "Environmental protection act"), "Law protecting the French language", MCQ),
          Question("What French term describes the specific local growing conditions for wine?", List("Terroir", "Vendange", "Millésime", "Cépage"), "Terroir", MCQ),
          Question("What is the French 'Grandes Écoles' system?", List("Elite higher education institutions", "Primary school curriculum", "Art education framework", "Military academies"), "Elite higher education institutions", MCQ)
        ),
        
        // Version 3 - Extremely specific cultural knowledge
        List(
          Question("Which French scientist won the Nobel Prize for discovering radioactivity?", List("Henri Becquerel", "Marie Curie", "Louis Pasteur", "Pierre Curie"), "Henri Becquerel", MCQ),
          Question("What is 'Alexandrin' in French poetry?", List("A twelve-syllable line", "A specific rhyme scheme", "A type of sonnet", "A structural device"), "A twelve-syllable line", MCQ),
          Question("What is the traditional 'Pot-au-Feu'?", List("A beef stew", "A ceramic cooking pot", "A fireplace design", "A holiday tradition"), "A beef stew", MCQ),
          Question("Which French philosopher coined the phrase 'I think, therefore I am'?", List("René Descartes", "Jean-Paul Sartre", "Albert Camus", "Voltaire"), "René Descartes", MCQ),
          Question("What is the oldest still-operating French perfume house?", List("Houbigant", "Guerlain", "Chanel", "Dior"), "Houbigant", MCQ)
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
      
      // Spanish MCQ quizzes - Impossible difficulty (3 versions)
      case (Spanish, Impossible, MCQ) => List(
        // Version 1 - Obscure history and politics
        List(
          Question("Which Spanish king abdicated in 2014?", List("Juan Carlos I", "Felipe VI", "Alfonso XIII", "Carlos IV"), "Juan Carlos I", MCQ),
          Question("In which year was the First Spanish Republic proclaimed?", List("1873", "1898", "1931", "1812"), "1873", MCQ),
          Question("Which Spanish politician negotiated the 'Pactos de la Moncloa'?", List("Adolfo Suárez", "Felipe González", "Santiago Carrillo", "Manuel Fraga"), "Adolfo Suárez", MCQ),
          Question("What was the name of the secret police during Franco's regime?", List("Brigada Político-Social", "Guardia Civil", "Policía Nacional", "Brigada Militar"), "Brigada Político-Social", MCQ),
          Question("Which Spanish region had its autonomous status temporarily suspended in 2017?", List("Catalonia", "Basque Country", "Galicia", "Andalusia"), "Catalonia", MCQ)
        ),
        
        // Version 2 - Specialized knowledge
        List(
          Question("What is the Spanish legal system based on?", List("Civil law", "Common law", "Religious law", "Customary law"), "Civil law", MCQ),
          Question("What percentage of Spain's land is used for agriculture?", List("Around 55%", "Around 25%", "Around 75%", "Around 35%"), "Around 55%", MCQ),
          Question("Which Spanish university is the oldest in operation?", List("University of Salamanca", "Complutense University of Madrid", "University of Barcelona", "University of Seville"), "University of Salamanca", MCQ),
          Question("What is Spain's national tree?", List("Holm oak", "Pine", "Olive", "Eucalyptus"), "Holm oak", MCQ),
          Question("Which traditional Spanish herding dog has distinctive two-colored eyes?", List("Catalonian Sheepdog", "Spanish Mastiff", "Ibizan Hound", "Andalusian Hound"), "Catalonian Sheepdog", MCQ)
        ),
        
        // Version 3 - Extremely specific cultural knowledge
        List(
          Question("Which Spanish chemist won the Nobel Prize in 1906?", List("Santiago Ramón y Cajal", "Severo Ochoa", "Antoni Gaudí", "Miguel Servet"), "Santiago Ramón y Cajal", MCQ),
          Question("What type of instrument is a 'gaita'?", List("Bagpipe", "Guitar", "Flute", "Drum"), "Bagpipe", MCQ),
          Question("In Spanish cuisine, what is 'socarrat'?", List("The crispy rice at the bottom of the paella pan", "A type of fish sauce", "Caramelized onions", "Smoked paprika"), "The crispy rice at the bottom of the paella pan", MCQ),
          Question("Which Spanish film won the Academy Award for Best Foreign Language Film in 2004?", List("The Sea Inside", "All About My Mother", "Talk to Her", "Volver"), "The Sea Inside", MCQ),
          Question("What is the meaning of the Spanish literary term 'esperpento'?", List("A grotesque distortion of reality", "A traditional form of Spanish poetry", "A type of theater performance", "A narrative technique with multiple viewpoints"), "A grotesque distortion of reality", MCQ)
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
        // Version 1 - Politics and government
        List(
          Question("How many chambers does the UK Parliament have?", List("Two", "One", "Three", "Four"), "Two", MCQ),
          Question("What is the official title of the UK's Prime Minister?", List("First Lord of the Treasury", "Chief Executive Officer", "Head of State", "President of Parliament"), "First Lord of the Treasury", MCQ),
          Question("How many constituencies are there in the UK (as of 2023)?", List("650", "500", "725", "850"), "650", MCQ),
          Question("Which of these is NOT a major political party in the UK?", List("Progressive Party", "Conservative Party", "Labour Party", "Liberal Democrats"), "Progressive Party", MCQ),
          Question("What is the Queen's (or King's) Speech?", List("The outline of the government's proposed policies", "The monarch's Christmas address", "The coronation oath", "The monarch's birthday celebration"), "The outline of the government's proposed policies", MCQ)
        ),
        
        // Version 2 - Education and culture
        List(
          Question("What are the divisions of the academic year called at Oxford and Cambridge?", List("Terms", "Semesters", "Quarters", "Sessions"), "Terms", MCQ),
          Question("What is a 'Blue' at Oxford and Cambridge universities?", List("A sports award", "An academic gown", "A final exam", "A student accommodation"), "A sports award", MCQ),
          Question("Which institution awards the Turner Prize?", List("Tate Gallery", "The British Museum", "The National Gallery", "The Royal Academy"), "Tate Gallery", MCQ),
          Question("What percentage of British people speak a second language fluently?", List("About 38%", "About 65%", "About 80%", "About 20%"), "About 38%", MCQ),
          Question("What is a 'listed building' in the UK?", List("A building of special architectural or historical interest", "A building for sale on the property market", "A government-owned building", "A building with UNESCO status"), "A building of special architectural or historical interest", MCQ)
        ),
        
        // Version 3 - Geography and science
        List(
          Question("Which English city has the highest population outside London?", List("Birmingham", "Manchester", "Liverpool", "Leeds"), "Birmingham", MCQ),
          Question("What percentage of the UK land area does England occupy?", List("About 53%", "About 65%", "About 40%", "About 75%"), "About 53%", MCQ),
          Question("Which English scientist discovered penicillin?", List("Alexander Fleming", "Isaac Newton", "Charles Darwin", "Stephen Hawking"), "Alexander Fleming", MCQ),
          Question("Which national park is the largest in England?", List("Lake District", "Peak District", "Dartmoor", "New Forest"), "Lake District", MCQ),
          Question("Which English city was the first industrialized city in the world?", List("Manchester", "Birmingham", "Liverpool", "Leeds"), "Manchester", MCQ)
        )
      )
      
      // English MCQ quizzes - Impossible difficulty (3 versions)
      case (English, Impossible, MCQ) => List(
        // Version 1 - Obscure history and traditions
        List(
          Question("What is the 'Cinque Ports' in English history?", List("A confederation of coastal towns", "A series of naval battles", "A type of fishing vessel", "A trade agreement with France"), "A confederation of coastal towns", MCQ),
          Question("What is 'Swan Upping'?", List("An annual census of swans on the River Thames", "A traditional rowing competition", "A festival celebrating spring", "A hunt for wild swans"), "An annual census of swans on the River Thames", MCQ),
          Question("Who was the last English monarch to lead troops in battle?", List("George II", "Victoria", "Henry VIII", "Elizabeth I"), "George II", MCQ),
          Question("What is a 'Yeoman Warder'?", List("A ceremonial guardian at the Tower of London", "A royal gardener", "A member of Parliament", "A town crier"), "A ceremonial guardian at the Tower of London", MCQ),
          Question("What was the 'Window Tax' in English history?", List("A property tax based on the number of windows", "A tax on glass manufacturing", "A tax on imported French windows", "A tax on housing with sea views"), "A property tax based on the number of windows", MCQ)
        ),
        
        // Version 2 - Specialized knowledge
        List(
          Question("What is the national plant of England?", List("Tudor rose", "Daffodil", "Thistle", "Shamrock"), "Tudor rose", MCQ),
          Question("What is the traditional English folk dance with bells and sticks?", List("Morris dancing", "Maypole dancing", "Square dancing", "Ballroom dancing"), "Morris dancing", MCQ),
          Question("What percentage of England's land is designated as 'Green Belt'?", List("About 13%", "About 5%", "About 25%", "About 30%"), "About 13%", MCQ),
          Question("Which English scientist discovered the law of universal gravitation?", List("Isaac Newton", "Michael Faraday", "Robert Hooke", "James Clerk Maxwell"), "Isaac Newton", MCQ),
          Question("What is 'Received Pronunciation' in English linguistics?", List("The standard accent associated with educated speakers", "The pronunciation used in TV news", "Regional dialect patterns", "The Queen's English dialect"), "The standard accent associated with educated speakers", MCQ)
        ),
        
        // Version 3 - Extremely specific cultural knowledge
        List(
          Question("What is a 'Mummers Play' in English folklore?", List("A folk play with masked characters", "A puppet show", "A children's fairy tale", "A type of musical performance"), "A folk play with masked characters", MCQ),
          Question("What ceremony takes place at the Tower of London each night?", List("The Ceremony of the Keys", "The Changing of the Guard", "The Ravens' Roll Call", "The Yeoman's Watch"), "The Ceremony of the Keys", MCQ),
          Question("Which English king introduced the Assize of Arms in 1181?", List("Henry II", "Richard I", "King John", "William I"), "Henry II", MCQ),
          Question("What is 'Plough Monday' in English traditions?", List("The first Monday after Epiphany", "The first day of spring planting", "The harvest festival", "The first Monday of the lunar year"), "The first Monday after Epiphany", MCQ),
          Question("What is the English legal concept of 'primogeniture'?", List("The right of the firstborn to inherit", "The right to a fair trial", "The monarch's divine right to rule", "The concept of innocent until proven guilty"), "The right of the firstborn to inherit", MCQ)
        )
      )

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
      
      // German MCQ quizzes - Impossible difficulty (3 versions)
      case (German, Impossible, MCQ) => List(
        // Version 1 - Obscure history and politics
        List(
          Question("Which German state was the last to join the North German Confederation?", List("Hesse-Darmstadt", "Bavaria", "Württemberg", "Baden"), "Hesse-Darmstadt", MCQ),
          Question("What was the 'Weimar Triangle'?", List("A political cooperation between Germany, France, and Poland", "A mathematical concept", "An architectural design", "A military alliance"), "A political cooperation between Germany, France, and Poland", MCQ),
          Question("Who was the first female minister in German history?", List("Elisabeth Schwarzhaupt", "Angela Merkel", "Ursula von der Leyen", "Hildegard Hamm-Brücher"), "Elisabeth Schwarzhaupt", MCQ),
          Question("Which German philosopher wrote 'Critique of Pure Reason'?", List("Immanuel Kant", "Friedrich Nietzsche", "Georg Hegel", "Karl Marx"), "Immanuel Kant", MCQ),
          Question("What was the 'Kulturnation' concept in German history?", List("Cultural unity despite political fragmentation", "State-sponsored cultural programs", "International cultural exchange", "Academic excellence initiative"), "Cultural unity despite political fragmentation", MCQ)
        ),
        
        // Version 2 - Specialized knowledge
        List(
          Question("What is 'Knappschaft' in German social history?", List("A miners' guild and insurance system", "A medieval craft guild", "A religious brotherhood", "A student association"), "A miners' guild and insurance system", MCQ),
          Question("What does the architectural term 'Plattenbau' refer to?", List("Prefabricated concrete buildings", "Medieval half-timbered houses", "Bauhaus design", "Castle architecture"), "Prefabricated concrete buildings", MCQ),
          Question("Which German physicist formulated the uncertainty principle?", List("Werner Heisenberg", "Max Planck", "Albert Einstein", "Wilhelm Röntgen"), "Werner Heisenberg", MCQ),
          Question("What percentage of German forests are mixed forests?", List("About 40%", "About 20%", "About 60%", "About 10%"), "About 40%", MCQ),
          Question("What is 'Klientelpolitik' in German political science?", List("Politics catering to specific interest groups", "Foreign policy doctrine", "Electoral system", "Political party structure"), "Politics catering to specific interest groups", MCQ)
        ),
        
        // Version 3 - Extremely specific cultural knowledge
        List(
          Question("What is 'Sehnsucht' in German philosophical and cultural context?", List("A deep yearning or longing", "A form of existential dread", "A mathematical theorem", "An architectural style"), "A deep yearning or longing", MCQ),
          Question("Which ancient Germanic tribe did NOT participate in the Germanic migrations?", List("Cimbri", "Goths", "Vandals", "Franks"), "Cimbri", MCQ),
          Question("What is 'Feierabend' in German culture?", List("Free time after work", "A type of celebration", "Weekend holiday", "Religious ceremony"), "Free time after work", MCQ),
          Question("What does the German legal term 'Drittwirkung' refer to?", List("The effect of constitutional rights on private relationships", "Triple damages in civil lawsuits", "Third-party liability", "Legal precedent system"), "The effect of constitutional rights on private relationships", MCQ),
          Question("What is the 'Nibelungentreue' concept in German cultural history?", List("Absolute loyalty even to certain doom", "A medieval inheritance system", "A musical composition technique", "A diplomatic alliance structure"), "Absolute loyalty even to certain doom", MCQ)
        )
      )
      
      // Arabic MCQ quizzes - Easy difficulty (3 versions)
      case (Arabic, Easy, MCQ) => List(
        // Version 1 - Cultural knowledge
        List(
          Question("Which city is the capital of Saudi Arabia?", List("Riyadh", "Cairo", "Dubai", "Doha"), "Riyadh", MCQ),
          Question("Which is the holiest city in Islam?", List("Mecca", "Medina", "Jerusalem", "Damascus"), "Mecca", MCQ),
          Question("Which Arab country has the largest population?", List("Egypt", "Saudi Arabia", "Morocco", "Iraq"), "Egypt", MCQ),
          Question("What is the most widely spoken dialect of Arabic?", List("Egyptian Arabic", "Levantine Arabic", "Gulf Arabic", "Maghrebi Arabic"), "Egyptian Arabic", MCQ),
          Question("Which of these is a traditional Arabic instrument?", List("Oud", "Sitar", "Violin", "Piano"), "Oud", MCQ)
        ),
        
        // Version 2 - Geography and history
        List(
          Question("Which desert covers much of the Arabian Peninsula?", List("Arabian Desert", "Sahara Desert", "Gobi Desert", "Kalahari Desert"), "Arabian Desert", MCQ),
          Question("Which Arab city is known as 'the Pearl of the Mediterranean'?", List("Alexandria", "Beirut", "Tunis", "Tripoli"), "Alexandria", MCQ),
          Question("Which body of water separates the Arabian Peninsula from Africa?", List("Red Sea", "Persian Gulf", "Mediterranean Sea", "Arabian Sea"), "Red Sea", MCQ),
          Question("Which Arab country was known as Mesopotamia in ancient times?", List("Iraq", "Syria", "Lebanon", "Jordan"), "Iraq", MCQ),
          Question("Which landmark is located in Giza, Egypt?", List("The Great Pyramids", "Petra", "Burj Khalifa", "Hagia Sophia"), "The Great Pyramids", MCQ)
        ),
        
        // Version 3 - Everyday knowledge
        List(
          Question("What is the traditional Arabic coffee called?", List("Qahwa", "Espresso", "Latte", "Mocha"), "Qahwa", MCQ),
          Question("What is the traditional greeting in Arabic?", List("As-salamu alaykum", "Marhaba", "Ahlan", "All of these are used"), "As-salamu alaykum", MCQ),
          Question("What type of bread is commonly eaten throughout the Arab world?", List("Khubz (pita)", "Baguette", "Naan", "Focaccia"), "Khubz (pita)", MCQ),
          Question("What is a 'souk'?", List("A traditional market", "A type of food", "A religious ceremony", "A musical instrument"), "A traditional market", MCQ),
          Question("What is the Arabic word for 'peace'?", List("Salam", "Harb", "Hob", "Nur"), "Salam", MCQ)
        )
      )
      
      // Arabic MCQ quizzes - Medium difficulty (3 versions)
      case (Arabic, Medium, MCQ) => List(
        // Version 1 - History and culture
        List(
          Question("During which century did Islam first emerge?", List("7th century CE", "5th century CE", "10th century CE", "3rd century CE"), "7th century CE", MCQ),
          Question("Which dynasty ruled the largest Arab empire in history?", List("Umayyad", "Abbasid", "Fatimid", "Ottoman"), "Umayyad", MCQ),
          Question("What was the 'House of Wisdom' in Baghdad?", List("A major intellectual center for science and literature", "A royal palace", "A religious school", "A government building"), "A major intellectual center for science and literature", MCQ),
          Question("Which Arab polymath is known as the 'father of modern medicine'?", List("Ibn Sina (Avicenna)", "Al-Kindi", "Ibn Rushd (Averroes)", "Al-Razi"), "Ibn Sina (Avicenna)", MCQ),
          Question("What is 'Al-Andalus'?", List("Islamic Spain", "Ancient Egypt", "Mesopotamia", "The Levant"), "Islamic Spain", MCQ)
        ),
        
        // Version 2 - Arts and literature
        List(
          Question("What are the 'Mu'allaqat' in Arabic literature?", List("Pre-Islamic suspended poems", "Religious texts", "Epic tales", "Love letters"), "Pre-Islamic suspended poems", MCQ),
          Question("Who wrote 'One Thousand and One Nights'?", List("Multiple anonymous authors", "Ibn Khaldun", "Al-Mutanabbi", "Naguib Mahfouz"), "Multiple anonymous authors", MCQ),
          Question("What is 'maqam' in Arabic music?", List("A system of melodic modes", "A type of drum", "A singing style", "A dance form"), "A system of melodic modes", MCQ),
          Question("Which art form is characterized by geometric patterns and calligraphy?", List("Islamic art", "Expressionism", "Surrealism", "Classicism"), "Islamic art", MCQ),
          Question("What is 'dabke'?", List("A traditional folk dance", "A style of poetry", "A type of food", "A religious ceremony"), "A traditional folk dance", MCQ)
        ),
        
        // Version 3 - Society and religion
        List(
          Question("What are the Five Pillars of Islam?", List("Shahada, Salat, Zakat, Sawm, Hajj", "Prayer, Fasting, Charity", "Faith, Hope, Charity", "Honesty, Piety, Charity, Faith, Prayer"), "Shahada, Salat, Zakat, Sawm, Hajj", MCQ),
          Question("What is 'wasta' in Arab society?", List("Using connections or influence", "A religious ceremony", "A traditional meal", "A marriage custom"), "Using connections or influence", MCQ),
          Question("What is 'hijab'?", List("A head covering worn by some Muslim women", "A religious holiday", "A type of prayer", "A religious text"), "A head covering worn by some Muslim women", MCQ),
          Question("What is the 'Arab League'?", List("A regional organization of Arab states", "A literary association", "A sports federation", "A trade agreement"), "A regional organization of Arab states", MCQ),
          Question("What is 'majlis' in Arab culture?", List("A gathering or council meeting", "A religious ceremony", "A type of dance", "A legal document"), "A gathering or council meeting", MCQ)
        )
      )
      
      // Arabic MCQ quizzes - Hard difficulty (3 versions)
      case (Arabic, Hard, MCQ) => List(
        // Version 1 - Language and literature
        List(
          Question("What is 'i'rab' in Arabic grammar?", List("Case endings", "Verb conjugation", "Plural formation", "Pronoun use"), "Case endings", MCQ),
          Question("Which book is considered the first Arabic dictionary?", List("Kitab al-'Ayn", "Lisan al-Arab", "Al-Qamus al-Muhit", "Al-Sihah"), "Kitab al-'Ayn", MCQ),
          Question("What is the 'hamza' in Arabic?", List("A glottal stop consonant", "A long vowel", "A diacritical mark for emphasis", "A type of rhyme"), "A glottal stop consonant", MCQ),
          Question("Who wrote 'Muqaddimah', considered one of the earliest works on social sciences?", List("Ibn Khaldun", "Al-Jahiz", "Al-Mutanabbi", "Ibn Rushd"), "Ibn Khaldun", MCQ),
          Question("What is the standard form of literary Arabic called?", List("Modern Standard Arabic (Fusha)", "Ammiyya", "Darija", "Khaliji"), "Modern Standard Arabic (Fusha)", MCQ)
        ),
        
        // Version 2 - History and politics
        List(
          Question("When did the Arab Spring begin?", List("2010", "2008", "2012", "2015"), "2010", MCQ),
          Question("Which agreement first divided the Middle East into French and British spheres of influence?", List("Sykes-Picot Agreement", "Camp David Accords", "Oslo Accords", "Treaty of Sèvres"), "Sykes-Picot Agreement", MCQ),
          Question("Which Arab country was the first to sign a peace treaty with Israel?", List("Egypt", "Jordan", "Lebanon", "UAE"), "Egypt", MCQ),
          Question("What was the Baath Party's core ideology?", List("Pan-Arab socialism", "Islamic fundamentalism", "Democratic liberalism", "Monarchism"), "Pan-Arab socialism", MCQ),
          Question("Which 20th-century Arab leader promoted the idea of Pan-Arabism?", List("Gamal Abdel Nasser", "King Faisal", "Saddam Hussein", "Yasser Arafat"), "Gamal Abdel Nasser", MCQ)
        ),
        
        // Version 3 - Religion and philosophy
        List(
          Question("What is 'ijtihad' in Islamic jurisprudence?", List("Independent reasoning", "Consensus of scholars", "Analogical deduction", "Literal interpretation"), "Independent reasoning", MCQ),
          Question("What are the main branches of Islam?", List("Sunni and Shia", "Sufi and Salafi", "Hanafi and Maliki", "Wahhabi and Alawi"), "Sunni and Shia", MCQ),
          Question("Which philosophical concept did Al-Farabi develop in Islamic philosophy?", List("The Virtuous City", "The Categorical Imperative", "The Great Chain of Being", "The Tabula Rasa"), "The Virtuous City", MCQ),
          Question("What is 'tafsir'?", List("Quranic exegesis", "Islamic law", "Spiritual purification", "Pilgrimage ritual"), "Quranic exegesis", MCQ),
          Question("What does 'jihad' primarily mean in Islamic theology?", List("Spiritual struggle", "Holy war", "Pilgrimage", "Charitable giving"), "Spiritual struggle", MCQ)
        )
      )
      
      // Arabic MCQ quizzes - Impossible difficulty (3 versions)
      case (Arabic, Impossible, MCQ) => List(
        // Version 1 - Obscure historical knowledge
        List(
          Question("Which pre-Islamic Arab queen ruled Palmyra and challenged Rome?", List("Zenobia", "Cleopatra", "Bilqis", "Kahina"), "Zenobia", MCQ),
          Question("Which Arab mathematician first used the term 'algebra'?", List("Al-Khwarizmi", "Ibn al-Haytham", "Al-Kindi", "Omar Khayyam"), "Al-Khwarizmi", MCQ),
          Question("What was the 'mihna' in Abbasid history?", List("An inquisition about religious doctrine", "A literary competition", "A tax system", "A military campaign"), "An inquisition about religious doctrine", MCQ),
          Question("Who wrote 'Kalila wa-Dimna', the Arabic adaptation of animal fables?", List("Ibn al-Muqaffa", "Al-Jahiz", "Ibn Tufail", "Al-Hariri"), "Ibn al-Muqaffa", MCQ),
          Question("Which medieval Arab scientist corrected Ptolemy's model of planetary motion?", List("Ibn al-Shatir", "Al-Battani", "Ibn Yunus", "Al-Zarqali"), "Ibn al-Shatir", MCQ)
        ),
        
        // Version 2 - Specialized linguistic knowledge
        List(
          Question("What is 'sarf' in Arabic linguistics?", List("Morphology", "Syntax", "Phonology", "Semantics"), "Morphology", MCQ),
          Question("Which Arabic dialect features the sound shift of 'q' to 'g'?", List("Egyptian", "Lebanese", "Moroccan", "Iraqi"), "Egyptian", MCQ),
          Question("What is 'sukun' in Arabic writing?", List("A mark indicating absence of a vowel", "A letter connector", "A long vowel marker", "A punctuation mark"), "A mark indicating absence of a vowel", MCQ),
          Question("What is the Arabic linguistics term for derivational morphology?", List("Ishtiqaq", "Tasrif", "Nahw", "Balaghah"), "Ishtiqaq", MCQ),
          Question("Which of these is NOT one of the six canonical hadith collections?", List("Sahih Al-Tabari", "Sahih Al-Bukhari", "Sunan Abu Dawood", "Sahih Muslim"), "Sahih Al-Tabari", MCQ)
        ),
        
        // Version 3 - Extremely specific cultural and historical knowledge
        List(
          Question("What was 'al-Andalus'?", List("Muslim-ruled Iberian Peninsula", "A type of Arabic calligraphy", "An ancient Arabic calendar", "The Arabic name for Constantinople"), "Muslim-ruled Iberian Peninsula", MCQ),
          Question("Which philosophical concept did Al-Farabi develop in Islamic philosophy?", List("The Virtuous City", "The Categorical Imperative", "The Great Chain of Being", "The Tabula Rasa"), "The Virtuous City", MCQ),
          Question("What is 'tafsir'?", List("Quranic exegesis", "Islamic law", "Spiritual purification", "Pilgrimage ritual"), "Quranic exegesis", MCQ),
          Question("What does 'jihad' primarily mean in Islamic theology?", List("Spiritual struggle", "Holy war", "Pilgrimage", "Charitable giving"), "Spiritual struggle", MCQ)
        )
      )
      

      
      // For other quiz types and language combinations, return an empty list
      case _ => List()
    }
  }
  

  
  private def getDefaultQuestionBank(language: Language, quizType: QuizType): List[Question] = {
    (language, quizType) match {
      case (Spanish, Vocabulary) =>
        List(
          Question("What is 'apple' in Spanish?", List("manzana", "plátano", "naranja", "uva"), "manzana", Vocabulary),
          Question("What is 'book' in Spanish?", List("libro", "cuaderno", "lápiz", "bolígrafo"), "libro", Vocabulary),
          Question("What is 'dog' in Spanish?", List("perro", "gato", "pájaro", "pez"), "perro", Vocabulary),
          Question("What is 'house' in Spanish?", List("casa", "apartamento", "edificio", "hotel"), "casa", Vocabulary),
          Question("What is 'car' in Spanish?", List("coche", "bicicleta", "tren", "avión"), "coche", Vocabulary)
        )

      case (French, Grammar) =>
        List(
          Question("What is the correct article to use with 'maison'?", List("le", "la", "les", "l'"), "la", Grammar),
          Question("Which is the correct form: 'Je ___ français'", List("suis", "es", "est", "sommes"), "suis", Grammar),
          Question("What is the plural of 'cheval'?", List("chevals", "chevaus", "chevaux", "chevauxs"), "chevaux", Grammar),
          Question("Which preposition follows 'aller': 'Je vais ___ Paris'", List("à", "en", "dans", "sur"), "à", Grammar),
          Question("What is the past participle of 'faire'?", List("fait", "faite", "faisant", "faire"), "fait", Grammar)
        )

      case (German, Translation) =>
        List(
          Question("Translate: 'Good morning'", List("Guten Morgen", "Guten Tag", "Guten Abend", "Gute Nacht"), "Guten Morgen", Translation),
          Question("Translate: 'How are you?'", List("Wie geht es dir?", "Wie heißt du?", "Woher kommst du?", "Was machst du?"), "Wie geht es dir?", Translation),
          Question("Translate: 'My name is...'", List("Ich heiße...", "Mein Name ist...", "Ich bin...", "A and B are correct"), "A and B are correct", Translation),
          Question("Translate: 'I don't understand'", List("Ich verstehe nicht", "Ich weiß nicht", "Ich spreche nicht", "Ich kenne nicht"), "Ich verstehe nicht", Translation),
          Question("Translate: 'Thank you very much'", List("Danke schön", "Vielen Dank", "Bitte schön", "Entschuldigung"), "Vielen Dank", Translation)
        )

      case (Arabic, MCQ) =>
        List(
          Question("Which means 'hello' in Arabic?", List("مرحبا", "شكرا", "وداعا", "نعم"), "مرحبا", MCQ),
          Question("Which word means 'yes' in Arabic?", List("نعم", "لا", "ربما", "جيد"), "نعم", MCQ),
          Question("Which phrase means 'How are you?' in Arabic?", List("كيف حالك؟", "ما اسمك؟", "من أين أنت؟", "كم عمرك؟"), "كيف حالك؟", MCQ),
          Question("Which is the correct Arabic word for 'water'?", List("ماء", "خبز", "حليب", "قهوة"), "ماء", MCQ),
          Question("What does 'شكرا' mean in English?", List("Thank you", "Please", "Excuse me", "Sorry"), "Thank you", MCQ)
        )

      case (English, Correction) =>
        List(
          Question("Correct this sentence: 'She don't like coffee.'", List("She doesn't like coffee.", "She don't likes coffee.", "She not like coffee.", "She isn't like coffee."), "She doesn't like coffee.", Correction),
          Question("Correct this sentence: 'I gone to the store yesterday.'", List("I went to the store yesterday.", "I have gone to the store yesterday.", "I was going to the store yesterday.", "I had gone to the store yesterday."), "I went to the store yesterday.", Correction),
          Question("Correct this sentence: 'He have three cars.'", List("He has three cars.", "He haves three cars.", "He having three cars.", "He had three cars."), "He has three cars.", Correction),
          Question("Correct this sentence: 'We was at the party.'", List("We were at the party.", "We are at the party.", "We is at the party.", "We be at the party."), "We were at the party.", Correction),
          Question("Correct this sentence: 'She more tall than her sister.'", List("She is taller than her sister.", "She taller than her sister.", "She is more tall than her sister.", "She is tallest than her sister."), "She is taller than her sister.", Correction)
        )

      // Default case - provide generic English questions
      case _ =>
        List(
          Question("Example question 1?", List("Option A", "Option B", "Option C", "Option D"), "Option A", quizType),
          Question("Example question 2?", List("Option A", "Option B", "Option C", "Option D"), "Option B", quizType),
          Question("Example question 3?", List("Option A", "Option B", "Option C", "Option D"), "Option C", quizType),
          Question("Example question 4?", List("Option A", "Option B", "Option C", "Option D"), "Option D", quizType),
          Question("Example question 5?", List("Option A", "Option B", "Option C", "Option D"), "Option A", quizType)
        )
    }
  }
}
