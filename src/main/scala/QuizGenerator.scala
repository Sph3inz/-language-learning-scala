package languagelearningbot

import scala.util.Random

// Quiz Generator using immutable data structures
object QuizGenerator {
  /**
   * Returns a list of questions based on the chosen language and quiz type
   */
  def selectQuizQuestions(language: Language, quizType: QuizType, count: Int = 5): List[Question] = {
    // Get all question banks for this language and quiz type
    val questionBanks = getQuestionBanks(language, quizType)
    
    // Randomly select one bank of questions
    val selectedBank = Random.shuffle(questionBanks).head
    
    // Randomize the order of questions within the selected bank
    Random.shuffle(selectedBank).take(count)
  }

  // Presents questions with appropriate format - always show multiple choice options
  def presentQuizQuestion(question: Question, questionNumber: Int): String = {
    s"Question $questionNumber: ${question.prompt}\n" +
    question.options.zipWithIndex.map { case (option, idx) =>
      val letter = ('a' + idx).toChar
      s"$letter) $option"
    }.mkString("\n")
  }
  
  // Evaluates a user's answer to a question - simplified for multiple choice only
  def evaluateQuizAnswer(userAnswer: String, correctAnswer: String, questionOpt: Option[Question] = None): (Boolean, String, String) = {
    // Process the user answer - trim, lowercase
    val processedAnswer = userAnswer.trim.toLowerCase
    
    // For multiple choice, handle letter answers (a, b, c, d)
    val normalizedAnswer = if (questionOpt.isDefined && processedAnswer.length == 1 && 
                             processedAnswer(0) >= 'a' && processedAnswer(0) <= 'd') {
      val optionIdx = processedAnswer(0) - 'a'
      if (questionOpt.get.options.length > optionIdx) {
        questionOpt.get.options(optionIdx).toLowerCase
      } else processedAnswer
    } else processedAnswer
    
    // Clean up the correct answer for comparison
    val cleanCorrectAnswer = correctAnswer.trim.toLowerCase
    
    // Check if answer is correct
    val isCorrect = normalizedAnswer == cleanCorrectAnswer
    
    // Generate feedback based on correctness
    val feedback = if (isCorrect) {
      "Correct! Well done!"
    } else {
      s"That's not correct. The correct answer is: $correctAnswer"
    }
    
    (isCorrect, feedback, normalizedAnswer)
  }
  
  // Generate a summary of quiz results
  def summarizeQuizResults(quizSession: QuizSession): String = {
      val score = quizSession.calculateScore
    val totalQuestions = quizSession.questions.length
    val accuracy = quizSession.calculateAccuracy.toInt
    
    val performanceFeedback = accuracy match {
      case a if a >= 90 => "Excellent work! You've mastered this material!"
      case a if a >= 80 => "Great job! You have a strong grasp of this content."
      case a if a >= 70 => "Good work! You're on the right track."
      case a if a >= 60 => "Not bad, but there's room for improvement."
      case _ => "You might want to review this material more thoroughly."
    }
    
    s"""Quiz Complete!
       |Score: $score out of $totalQuestions correct
       |Accuracy: $accuracy%
       |$performanceFeedback""".stripMargin
  }
  
  // Private method to get question banks based on language and quiz type
  private def getQuestionBanks(language: Language, quizType: QuizType): List[List[Question]] = {
    // For vocabulary quizzes
    (language, quizType) match {
      // Spanish vocabulary quizzes - Using MultipleChoice format
      case (Spanish, Vocabulary) => List(
        // Spanish vocabulary bank 1
        List(
          Question("What is 'house' in Spanish?", 
                  List("casa", "coche", "perro", "libro"), 
                  "casa", 
                  Vocabulary,
                  MultipleChoice),
          Question("What is 'dog' in Spanish?", 
                  List("gato", "perro", "pájaro", "ratón"), 
                  "perro", 
                  Vocabulary,
                  MultipleChoice),
          Question("What is 'food' in Spanish?", 
                  List("agua", "comida", "ropa", "dinero"), 
                  "comida", 
                  Vocabulary,
                  MultipleChoice),
          Question("What is 'book' in Spanish?", 
                  List("libro", "lápiz", "cuaderno", "bolígrafo"), 
                  "libro", 
                  Vocabulary,
                  MultipleChoice),
          Question("What is 'family' in Spanish?", 
                  List("amigos", "vecinos", "familia", "trabajo"), 
                  "familia", 
                  Vocabulary,
                  MultipleChoice)
        ),
        // Spanish vocabulary bank 2 (NEW)
        List(
          Question("What is 'cat' in Spanish?", 
                  List("gato", "perro", "ratón", "pájaro"), 
                  "gato", 
                  Vocabulary,
                  MultipleChoice),
          Question("What is 'school' in Spanish?", 
                  List("escuela", "trabajo", "coche", "cocina"), 
                  "escuela", 
                  Vocabulary,
                  MultipleChoice),
          Question("What is 'table' in Spanish?", 
                  List("silla", "mesa", "puerta", "ventana"), 
                  "mesa", 
                  Vocabulary,
                  MultipleChoice),
          Question("What is 'sun' in Spanish?", 
                  List("luna", "estrella", "sol", "cielo"), 
                  "sol", 
                  Vocabulary,
                  MultipleChoice),
          Question("What is 'car' in Spanish?", 
                  List("coche", "bicicleta", "avión", "barco"), 
                  "coche", 
                  Vocabulary,
                  MultipleChoice)
        )
      )
      
      // Spanish grammar quizzes - Using MultipleChoice format
      case (Spanish, Grammar) => List(
        // Spanish grammar bank
        List(
          Question("Which is the correct conjugation of 'hablar' for 'yo'?", 
                  List("hablo", "hablas", "habla", "hablan"), 
                  "hablo", 
                  Grammar,
                  MultipleChoice),
          Question("Which article matches with 'casa'?", 
                  List("el", "la", "los", "las"), 
                  "la", 
                  Grammar,
                  MultipleChoice),
          Question("What is the plural form of 'el libro'?", 
                  List("la libra", "los libro", "los libros", "las libros"), 
                  "los libros", 
                  Grammar,
                  MultipleChoice),
          Question("Which word is a verb?", 
                  List("bonito", "casa", "correr", "rápido"), 
                  "correr", 
                  Grammar,
                  MultipleChoice),
          Question("Select the correct sentence:", 
                  List("Yo come manzanas", "Tú comes manzanas", "Él comen manzanas", "Nosotros come manzanas"), 
                  "Tú comes manzanas", 
                  Grammar,
                  MultipleChoice)
        )
      )
      
      // Spanish translation quizzes - Using MultipleChoice format
      case (Spanish, Translation) => List(
        // Spanish translation bank
        List(
          Question("Translate: 'I eat breakfast every day'", 
                  List("Yo como el desayuno todos los días", "Yo comer desayuno cada día", "Yo desayuno todos los días", "Yo desayunar cada día"), 
                  "Yo como el desayuno todos los días", 
                  Translation,
                  MultipleChoice),
          Question("Translate: 'Where is the bathroom?'", 
                  List("¿Dónde es el baño?", "¿Dónde está el baño?", "¿Dónde hay el baño?", "¿Dónde el baño está?"), 
                  "¿Dónde está el baño?", 
                  Translation,
                  MultipleChoice),
          Question("Translate: 'My name is John'", 
                  List("Mi nombre es John", "Yo llamo John", "Me llamo John", "Soy llamado John"), 
                  "Me llamo John", 
                  Translation,
                  MultipleChoice),
          Question("Translate: 'I like to travel'", 
                  List("Me gusta viajar", "Yo gusto viajar", "A mí me gustas viajando", "Yo gustar a viajar"), 
                  "Me gusta viajar", 
                  Translation,
                  MultipleChoice),
          Question("Translate: 'She works at a hospital'", 
                  List("Ella trabaja en un hospital", "Ella trabajar en un hospital", "Ella está trabajando en hospital", "Ella es trabajando a un hospital"), 
                  "Ella trabaja en un hospital", 
                  Translation,
                  MultipleChoice)
        )
      )
      
      // Spanish MCQ quizzes - Keep MultipleChoice format
      case (Spanish, MCQ) => List(
        // Spanish cultural MCQ bank
        List(
          Question("Which Spanish artist painted 'Guernica'?", 
                  List("Pablo Picasso", "Salvador Dalí", "Francisco Goya", "Joan Miró"), 
                  "Pablo Picasso", 
                  MCQ,
                  MultipleChoice),
          Question("Which Spanish football club has won the most UEFA Champions League titles?", 
                  List("Real Madrid", "Barcelona", "Atlético Madrid", "Valencia"), 
                  "Real Madrid", 
                  MCQ,
                  MultipleChoice),
          Question("On which day do Spaniards traditionally eat twelve grapes at midnight?", 
                  List("New Year's Eve", "Christmas", "Three Kings Day", "Easter"), 
                  "New Year's Eve", 
                  MCQ,
                  MultipleChoice),
          Question("Which Spanish dish is made with rice and seafood?", 
                  List("Paella", "Gazpacho", "Tortilla", "Churros"), 
                  "Paella", 
                  MCQ,
                  MultipleChoice),
          Question("Which city is the capital of Spain?", 
                  List("Madrid", "Barcelona", "Sevilla", "Valencia"), 
                  "Madrid", 
                  MCQ,
                  MultipleChoice)
        )
      )
      
      // French vocabulary quizzes
      case (French, Vocabulary) => List(
        // French vocabulary bank 1
        List(
          Question("What is 'house' in French?", 
                  List("maison", "voiture", "chien", "livre"), 
                  "maison", 
                  Vocabulary,
                  MultipleChoice),
          Question("What is 'dog' in French?", 
                  List("chat", "chien", "oiseau", "souris"), 
                  "chien", 
                  Vocabulary,
                  MultipleChoice),
          Question("What is 'food' in French?", 
                  List("eau", "nourriture", "vêtements", "argent"), 
                  "nourriture", 
                  Vocabulary,
                  MultipleChoice),
          Question("What is 'book' in French?", 
                  List("livre", "stylo", "cahier", "crayon"), 
                  "livre", 
                  Vocabulary,
                  MultipleChoice),
          Question("What is 'family' in French?", 
                  List("amis", "voisins", "famille", "travail"), 
                  "famille", 
                  Vocabulary,
                  MultipleChoice)
        ),
        // French vocabulary bank 2
        List(
          Question("What is 'car' in French?", 
                  List("voiture", "vélo", "bus", "train"), 
                  "voiture", 
                  Vocabulary,
                  MultipleChoice),
          Question("What is 'friend' in French?", 
                  List("famille", "voisin", "ami", "collègue"), 
                  "ami", 
                  Vocabulary,
                  MultipleChoice),
          Question("What is 'water' in French?", 
                  List("vin", "lait", "jus", "eau"), 
                  "eau", 
                  Vocabulary,
                  MultipleChoice),
          Question("What is 'phone' in French?", 
                  List("ordinateur", "téléphone", "tablette", "radio"), 
                  "téléphone", 
                  Vocabulary,
                  MultipleChoice),
          Question("What is 'school' in French?", 
                  List("hôpital", "école", "restaurant", "magasin"), 
                  "école", 
                  Vocabulary,
                  MultipleChoice)
        ),
        // French vocabulary bank 3
        List(
          Question("What is 'time' in French?", 
                  List("heure", "jour", "mois", "année"), 
                  "heure", 
                  Vocabulary,
                  MultipleChoice),
          Question("What is 'city' in French?", 
                  List("pays", "ville", "région", "continent"), 
                  "ville", 
                  Vocabulary,
                  MultipleChoice),
          Question("What is 'weather' in French?", 
                  List("temps", "saison", "climat", "température"), 
                  "temps", 
                  Vocabulary,
                  MultipleChoice),
          Question("What is 'food' in French?", 
                  List("boisson", "repas", "nourriture", "cuisine"), 
                  "nourriture", 
                  Vocabulary,
                  MultipleChoice),
          Question("What is 'work' in French?", 
                  List("école", "bureau", "travail", "maison"), 
                  "travail", 
                  Vocabulary,
                  MultipleChoice)
        )
      )

      // French grammar quizzes
      case (French, Grammar) => List(
        // French grammar bank
        List(
          Question("Which is the correct conjugation of 'parler' for 'je'?", 
                  List("parle", "parles", "parlons", "parlez"), 
                  "parle", 
                  Grammar,
                  MultipleChoice),
          Question("Which article matches with 'maison'?", 
                  List("le", "la", "les", "l'"), 
                  "la", 
                  Grammar,
                  MultipleChoice),
          Question("What is the plural form of 'le livre'?", 
                  List("la livre", "le livres", "les livre", "les livres"), 
                  "les livres", 
                  Grammar,
                  MultipleChoice),
          Question("Which word is a verb?", 
                  List("beau", "maison", "manger", "vite"), 
                  "manger", 
                  Grammar,
                  MultipleChoice),
          Question("Select the correct sentence:", 
                  List("Je mange une pomme", "Je manges une pomme", "Je manger une pomme", "Je mangent une pomme"), 
                  "Je mange une pomme", 
                  Grammar,
                  MultipleChoice)
        )
      )

      // French translation quizzes
      case (French, Translation) => List(
        // French translation bank
        List(
          Question("Translate: 'I eat breakfast every day'", 
                  List("Je mange le petit déjeuner tous les jours", "Je manger petit déjeuner chaque jour", "Je prends le petit déjeuner tous les jours", "Je déjeuner tous les jours"), 
                  "Je mange le petit déjeuner tous les jours", 
                  Translation,
                  MultipleChoice),
          Question("Translate: 'Where is the bathroom?'", 
                  List("Où est la salle de bain?", "Où est la toilette?", "Où sont les toilettes?", "Où les toilettes sont?"), 
                  "Où sont les toilettes?", 
                  Translation,
                  MultipleChoice),
          Question("Translate: 'My name is John'", 
                  List("Mon nom est John", "Je suis John", "Je m'appelle John", "J'appelle John"), 
                  "Je m'appelle John", 
                  Translation,
                  MultipleChoice),
          Question("Translate: 'I like to travel'", 
                  List("J'aime voyager", "Je veux voyager", "Je voyage souvent", "Je préfère voyager"), 
                  "J'aime voyager", 
                  Translation,
                  MultipleChoice),
          Question("Translate: 'She works at a hospital'", 
                  List("Elle travaille dans un hôpital", "Elle travail à l'hôpital", "Elle est travaillant à un hôpital", "Elle fait travail dans hôpital"), 
                  "Elle travaille dans un hôpital", 
                  Translation,
                  MultipleChoice)
        )
      )

      // French MCQ quizzes
      case (French, MCQ) => List(
        // French cultural MCQ bank
        List(
          Question("Which French artist painted 'Water Lilies'?", 
                  List("Claude Monet", "Pierre-Auguste Renoir", "Paul Cézanne", "Edgar Degas"), 
                  "Claude Monet", 
                  MCQ,
                  MultipleChoice),
          Question("Which French writer wrote 'Les Misérables'?", 
                  List("Victor Hugo", "Albert Camus", "Gustave Flaubert", "Émile Zola"), 
                  "Victor Hugo", 
                  MCQ,
                  MultipleChoice),
          Question("What is the national holiday of France?", 
                  List("Bastille Day", "Christmas", "New Year's Day", "Easter"), 
                  "Bastille Day", 
                  MCQ,
                  MultipleChoice),
          Question("Which French cheese is known for its blue veins?", 
                  List("Roquefort", "Brie", "Camembert", "Chèvre"), 
                  "Roquefort", 
                  MCQ,
                  MultipleChoice),
          Question("Which city is the capital of France?", 
                  List("Paris", "Lyon", "Marseille", "Nice"), 
                  "Paris", 
                  MCQ,
                  MultipleChoice)
        )
      )

      // German vocabulary quizzes
      case (German, Vocabulary) => List(
        // German vocabulary bank 1
        List(
          Question("What is 'house' in German?", 
                  List("Haus", "Auto", "Hund", "Buch"), 
                  "Haus", 
                  Vocabulary,
                  MultipleChoice),
          Question("What is 'dog' in German?", 
                  List("Katze", "Hund", "Vogel", "Maus"), 
                  "Hund", 
                  Vocabulary,
                  MultipleChoice),
          Question("What is 'food' in German?", 
                  List("Wasser", "Essen", "Kleidung", "Geld"), 
                  "Essen", 
                  Vocabulary,
                  MultipleChoice),
          Question("What is 'book' in German?", 
                  List("Buch", "Stift", "Heft", "Bleistift"), 
                  "Buch", 
                  Vocabulary,
                  MultipleChoice),
          Question("What is 'family' in German?", 
                  List("Freunde", "Nachbarn", "Familie", "Arbeit"), 
                  "Familie", 
                  Vocabulary,
                  MultipleChoice)
        ),
        // German vocabulary bank 2
        List(
          Question("What is 'car' in German?", 
                  List("Auto", "Fahrrad", "Bus", "Zug"), 
                  "Auto", 
                  Vocabulary,
                  MultipleChoice),
          Question("What is 'friend' in German?", 
                  List("Familie", "Nachbar", "Freund", "Kollege"), 
                  "Freund", 
                  Vocabulary,
                  MultipleChoice),
          Question("What is 'water' in German?", 
                  List("Wein", "Milch", "Saft", "Wasser"), 
                  "Wasser", 
                  Vocabulary,
                  MultipleChoice),
          Question("What is 'phone' in German?", 
                  List("Computer", "Telefon", "Tablet", "Radio"), 
                  "Telefon", 
                  Vocabulary,
                  MultipleChoice),
          Question("What is 'school' in German?", 
                  List("Krankenhaus", "Schule", "Restaurant", "Geschäft"), 
                  "Schule", 
                  Vocabulary,
                  MultipleChoice)
        ),
        // German vocabulary bank 3
        List(
          Question("What is 'time' in German?", 
                  List("Zeit", "Tag", "Monat", "Jahr"), 
                  "Zeit", 
                  Vocabulary,
                  MultipleChoice),
          Question("What is 'city' in German?", 
                  List("Land", "Stadt", "Region", "Kontinent"), 
                  "Stadt", 
                  Vocabulary,
                  MultipleChoice),
          Question("What is 'weather' in German?", 
                  List("Wetter", "Jahreszeit", "Klima", "Temperatur"), 
                  "Wetter", 
                  Vocabulary,
                  MultipleChoice),
          Question("What is 'work' in German?", 
                  List("Schule", "Büro", "Arbeit", "Haus"), 
                  "Arbeit", 
                  Vocabulary,
                  MultipleChoice),
          Question("What is 'money' in German?", 
                  List("Zeit", "Geld", "Preis", "Konto"), 
                  "Geld", 
                  Vocabulary,
                  MultipleChoice)
        )
      )

      // German grammar quizzes
      case (German, Grammar) => List(
        // German grammar bank 1
        List(
          Question("Which is the correct conjugation of 'sprechen' for 'ich'?", 
                  List("spreche", "sprichst", "spricht", "sprechen"), 
                  "spreche", 
                  Grammar,
                  MultipleChoice),
          Question("Which article matches with 'Haus'?", 
                  List("der", "die", "das", "den"), 
                  "das", 
                  Grammar,
                  MultipleChoice),
          Question("What is the plural form of 'das Buch'?", 
                  List("das Bücher", "die Buch", "der Bücher", "die Bücher"), 
                  "die Bücher", 
                  Grammar,
                  MultipleChoice),
          Question("Which word is a verb?", 
                  List("schön", "Haus", "lesen", "schnell"), 
                  "lesen", 
                  Grammar,
                  MultipleChoice),
          Question("Select the correct sentence:", 
                  List("Ich esse ein Apfel", "Ich essen ein Apfel", "Ich isst ein Apfel", "Ich esse einen Apfel"), 
                  "Ich esse einen Apfel", 
                  Grammar,
                  MultipleChoice)
        ),
        // German grammar bank 2
        List(
          Question("Which is the correct conjugation of 'sein' for 'du'?", 
                  List("bist", "bin", "ist", "sind"), 
                  "bist", 
                  Grammar,
                  MultipleChoice),
          Question("Which article is used with 'Frau'?", 
                  List("die", "der", "das", "den"), 
                  "die", 
                  Grammar,
                  MultipleChoice),
          Question("What is the plural form of 'der Tisch'?", 
                  List("die Tische", "die Tisch", "der Tische", "das Tische"), 
                  "die Tische", 
                  Grammar,
                  MultipleChoice),
          Question("Which is the past participle of 'machen'?", 
                  List("gemacht", "machte", "macht", "machen"), 
                  "gemacht", 
                  Grammar,
                  MultipleChoice),
          Question("Select the correct sentence:", 
                  List("Er hat ein Buch gelesen", "Er hat ein Buch gelest", "Er hast ein Buch gelesen", "Er haben ein Buch gelesen"), 
                  "Er hat ein Buch gelesen", 
                  Grammar,
                  MultipleChoice)
        ),
        // German grammar bank 3
        List(
          Question("Which case is used after 'mit'?", 
                  List("Dativ", "Akkusativ", "Nominativ", "Genitiv"), 
                  "Dativ", 
                  Grammar,
                  MultipleChoice),
          Question("What is the comparative form of 'gut'?", 
                  List("besser", "guter", "mehr gut", "am gutesten"), 
                  "besser", 
                  Grammar,
                  MultipleChoice),
          Question("Which is the correct word order?", 
                  List("Ich habe gestern gegessen", "Ich gestern habe gegessen", "Ich habe gegessen gestern", "Gestern ich habe gegessen"), 
                  "Ich habe gestern gegessen", 
                  Grammar,
                  MultipleChoice),
          Question("What is the correct modal verb form?", 
                  List("Ich kann schwimmen", "Ich könne schwimmen", "Ich können schwimmen", "Ich kannt schwimmen"), 
                  "Ich kann schwimmen", 
                  Grammar,
                  MultipleChoice),
          Question("Which preposition goes with 'Schule'?", 
                  List("in die", "in der", "in dem", "in das"), 
                  "in die", 
                  Grammar,
                  MultipleChoice)
        )
      )

      // German translation quizzes
      case (German, Translation) => List(
        // German translation bank 1
        List(
          Question("Translate: 'I eat breakfast every day'", 
                  List("Ich esse Frühstück jeden Tag", "Ich essen Frühstück jeder Tag", "Ich esse das Frühstück alle Tage", "Ich frühstücke jeden Tag"), 
                  "Ich esse Frühstück jeden Tag", 
                  Translation,
                  MultipleChoice),
          Question("Translate: 'Where is the bathroom?'", 
                  List("Wo ist das Badezimmer?", "Wo ist die Toilette?", "Wo sind die Toilettes?", "Wo das Badezimmer ist?"), 
                  "Wo ist das Badezimmer?", 
                  Translation,
                  MultipleChoice),
          Question("Translate: 'My name is John'", 
                  List("Mein Name ist John", "Ich bin John", "Ich heiße John", "Ich rufe John"), 
                  "Ich heiße John", 
                  Translation,
                  MultipleChoice),
          Question("Translate: 'I like to travel'", 
                  List("Ich mag reisen", "Ich will reisen", "Ich reise gern", "Ich möchte zu reisen"), 
                  "Ich reise gern", 
                  Translation,
                  MultipleChoice),
          Question("Translate: 'She works at a hospital'", 
                  List("Sie arbeitet in einem Krankenhaus", "Sie arbeit im Krankenhaus", "Sie ist arbeitend in einem Krankenhaus", "Sie macht Arbeit im Krankenhaus"), 
                  "Sie arbeitet in einem Krankenhaus", 
                  Translation,
                  MultipleChoice)
        ),
        // German translation bank 2
        List(
          Question("Translate: 'What time is it?'", 
                  List("Wie spät ist es?", "Was ist die Zeit?", "Welche Zeit ist es?", "Wann ist es?"), 
                  "Wie spät ist es?", 
                  Translation,
                  MultipleChoice),
          Question("Translate: 'I would like a coffee'", 
                  List("Ich möchte einen Kaffee", "Ich will Kaffee", "Ich habe Kaffee", "Ich brauche Kaffee"), 
                  "Ich möchte einen Kaffee", 
                  Translation,
                  MultipleChoice),
          Question("Translate: 'How are you?'", 
                  List("Wie geht es dir?", "Wie bist du?", "Was machst du?", "Wie fühlst du?"), 
                  "Wie geht es dir?", 
                  Translation,
                  MultipleChoice),
          Question("Translate: 'I don't understand'", 
                  List("Ich verstehe nicht", "Ich weiß nicht", "Ich kann nicht", "Ich habe nicht verstanden"), 
                  "Ich verstehe nicht", 
                  Translation,
                  MultipleChoice),
          Question("Translate: 'See you tomorrow'", 
                  List("Bis morgen", "Sehe dich morgen", "Auf Wiedersehen morgen", "Tschüss bis morgen"), 
                  "Bis morgen", 
                  Translation,
                  MultipleChoice)
        ),
        // German translation bank 3
        List(
          Question("Translate: 'Can you help me?'", 
                  List("Können Sie mir helfen?", "Helfen Sie mir?", "Wollen Sie mir helfen?", "Müssen Sie mir helfen?"), 
                  "Können Sie mir helfen?", 
                  Translation,
                  MultipleChoice),
          Question("Translate: 'I have a question'", 
                  List("Ich habe eine Frage", "Ich möchte fragen", "Ich will wissen", "Ich brauche Antwort"), 
                  "Ich habe eine Frage", 
                  Translation,
                  MultipleChoice),
          Question("Translate: 'What's your name?'", 
                  List("Wie heißt du?", "Was ist dein Name?", "Wer bist du?", "Wie ist dein Name?"), 
                  "Wie heißt du?", 
                  Translation,
                  MultipleChoice),
          Question("Translate: 'I'm learning German'", 
                  List("Ich lerne Deutsch", "Ich studiere Deutsch", "Ich spreche Deutsch", "Ich kann Deutsch"), 
                  "Ich lerne Deutsch", 
                  Translation,
                  MultipleChoice),
          Question("Translate: 'Have a nice day'", 
                  List("Einen schönen Tag", "Guten Tag", "Schönes Leben", "Tag gut"), 
                  "Einen schönen Tag", 
                  Translation,
                  MultipleChoice)
        )
      )

      // German MCQ quizzes
      case (German, MCQ) => List(
        // German cultural MCQ bank
        List(
          Question("Which German composer wrote 'Symphony No. 9'?", 
                  List("Ludwig van Beethoven", "Johann Sebastian Bach", "Wolfgang Amadeus Mozart", "Richard Wagner"), 
                  "Ludwig van Beethoven", 
                  MCQ,
                  MultipleChoice),
          Question("Which German festival is celebrated in October?", 
                  List("Oktoberfest", "Christmas Market", "Carnival", "Easter"), 
                  "Oktoberfest", 
                  MCQ,
                  MultipleChoice),
          Question("What is the currency of Germany?", 
                  List("Euro", "Mark", "Pound", "Dollar"), 
                  "Euro", 
                  MCQ,
                  MultipleChoice),
          Question("Which German car manufacturer makes the 911 model?", 
                  List("Porsche", "BMW", "Mercedes-Benz", "Volkswagen"), 
                  "Porsche", 
                  MCQ,
                  MultipleChoice),
          Question("Which city is the capital of Germany?", 
                  List("Berlin", "Munich", "Hamburg", "Frankfurt"), 
                  "Berlin", 
                  MCQ,
                  MultipleChoice)
        )
      )

      // English vocabulary quizzes
      case (English, Vocabulary) => List(
        // English vocabulary bank 1
        List(
          Question("What does 'erudite' mean?", 
                  List("scholarly", "hungry", "angry", "confused"), 
                  "scholarly", 
                  Vocabulary,
                  MultipleChoice),
          Question("What does 'ameliorate' mean?", 
                  List("improve", "worsen", "remain", "describe"), 
                  "improve", 
                  Vocabulary,
                  MultipleChoice),
          Question("What does 'ubiquitous' mean?", 
                  List("found everywhere", "rare", "hidden", "difficult"), 
                  "found everywhere", 
                  Vocabulary,
                  MultipleChoice),
          Question("What does 'ephemeral' mean?", 
                  List("lasting a short time", "permanent", "important", "ancient"), 
                  "lasting a short time", 
                  Vocabulary,
                  MultipleChoice),
          Question("What does 'loquacious' mean?", 
                  List("talkative", "silent", "angry", "happy"), 
                  "talkative", 
                  Vocabulary,
                  MultipleChoice)
        ),
        // English vocabulary bank 2
        List(
          Question("What does 'pragmatic' mean?", 
                  List("practical", "theoretical", "emotional", "spiritual"), 
                  "practical", 
                  Vocabulary,
                  MultipleChoice),
          Question("What does 'benevolent' mean?", 
                  List("kind", "strict", "angry", "sad"), 
                  "kind", 
                  Vocabulary,
                  MultipleChoice),
          Question("What does 'ambiguous' mean?", 
                  List("unclear", "certain", "direct", "simple"), 
                  "unclear", 
                  Vocabulary,
                  MultipleChoice),
          Question("What does 'verbose' mean?", 
                  List("wordy", "brief", "quiet", "loud"), 
                  "wordy", 
                  Vocabulary,
                  MultipleChoice),
          Question("What does 'tenacious' mean?", 
                  List("persistent", "weak", "flexible", "careless"), 
                  "persistent", 
                  Vocabulary,
                  MultipleChoice)
        ),
        // English vocabulary bank 3
        List(
          Question("What does 'eloquent' mean?", 
                  List("well-spoken", "silent", "confused", "angry"), 
                  "well-spoken", 
                  Vocabulary,
                  MultipleChoice),
          Question("What does 'meticulous' mean?", 
                  List("careful", "careless", "quick", "lazy"), 
                  "careful", 
                  Vocabulary,
                  MultipleChoice),
          Question("What does 'enigmatic' mean?", 
                  List("mysterious", "clear", "simple", "obvious"), 
                  "mysterious", 
                  Vocabulary,
                  MultipleChoice),
          Question("What does 'diligent' mean?", 
                  List("hardworking", "lazy", "careless", "slow"), 
                  "hardworking", 
                  Vocabulary,
                  MultipleChoice),
          Question("What does 'prudent' mean?", 
                  List("wise", "foolish", "reckless", "hasty"), 
                  "wise", 
                  Vocabulary,
                  MultipleChoice)
        )
      )

      // English grammar quizzes
      case (English, Grammar) => List(
        // English grammar bank 1
        List(
          Question("Which sentence uses the correct form of 'their'?", 
                  List("They're going to their house", "Their going to they're house", "They're going to they're house", "Their going to their house"), 
                  "They're going to their house", 
                  Grammar,
                  MultipleChoice),
          Question("Which is the past participle of 'go'?", 
                  List("gone", "went", "going", "goed"), 
                  "gone", 
                  Grammar,
                  MultipleChoice),
          Question("Which sentence is in the passive voice?", 
                  List("The book was written by John", "John wrote the book", "John is writing the book", "John has written the book"), 
                  "The book was written by John", 
                  Grammar,
                  MultipleChoice),
          Question("Which is the correct relative pronoun?", 
                  List("The man who lives next door is friendly", "The man which lives next door is friendly", "The man what lives next door is friendly", "The man whom lives next door is friendly"), 
                  "The man who lives next door is friendly", 
                  Grammar,
                  MultipleChoice),
          Question("Which sentence contains a gerund?", 
                  List("Swimming is my favorite sport", "I swim every day", "I will swim tomorrow", "I have swum in the ocean"), 
                  "Swimming is my favorite sport", 
                  Grammar,
                  MultipleChoice)
        ),
        // English grammar bank 2
        List(
          Question("Which is the correct comparative form?", 
                  List("This book is more interesting than that one", "This book is interestinger than that one", "This book is more interestinger than that one", "This book is much interesting than that one"), 
                  "This book is more interesting than that one", 
                  Grammar,
                  MultipleChoice),
          Question("Which sentence uses the present perfect correctly?", 
                  List("I have lived here for 10 years", "I am living here for 10 years", "I live here for 10 years", "I had lived here for 10 years"), 
                  "I have lived here for 10 years", 
                  Grammar,
                  MultipleChoice),
          Question("Which modal verb expresses necessity?", 
                  List("must", "might", "could", "would"), 
                  "must", 
                  Grammar,
                  MultipleChoice),
          Question("Which is the correct conditional sentence?", 
                  List("If it rains, I will stay home", "If it rains, I would stay home", "If it rains, I stay home", "If it rains, I am staying home"), 
                  "If it rains, I will stay home", 
                  Grammar,
                  MultipleChoice),
          Question("Which sentence has the correct word order?", 
                  List("I always drink coffee in the morning", "I drink always coffee in the morning", "Always I drink coffee in the morning", "I drink coffee always in the morning"), 
                  "I always drink coffee in the morning", 
                  Grammar,
                  MultipleChoice)
        ),
        // English grammar bank 3
        List(
          Question("Which is the correct reported speech?", 
                  List("He said he was tired", "He said he is tired", "He said he were tired", "He said he being tired"), 
                  "He said he was tired", 
                  Grammar,
                  MultipleChoice),
          Question("Which sentence uses articles correctly?", 
                  List("I saw an elephant at the zoo", "I saw a elephant at the zoo", "I saw an elephant at a zoo", "I saw the elephant at an zoo"), 
                  "I saw an elephant at the zoo", 
                  Grammar,
                  MultipleChoice),
          Question("Which is the correct passive construction?", 
                  List("The letter will be delivered tomorrow", "The letter will delivered tomorrow", "The letter will being delivered tomorrow", "The letter will be deliver tomorrow"), 
                  "The letter will be delivered tomorrow", 
                  Grammar,
                  MultipleChoice),
          Question("Which sentence uses the past perfect correctly?", 
                  List("I had finished my work before he arrived", "I have finished my work before he arrived", "I finished my work before he had arrived", "I was finishing my work before he arrived"), 
                  "I had finished my work before he arrived", 
                  Grammar,
                  MultipleChoice),
          Question("Which is the correct question tag?", 
                  List("You like coffee, don't you?", "You like coffee, aren't you?", "You like coffee, do you?", "You like coffee, haven't you?"), 
                  "You like coffee, don't you?", 
                  Grammar,
                  MultipleChoice)
        )
      )

      // English translation quizzes
      case (English, Translation) => List(
        // English translation bank 1
        List(
          Question("What is the meaning of 'break a leg'?", 
                  List("good luck", "be careful", "move quickly", "take a rest"), 
                  "good luck", 
                  Translation,
                  MultipleChoice),
          Question("What is the meaning of 'a piece of cake'?", 
                  List("something easy", "something sweet", "something boring", "something incomplete"), 
                  "something easy", 
                  Translation,
                  MultipleChoice),
          Question("What is the meaning of 'cost an arm and a leg'?", 
                  List("very expensive", "very dangerous", "very difficult", "very complicated"), 
                  "very expensive", 
                  Translation,
                  MultipleChoice),
          Question("What is the meaning of 'hit the books'?", 
                  List("study", "become angry", "leave quickly", "fall asleep"), 
                  "study", 
                  Translation,
                  MultipleChoice),
          Question("What is the meaning of 'under the weather'?", 
                  List("feeling sick", "feeling sad", "feeling confused", "feeling excited"), 
                  "feeling sick", 
                  Translation,
                  MultipleChoice)
        ),
        // English translation bank 2
        List(
          Question("What is the meaning of 'bite off more than you can chew'?", 
                  List("take on too much", "eat too much", "speak too much", "work too much"), 
                  "take on too much", 
                  Translation,
                  MultipleChoice),
          Question("What is the meaning of 'beat around the bush'?", 
                  List("avoid the main topic", "win easily", "work hard", "waste time"), 
                  "avoid the main topic", 
                  Translation,
                  MultipleChoice),
          Question("What is the meaning of 'pull someone's leg'?", 
                  List("joke with someone", "help someone", "annoy someone", "follow someone"), 
                  "joke with someone", 
                  Translation,
                  MultipleChoice),
          Question("What is the meaning of 'once in a blue moon'?", 
                  List("very rarely", "very often", "very soon", "very late"), 
                  "very rarely", 
                  Translation,
                  MultipleChoice),
          Question("What is the meaning of 'get your act together'?", 
                  List("organize yourself", "perform well", "work harder", "be creative"), 
                  "organize yourself", 
                  Translation,
                  MultipleChoice)
        ),
        // English translation bank 3
        List(
          Question("What is the meaning of 'barking up the wrong tree'?", 
                  List("looking in the wrong place", "making too much noise", "causing trouble", "wasting time"), 
                  "looking in the wrong place", 
                  Translation,
                  MultipleChoice),
          Question("What is the meaning of 'let the cat out of the bag'?", 
                  List("reveal a secret", "create confusion", "solve a problem", "make a mistake"), 
                  "reveal a secret", 
                  Translation,
                  MultipleChoice),
          Question("What is the meaning of 'kill two birds with one stone'?", 
                  List("solve two problems at once", "be very efficient", "work very hard", "make difficult choices"), 
                  "solve two problems at once", 
                  Translation,
                  MultipleChoice),
          Question("What is the meaning of 'cut corners'?", 
                  List("do something poorly to save time", "make sharp turns", "be very precise", "work efficiently"), 
                  "do something poorly to save time", 
                  Translation,
                  MultipleChoice),
          Question("What is the meaning of 'break the ice'?", 
                  List("start a conversation", "solve a problem", "end a relationship", "create trouble"), 
                  "start a conversation", 
                  Translation,
                  MultipleChoice)
        )
      )

      // English MCQ quizzes
      case (English, MCQ) => List(
        // English cultural MCQ bank
        List(
          Question("Who wrote 'Romeo and Juliet'?", 
                  List("William Shakespeare", "Charles Dickens", "Jane Austen", "Mark Twain"), 
                  "William Shakespeare", 
                  MCQ,
                  MultipleChoice),
          Question("What is the capital of the United Kingdom?", 
                  List("London", "Paris", "Berlin", "Madrid"), 
                  "London", 
                  MCQ,
                  MultipleChoice),
          Question("Which of these is a traditional English breakfast item?", 
                  List("Baked beans", "Croissant", "Waffles", "Tacos"), 
                  "Baked beans", 
                  MCQ,
                  MultipleChoice),
          Question("Which famous band originated from Liverpool?", 
                  List("The Beatles", "The Rolling Stones", "Queen", "Led Zeppelin"), 
                  "The Beatles", 
                  MCQ,
                  MultipleChoice),
          Question("What is the national sport of England?", 
                  List("Cricket", "Football", "Rugby", "Tennis"), 
                  "Cricket", 
                  MCQ,
                  MultipleChoice)
        )
      )
      
      // Default case - now we have covered all combinations, but keep this just in case
      case _ => List(
        // Generic quiz questions about language learning
        List(
          Question("Which of these is NOT one of the four main language skills?", 
                  List("Reading", "Writing", "Speaking", "Memorizing"), 
                  "Memorizing", 
                  Vocabulary,
                  MultipleChoice),
          Question("What is the most spoken language in the world by number of native speakers?", 
                  List("Mandarin Chinese", "English", "Spanish", "Hindi"), 
                  "Mandarin Chinese", 
                  MCQ,
                  MultipleChoice),
          Question("Which language family includes English, German and Dutch?", 
                  List("Germanic", "Romance", "Slavic", "Celtic"), 
                  "Germanic", 
                  MCQ,
                  MultipleChoice),
          Question("What is the study of sentence structure called?", 
                  List("Syntax", "Phonetics", "Morphology", "Semantics"), 
                  "Syntax", 
                  Grammar,
                  MultipleChoice),
          Question("What do we call a word that sounds the same as another but has a different meaning?", 
                  List("Homophone", "Synonym", "Antonym", "Homonym"), 
                  "Homophone", 
                  Vocabulary,
                  MultipleChoice)
        )
      )
    }
  }
}
