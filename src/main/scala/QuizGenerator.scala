package languagelearningbot

import scala.util.Random

object QuizGenerator {
  // Required quiz generator functions as specified in the requirements
  
  /**
   * Returns a list of questions based on the chosen language, difficulty, and quiz type
   */
  def selectQuizQuestions(language: Language, difficulty: Difficulty, quizType: QuizType, count: Int = 5): List[Question] = {
    // In a real implementation, this would pull from a database or file
    // For now, we'll use sample data per language
    val questionBank = getQuestionBank(language, quizType)
    
    // Filter by difficulty if needed and select random questions
    Random.shuffle(questionBank).take(count)
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
   */
  def evaluateQuizAnswer(userAnswer: String, correctAnswer: String): (Boolean, String) = {
    // Use pattern matching for answer evaluation
    (userAnswer.toLowerCase, correctAnswer.toLowerCase) match {
      case (u, c) if u == c => 
        (true, "Correct! Well done!")
      
      case (u, c) if u.trim.nonEmpty && c.startsWith(u) =>
        (false, s"Close, but not quite. The correct answer is: $correctAnswer")
      
      case ("", _) =>
        (false, "You didn't provide an answer. The correct answer is: " + correctAnswer)
      
      case _ =>
        (false, s"That's not correct. The correct answer is: $correctAnswer")
    }
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
  private def getQuestionBank(language: Language, quizType: QuizType): List[Question] = {
    (language, quizType) match {
      case (Spanish, Vocabulary) =>
        List(
          Question("What is 'apple' in Spanish?", List("manzana", "plátano", "naranja", "uva"), "manzana", Vocabulary),
          Question("What is 'book' in Spanish?", List("libro", "cuaderno", "lápiz", "bolígrafo"), "libro", Vocabulary),
          Question("What is 'dog' in Spanish?", List("perro", "gato", "pájaro", "pez"), "perro", Vocabulary),
          Question("What is 'house' in Spanish?", List("casa", "apartamento", "edificio", "oficina"), "casa", Vocabulary),
          Question("What is 'water' in Spanish?", List("agua", "leche", "jugo", "vino"), "agua", Vocabulary)
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
