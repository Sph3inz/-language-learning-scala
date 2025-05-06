package languagelearningbot.translation_quizzes

import languagelearningbot._

/**
 * Translation Quizzes
 * Centralizes access to all language-specific translation quizzes
 */
object TranslationQuizzes {
  
  /**
   * Get all translation quizzes for a specific language and difficulty level
   * 
   * @param language The language of the quizzes
   * @param difficulty The difficulty level
   * @return A list of question banks, where each bank is a list of questions
   */
  def getQuizzes(language: Language, difficulty: Difficulty): List[List[Question]] = {
    language match {
      case Spanish => SpanishTranslationQuizzes.getQuizzes(difficulty)
      case French => FrenchTranslationQuizzes.getQuizzes(difficulty)
      case English => EnglishTranslationQuizzes.getQuizzes(difficulty)
      case German => GermanTranslationQuizzes.getQuizzes(difficulty)
      case Arabic => ArabicTranslationQuizzes.getQuizzes(difficulty)
      case _ => List.empty // Fallback for any future languages
    }
  }
}
