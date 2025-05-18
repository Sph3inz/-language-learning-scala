package languagelearningbot

// ADTs and Case Classes for Domain Modeling
// Using immutable data structures as required

// Language enumeration
sealed trait Language
case object German extends Language
case object English extends Language
case object French extends Language 
case object Spanish extends Language

// Quiz Type enumeration
sealed trait QuizType
case object Complete extends QuizType
case object Grammar extends QuizType
case object Vocabulary extends QuizType 
case object MCQ extends QuizType
case object Correction extends QuizType
case object ScenarioBased extends QuizType
case object Translation extends QuizType

// Question format enumeration
sealed trait QuestionFormat
case object MultipleChoice extends QuestionFormat
case object DirectInput extends QuestionFormat
case object FillInBlank extends QuestionFormat

// User preferences - immutable structure
case class UserPreferences(
  motherLanguage: Language,
  targetLanguage: Language,
  name: Option[String] = None
)

// Quiz Question model - immutable
case class Question(
  prompt: String,
  options: List[String],
  correctAnswer: String,
  category: QuizType,
  format: QuestionFormat = MultipleChoice  // Default to MultipleChoice for backward compatibility
)

// Quiz session - immutable record of a quiz
case class QuizSession(
  questions: List[Question],
  userAnswers: List[String] = List.empty,
  quizType: QuizType
) {
  // Pure function to calculate score
  def calculateScore: Int = {
    if (userAnswers.isEmpty) 0
    else {
      val correctAnswers = questions.zip(userAnswers).count {
        case (question, answer) => question.correctAnswer.toLowerCase == answer.toLowerCase
      }
      correctAnswers
    }
  }
  
  // Pure function to calculate accuracy percentage
  def calculateAccuracy: Double = {
    if (userAnswers.isEmpty || questions.isEmpty) 0.0
    else calculateScore.toDouble / questions.size * 100
  }
}

// Models for Analytics
case class InteractionLog(
  id: Int,
  userInput: String,
  chatbotResponse: String,
  timestamp: Long
)

case class QuizLog(
  id: Int,
  quizType: QuizType,
  questions: List[Question],
  userAnswers: List[String],
  score: Int,
  timestamp: Long
)
