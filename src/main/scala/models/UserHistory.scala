package models

import java.time.Instant

case class UserInteraction(
  id: String = java.util.UUID.randomUUID().toString,
  userId: String,
  timestamp: Long = System.currentTimeMillis(),
  question: String,
  response: String,
  category: String // e.g., "language_question", "quiz", "settings", etc.
)

case class QuizResult(
  id: String = java.util.UUID.randomUUID().toString,
  userId: String,
  timestamp: Long = System.currentTimeMillis(),
  quizType: String, // e.g., "vocabulary", "grammar", etc.
  score: Double,
  totalQuestions: Int,
  correctAnswers: Int,
  questions: List[QuizQuestion],
  userAnswers: List[String]
)

case class QuizQuestion(
  question: String,
  correctAnswer: String,
  userAnswer: String,
  isCorrect: Boolean
)

case class UserHistory(
  userId: String,
  username: String,
  email: String,
  interactions: List[UserInteraction] = List.empty,
  quizResults: List[QuizResult] = List.empty
) 