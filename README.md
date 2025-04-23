# Language Learning Bot

A rule-based chatbot application implemented in Scala to facilitate language learning through interactive conversations and quizzes.

## Project Overview

This project is a rule-based language learning chatbot that integrates functional programming principles in Scala. The bot offers:

- Interactive conversations for language learning
- Multiple quiz types (vocabulary, grammar, translation, etc.)
- Personalized feedback and progress tracking
- Analytics dashboard for monitoring learning progress

## Features

1. **Core Chatbot Module**
   - Natural language processing for understanding user queries
   - Context-aware conversations
   - Pattern matching for intent recognition
   - User preferences management

2. **Functional Quiz Generator**
   - Multiple quiz types (Vocabulary, Grammar, Translation, MCQ, Correction)
   - Adaptive difficulty levels
   - Immediate feedback on answers
   - Performance summaries

3. **Immutable Analytics Dashboard**
   - Interaction logging and analysis
   - Quiz performance tracking
   - Language proficiency assessment
   - Learning trend visualization

## Getting Started

### Prerequisites

- Java JDK 8 or higher
- Scala 2.13.x
- SBT (Scala Build Tool)

### Installation

1. Clone the repository:
   ```
   git clone <repository-url>
   cd language-learning-bot
   ```

2. Build the project:
   ```
   sbt compile
   ```

3. Run the application:
   ```
   sbt run
   ```

## Usage

After starting the application, you can interact with the bot through a command-line interface:

1. **Set your preferences**:
   ```
   set mother language English
   set target language Spanish
   set difficulty Medium
   ```

2. **Start a quiz**:
   ```
   quiz
   ```
   Then follow the prompts to select a quiz type.

3. **View your analytics**:
   ```
   analytics
   ```

4. **Get help**:
   ```
   help
   ```

5. **Exit the application**:
   ```
   exit
   ```

## Project Structure

- `Models.scala`: Domain models and data structures
- `ChatbotCore.scala`: Core chatbot functionality
- `QuizGenerator.scala`: Quiz generation and evaluation
- `AnalyticsDashboard.scala`: Analytics and logging
- `Main.scala`: Application entry point

## Functional Programming Concepts

This project demonstrates several functional programming concepts:

1. **Pattern Matching**: Used for intent recognition and input processing
2. **Immutable Data Structures**: All data is handled immutably
3. **Higher-Order Functions**: Used for analytics calculations and data transformations
4. **Pure Functions**: Ensures testability and reliability
5. **Option Types**: Used for error handling without exceptions

## License

This project is available under the MIT License.
