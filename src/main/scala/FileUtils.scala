package languagelearningbot

import java.io.{File, PrintWriter, FileWriter, BufferedWriter}
import java.nio.file.{Files, Paths}
import scala.io.Source
import scala.util.{Try, Success, Failure}

/**
 * Utility class for handling file operations
 */
object FileUtils {
  
  val SETTINGS_FILE = "user_preferences.txt"
  
  /**
   * Save user preferences to file
   */
  def savePreferences(preferences: UserPreferences): Try[Unit] = {
    Try {
      // Convert preferences to a string representation
      val prefsString = serializePreferences(preferences)
      
      // Write to file
      val writer = new BufferedWriter(new FileWriter(SETTINGS_FILE))
      writer.write(prefsString)
      writer.close()
    }
  }
  
  /**
   * Load user preferences from file
   */
  def loadPreferences(): Try[Option[UserPreferences]] = {
    if (Files.exists(Paths.get(SETTINGS_FILE))) {
      Try {
        val source = Source.fromFile(SETTINGS_FILE)
        val content = source.getLines().mkString("\n")
        source.close()
        
        deserializePreferences(content)
      }
    } else {
      Success(None) // File doesn't exist yet
    }
  }
  
  /**
   * Convert preferences to string format for file storage
   */
  private def serializePreferences(prefs: UserPreferences): String = {
    // Simple format: motherLanguage|targetLanguage|difficulty|name
    val motherLang = languageToString(prefs.motherLanguage)
    val targetLang = languageToString(prefs.targetLanguage)
    val diff = difficultyToString(prefs.difficulty)
    val name = prefs.name.getOrElse("")
    
    s"$motherLang|$targetLang|$diff|$name"
  }
  
  /**
   * Parse preferences string back into UserPreferences object
   */
  private def deserializePreferences(prefsString: String): Option[UserPreferences] = {
    val parts = prefsString.split("\\|")
    
    if (parts.length >= 3) {
      val motherLang = parseLanguage(parts(0))
      val targetLang = parseLanguage(parts(1))
      val difficulty = parseDifficulty(parts(2))
      
      val name = if (parts.length > 3 && parts(3).nonEmpty) Some(parts(3)) else None
      
      for {
        ml <- motherLang
        tl <- targetLang
        d <- difficulty
      } yield UserPreferences(ml, tl, d, name)
    } else {
      None
    }
  }
  
  // Helper methods to convert between strings and enum values
  private def languageToString(language: Language): String = {
    language match {
      case English => "English"
      case Spanish => "Spanish"
      case French => "French"
      case German => "German"
      case Arabic => "Arabic"
      case _ => "Unknown"
    }
  }
  
  private def parseLanguage(lang: String): Option[Language] = {
    lang.toLowerCase match {
      case "english" => Some(English)
      case "spanish" => Some(Spanish)
      case "french" => Some(French)
      case "german" => Some(German)
      case "arabic" => Some(Arabic)
      case _ => None
    }
  }
  
  private def difficultyToString(difficulty: Difficulty): String = {
    difficulty match {
      case Easy => "Easy"
      case Medium => "Medium"
      case Hard => "Hard"
      case Impossible => "Impossible"
      case _ => "Unknown"
    }
  }
  
  private def parseDifficulty(diff: String): Option[Difficulty] = {
    diff.toLowerCase match {
      case "easy" => Some(Easy)
      case "medium" => Some(Medium)
      case "hard" => Some(Hard)
      case "impossible" => Some(Impossible)
      case _ => None
    }
  }
}
