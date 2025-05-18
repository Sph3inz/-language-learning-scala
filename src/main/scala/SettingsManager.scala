package languagelearningbot

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}
import scala.io.Source
import scala.util.{Try, Success, Failure}

/**
 * Class to manage persistent settings storage
 * Using immutable data structures and functional error handling with Try
 */
class SettingsManager {
  // Settings directory and file paths
  private val SETTINGS_DIR = "settings"
  private val MOTHER_LANG_FILE = s"$SETTINGS_DIR/mother_language.txt"
  private val TARGET_LANG_FILE = s"$SETTINGS_DIR/target_language.txt"
  
  // Create settings directory if it doesn't exist
  {
    val dir = new File(SETTINGS_DIR)
    if (!dir.exists()) {
      dir.mkdirs()
    }
  }
  
  /**
   * Save mother language preference to file
   */
  def saveMotherLanguage(language: Language): Try[Unit] = {
    Try {
      val writer = new PrintWriter(new File(MOTHER_LANG_FILE))
      writer.write(languageToString(language))
      writer.close()
    }
  }
  
  /**
   * Save target language preference to file
   */
  def saveTargetLanguage(language: Language): Try[Unit] = {
    Try {
      val writer = new PrintWriter(new File(TARGET_LANG_FILE))
      writer.write(languageToString(language))
      writer.close()
    }
  }
  
  /**
   * Save all preferences at once - useful for bulk operations
   */
  def saveAllPreferences(preferences: UserPreferences): Try[Unit] = {
    for {
      _ <- saveMotherLanguage(preferences.motherLanguage)
      _ <- saveTargetLanguage(preferences.targetLanguage)
    } yield ()
  }
  
  /**
   * Load mother language from file
   */
  def loadMotherLanguage(): Try[Option[Language]] = {
    if (Files.exists(Paths.get(MOTHER_LANG_FILE))) {
      Try {
        val source = Source.fromFile(MOTHER_LANG_FILE)
        val content = source.mkString.trim
        source.close()
        parseLanguage(content)
      }
    } else {
      Success(None) // No file exists yet
    }
  }
  
  /**
   * Load target language from file
   */
  def loadTargetLanguage(): Try[Option[Language]] = {
    if (Files.exists(Paths.get(TARGET_LANG_FILE))) {
      Try {
        val source = Source.fromFile(TARGET_LANG_FILE)
        val content = source.mkString.trim
        source.close()
        parseLanguage(content)
      }
    } else {
      Success(None) // No file exists yet
    }
  }
  
  /**
   * Load all preferences from files
   */
  def loadAllPreferences(): Try[Option[UserPreferences]] = {
    val motherLangResult = loadMotherLanguage()
    val targetLangResult = loadTargetLanguage()
    
    if (motherLangResult.isSuccess && targetLangResult.isSuccess) {
      val motherLang = motherLangResult.getOrElse(None)
      val targetLang = targetLangResult.getOrElse(None)
      
      if (motherLang.isDefined && targetLang.isDefined) {
        Success(Some(UserPreferences(motherLang.get, targetLang.get, None)))
      } else {
        Success(None)
      }
    } else {
      // If any load failed, propagate the failure
      val failure = motherLangResult.isFailure match {
        case true => motherLangResult
        case false => targetLangResult
      }
      Failure(failure.failed.get)
    }
  }
  
  /**
   * Helper method to convert Language to a string representation
   */
  private def languageToString(language: Language): String = {
    language match {
      case English => "English"
      case Spanish => "Spanish"
      case French => "French"
      case German => "German"
      case _ => "Unknown"
    }
  }
  
  /**
   * Parse language string to Language type
   */
  private def parseLanguage(lang: String): Option[Language] = {
    lang.trim match {
      case "English" => Some(English)
      case "Spanish" => Some(Spanish)
      case "French" => Some(French)
      case "German" => Some(German)
      case _ => None
    }
  }
} 