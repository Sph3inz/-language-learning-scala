package languagelearningbot

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}
import scala.io.Source
import scala.util.{Try, Success, Failure}

/**
 * Utility functions for file operations
 */
object FileUtils {
  /**
   * Ensures the directory exists, creating it if necessary
   */
  def ensureDirectoryExists(path: String): Boolean = {
    val dir = new File(path)
    if (!dir.exists()) {
      dir.mkdirs()
    } else if (!dir.isDirectory) {
      false // Path exists but is not a directory
    } else {
      true  // Directory already exists
    }
  }
  
  /**
   * Writes content to a file, ensuring the parent directory exists
   */
  def writeToFile(path: String, content: String): Try[Unit] = {
    Try {
      val file = new File(path)
      
      // Ensure parent directory exists
      val parentDir = file.getParentFile
      if (parentDir != null && !parentDir.exists()) {
        parentDir.mkdirs()
      }
      
      val writer = new PrintWriter(file)
      try {
        writer.write(content)
      } finally {
        writer.close()
      }
    }
  }
  
  /**
   * Reads content from a file as a string
   */
  def readFromFile(path: String): Try[String] = {
    if (!Files.exists(Paths.get(path))) {
      Failure(new Exception(s"File does not exist: $path"))
    } else {
      Try {
        val source = Source.fromFile(path)
        try {
          source.mkString
        } finally {
          source.close()
        }
      }
    }
  }
  
  /**
   * Serializes user preferences to a simple string format
   */
  def serializePreferences(prefs: UserPreferences): String = {
    // Simple format: motherLanguage|targetLanguage|name
    val motherLang = languageToString(prefs.motherLanguage)
    val targetLang = languageToString(prefs.targetLanguage)
    val name = prefs.name.getOrElse("")
    
    s"$motherLang|$targetLang|$name"
  }
  
  /**
   * Deserializes user preferences from a string
   */
  def deserializePreferences(data: String): Option[UserPreferences] = {
    val parts = data.split('|')
    if (parts.length >= 2) {
      val motherLanguage = parseLanguage(parts(0))
      val targetLanguage = parseLanguage(parts(1))
      val name = if (parts.length > 2 && parts(2).nonEmpty) Some(parts(2)) else None
      
      for {
        m <- motherLanguage
        t <- targetLanguage
      } yield UserPreferences(m, t, name)
    } else {
      None
    }
  }
  
  /**
   * Helper method to convert Language to string
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
   * Helper method to parse language string
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
