package com.topaz.utils

import java.security.{MessageDigest, SecureRandom}

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory
import com.topaz.TopazCodingError
import com.topaz.utils.RandomGeneratorPimps._
import org.apache.commons.math3.random.{MersenneTwister, RandomGenerator}

import scala.util.Random

object StringUtils {

  private val rng: RandomGenerator = new MersenneTwister()
  val localNewline = sys.props("line.separator")
  val localNewParagraph = localNewline + localNewline
  val linuxNewline = "\n"
  val windowsNewline = "\r\n"

  implicit class CaseInsensitiveRegex(sc: StringContext) {
    def ci = ( "(?i)" + sc.parts.mkString ).r
  }

  def removeNewLines(text: String): String = text.replaceAll(StringUtils.localNewline, " ")
  
  def replaceAnyNewLines(text: String, replaceWith: String): String = 
    text.replaceAll("\\r\\n|\\r|\\n", replaceWith)

  def centreFill(text: String, width: Int): String = {
    val rightFill = " " * ((width - text.length) / 2)
    val leftFill = " " * (width - text.length - rightFill.length)
    val res = leftFill + text + rightFill
    require (res.length == width, s"length ${res.length} != $width")
    res
  }
  
  def quote(string: String): String = s""""$string""""
  
  def unquote(string: String): String = string.replaceAll("^\"|\"$", "")

  def rightJustify(text: String, width: Int): String = {
    " " * (width - text.length) + text
  }
  def leftJustify(text: String, width: Int): String = {
    text + " " * (width - text.length) 
  }
  def maxSize(strings: Seq[String]): Int = strings.foldLeft(0)(_ max _.length)

  def isNullOrEmptyOrWhitespace(s: String): Boolean = Option(s).forall(_.trim.isEmpty)

  def notNullOrEmptyOrWhitespace(s: String): Boolean = !isNullOrEmptyOrWhitespace(s)

  /**
    * Gets the mds for `s` and returns the hex encoded string. e.g. "D41D8CD98F00B204E9800998ECF8427E"
    */
  def md5(s: String): String = {
    val md5 = MessageDigest.getInstance("MD5")
    md5.digest(s.getBytes("UTF-8")).map("%02X".format(_)).mkString
  }


  // full dictionaries at https://github.com/bmarcot/haiku
  val adjs = List("autumn", "hidden", "bitter", "misty", "silent",
    "empty", "dry", "dark", "summer", "icy", "delicate", "quiet", "white", "cool",
    "spring", "winter", "patient", "twilight", "dawn", "crimson", "wispy",
    "weathered", "blue", "billowing", "broken", "cold", "damp", "falling",
    "frosty", "green", "long", "late", "lingering", "bold", "little", "morning",
    "muddy", "old", "red", "rough", "still", "small", "sparkling", "throbbing",
    "shy", "wandering", "withered", "wild", "black", "holy", "solitary",
    "fragrant", "aged", "snowy", "proud", "floral", "restless", "divine",
    "polished", "purple", "lively", "nameless", "puffy", "fluffy",
    "calm", "young", "golden", "avenging", "ancestral", "ancient", "argent",
    "reckless", "daunting", "short", "rising", "strong", "timber", "tumbling",
    "silver", "dusty", "celestial", "cosmic", "crescent", "double", "far", "half",
    "inner", "milky", "northern", "southern", "eastern", "western", "outer",
    "terrestrial", "huge", "deep", "epic", "titanic", "mighty", "powerful")

  val nouns = List("waterfall", "river", "breeze", "moon", "rain",
    "wind", "sea", "morning", "snow", "lake", "sunset", "pine", "shadow", "leaf",
    "dawn", "glitter", "forest", "hill", "cloud", "meadow", "glade",
    "bird", "brook", "butterfly", "bush", "dew", "dust", "field",
    "flower", "firefly", "feather", "grass", "haze", "mountain", "night", "pond",
    "darkness", "snowflake", "silence", "sound", "sky", "shape", "surf",
    "thunder", "violet", "wildflower", "wave", "water", "resonance",
    "sun", "wood", "dream", "cherry", "tree", "fog", "frost", "voice", "paper",
    "frog", "smoke", "star", "sierra", "castle", "fortress", "tiger", "day",
    "sequoia", "cedar", "wrath", "blessing", "spirit", "nova", "storm", "burst",
    "protector", "drake", "dragon", "knight", "fire", "king", "jungle", "queen",
    "giant", "elemental", "throne", "game", "weed", "stone", "apogee", "bang",
    "cluster", "corona", "cosmos", "equinox", "horizon", "light", "nebula",
    "solstice", "spectrum", "universe", "magnitude", "parallax")

  def getRandElement[A](random: RandomGenerator, xs: List[A]): A = xs(random.nextInt(xs.size))

  def randomName(random: RandomGenerator = rng): String = {
    (Seq(adjs, nouns).map(getRandElement(random, _)) :+ random.nextInt(1000, 9999)).mkString("-") 
  }

  def splitOnFieldSeparator2(str: String): (String, String) = {
    str.split(FIELD_SEPARATOR) match {
      case Array(a, b) => a -> b
      case o => throw TopazCodingError(s"Expected to split '$str' into two but got ${o.length} elements.")
    }
  }

  val FIELD_SEPARATOR = 28.toChar.toString  /* Used when representing a sequence as a single string */

  // https://stackoverflow.com/questions/7539831/scala-draw-table-to-console
  private object TableFormat {
    def formatRows(rowSeparator: String, rows: Seq[String]): String = (
      rowSeparator ::
        rows.head ::
        rowSeparator ::
        rows.tail.toList :::
        rowSeparator ::
        List()).mkString("\n")

    def formatRow(row: Seq[Any], colSizes: Seq[Int]): String = {
      val cells = for ((item, size) <- row.zip(colSizes)) yield if (size == 0) "" else ("%" + size + "s").format(item)
      cells.mkString("|", "|", "|")
    }

    def rowSeparator(colSizes: Seq[Int]): String = colSizes map { "-" * _ } mkString("+", "+", "+")
  }

  def formatAsTable(table: Seq[Seq[Any]]): String = {
    import TableFormat._

    table match {
      case Seq() => ""
      case _ =>
        val sizes = for (row <- table) yield for (cell <- row) yield if (cell == null) 0 else cell.toString.length
        val colSizes = for (col <- sizes.transpose) yield col.max
        val rows = for (row <- table) yield formatRow(row, colSizes)
        formatRows(rowSeparator(colSizes), rows)
    }
  }

  def yamlToJson(yaml: String): String = {
    val mapper = new ObjectMapper(new YAMLFactory)
    val jsonNode = mapper.readTree(yaml)
    new ObjectMapper().writeValueAsString(jsonNode)
  }
  
  object SecureRandomString {
    private val rand = new SecureRandom() // SecureRandom.nextInt is thread-safe
    private val chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".toCharArray
    private val nums = "0123456789".toCharArray
    private val alphaNumeric: Array[Char] = chars ++ chars.map(_.toLower) ++ nums

    def nextString(len: Int): String = {
      val chars = new Array[Char](len)
      chars.indices.foreach {
        i =>
          chars(i) = alphaNumeric(rand.nextInt(alphaNumeric.length))
      }
      new String(chars)
    }
  }

  def ordinal(n: Int): String = {
    if (n >= 10 && n <= 20) {
      s"${n}th"
    } else {
      n % 10 match {
        case 1 => s"${n}st"
        case 2 => s"${n}nd"
        case 3 => s"${n}rd"
        case _ => s"${n}th"
      }
    }
  }
}
