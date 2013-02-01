package ezml

object RomanNumberUtils {
  private val PAIRS: Seq[(Int, String)] =
    List((1000, "M"), (900, "CM"), (500, "D"), (400, "CD"), (100, "C"),
         (90, "XC"), (50, "L"), (40, "XL"), (10, "X"),
         (9, "IX"), (5, "V"), (4, "IV"), (1, "I"))

  private val THOUSANDS = "M{0,3}"
  private val HUNDREDS = "CM|CD|D?C{0,3}"
  private val TENS = "XC|XL|L?X{0,3}"
  private val ONES = "IX|IV|V?I{0,3}"
  val legalNonemptyPatterns =
    List("(M{0,2}|C)M",
         "M{0,3}C?D",
         "M{0,3}D?(C{0,2}|X)C",
         "M{0,3}(CM|CD|D?C{0,3})X?L",
         "M{0,3}(CM|CD|D?C{0,3})(L?X{0,2}|I)X",
         "M{0,3}(CM|CD|D?C{0,3})L?X{0,3}I?V",
         "M{0,3}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})V?I{0,2}I")
  val regexString = legalNonemptyPatterns.map("(?:%s)".format(_)).mkString("|")

  def pattern(capture: Boolean = true): String = {
    val prefix = if (capture) "" else "?:"
    "(%s%s)(%s%s)(%s%s)(%s%s)".format(prefix, THOUSANDS, prefix, HUNDREDS, prefix, TENS, prefix, ONES)
  }

  def arabicToRoman(arabic: Int): Option[String] = {
    if (arabic > 3999 || arabic < 1) {
      None
    } else {
      val roman = new StringBuilder()
      var tempArabic = arabic
      for ((num, str) <- PAIRS) {
        while (tempArabic >= num) {
          roman ++= str
          tempArabic = tempArabic - num
        }
      }
      Some(roman.toString)
    }
  }

  def romanToArabic(roman: String): Option[Int] = {
    val RomanPatt = pattern().r
    roman.trim.toUpperCase match {
      case RomanPatt(thou, hun, ten, one) => Some(thouVal(thou) + hunVal(hun) + tenVal(ten) + oneVal(one))
      case _ => None
    }
  }

  private def thouVal(thou: String): Int = 1000 * thou.length
  private def romanConcat(one: String, five: String, ten: String, multiplier: Int): (String => Int) = {
    (str: String) => {
      str match {
        case nine if (nine == one + ten) => 9 * multiplier
        case four if (four == one + five) => 4 * multiplier
        case other => if (other startsWith five) {
          (5 + other.length - 1) * multiplier
        } else {
          other.length * multiplier
        }
      }
    }
  }
  private def hunVal = romanConcat("C", "D", "M", 100)
  private def tenVal = romanConcat("X", "L", "C", 10)
  private def oneVal = romanConcat("I", "V", "X", 1)
}