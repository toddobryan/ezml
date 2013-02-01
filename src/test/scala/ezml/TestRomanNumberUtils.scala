package ezml

import RomanNumberUtils.{romanToArabic, arabicToRoman}
import org.scalatest.FunSuite

class TestRomanNumberUtils extends FunSuite {
  test("convert Roman to Arabic") {
    assert(romanToArabic("i") === Some(1))
    assert(romanToArabic("V") === Some(5))
    assert(romanToArabic("X") === Some(10))
    assert(romanToArabic("l") === Some(50))
    assert(romanToArabic("C") === Some(100))
    assert(romanToArabic("d") === Some(500))
    assert(romanToArabic("M") === Some(1000))
    assert(romanToArabic("ii") === Some(2))
    assert(romanToArabic("iv") === Some(4))
    assert(romanToArabic("XIII") === Some(13))
    assert(romanToArabic("xiv") === Some(14))
    assert(romanToArabic("XVIII") === Some(18))
    assert(romanToArabic("lxxix") === Some(79))
    assert(romanToArabic("cccxliv") === Some(344))
    assert(romanToArabic("CDL") === Some(450))
    assert(romanToArabic("MCMLXXXIX") === Some(1989))
    assert(romanToArabic("cmxcix") === Some(999))
    assert(romanToArabic("mmmxv") === Some(3015))
    assert(romanToArabic("iiii") === None)
    assert(romanToArabic("roman") === None)
    assert(romanToArabic("mmmcccddd") === None)
    assert(romanToArabic("CCCCX") === None)
    assert(romanToArabic("VV") === None)
  }

  test("convert Arabic to Roman") {
    assert(arabicToRoman(1) === Some("I"))
    assert(arabicToRoman(3) === Some("III"))
    assert(arabicToRoman(4) === Some("IV"))
    assert(arabicToRoman(7) === Some("VII"))
    assert(arabicToRoman(18) === Some("XVIII"))
    assert(arabicToRoman(239) === Some("CCXXXIX"))
    assert(arabicToRoman(492) === Some("CDXCII"))
    assert(arabicToRoman(3999) === Some("MMMCMXCIX"))
    assert(arabicToRoman(1444) === Some("MCDXLIV"))
    assert(arabicToRoman(2581) === Some("MMDLXXXI"))
    assert(arabicToRoman(0) === None)
    assert(arabicToRoman(4000) === None)
  }
}