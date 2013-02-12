

import scala.util.parsing.combinator.token.Tokens
import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.TokenParsers
import scala.util.parsing.input.CharSequenceReader

trait TestTokens extends Tokens {
  case class VOWEL(s: String) extends Token {
    def chars = s
  }
  
}

class TestLexer extends Lexical with RegexParsers with TestTokens {
  override type Elem = Char
  type Tokens = TestTokens
  
  import util.parsing.input.CharSequenceReader.EofCh
  
  override val whiteSpace = "".r
  def whitespace = success()
  
  def token: Parser[Token] = (
    elem(EofCh) ^^^ EOF |
    "[aeiou]".r ^^ (s => VOWEL(s))
  )
}

class TestParser extends TokenParsers with PackratParsers {
  override type Tokens = TestTokens
  val lexical = new TestLexer()
  override type Elem = lexical.Token
  
  def parse(input: String) = phrase(doc)(new PackratReader(new lexical.Scanner(input)))
  
  lazy val doc: PackratParser[Int] = vowel.+ ~ lexical.EOF ^^ { case vs ~ eof => vs.length }
  
  lazy val vowel: PackratParser[String] = elem("VOWEL", _.isInstanceOf[lexical.VOWEL]) ^^ (_.chars)
}