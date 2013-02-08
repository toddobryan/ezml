package ezml

import util.parsing.combinator.RegexParsers
import io.Source
import util.parsing.combinator.token.Tokens
import util.matching.Regex.Match
import util.parsing.combinator.syntactical.TokenParsers
import util.parsing.combinator.lexical.{Scanners, Lexical}
import util.parsing.input.{NoPosition, Reader}

trait EzmlTokens extends Tokens {
  val entities =
    List("""['"][aeiouyAEIOUY]""",
         """[`^][aeiouAEIOU]""",
         """~[anoANO]""",
         """,[cC]""",
         """\^[\!\?]""",
         """ae""",
         """AE""",
         """oe""",
         """OE""",
         """sz""",
         """\.\.\.""",
         """o[aA]""",
         """/[oO]""").map("(?:%s)".format(_)).mkString("|")

  val listStarts =
    List("""#""",
         """\d+""",
         """[a-zA-Z]""",
         RomanNumberUtils.regexString,
         RomanNumberUtils.regexString.toLowerCase).map("(?:%s)".format(_)).mkString("|")

  case object L_BRACKET extends Token {
    def chars = "["
  }
  case object R_BRACKET extends Token {
    def chars = "]"
  }
  case object BREAK extends Token {
    def chars ="[/]"
  }
  case class L_HEADER(level: Int) extends Token {
    def chars = "[" + "!" * level
  }

  case class R_HEADER(level: Int) extends Token {
    def chars = "!" * level + "]"
  }

  case object NEWLINE extends Token {
    def chars = "\n"
  }

  case class QUOTE_MARK(spaces: Int) extends Token {
    def chars = ">" + (" " * spaces)
  }

  case class TABLE(s: String) extends Token {
    def chars = s
  }
  
  case class DASH(num: Int) extends Token {
    def chars = "-" * num
  }

  case class SPACE(num: Int) extends Token {
    def chars = " " * num
  }

  case object UNORDERED_BULLET extends Token {
    def chars = "*"
  }

  case class NUMBERED_BULLET(s: String) extends Token {
    def chars = s
  }

  case class ENTITY(s: String) extends Token {
    def chars = s
  }

  case class L_TAG(s: String) extends Token {
    def chars = s
  }

  case class R_TAG(s: String) extends Token {
    def chars = s
  }

  case class TEXT(s: String) extends Token {
    def chars = s
  }

  case class MY_EOF() extends Token {
    def chars = ""
  }
}


object EzmlLexer extends Lexical with RegexParsers with EzmlTokens {
  override type Elem = Char
  type Tokens = EzmlTokens
  val TAB_WIDTH = 4

  import util.parsing.input.CharSequenceReader.EofCh

  def tokens: Parser[List[Token]] = (token.+)~eof ^^ { case tl~_ => tl :+ MY_EOF() }

  def eof: Parser[Token] = EofCh ^^^ MY_EOF()
  
  override val whiteSpace = "".r

  def whitespace = success()

  def token: Parser[Token] = (
    //TODO: need to deal with tabs that aren't at the beginning of a tab-stop?
    "[ \t]+".r ^^ (s => SPACE(s.replace("\t", " " * TAB_WIDTH).length))
      | "[[" ^^^ L_BRACKET
      | "]]" ^^^ R_BRACKET
      | "[/]" ^^^ BREAK
      | """\[(%s)\]""".format(entities).r ^^ (s => ENTITY(s.substring(1, s.length - 1)))
      | """\[!{1,6}""".r ^^ (s => L_HEADER(s.length - 1))
      | """!{1,6}\](?!\])""".r ^^ (s => R_HEADER(s.length - 1))
      | """\[[\*=/^_8@#\(\{]""".r ^^ (s => L_TAG(s.substring(1)))
      | """[\*=/^_8@#\)\}]\](?!\])""".r ^^ (s => R_TAG(s.substring(0, s.length - 1)))
      | """>[ \t]*""".r ^^ (s => QUOTE_MARK(s.replace("\t", " " * TAB_WIDTH).length - 1))
      | """-+""".r ^^ (s => DASH(s.length))
      | """\*\s*""".r ^^^ UNORDERED_BULLET
      | """((%s)\.)\s*""".format(listStarts).r ^^ (s => NUMBERED_BULLET(s))
      | """\r\n|\n|\r""".r ^^^ NEWLINE
      | """([^\[\] \n\-](?!(?:!{1,6}|[\*=/^_8@#\)\}])\]))*[^\[\] \n\-]""".r ^^ (s => TEXT(s))
  )
    
  def Scanner(input: String) = new Scanner(input)
  def Scanner(input: Reader[Char]) = new Scanner(input)
  
  def tokens(input: String): List[Token] = 
    Stream.iterate(EzmlLexer.Scanner(input))(_.rest).takeWhile(!_.atEnd).map(_.first).toList

  
//  def Scanner(input: String): Reader[Token] = {
//    class TokenReader(val theTokens: List[Token]) extends Reader[Token] {
//      def atEnd = theTokens.isEmpty
//      def first = {
//        if (theTokens.isEmpty) {
//          MY_EOF()
//        } else {
//          //println(theTokens.head)
//          theTokens.head
//        }
//      }
//      def pos = NoPosition
//      def rest = {
//        if (theTokens.isEmpty) {
//          new TokenReader(Nil)
//        } else {
//          new TokenReader(theTokens.tail)
//        }
//      }
//
//    }
//    val allTheTokens = parseAll(tokens, input).get
//    new TokenReader(allTheTokens)
//  }
}

/*

object EzmlSyntax {
  abstract class TextFragment
  case class TaggedText(kind: String, contents: List[TextFragment]) extends TextFragment
  case class PlainText(contents: String) extends TextFragment

  abstract class EzmlExpr
  case class Document(contents: List[EzmlExpr]) extends EzmlExpr

  abstract class Block extends EzmlExpr
  case class Paragraph(contents: List[ParBlock]) extends Block
  case class HeaderBlock(level: Int, contents: List[EzmlParser.lexical.Token]) extends Block

  abstract class ParBlock extends EzmlExpr
  case class PreBlock(lines: List[Line]) extends ParBlock
  case class TextBlock(lines: List[TextLine]) extends ParBlock
  case class QuoteBlock(lines: List[QuoteLine]) extends ParBlock

  abstract class ListBlock extends ParBlock
  case class UnorderedListBlock(items: List[UnorderedListItem]) extends ListBlock
  case class NumberedListBlock(items: List[NumberedListItem]) extends ListBlock

  abstract class ListItem extends EzmlExpr
  case class UnorderedListItem(lines: List[Line]) extends ListItem
  case class NumberedListItem(number: String, lines: List[Line]) extends ListItem

  abstract class Line extends EzmlExpr
  case class TextLine(tokens: List[EzmlParser.lexical.Token]) extends Line
  case class IndentedLine(spaces: Int, tokens: List[EzmlParser.lexical.Token]) extends Line
  case class QuoteLine(tokens: List[EzmlParser.lexical.Token]) extends Line
  case class NumberedBulletLine(bullet: String, space: Int, tokens: List[EzmlParser.lexical.Token]) extends Line
  case class EmptyLine() extends Line

  def tagType(leftKind: String): String = {
    leftKind match {
      case "=" => "code"
      case "*" => "strong"
      case "/" => "em"
      case "^" => "sup"
      case "_" => "sub"
    }
  }
}

object EzmlParser extends TokenParsers {
  type Tokens = EzmlTokens
  import EzmlSyntax._
  val lexical = new EzmlLexical

  def TokenParser[T](name: String, cond: (lexical.Token => Boolean) = ((e: lexical.Token) => e.isInstanceOf[T])): Parser[T] = {
    elem(name, cond) ^^
      ((e: lexical.Token) => e.asInstanceOf[T])
  }

  val TAB = TokenParser[lexical.SPACE]("TAB",
    ((e: lexical.Token) => e.isInstanceOf[lexical.SPACE] &&
      e.asInstanceOf[lexical.SPACE].num >= lexical.TAB_WIDTH))
  val SPACE = TokenParser[lexical.SPACE]("SPACE")
  val ENTITY = TokenParser[lexical.ENTITY]("ENTITY")
  val UNORDERED_BULLET = TokenParser[lexical.UNORDERED_BULLET]("UNORDERED_BULLET")
  val NUMBERED_BULLET = TokenParser[lexical.NUMBERED_BULLET]("NUMBERED_BULLET")
  val QUOTE_MARK = TokenParser[lexical.QUOTE_MARK]("QUOTE_MARK")
  val NEWLINE = TokenParser[lexical.NEWLINE]("NEWLINE")
  val TEXT = TokenParser[lexical.TEXT]("TEXT")
  val L_TAG = TokenParser[lexical.L_TAG]("L_TAG")
  val R_TAG = TokenParser[lexical.R_TAG]("R_TAG")
  def L_HEADER(level: Int) = TokenParser[lexical.L_HEADER]("L_HEADER",
    ((e: lexical.Token) => e.isInstanceOf[lexical.L_HEADER] &&
      e.asInstanceOf[lexical.L_HEADER].level == level))
  def R_HEADER(level: Int) = TokenParser[lexical.R_HEADER]("R_HEADER",
    ((e: lexical.Token) => e.isInstanceOf[lexical.R_HEADER] &&
      e.asInstanceOf[lexical.R_HEADER].level == level))
  val MY_EOF = TokenParser[lexical.MY_EOF]("MY_EOF")

  def doc: Parser[Document] = rep1(header)<~eof ^^ { case blocks => Document(blocks) }

  def block: Parser[List[Block]] = (
    rep1sep(paragraph, emptyLine) ^^ { case paragraphs => paragraphs }
    | rep1(rep(emptyLine)~>header<~rep(emptyLine)) ^^ { case headers => headers }
  )

  def header: Parser[HeaderBlock] = (
    // TODO: I should be able to abstract this
    L_HEADER(1)~>rep(inlineToken)<~R_HEADER(1)<~(NEWLINE|eof) ^^ {
      case tokens => {
        println(HeaderBlock(1, tokens))
        HeaderBlock(1, tokens)
      }
    }
    | L_HEADER(2)~>rep(inlineToken)<~R_HEADER(2)<~(NEWLINE|eof) ^^ {
      case tokens => {
        println(HeaderBlock(2, tokens))
        HeaderBlock(2, tokens)
      }
    }
    | L_HEADER(3)~>rep(inlineToken)<~R_HEADER(3)<~(NEWLINE|eof) ^^ {
      case tokens => {
        println(HeaderBlock(3, tokens))
        HeaderBlock(3, tokens)
      }
    }
    | L_HEADER(4)~>rep(inlineToken)<~R_HEADER(4)<~(NEWLINE|eof) ^^ {
      case tokens => {
        println(HeaderBlock(4, tokens))
        HeaderBlock(4, tokens)
      }
    }
    | L_HEADER(5)~>rep(inlineToken)<~R_HEADER(5)<~(NEWLINE|eof) ^^ {
      case tokens => {
        println(HeaderBlock(5, tokens))
        HeaderBlock(5, tokens)
      }
    }
    | L_HEADER(6)~>rep(inlineToken)<~R_HEADER(6)<~(NEWLINE|eof) ^^ {
      case tokens => {
        println(HeaderBlock(6, tokens))
        HeaderBlock(6, tokens)
      }
    })

  def paragraph: Parser[Paragraph] = rep1(parBlock) ^^ { case blocks => Paragraph(blocks) }

  def parBlock: Parser[ParBlock] = (
    listBlock
    | preBlock
    | quoteBlock
    | textBlock
  )

  def listBlock: Parser[ListBlock] = (
    numberedListBlock
    | unorderedListBlock
  )

  def numberedListBlock: Parser[NumberedListBlock] =
    rep1(numberedListItem) ^^ { case items => NumberedListBlock(items) }

  def numberedListItem: Parser[NumberedListItem] =
    numberedBulletLine~listSuffix ^^ {
      case NumberedBulletLine(number, spaces, tokens)~rest =>
        NumberedListItem(number, IndentedLine(spaces, tokens) :: rest) }

  def numberedBulletLine: Parser[NumberedBulletLine] =
    SPACE~NUMBERED_BULLET~rep(inlineToken)<~(NEWLINE|eof) ^^ {
      case lexical.SPACE(x)~lexical.NUMBERED_BULLET(s)~tokens => NumberedBulletLine(s, x, tokens)
    }

  def unorderedListBlock: Parser[UnorderedListBlock] =
    rep1(unorderedlistItem) ^^ { case items => UnorderedListBlock(items) }

  def unorderedlistItem: Parser[UnorderedListItem] =
    unorderedBulletLine~listSuffix ^^ { case first~rest => UnorderedListItem(first :: rest) }

  def unorderedBulletLine: Parser[IndentedLine] =
    SPACE~UNORDERED_BULLET~rep(inlineToken)<~(NEWLINE|eof) ^^ {
      case lexical.SPACE(x)~lexical.UNORDERED_BULLET()~tokens => IndentedLine(x, tokens)
    }

  def listSuffix: Parser[List[Line]] =
    rep(rep(emptyLine)~listLine ^^ { case lines~line => lines :+ line }) ^^ {
      case listOfLineLists => listOfLineLists.flatten
    }

  def listLine: Parser[Line] =
    SPACE~rep(inlineToken)<~(NEWLINE|eof) ^^ {
      case lexical.SPACE(x)~tokens => IndentedLine(x, tokens)
    }

  def textBlock: Parser[TextBlock] =
    rep1(textLine) ^^ { case lines => {
      println(TextBlock(lines))
      TextBlock(lines)
    } }

  def textLine: Parser[TextLine] =
    textToken~rep(inlineToken)~(NEWLINE|eof) ^^ { case token~tokens~_ => {
      println(TextLine(token :: tokens))
      TextLine(token :: tokens)
    } }

  def preBlock: Parser[PreBlock] = (
    tabbedLine~preBlockSuffix ^^ { case first~rest => PreBlock(first :: rest) }
    | tabbedLine ^^ { case line => PreBlock(List(line)) }
  )

  def preBlockSuffix: Parser[List[Line]] =
    rep1(rep(emptyLine)~tabbedLine ^^ { case lines~line => lines :+ line } ) ^^
      { case listOfLineLists => listOfLineLists.flatten }

  def quoteBlock: Parser[QuoteBlock] =
    rep1(quoteLine) ^^ { case lines => QuoteBlock(lines) }

  def quoteLine: Parser[QuoteLine] =
    QUOTE_MARK~>rep(inlineToken)<~(NEWLINE|eof) ^^ { case tokens => QuoteLine(tokens) }

  def tabbedLine: Parser[IndentedLine] =
    TAB~rep1(inlineToken)~(NEWLINE|eof) ^^ { case lexical.SPACE(x)~tokens~_ => {
      println(IndentedLine(x, tokens))
      IndentedLine(x, tokens)
    } }

  def emptyLine: Parser[EmptyLine] =
    opt(SPACE)~NEWLINE ^^^ {
      println(EmptyLine())
      EmptyLine()
    }

  /*def matches(leftTag: String, rightTag: String): Boolean = {
    leftTag match {
      case "(" => rightTag == ")"
      case "{" => rightTag == "}"
      case x => rightTag == x
    }
  }

  def paragraph: Parser[Paragraph] =
    rep1(block)~paragraphBreak ^^ { case blocks~end => {
      println(blocks)
      Paragraph(blocks)
    } }

  def block: Parser[Block] = (
    preBlock
    | textBlock
  )

  def preBlock: Parser[PreBlock] = (
    preLine~rep(preLine|emptyLine)~preLine ^^ {
      case first~middle~last => PreBlock(first :: (middle :+ last))
    }
    |
    preLine ^^ { case line => PreBlock(List(line)) }
  )

  def preLine: Parser[IndentedLine] =
    TAB~rep(inlineToken)~(NEWLINE|MY_EOF) ^^ { case lexical.SPACE(x)~tokens~_ => IndentedLine(x, tokens)}

  def emptyLine: Parser[IndentedLine] =
    opt(SPACE)~(NEWLINE|MY_EOF) ^^^ IndentedLine(0, Nil)

  def textBlock: Parser[TextBlock] = rep1(fragment) ^^ { case fragments => TextBlock(fragments) }

  def paragraphBreak: Parser[EzmlExpr] = ((NEWLINE~emptyLine)|MY_EOF) ^^^ null

  def fragment: Parser[TextFragment] = (
    L_TAG~rep(fragment)~R_TAG ^^ {
      case lexical.L_TAG(leftKind)~fragments~lexical.R_TAG(rightKind) if matches(leftKind, rightKind) =>
        TaggedText(tagType(leftKind), fragments) }
    | token
  )*/

  def inlineToken: Parser[lexical.Token] = (
    spaceToken
    | textToken
  )

  def spaceToken: Parser[lexical.Token] = SPACE

  def textToken: Parser[lexical.Token] = (
    L_TAG
    | R_TAG
    | TEXT
    | ENTITY
    | NUMBERED_BULLET
    | UNORDERED_BULLET
    | QUOTE_MARK
  )

  /*def token: Parser[TextFragment] = (
    SPACE ^^ { case lexical.SPACE(i) => PlainText("(" + i.toString + " spaces)") }
    | NEWLINE~not(TAB|emptyLine) ^^ { case lexical.NEWLINE()~_ => PlainText("\\n") }
    | TEXT ^^ { case lexical.TEXT(s) => PlainText(s) }
  )*/

  def eof: Parser[Any] = MY_EOF

  def parse[T](parseFun: Parser[T], s: String): T = {
    val scanner = lexical.Scanner(s)
    parseFun(scanner) match {
      case Success(expr, _) => expr
      case Failure(msg, _) => throw new Exception("parse failed: " + msg)
      case Error(msg, _) => throw new Exception("parse error: " + msg)
    }
  }
}
*/