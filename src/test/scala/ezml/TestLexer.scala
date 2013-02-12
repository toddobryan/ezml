package ezml

import org.scalatest.FunSuite
import scala.io.Source
import scala.util.parsing.input.CharSequenceReader
import document._

class TestLexer extends FunSuite {
  val lexer = new EzmlLexer()
  import lexer._
  
  def tokens(input: String): List[Token] = 
    lexer.tokens(new CharSequenceReader(input)).get
  
  test("lexify") {
    assert(tokens("[[ ]]") === List(L_BRACKET, SPACE(1), R_BRACKET))
    assert(tokens("[/]") === List(BREAK))
    assert(tokens("[! Text !]") ===
             List(L_HEADER(1), SPACE(1), TEXT("Text"), SPACE(1), R_HEADER(1)))
    assert(tokens("[!! Text !!]") ===
             List(L_HEADER(2), SPACE(1), TEXT("Text"), SPACE(1), R_HEADER(2)))
    //TODO: rest of the header types
    assert(tokens("[*bold*]") === List(L_TAG("*"), TEXT("bold"), R_TAG("*")))
    assert(tokens("[* bold *]") === List(L_TAG("*"), SPACE(1), TEXT("bold"), SPACE(1), R_TAG("*")))
    assert(tokens("[/italic/]") === List(L_TAG("/"), TEXT("italic"), R_TAG("/")))
    assert(tokens("[=monospace=]") === List(L_TAG("="), TEXT("monospace"), R_TAG("=")))
    
  }
  
  test("entities") {
    assert(tokens("['a]['e]['i]['o]['u]['y]['A]['E]['I]['O]['U]['Y]") ===
      List(ENTITY("'a"), ENTITY("'e"), ENTITY("'i"), ENTITY("'o"), ENTITY("'u"), ENTITY("'y"),
           ENTITY("'A"), ENTITY("'E"), ENTITY("'I"), ENTITY("'O"), ENTITY("'U"), ENTITY("'Y")))
   //TODO: rest of the entities
  }
  
  //test("files") {
  //  import Examples.fileContents
  //  assert(tokens(fileContents("/blockquotes.ezml")))
  //}
}

class TestParser extends FunSuite {
  val parser = new EzmlParser()
  
  test("paragraphs") {
    assert(parser.parse(Examples.pars).get.toString === 
      Pandoc(Meta(List(),List(),List()),
        List(
          Para(List(Str("These"), Space, Str("are"), Space, Str("some"), Space, Str("short"), Space, Str("paragraphs."))), 
          Para(List(Str("Hopefully,"), Space, Str("we"), Space, Str("think."))), 
          Para(List(Str("Maybe."))))).toString)
    assert(parser.parse(Examples.hs).get.toString ===
      Pandoc(Meta(List(),List(),List()),
        List(
          Header(1,List(Str("Header1"))), 
          Para(List(Str("A"), Space, Str("short"), Space, Str("paragraph."))), 
          Header(2,List(Str("Header2"))), 
          Para(List(Str("Another"), Space, Str("short"), Space, Str("paragraph."))))).toString)
  }
}

object Examples {
  def fileContents(filename: String): String = {
    Source.fromInputStream(getClass.getResourceAsStream(filename)).mkString
  }
  
  val pars = 
"""These are some short paragraphs.

Hopefully, we think.

Maybe."""
    
  val hs =
"""[! Header1 !]
    
A short paragraph.
    
[!! Header2 !!]
    
Another short paragraph."""

  val e1 = 
"""[! Syntax !]

EZML is designed to be easy to remember and use. Every formatting command
consists of a bracket followed by a character designed to remind you of what
it means. For example, headers are important! So the header above is written
by enclosing the title in exclamation points with brackets--[=[[! Syntax !]]=].
The bracket lets you know that the next character is a formatting character and
the exclamation point should remind you of a header, because headers are
important! After you've typed the header, you use the same formatting character
and a close bracket to let EZML know you're done.

All the other formatting characters are similar. If you're formatting text, you
use an open-bracket, a formatting character, type the text, same formatting
character, and a close bracket. Since the formatting characters have been
chosen to be easy to remember, you should be able to figure them out without a
lot of trouble."""
    
  val e2 =
"""[!![@chars@] Special Characters !!]

One of the strengths of HTML is that it's designed to be used by people all
over the world and it has been able to handle international character sets from
its beginning. Unfortunately, it has sometimes been difficult to type those
characters, but with EZML it's--you guessed it--easy.

To type an accented character, you use brackets followed by a character that
should remind you of the accent. For an acute accent, we use the apostrophe: '.
For a grave accent, we use the grave accent at the top left of the keyboard: `.
A [,c] is created by typing [=[[,c]]=]. It's that simple. There are a few characters
that aren't really accented, but are a combination of two or more characters. Examples
include the [sz] from German, the [ae] and [oe] ligatures, or the ellipsis [...]
character. As you might expect, there are codes to handle these cases, too.
Here's a collection of some of the characters you might need to type:"""
    
}
