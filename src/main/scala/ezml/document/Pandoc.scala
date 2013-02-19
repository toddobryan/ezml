package ezml.document

case class Pandoc(meta: Meta, content: List[Block])

case class Meta(title: List[Inline], authors: List[List[Inline]], date: List[Inline])

sealed abstract class Alignment
case object AlignLeft extends Alignment
case object AlignRight extends Alignment
case object AlignCenter extends Alignment
case object AlignDefault extends Alignment

case class ListAttributes(num: Int, style: ListNumberStyle, delim: ListNumberDelim)

sealed abstract class ListNumberStyle
case object DefaultStyle extends ListNumberStyle
case object Example extends ListNumberStyle
case object Decimal extends ListNumberStyle
case object LowerRoman extends ListNumberStyle
case object UpperRoman extends ListNumberStyle
case object LowerAlpha extends ListNumberStyle
case object UpperAlpha extends ListNumberStyle

sealed abstract class ListNumberDelim
case object DefaultDelim extends ListNumberDelim
case object Period extends ListNumberDelim
case object OneParen extends ListNumberDelim
case object TwoParens extends ListNumberDelim

case class KeyValue(key: String, value: String) {
  override def toString = "KeyValue(%s, %s)".format(Util.repr(key), Util.repr(value))
}
case class Attr(id: String, classes: List[String], attrs: List[KeyValue]) {
  override def toString = "Attr(%s, %s, %s)".format(Util.repr(id), classes.map(Util.repr(_)), attrs)
}
object NullAttr extends Attr("", Nil, Nil)

case class TableCell(wrapped: List[Block])

case class Format(wrapped: String) {
  override def toString = "Format(%s)".format(Util.repr(wrapped))
}

case class DefnItem(item: List[Inline], defn: List[List[Block]])

sealed abstract class Block
case class Plain(content: List[Inline]) extends Block
case class Para(content: List[Inline]) extends Block
case class CodeBlock(attr: Attr, str: String) extends Block {
  override def toString = "CodeBlock(%s, %s)".format(attr, Util.repr(str))
}
case class RawBlock(format: Format, str: String) extends Block {
  override def toString = "RawBlock(%s, %s)".format(format, Util.repr(str))
}
case class BlockQuote(content: List[Block]) extends Block
case class OrderedList(attrs: ListAttributes, items: List[List[Block]]) extends Block
case class BulletList(items: List[List[Block]]) extends Block
case class DefinitionList(items: List[DefnItem]) extends Block
case class Header(level: Int, content: List[Inline]) extends Block
case object HorizontalRule extends Block
case class Table(caption: List[Inline], alignments: List[Alignment], widths: List[Double], 
		headers: List[TableCell], rows: List[List[TableCell]]) extends Block
case object EmptyBlock extends Block // Null


sealed abstract class QuoteType
case object SingleQuote extends QuoteType
case object DoubleQuote extends QuoteType

case class Target(url: String, title: String) {
  override def toString = "Target(%s, %s)".format(Util.repr(url), Util.repr(title))
}

sealed abstract class MathType
case object DisplayMath extends MathType
case object InlineMath extends MathType

sealed abstract class Inline
case class Str(str: String) extends Inline {
  override def toString = "Str(%s)".format(Util.repr(str))
}
case class Emph(content: List[Inline]) extends Inline
case class Strong(content: List[Inline]) extends Inline
case class Strikeout(content: List[Inline]) extends Inline
case class Superscript(content: List[Inline]) extends Inline
case class Subscript(content: List[Inline]) extends Inline
case class SmallCaps(content: List[Inline]) extends Inline
case class Quoted(kind: QuoteType, content: List[Inline]) extends Inline
case class Cite(citations: List[Citation], content: List[Inline]) extends Inline
case class Code(attr: Attr, content: List[Inline]) extends Inline
case object Space extends Inline
case object LineBreak extends Inline
case class Math(kind: MathType, str: String) extends Inline {
  override def toString = "Math(%s, %s)".format(kind, Util.repr(str))
}
case class RawInline(format: Format, str: String) extends Inline {
  override def toString = "RawInline(%s, %s)".format(format, Util.repr(str))
}
case class Link(linkStr: List[Inline], target: Target) extends Inline
case class Image(altStr: List[Inline], target: Target) extends Inline
case class Note(content: List[Block]) extends Inline

case class Citation(id: String, prefix: List[Inline], suffix: List[Inline], 
		mode: CitationMode, noteNum: Int, hash: Int)

sealed abstract class CitationMode
case object AuthorInText extends CitationMode 
case object SuppressAuthor extends CitationMode
case object NormalCitation extends CitationMode

object Util {
  def repr(s: String): String = {
    if (s == null) "null"
    else s.toList.map {
      case '\0' => "\\0"
      case '\t' => "\\t"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\"' => "\\\""
      case '\\' => "\\\\"
      case ch if (' ' <= ch && ch <= '\u007e') => ch.toString
      case ch => {
        val hex = Integer.toHexString(ch.toInt)
        "\\u%s%s".format("0" * (4 - hex.length), hex)
      }
    }.mkString("\"", "", "\"")
  }
}
