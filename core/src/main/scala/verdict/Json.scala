package verdict

/** Just enough JSON to store a rule.
  *
  * Hand-written because the core module has no dependencies, and because the
  * document shape here is small and fixed: six kinds of value, no streaming, no
  * configuration.
  */
enum Json:
  case Null
  case Bool(value: Boolean)
  case Num(value: BigDecimal)
  case Str(value: String)
  case Arr(values: List[Json])
  case Obj(fields: List[(String, Json)])

  def render: String = this match
    case Null       => "null"
    case Bool(v)    => v.toString
    case Num(v)     => v.bigDecimal.stripTrailingZeros.toPlainString
    case Str(v)     => Json.quote(v)
    case Arr(vs)    => vs.map(_.render).mkString("[", ",", "]")
    case Obj(fs)    => fs.map((k, v) => s"${Json.quote(k)}:${v.render}").mkString("{", ",", "}")

  def field(name: String): Option[Json] = this match
    case Obj(fields) => fields.collectFirst { case (key, value) if key == name => value }
    case _           => None

object Json:

  final class Malformed(val reason: String) extends RuntimeException(reason)

  def parse(input: String): Either[String, Json] =
    val parser = Parser(input)
    try
      val value = parser.value()
      parser.skipWhitespace()
      if !parser.atEnd then Left(s"unexpected content after the document at offset ${parser.offset}")
      else Right(value)
    catch case malformed: Malformed => Left(malformed.reason)

  def quote(raw: String): String =
    val out = new StringBuilder("\"")
    raw.foreach {
      case '"'                     => out ++= "\\\""
      case '\\'                    => out ++= "\\\\"
      case '\b'                    => out ++= "\\b"
      case '\f'                    => out ++= "\\f"
      case '\n'                    => out ++= "\\n"
      case '\r'                    => out ++= "\\r"
      case '\t'                    => out ++= "\\t"
      case c if c.toInt < 0x20     => out ++= f"\\u${c.toInt}%04x"
      case c                       => out += c
    }
    (out += '"').toString

  private final class Parser(input: String):
    var offset = 0

    def atEnd: Boolean = offset >= input.length

    private def fail(what: String): Nothing =
      throw Malformed(s"$what at offset $offset")

    private def peek: Char = if atEnd then fail("unexpected end of input") else input(offset)

    private def expect(c: Char): Unit =
      if atEnd || input(offset) != c then fail(s"expected '$c'") else offset += 1

    def skipWhitespace(): Unit =
      while !atEnd && (input(offset) match { case ' ' | '\t' | '\n' | '\r' => true; case _ => false }) do offset += 1

    def value(): Json =
      skipWhitespace()
      peek match
        case '{'                   => obj()
        case '['                   => arr()
        case '"'                   => Json.Str(string())
        case 't'                   => literal("true", Json.Bool(true))
        case 'f'                   => literal("false", Json.Bool(false))
        case 'n'                   => literal("null", Json.Null)
        case c if c == '-' || c.isDigit => Json.Num(number())
        case _                     => fail("expected a JSON value")

    private def literal(text: String, result: Json): Json =
      if input.startsWith(text, offset) then
        offset += text.length
        result
      else fail(s"expected $text")

    private def obj(): Json =
      expect('{')
      skipWhitespace()
      if peek == '}' then
        offset += 1
        Json.Obj(Nil)
      else
        val fields = List.newBuilder[(String, Json)]
        var more   = true
        while more do
          skipWhitespace()
          val key = string()
          skipWhitespace()
          expect(':')
          fields += (key -> value())
          skipWhitespace()
          peek match
            case ',' => offset += 1
            case '}' => offset += 1; more = false
            case _   => fail("expected ',' or '}'")
        Json.Obj(fields.result())

    private def arr(): Json =
      expect('[')
      skipWhitespace()
      if peek == ']' then
        offset += 1
        Json.Arr(Nil)
      else
        val values = List.newBuilder[Json]
        var more   = true
        while more do
          values += value()
          skipWhitespace()
          peek match
            case ',' => offset += 1
            case ']' => offset += 1; more = false
            case _   => fail("expected ',' or ']'")
        Json.Arr(values.result())

    private def string(): String =
      expect('"')
      val out = new StringBuilder
      var done = false
      while !done do
        if atEnd then fail("unterminated string")
        input(offset) match
          case '"'  => offset += 1; done = true
          case '\\' =>
            offset += 1
            if atEnd then fail("unterminated escape")
            val escaped = input(offset)
            offset += 1
            escaped match
              case '"'  => out += '"'
              case '\\' => out += '\\'
              case '/'  => out += '/'
              case 'b'  => out += '\b'
              case 'f'  => out += '\f'
              case 'n'  => out += '\n'
              case 'r'  => out += '\r'
              case 't'  => out += '\t'
              case 'u' =>
                if offset + 4 > input.length then fail("truncated unicode escape")
                val hex = input.substring(offset, offset + 4)
                offset += 4
                try out += Integer.parseInt(hex, 16).toChar
                catch case _: NumberFormatException => fail(s"'$hex' is not a unicode escape")
              case other => fail(s"'\\$other' is not an escape")
          case c =>
            offset += 1
            out += c
      out.toString

    private def number(): BigDecimal =
      val start = offset
      if !atEnd && input(offset) == '-' then offset += 1
      while !atEnd && input(offset).isDigit do offset += 1
      if !atEnd && input(offset) == '.' then
        offset += 1
        while !atEnd && input(offset).isDigit do offset += 1
      if !atEnd && (input(offset) == 'e' || input(offset) == 'E') then
        offset += 1
        if !atEnd && (input(offset) == '+' || input(offset) == '-') then offset += 1
        while !atEnd && input(offset).isDigit do offset += 1
      val text = input.substring(start, offset)
      try BigDecimal(text)
      catch case _: NumberFormatException => fail(s"'$text' is not a number")
