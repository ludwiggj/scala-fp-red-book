package fpinscala.parsing.take3

import language.{higherKinds, postfixOps}

// NOTE: JSON parser, exercise 9.9

trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  // TODO - Re-read actual answer, possibly improve above based on it (especially wrt forward references)

  // Answer hints:
  // For the tokens of your grammar, it's often a good idea to skip any trailing whitespace, to avoid having to deal
  // with whitespace everywhere in your grammar. Try introducing a combinator for this.
  //
  // When sequencing parsers with `**`, it's common to want to ignore one of the parsers in the sequence, and you'll
  // probably want to introduce combinators for this.
  def jsonParser[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[JSON] = {
    import P._

    val letterOrSpace = "[a-zA-Z]".r // Could extend to permit any other characters allowed in string

    val digit = "[0-9]".r
    val point = "\\.".r
    val quote = "\"".r
    val openSquareBracket = "\\[".r
    val closeSquareBracket = "]".r
    val comma = ",".r
    val openCurlyBracket = "\\{".r
    val closeCurlyBracket = "}".r
    val colon = ":".r

    // TODO Not sure why unbiasL cannot be overloaded in this way

    //    def unbiasL[A, B, C](p: ((A, B), C)): (A, B, C) = (p._1._1, p._1._2, p._2)
    //
    //    def unbiasL[A, B, C, D](p: (((A, B), C), D)): (A, B, C, D) = {
    //      val first = p._1
    //      (first._1._1, first._1._2, first._2, p._2)
    //    }

    def unbiasL3[A, B, C](p: ((A, B), C)): (A, B, C) = (p._1._1, p._1._2, p._2)

    def unbiasL4[A, B, C, D](p: (((A, B), C), D)): (A, B, C, D) = (p._1._1._1, p._1._1._2, p._1._2, p._2)

    // (1) String parser
    val stringParser: Parser[String] =
      (quote ignoreLeadingWhitespace) ** slice(letterOrSpace) ** quote map unbiasL3 map (_._2)

    val jsonParserString: Parser[JSON] = slice(stringParser) map (s => JString(s))

    // (2) Number parser
    val digitsParser: Parser[String] = slice(digit)

    // Could also handle 8.38e9
    val numbersParser: Parser[String] =
      (digitsParser | slice(digitsParser ** point ** digitsParser)) ignoreLeadingWhitespace

    val jsonParserNumber: Parser[JSON] = numbersParser map (s => JNumber(s.toDouble))

    // (3) Boolean parser
    val booleanParser = ("true" | "TRUE" | "false" | "FALSE") ignoreLeadingWhitespace

    val jsonParserBoolean: Parser[JSON] = booleanParser map (s => JBool(s.toBoolean))

    // (4) JSON parser
    // TODO Issue with forward references
    val jsonParserJSON: Parser[JSON] =
    jsonParserString | jsonParserNumber | jsonParserBoolean // | jsonParserArray | jsonParserJObject

    // (5) Array parser
    val commaParser: Parser[String] = comma ignoreLeadingWhitespace

    def listParser[A](startSymbolParser: Parser[String],
                      internalElementParser: Parser[A],
                      elementParser: Parser[A],
                      closeSymbolParser: Parser[String]): Parser[List[A]] =
      startSymbolParser ** many(internalElementParser) ** elementParser ** closeSymbolParser map unbiasL4 map (m => m._2 :+ m._3)

    val arrayElementParser: Parser[JSON] = jsonParserJSON ** commaParser map (_._1)

    val openSquareBracketParser: Parser[String] = openSquareBracket ignoreLeadingWhitespace

    val closeSquareBracketParser: Parser[String] = closeSquareBracket ignoreLeadingWhitespace

    val arrayParser: Parser[List[JSON]] =
      listParser(openSquareBracketParser, arrayElementParser, jsonParserJSON, closeSquareBracketParser)

    val jsonParserArray: Parser[JSON] = arrayParser map (l => JArray(l.toIndexedSeq))

    // (6) JObject parser
    val openCurlyBracketParser: Parser[String] = openCurlyBracket ignoreLeadingWhitespace

    val closeCurlyBracketParser: Parser[String] = closeCurlyBracket ignoreLeadingWhitespace

    val colonParser: Parser[String] = colon ignoreLeadingWhitespace

    val mapEntryParser: Parser[(String, JSON)] =
      stringParser ** colonParser ** jsonParserJSON map unbiasL3 map (m => (m._1, m._3))

    val internalMapEntryParser: Parser[(String, JSON)] = mapEntryParser ** commaParser map (_._1)

    val jObjectParser: Parser[List[(String, JSON)]] =
      listParser(openCurlyBracketParser, internalMapEntryParser, mapEntryParser, closeCurlyBracketParser)

    val jsonParserJObject: Parser[JSON] = jObjectParser map (l => JObject(l.toMap))

    jsonParserJObject
  }
}
