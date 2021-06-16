package fpinscala.parsing.take6

// Taken from take 3, and adapted to work with the take 6 implementation

import language.{higherKinds, postfixOps}

trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON {
    override def toString: String =
      s"JObject(\n${(get map { case (k, v) => s"    $k -> $v" }).mkString(",\n")}\n  ),"
  }

  // TODO - Re-read actual answer, possibly improve above based on it (especially wrt forward references)

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    // TODO BUGFIX - Changed it to 1 or more (hmm, relationship between slice and many?)
    val lettersAndSpaces = "[a-zA-Z ]+".r // Could extend to permit any other characters allowed in string

    // TODO BUGFIX - Changed it to 1 or more (hmm, relationship between slice and many?)
    val digits = "[0-9]+".r

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
      (quote ignoreLeadingWhitespace) ** slice(lettersAndSpaces) ** quote map unbiasL3 map (_._2)

    val jsonParserString: Parser[JSON] = slice(stringParser) map (s => JString(s))

    // (2) Number parser
    val digitsParser: Parser[String] = slice(digits)

    // Could also handle 8.38e9
    val numbersParser: Parser[String] = {
      // Changed order over so that 30.66 is matched before 30
      (slice(digitsParser ** point ** digitsParser) | digitsParser) ignoreLeadingWhitespace
    }

    val jsonParserNumber: Parser[JSON] = numbersParser map (s => JNumber(s.toDouble))

    // (3) Boolean parser
    val booleanParser = ("true" | "TRUE" | "false" | "FALSE") ignoreLeadingWhitespace

    val jsonParserBoolean: Parser[JSON] = booleanParser map (s => JBool(s.toBoolean))

    // (4) JSON parser
    // TODO Issue with forward references
    val jsonParserJSON: Parser[JSON] =
    attempt(jsonParserString | jsonParserNumber | jsonParserBoolean) // | jsonParserArray | jsonParserJObject

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
    // TODO Issue with forward references
    val jsonParserJSON2: Parser[JSON] =
      attempt(jsonParserString | jsonParserNumber | jsonParserBoolean | jsonParserArray) // | jsonParserJObject

    val openCurlyBracketParser: Parser[String] = openCurlyBracket ignoreLeadingWhitespace

    val closeCurlyBracketParser: Parser[String] = closeCurlyBracket ignoreLeadingWhitespace

    val colonParser: Parser[String] = colon ignoreLeadingWhitespace

    val mapEntryParser: Parser[(String, JSON)] =
      stringParser ** colonParser ** jsonParserJSON2 map unbiasL3 map (m => (m._1, m._3))

    val internalMapEntryParser: Parser[(String, JSON)] = mapEntryParser ** commaParser map (_._1)

    val jObjectParser: Parser[List[(String, JSON)]] =
      listParser(openCurlyBracketParser, internalMapEntryParser, mapEntryParser, closeCurlyBracketParser)

    val jsonParserJObject: Parser[JSON] = jObjectParser map (l => JObject(l.toMap))

    jsonParserJObject
  }
}