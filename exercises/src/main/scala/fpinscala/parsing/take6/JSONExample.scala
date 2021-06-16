package fpinscala.parsing.take6

import fpinscala.parsing.take6.MyParser.Parser

object JSONExample extends App {
  val jsonTxt = """
{
  "Company name" : "Microsoft Corporation",
  "Ticker"  : "MSFT",
  "Active"  : true,
  "Price"   : 30.66,
  "Year Listed" : 1985,
  "Shares outstanding" : 8380000000,
  "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
}
"""

  val malformedJson1 = """
{
  "Company name" ; "Microsoft Corporation"
}
"""

  val malformedJson2 = """
[
  [ "HPQ", "IBM",
  "YHOO", "DELL" ++
  "GOOG"
  ]
]
"""

  val P = Parser
  val json: Parser[JSON] = JSON.jsonParser(P)

  println(P.run(json)(Location(jsonTxt)))
  println("--")
  println(P.run(json)(Location(malformedJson1)))
  println("--")
  println(P.run(json)(Location(malformedJson2)))
}