package fpinscala.parsing

import language.higherKinds
import language.implicitConversions
import please_refactor.rockpaperscissors.TournamentConfigParser.unpack

trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    // we'll hide the string implicit conversion and promote strings to tokens instead
    // this is a bit nicer than having to write token everywhere
    import P.{string => _, _}
    implicit def tok(s: String) = token(P.string(s))

    def array = surround("[","]")(
      value sep "," map (vs => JArray(vs.toIndexedSeq))) scope "array"
    def obj = surround("{","}")(
      keyval sep "," map (kvs => JObject(kvs.toMap))) scope "object"
    def keyval = escapedQuoted ** (":" *> value)
    def lit = scope("literal") {
      "null".as(JNull) |
      double.map(JNumber(_)) |
      escapedQuoted.map(JString(_)) |
      "true".as(JBool(true)) |
      "false".as(JBool(false))
    }
    def value: Parser[JSON] = lit | obj | array
    root(whitespace *> (obj | array))
  }
}

/**
 * JSON parsing example.
 */
object JSONExample extends App {

//  val jsonTxt = """
//{
//  "Company name" : "Microsoft Corporation",
//  "Ticker"  : "MSFT",
//  "Active"  : true,
//  "Price"   : 30.66,
//  "Shares outstanding" : 8.38e9,
//  "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
//}
//"""

  val jsonTxt =
    """
    {
      "tournaments": 1000,
      "roundsPerMatch": 1000,
      "randomSeed": 12345,
      "players": [
        {
          "name": "Last Losing 1",
          "type": "LastLosingMovePlayer"
        },
        {
          "name": "Last Losing 2",
          "type": "LastLosingMovePlayer",
        },
        {
          "name": "Last Winning 1",
          "type": "LastWinningMovePlayer",
        },
        {
          "name": "Majority Losing 1",
          "type": "MajorityLosingMovePlayer"
        },
        {
          "name": "Majority Winning 1",
          "type": "MajorityWinningMovePlayer"
        },
        {
          "name": "Unbiased Random 1",
          "type": "RandomMovePlayer"
        },
        {
          "name": "Biased Random 1",
          "type": "BiasedRandomMovePlayer",
          "weights": {
            "rock": 0.5,
            "paper": 0.25,
            "scissors": 0.25
          }
        },
        {
          "name": "Biased Random 2",
          "type": "BiasedRandomMovePlayer",
          "weights": {
            "rock": 0.8,
            "paper": 0.1,
            "scissors": 0.1
          }
        }
      ]
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

  val P = fpinscala.parsing.Reference
  import fpinscala.parsing.ReferenceTypes.Parser

  def printResult[E](e: Either[E,JSON]) =
    e.fold(println, println)

  val json: Parser[JSON] = JSON.jsonParser(P)
  printResult { P.run(json)(jsonTxt) }
  println("--")
  printResult { P.run(json)(malformedJson1) }
  println("--")
  printResult { P.run(json)(malformedJson2) }
}


