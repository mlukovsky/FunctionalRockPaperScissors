package please_refactor.rockpaperscissors

import fpinscala.parsing.JSON.{JArray, JNumber, JObject, JString}
import fpinscala.parsing.ReferenceTypes.Parser
import fpinscala.parsing.{JSON, Location, ParseError}

object TournamentConfigParser{
  val jsonTxt =
    """
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
  def go : Unit = {
    val P = fpinscala.parsing.Reference
    val json: Parser[JSON] = JSON.jsonParser(P)
    val parsedObject = P.run(json)(jsonTxt) // deserialize json string
    println(parsedObject)
    println(parsedObject.flatMap(j => unpack(j)).map(dto => println(dto)).map(_ => ()))
  }

  case class TournamentSeason(
                               tournaments: Int ,
                               roundsPerMatch: Int,
                               randomSeed: Int,
                               players: List[Map[String, Any]]
                             )



  //unpack using flatMap
  def unpack(json: JSON): Either[ParseError,TournamentSeason] = {
    json match {
      case jObject: JObject =>
        for {
          tournaments <- jObject.get("tournaments") match {
            case jNumber: JNumber => Right(jNumber.get.intValue)
            case _ => Left(ParseError(List((Location("Could not unpack tournaments"), "tournaments"))))
          }
          roundsPerMatch <- jObject.get("roundsPerMatch") match {
            case jNumber: JNumber => Right(jNumber.get.intValue)
            case _ => Left(ParseError(List((Location("Could not unpack roundsPerMatch"), "roundsPerMatch"))))
          }
          randomSeed <- jObject.get("randomSeed") match {
            case jNumber: JNumber => Right(jNumber.get.intValue)
            case _ => Left(ParseError(List((Location("Could not unpack randomSeed"), "randomSeed"))))
          }
          players <- jObject.get("players") match {
            case jArray: JArray => Right(jArray.get)
            case _ => Left(ParseError(List((Location("Could not unpack players"), "players"))))
          }
          players <- unpackPlayersArray(players.toList, Right(List.empty))
        } yield TournamentSeason(tournaments, roundsPerMatch, randomSeed, players)
      case _ => Left(ParseError(List((Location("Could not unpack JSON contents"), "Could not unpack JSON contents"))))
    }
    }


  def unpackPlayersArray(list: List[JSON], playerList: Either[ParseError, List[Map[String, Any]]]): Either[ParseError, List[Map[String, Any]]] = {
    list match {
      case ::(head, next) => head match {
        case JObject(v) => unpackPlayersArray(next, playerList.flatMap(list => Right(v :: list)))
        case p: ParseError => Left(p)
      }
      case Nil => playerList
    }
  }


  //
//  def unpackString(jObject: JObject, key: String): Either[ParseError,String] = jObject.get(key) match {
//    case jString: JString => Right(jString.get)
//    case _ => Left(ParseError(List((Location("Could not unpack ticker"), "ticker"))))
//  }
//
//  def unpackNumber(jObject: JObject, key: String): Either[ParseError, Int] = jObject.get(key) match {
//    case jNumber: JNumber => Right(jNumber.get.toInt)
//    case _ => Left(ParseError(List((Location("Could not unpack number"), "ticker"))))
//  }

//  def unpackList(c: List[JSON], r: Either[ParseError, List[JSON]]): Either[ParseError, List[JSON]] =
//    c match {
//      case ::(head, next) => head match {
//        case JString(v) => unpackList(next, r.flatMap(list => Right(v :: list)))
//        case p: ParseError => Left(p)
//      }
//      case Nil => r
//    }


}

