import scala.io.Source

object Day11 extends App {

    case class Seat(x: Int, y: Int, occupied: Boolean)

    def validate(x: Int, y: Int, char: String): Option[Seat] = {
            char match {
                case "#" => Some(Seat(x, y, true))
                case "L" => Some(Seat(x, y, false))
                case _ => None
            }
        }

    val data: Vector[Vector[String]] = Source.fromFile("day11in").mkString.split("\\n").toVector.map(_.split("").toVector)

    //data.zipWithIndex.map{case (digit, index) => ((index / 7, index % 7), digit.asDigit)}.toMap
    val seats: Set[Seat] = { for {
            y <- 0 to data.length - 1
            x <- 0 to data(0).length - 1
        } yield validate(x, y, data(y)(x))
    }.flatten.toSet

    println(seats)
}
