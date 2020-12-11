import scala.io.Source

object Day11 extends App {

    case class Seat(x: Int, y: Int, occupied: Boolean)

    def validate(c: Char): Option[Char] =
        c match {
            case '#' => Some('#')
            case 'L' => Some('L')
            case '.' => Some('.')
            case _ => None
        }

    val data: Vector[Vector[Char]] = Source.fromFile("day11in").mkString.split("\\n").toVector.map(_.flatMap(validate(_)).toVector)

    data map println
}
