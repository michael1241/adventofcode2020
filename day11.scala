import scala.io.Source

object Day11 extends App {

    case class Seat(x: Int, y: Int, occupied: Boolean)

    def validate(x: Int, y: Int, char: String): Option[Seat] =
            char match {
                case "#" => Some(Seat(x, y, true))
                case "L" => Some(Seat(x, y, false))
                case _ => None
            }

    val data: Vector[Vector[String]] = Source.fromFile("day11in").mkString.split("\\n").toVector.map(_.split("").toVector)

    val seats: Set[Seat] = { for {
            y <- 0 to data.length - 1
            x <- 0 to data(0).length - 1
        } yield validate(x, y, data(y)(x))
    }.flatten.toSet

    def countNeighbors(current: Seat, seats: Set[Seat]): Int = {
        seats filter {
            seat: Seat =>
                seat != current &&
                math.abs(seat.x - current.x) <= 1 &&
                math.abs(seat.y - current.y) <= 1 &&
                seat.occupied
        }
    }.size

    def nextSeat(seat: Seat, seats: Set[Seat]): Seat = {
        val neighbors: Int = countNeighbors(seat, seats)
        if (!seat.occupied && neighbors == 0) seat.copy(occupied = true)
        else if (seat.occupied && neighbors >= 4) seat.copy(occupied = false)
        else seat
    }

    while (true) {
        val newSeats: Set[Seat] = seats.map(nextSeat(_, seats))
        if (newSeats.count(_.occupied) != seats.count(_.occupied))
            println(newSeats.count(_.occupied))
        val seats: Set[Seat] = newSeats
    }

}
