import scala.io.Source

object Day10 extends App {
    val data: List[Int] = Source.fromFile("day10in").mkString.split("\\n").toList.flatMap(_.toIntOption).sorted

    def getDiffs(l: List[Int], output: List[Int]): List[Int] = {
        data match {
            case a :: b :: rest => getDiffs(l.drop(1), output :+ (b-a))
            case _ => output
        }
    }

    println(getDiffs(data, Nil))
}
