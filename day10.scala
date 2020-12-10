import scala.io.Source

object Day10 extends App {
    val data: List[Int] = Source.fromFile("day10in").mkString.split("\\n").toList.flatMap(_.toIntOption).sorted

    def getDiffs(l: List[Int], output: List[Int]): List[Int] = {
        data match {
            case a :: b :: Nil => output
            case a :: b :: rest => getDiffs(b :: rest, output :+ (b-a))
            case _ => output
        }
    }

    println(getDiffs(data, Nil))
}
