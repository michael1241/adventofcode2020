import scala.io.Source

object Day6 extends App {
    val data: Array[Set[String]] = Source.fromFile("day6in").mkString.split("\\n\\n").map(_.replace("\n","")).map(_.split("").toSet)
    val counts: Array[Int] = data.map(_.size)

    println(counts.sum)
}
