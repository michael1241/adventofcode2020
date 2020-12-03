import scala.io.Source

object Day3 extends App {

    def parse(data: List[Char]):List[Boolean] = {
        def convert(char: Char): Option[Boolean] = {
            char match {
                case '#' => Some(true)
                case '.' => Some(false)
                case _ => None
            }
        }
        data.flatMap(convert(_))
    }

    val data = Source.fromFile("day3in").getLines.toList
    val chardata = data.map(_.toList)
    val parsed = chardata.map(parse(_))

    def calculate(parsed: List[List[Boolean]], xstep: Int, ystep: Int): Int = {

        val ys: Range = Range(ystep, parsed.length + ystep, ystep)
        val xs: Range = Range(xstep, ys.length * xstep, xstep)
        val coords = xs zip ys
        val selection = coords.map(coords => parsed(coords._2)(coords._1 % 31))
        selection.count(_ == true)
    }

    val directions: List[(Int, Int)] = List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
    val results = directions.map( { case (x, y) => calculate(parsed, x, y) } )
    val output: Long = results.map(_.toLong).product
    println(output)
}
