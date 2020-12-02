import scala.io.Source

val lines = Source.fromFile("day2in").getLines.toList

def isValid(line: String): Boolean = {
    val range :: char :: password :: _ = line.split("[ ]").toList
    val low :: high :: _ = range.split("-").toList
    val count = password.count(_ == char.charAt(0)) 
    (count >= low.toInt) && (count <= high.toInt)
}

lines.filter(isValid).length


