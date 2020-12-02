import scala.io.Source

val lines = Source.fromFile("day2in").getLines.toList

def isValid(line: String): Boolean = {
    val range :: char :: password :: _ = line.split("[ ]").toList
    val low :: high :: _ = range.split("-").toList
    val count = password.count(_ == char.charAt(0)) 
    (count >= low.toInt) && (count <= high.toInt)
}

def isValid2(line: String): Boolean = {
    val range :: c :: password :: _ = line.split("[ ]").toList
    val low :: high :: _ = range.split("-").toList
    val char = c.charAt(0)
    ((password.charAt(low.toInt -1) == char) && (password.charAt(high.toInt -1) != char) || (password.charAt(low.toInt -1) != char) && (password.charAt(high.toInt -1) == char))
}

lines.filter(isValid2).length


