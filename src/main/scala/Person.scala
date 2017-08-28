import org.scalactic._

case class Person(name: String, age: Int)
object Person {
  import Accumulation._

  def parseName(input: String): String Or One[ErrorMessage] = {
    val trimmed = input.trim
    if (!trimmed.isEmpty) Good(trimmed) else Bad(One(s""""${input}" is not a valid name"""))
  }
  def parseAge(input: String): Int Or One[ErrorMessage] = {
    try {
      val age = input.trim.toInt
      if (age >= 0) Good(age) else Bad(One(s""""${age}" is not a valid age"""))
    }
    catch {
      case _: NumberFormatException => Bad(One(s""""${input}" is not a valid integer"""))
    }
  }
  def parsePerson(inputName: String, inputAge: String): Person Or Every[ErrorMessage] = {
    val name = parseName(inputName)
    val age = parseAge(inputAge)
    withGood(name, age) { Person(_, _) }
  }
}