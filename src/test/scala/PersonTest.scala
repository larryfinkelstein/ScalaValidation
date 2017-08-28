import org.scalatest.{FunSuite, MustMatchers}
import Person._

class PersonTest extends FunSuite with MustMatchers {

  test("testParsePerson") {

    parsePerson("Bridget Jones", "29").isGood mustBe true

    parsePerson("Bridget Jones", "").isBad mustBe true
    parsePerson("Bridget Jones", "").badMap(_.toList.foreach(println))

    parsePerson("Bridget Jones", "-29").isBad mustBe true
    parsePerson("Bridget Jones", "-29").badMap(_.toList.foreach(println))

    parsePerson("", "").isBad mustBe true
    parsePerson("", "").badMap(_.toList.foreach(println))
  }

}
