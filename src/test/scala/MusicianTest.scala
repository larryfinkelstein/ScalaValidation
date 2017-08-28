import java.time.LocalDate

import org.scalatest.{FunSuite, MustMatchers}
import Musician.validate

class MusicianTest extends FunSuite with MustMatchers {

  test("testValidate") {
    val opeth = Bands("Opeth")
    val mikael = Musician(
      name = "Mikael Åkerfeldt",
      born = LocalDate.parse("1974-04-17"),
      instruments = List(Guitar, BassGuitar),
      currentBand = Option(opeth))

    val badMikael = mikael.copy(born = LocalDate.now.minusYears(2))
      .copy(instruments = Nil)

    validate(mikael).isSuccess mustBe true
    // -> Success(Musician(Mikael Åkerfeldt,1974-04-17, List(Guitar, BassGuitar),Some(Band(Opeth)),List()))

    validate(badMikael).isFailure mustBe true
    // -> Failure(java.lang.IllegalArgumentException: too young)
    validate(badMikael).foreach(println)
    println(validate(badMikael))
  }

  test("test Validate Scalaz") {

  }

}
