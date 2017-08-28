import java.time.{LocalDate, Period}

import scala.util.{Failure, Success, Try}
import scalaz.{NonEmptyList, Validation}

trait Classification

case object StringInstrument extends Classification

case object Keyboard extends Classification

abstract class Instrument(val classification: Classification)

case object BassGuitar extends Instrument(StringInstrument)

case object Guitar extends Instrument(StringInstrument)

case object Piano extends Instrument(Keyboard)

case class Bands(name: String)

case class MemberOfBand(from: LocalDate, membership: Period)

case class Musician(
                     name: String,
                     born: LocalDate,
                     instruments: Seq[Instrument],
                     currentBand: Option[Bands] = None,
                     formerBands: Seq[MemberOfBand] = Nil)
object Musician {
  def validate(musician: Musician): Try[Musician] = {
    def validCurrentBand(band: Option[Bands]) = Success(band)

    def validName(name: String): Try[String] = Success(name)

    def validateAge(born: LocalDate): Try[LocalDate] = {
      if(born.isAfter(LocalDate.now().minusYears(12)))
        Failure(new IllegalArgumentException("too young"))
      else Success(born)
    }

    def validInstrument(instruments: Seq[Instrument]) = Success(instruments)

    for {
      band <- validCurrentBand(musician.currentBand)
      name <- validName(musician.name)
      born <- validateAge(musician.born)
      instruments <- validInstrument(musician.instruments)
    } yield musician
  }

  type StringValidation[T] = Validation[String, T]
//  type ValidationNel[+E, +X] = Validation[NonEmptyList[E], X]

  def validateZ(musician: Musician): StringValidation[Musician] = {
    import scalaz._
    import scalaz.Scalaz._

    def validCurrentBand(band: Option[Bands]): StringValidation[Option[Bands]] =
      band.success

    def validName(name: String): StringValidation[String] = name.success

    def validateAge(born: LocalDate): StringValidation[LocalDate] =
      if (born.isAfter(LocalDate.now().minusYears(12))) "too young".failure
      else born.success

    def validInstrument(instruments: Seq[Instrument]): StringValidation[Seq[Instrument]] =
      instruments.success

    /*
    (validName(musician.name)
      |@| validateAge(musician.born)
      |@| validInstrument(musician.instruments)
      |@| validCurrentBand(musician.currentBand))(_ => musician)
      */
    musician.success
  }
}