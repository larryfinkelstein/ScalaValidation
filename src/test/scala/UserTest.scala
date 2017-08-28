import org.scalatest.{FunSuite, MustMatchers}

class UserTest extends FunSuite with MustMatchers {

  test("Valid user") {
    //val user = User("JohnDoe", "john.doe@example.com", "password", 30, None, Seq(Address("Main St.", 123)))
    val user = User("JohnDoe", 30)
    user.validate.isGood mustBe true
//    println(isValid.size)
//    isValid.length mustEqual 0
  }

  test("Invalid user") {
//    val user = User("John Doe", "john.doe#example.com", "password", 30, None, Seq(Address("Main St.", 123)))
val user = User("John # Doe", -30)
    val validUser = user.validate
    validUser.isBad mustBe true
//    println(validUser.toString)
    validUser.badMap(_.toList.foreach(println))
  }
}
