import org.scalatest.{FunSuite, MustMatchers}

import Camera._

class CameraTest extends FunSuite with MustMatchers {

  test("testTryCamera") {
    println(tryCamera(Camera(false, true, 500)))
    println(tryCamera(Camera(true, false, 1500)))
  }

  test("testCheckCamera") {
    checkCamera(Camera(false, true, 500)).isGood mustBe true

    checkCamera(Camera(false, true, 1500)).isGood mustBe false
    checkCamera(Camera(false, true, 1500)).badMap(_.toList.foreach(println))

    checkCamera(Camera(true, false, 1500)).isBad mustBe true
    checkCamera(Camera(true, false, 1500)).badMap(_.toList.foreach(println))

    checkCamera(Camera(false, false, 1500)).isBad mustBe true
    checkCamera(Camera(false, false, 1500)).badMap(_.toList.foreach(println))
  }

  test("testCheckzCamera") {
    checkzCamera(Camera(false, true, 500)).isSuccess mustBe true
    checkzCamera(Camera(false, true, 500)).fold(f => f.foreach(println), c => println(c))

    println
    checkzCamera(Camera(false, true, 1500)).isFailure mustBe true
    //println(checkzCamera(Camera(false, true, 1500)))
    checkzCamera(Camera(false, true, 1500)).fold(f => f.foreach(println), c => println(c))

    println
    checkzCamera(Camera(true, false, 1500)).isFailure mustBe true
    checkzCamera(Camera(true, false, 1500)).fold(f => f.foreach(println), c => println(c))

    println
    checkzCamera(Camera(false, false, 1500)).isFailure mustBe true
    checkzCamera(Camera(false, false, 1500)).fold(f => f.foreach(println), c => println(c))
  }

}
