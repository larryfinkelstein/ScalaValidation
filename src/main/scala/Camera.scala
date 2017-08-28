import org.scalactic._

import scala.util.{Failure, Success, Try}
import scalaz.ValidationNel
import scalaz.Scalaz._

case class Camera(  isUsed: Boolean,
                    hasFullAccessories: Boolean,
                    expectedPrice: Double)
object Camera {
  def tryCamera(camera: Camera): Unit = {
    val result = for {
      newCamera <- tryUsed(camera)
      fullCamera <- tryFullAccessories(camera)
      goodCamera <- trySuitablePrice(camera)
    } yield goodCamera

    result match {
      case Success(camera) => // Okay I will buy it
      case Failure(issue) => // I'm considering
    }
  }

  def tryUsed(camera: Camera): Try[Camera] =
    if (camera.isUsed)
      throw new Exception("Camera was used")
    else Try(camera)

  def tryFullAccessories(camera: Camera): Try[Camera] =
    if (!camera.hasFullAccessories)
      throw new Exception("Camera didn't have full accessories")
    else Try(camera)

  def trySuitablePrice(camera: Camera): Try[Camera] = {
    val budget = 1000 //USD

    if (budget < camera.expectedPrice)
    throw new Exception("Too high !!!")
    else Try(camera)
  }

  def checkUsedCondition(camera: Camera): Camera Or One[ErrorMessage] =
    if (camera.isUsed)
      Bad(One("Camera was used"))
    else Good(camera)

  def checkFullAccessories(camera: Camera): Camera Or One[ErrorMessage] =
    if (!camera.hasFullAccessories)
      Bad(One("Camera didn't have full accessories"))
    else Good(camera)

  def checkSuitablePrice(camera: Camera): Camera Or One[ErrorMessage] = {
    val budget = 1000 //USD

    if (budget < camera.expectedPrice)
    Bad(One("Too high !!!"))
    else Good(camera)
  }

  def checkCamera(camera: Camera): Camera Or Every[ErrorMessage] = {
    for {
      newCamera <- checkUsedCondition(camera)
      fullCamera <- checkFullAccessories(camera)
      goodCamera <- checkSuitablePrice(camera)
    } yield goodCamera
  }

  def checkzUsedCondition(camera: Camera): ValidationNel[String, Camera] =
    if (camera.isUsed) "Camera was used".failureNel
    else camera.success

  def checkzFullAccessories(camera: Camera): ValidationNel[String, Camera] =
    if (!camera.hasFullAccessories) "Camera didn't have full accessories".failureNel
    else camera.success

  def checkzSuitablePrice(camera: Camera): ValidationNel[String, Camera] = {
    val budget = 1000 //USD

    if (budget < camera.expectedPrice) "Too high !!!".failureNel
    else camera.success
  }

  def checkzCamera(camera: Camera): ValidationNel[String, Camera] = {
    (checkzUsedCondition(camera)
      |@| checkzFullAccessories(camera)
      |@| checkzSuitablePrice(camera)) {
      case (_, _, _) => camera
    }
  }
}