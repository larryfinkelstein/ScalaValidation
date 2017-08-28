import org.scalactic._

  case class Phone(value: String)
  case class Address(street: String, house: Int)
  case class User(username: String,
//                  email: String,
//                  password: String,
                  age: Int
//                  phone: Option[Phone],
//                  addresses: Seq[Address]
                 ) {
    /*
    def validate: List[String] = {
      def check[A](failMsg: String, value: A)(f: A => Boolean) = if (f(value)) List.empty[String] else List(failMsg)
      (
        check("Username cannot be empty", username)(_.trim.size > 0)
          ++ check("Username cannot contain whitespace", username)(!_.contains(" "))
          ++ check("Username contains invalid characters", username)(s => s.trim.forall(_.isLetterOrDigit))
        )
    }
    */
    def validate: User Or Every[ErrorMessage] = {
      import Accumulation._
      def validateUsername: String Or Every[ErrorMessage] = {
        if (this.username.trim.size <= 0) Bad(One("Username cannot be empty"))
        else if (this.username.contains(" ")) Bad(One("Username cannot contain whitespace"))
        else if (!this.username.trim.forall(_.isLetterOrDigit)) Bad(One("Username contains invalid characters"))
          else Good(this.username)
      }
      /*
      def validateUsernameC: String Or Every[ErrorMessage] = {
        def v1: String Or One[ErrorMessage] =
          if (this.username.trim.size <= 0) Bad(One("Username cannot be empty")) else Good(this.username)
        def v2: String Or One[ErrorMessage] =
          if (this.username.contains(" ")) Bad(One("Username cannot contain whitespace")) else Good(this.username)
        def v3: String Or One[ErrorMessage] =
          if (!this.username.trim.forall(_.isLetterOrDigit)) Bad(One("Username contains invalid characters"))
            else Good(this.username)
      }
      */
      def validateAge: Int Or Every[ErrorMessage] =
        if (age > 0) Good(this.age) else Bad(One("Age is negative"))
      withGood(validateUsername, validateAge) {User(_, _)}
//      withGood(validateUsername, validateAge) {this}
    }
  }
