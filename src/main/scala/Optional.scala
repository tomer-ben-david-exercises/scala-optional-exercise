trait Optional[A] {
  // single abstract method
  def fold[X](some: A => X, none: => X): X

  import Optional._

  // Done for you.
  def map[B](f: A => B): Optional[B] =
    fold(f andThen some, none[B])

  // Done for you.
  // WARNING: undefined for None
  def get: A =
    fold(a => a, error("None.get"))

  // Exercise 1
  def flatMap[B](f: A => Optional[B]): Optional[B] = fold(f, none[B])


  // Exercise 2
  // Rewrite map but use flatMap, not fold.
  def mapAgain[B](f: A => B): Optional[B] = flatMap(f andThen some)


  // Exercise 3
  def getOrElse(e: => A): A = fold(Unit => this.get, e)

  // Exercise 4
  def filter(p: A => Boolean): Optional[A] = if (map(p).getOrElse(false)) this else none

  // Exercise 5
  def exists(p: A => Boolean): Boolean = filter(p).fold(Unit => true, false)

  // Exercise 6
  def forall(p: A => Boolean): Boolean = map(p).getOrElse(true) || exists(p)
//    this.toOption forall p

  // Exercise 7
  def foreach(f: A => Unit): Unit = fold(f, none)

  // Exercise 8
  def isDefined: Boolean = exists(_ => true)

  // Exercise 9
  def isEmpty: Boolean = !isDefined

  // Exercise 10
  def orElse(o: => Optional[A]): Optional[A] = if (!isEmpty) this else o

  // Exercise 11
  def toLeft[X](right: => X): Either[A, X] = if (isEmpty) Right(right) else Left(this.get)

  // Exercise 12
  def toRight[X](left: => X): Either[X, A] = if (isEmpty) Left(left) else Right(this.get)

  // Exercise 13
  def toList: List[A] = ???

  // Exercise 14
  def iterator: Iterator[A] = ???

  // Exercise 15 The Clincher!
  // Return a none value if either this or the argument is none.
  // Otherwise apply the function to the argument in some.
  // Don't be afraid to use functions you have written.
  // Better style, more points!
  def applic[B](f: Optional[A => B]): Optional[B] = ???

  // Utility
  def toOption: Option[A] = fold(Some(_), None)

  // Utility
  override def toString =
    fold("some[" + _ + "]", "none")

  // Utility
  override def equals(o: Any) =
    o.isInstanceOf[Optional[_]] && {
      val q = o.asInstanceOf[Optional[_]]
      fold(a => q.exists(a == _),
        q.isEmpty)
    }
}

object Optional {
  // Done for you
  def none[A]: Optional[A] = new Optional[A] {
    def fold[X](some: A => X, none: => X) = none
  }

  // Done for you
  def some[A](a: A): Optional[A] = new Optional[A] {
    def fold[X](some: A => X, none: => X) = some(a)
  }

  // Utility
  def fromOption[A](o: Option[A]): Optional[A] = o match {
    case None    => none
    case Some(a) => some(a)
  }

  // *** Special note ***
  // Some of these functions are likely to be familiar List functions,
  // but with one specific distinction: in every covariant value appearing in
  // the type signature, this value is wrapped in Optional.
  // For example, the unwrapped:
  // filter:          (A => Boolean) => List[A] => List[A]
  // and the wrapped:
  // filterOptionals: (A => Optional[Boolean]) => List[A] => Optional[List[A]]
  //
  // There are other functions of a similar nature below.

  // Exercise 16
  // If a none is encountered, then return a none, otherwise,
  // accumulate all the values in Optional.
  def mapOptionals[A, B](f: A => Optional[B], a: List[A]): Optional[List[B]] = ???

  // Exercise 17
  // If a none is encountered, then return a none, otherwise,
  // accumulate all the values in Optional.
  def sequenceOptionals[A](a: List[Optional[A]]): Optional[List[A]] = ???

  // Exercise 18
  // Use sequenceOptionals
  def mapOptionalsAgain[A, B](f: A => Optional[B], a: List[A]): Optional[List[B]] =
    ???

  // Exercise 19
  // Use mapOptionals
  def sequenceOptionalsAgain[A](a: List[Optional[A]]): Optional[List[A]] =
    ???

  // Exercise 20
  // If a none is encountered, return none, otherwise,
  // flatten/join by one level.
  def joinOptionals[A](a: Optional[Optional[A]]): Optional[A] =
    ???

  // Exercise 21
  def filterOptionals[A](p: A => Optional[Boolean], a: List[A]): Optional[List[A]] =
    ???

  // Exercise 22
  def fillOptionals[A](n: Int, a: Optional[A]): Optional[List[A]] =
    ???

  // Exercise 23
  // Use sequenceOptionals
  def fillOptionalsAgain[A](n: Int, a: Optional[A]): Optional[List[A]] =
    ???

  // Exercise 24
  // Methods mentioning Optional in the type signature are prohibited, except applic and map
  def mapOptionalsYetAgain[A, B](f: A => Optional[B], a: List[A]): Optional[List[B]] =
    ???

  // Consider: def joinOptional[A](a: Optional[Optional[A]]): Optional[A]
  // This function "flattens" the Optional into a Some value if possible.
  // It is not possible to write this using only applic and map (try it!).

  // Bye bye Option-specificity!
  // (setting up for Exercise 25)
  trait Applic[F[_]] {
    def point[A](a: A): F[A]
    def applic[A, B](f: F[A => B], a: F[A]): F[B]

    final def map[A, B](f: A => B, a: F[A]): F[B] =
      applic(point(f), a)
  }

  object Applic {
    implicit val OptionalApplic: Applic[Optional] = new Applic[Optional] {
      def point[A](a: A): Optional[A] = some(a)
      def applic[A, B](f: Optional[A => B], a: Optional[A]): Optional[B] = a applic f
    }
  }

  // Exercise 25
  // The Double-Clincher!
  def mapWhatever[A, B, F[_]](f: A => F[B], a: List[A])(implicit z: Applic[F]): F[List[B]] =
    ???
}