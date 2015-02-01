import org.scalacheck._
import Arbitrary.arbitrary
import Prop._

object TestOptional extends Properties("Optional") {

  import Optional._

  implicit def ArbitraryOptional[A](implicit a: Arbitrary[A]): Arbitrary[Optional[A]] =
    Arbitrary(arbitrary[Option[A]] map fromOption)

  property("map") = forAll((o: Optional[Int], f: Int => String) =>
    (o map f).toOption == (o.toOption map f))

  property("get") = forAll((o: Optional[Int]) =>
    o.isDefined ==>
      (o.get == o.toOption.get))

  property("flatMap") = forAll((o: Optional[Int], f: Int => Optional[String]) =>
    (o flatMap f).toOption == (o.toOption flatMap (f(_).toOption)))

  property("mapAgain") = forAll((o: Optional[Int], f: Int => String) =>
    (o mapAgain f).toOption == (o map f).toOption)

  property("getOrElse") = forAll((o: Optional[Int], n: Int) =>
    (o getOrElse n) == (o.toOption getOrElse n))

  property("filter") = forAll((o: Optional[Int], f: Int => Boolean) =>
    (o filter f).toOption == (o.toOption filter f))

  property("exists") = forAll((o: Optional[Int], f: Int => Boolean) =>
    (o exists f) == (o.toOption exists f))

  property("forall") = forAll((o: Optional[Int], f: Int => Boolean) =>
    (o forall f) == (o.toOption forall f))

  property("foreach") = forAll((o: Optional[Int], f: Int => Unit, n: Int) => {
    var x: Int = n
    var y: Int = x

    o foreach (t => x = x + t)
    o.toOption foreach (t => y = y + t)

    x == y
  })

  property("isDefined") = forAll((o: Optional[Int]) =>
    (o.isDefined) == (o.toOption.isDefined))

  property("isEmpty") = forAll((o: Optional[Int]) =>
    o.isEmpty == o.toOption.isEmpty)

  property("orElse") = forAll((o: Optional[Int], p: Optional[Int]) =>
    (o orElse p).toOption == (o.toOption orElse p.toOption))

  property("toLeft") = forAll((o: Optional[Int], n: Int) =>
    (o toLeft n) == (o.toOption toLeft n))

  property("toRight") = forAll((o: Optional[Int], n: Int) =>
    (o toRight n) == (o.toOption toRight n))

  property("toList") = forAll((o: Optional[Int]) =>
    o.toList == o.toOption.toList)

  property("iterator") = forAll((o: Optional[Int]) =>
    o.iterator sameElements o.toOption.iterator)

  // *** READ THIS COMMENT FIRST ***
  // Note that scala.Option has no such equivalent to this method
  // Therefore, reading this test may give away clues to how it might be solved.
  // If you do not wish to spoil it, look away now and follow the
  // instruction in the Exercise comment.
  property("applic") = forAll((o: Optional[Int => String], p: Optional[Int]) =>
    (p applic o).toOption ==
      (for (f <- o.toOption;
            n <- p.toOption)
      yield f(n)))

  def trace[A](a: A) = {
    println(a)
    a
  }

  property("mapOptionals") = forAll((f: Int => Optional[String], o: List[Int]) => {
    val i = o map f
    mapOptionals(f, o) == (if (i forall (_.isDefined)) some(i map (_.get)) else none)
  })

  property("sequenceOptionals") = forAll((o: List[Optional[String]]) =>
    sequenceOptionals(o) == (if (o exists (_.isEmpty)) none else some(o map (_.get))))

  property("mapOptionalsAgain") = forAll((f: Int => Optional[String], o: List[Int]) =>
    mapOptionalsAgain(f, o) == mapOptionals(f, o))

  property("sequenceOptionalsAgain") = forAll((o: List[Optional[String]]) =>
    sequenceOptionalsAgain(o) == sequenceOptionals(o))

  property("joinOptionals") = forAll((o: Optional[Optional[String]]) =>
    joinOptionals(o) == (if (o.isDefined && o.get.isDefined) o.get else none))

  property("filterOptionals") = forAll((f: Int => Optional[Boolean], o: List[Int]) =>
    filterOptionals(f, o) == (if (o exists (f(_).isEmpty)) none else some(o filter (f(_).get))))

  property("fillOptionals") = forAll((n: Int, o: Optional[String]) =>
    (n < 1000) ==> // prevent stack consumption
      (fillOptionals(n, o) == (if (n <= 0) some(Nil) else (o map (List.fill(n)(_))))))

  property("fillOptionalsAgain") = forAll((n: Int, o: Optional[String]) =>
    (n < 1000) ==> // prevent stack consumption
      (fillOptionalsAgain(n, o) == fillOptionals(n, o)))

  property("mapOptionalsYetAgain") = forAll((f: Int => Optional[String], o: List[Int]) =>
    mapOptionalsYetAgain(f, o) == mapOptionals(f, o))

  property("mapWhatever") = forAll((f: Int => Optional[String], o: List[Int]) =>
    mapWhatever(f, o) == mapOptionals(f, o))
}