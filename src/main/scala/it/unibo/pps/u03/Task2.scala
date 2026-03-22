package it.unibo.pps.u03
import u03.Sequences.*
import u03.Sequences.Sequence.*
import u03.Person
import u03.Person.{Student, Teacher}
import scala.collection.immutable.Set

object Task2 {
  // V1
  /*def teacherCourses(s: Sequence[Person]): Sequence[String] = {
    Sequence.map(
      Sequence.filter(s):
        case Teacher(name, course) => true
        case _ => false
    ) {
      case Teacher(name, course) => course
      case _ => ???
    }
  }*/

  // V2
  def teacherCourses(s: Sequence[Person]): Sequence[String] =
    Sequence.flatMap(s):
      case Teacher(_, course) => Sequence.Cons(course, Sequence.Nil())
      case _ => Sequence.Nil()


  def foldLeft(s: Sequence[Int])(v: Int)(f: ((Int, Int) => Int)): Int = s match {
    case Cons(h, t) => foldLeft(t)(f(v, h))(f)
    case Nil() => v
  }

  def differentCourses(s: Sequence[Person]): Int = {
    foldLeft(
      map(
        distinct(
          map(
            filter(s):
              case Teacher(_, _) => true
              case _ => false

          ):
            case Teacher(n, c) => c
            case _ => ???

        )
      ):
        case x: String => 1
        case _ => 0

    )(0)(
      (acc: Int, _: Int) => acc + 1
    )
  }
}
