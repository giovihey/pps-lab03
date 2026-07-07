package it.unibo.pps.u03

import it.unibo.pps.u03.Person.Teacher
import u03.Sequences.Sequence
import u03.Sequences.Sequence.*

enum Person:
  case Student(name: String, year: Int)
  case Teacher(name: String, course: String)

object Person:
  def course(s: Sequence[Person]): Sequence[String] =
    flatMap(s) {
      case Teacher(_, c) => Cons(c, Nil())
      case _ => Nil()
    }

  def distinctCourse(s: Sequence[Person]): Int =
    foldLeft(distinct(course(s)))(0)((acc, _) => acc + 1)


@main def main(): Unit =
  val p : Sequence[Person] = Cons(Person.Student("mario", 2015), Cons(Person.Teacher("ma", "PPS"), Cons(Person.Teacher("ma", "PCD"), Nil())))
  println(Person.course(p))
  val list: Sequence[Person] = Cons(Teacher("Viroli", "PPS"), Cons(Teacher("Aguzzi", "PPS"), Cons(Teacher("Ricci", "PCD"), Nil())))
  println(Person.distinctCourse(list))