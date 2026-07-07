package u03

import u03.Optionals.Optional

import scala.annotation.tailrec
import u03.Optionals.Optional.{Empty, Just}

object Sequences: // Essentially, generic linkedlists
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil()      => Nil()

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t)            => filter(t)(pred)
      case Nil()                 => Nil()

    // Lab 03

    /*
     * Skip the first n elements of the sequence
     * E.g., [10, 20, 30], 2 => [30]
     * E.g., [10, 20, 30], 3 => []
     * E.g., [10, 20, 30], 0 => [10, 20, 30]
     * E.g., [], 2 => []
     */
    @tailrec
    def skip[A](s: Sequence[A])(n: Int): Sequence[A] = s match
      case Cons(_, t) if n >= 1 => skip(t)(n-1)
      case _ => s

    /*
     * Zip two sequences
     * E.g., [10, 20, 30], [40, 50] => [(10, 40), (20, 50)]
     * E.g., [10], [] => []
     * E.g., [], [] => []
     */
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
      case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))
      case _ => Nil()

    /*
     * Concatenate two sequences
     * E.g., [10, 20, 30], [40, 50] => [10, 20, 30, 40, 50]
     * E.g., [10], [] => [10]
     * E.g., [], [] => []
     */
    def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = (s1, s2) match
      case (Cons(h1, t1), _) => Cons(h1, concat(t1, s2))
      case (_, Cons(h2, t2)) => Cons(h2, concat(t2, Nil()))
      case _ => Nil()


    /*
     * Reverse the sequence
     * E.g., [10, 20, 30] => [30, 20, 10]
     * E.g., [10] => [10]
     * E.g., [] => []
     */
    def reverse[A](s: Sequence[A]): Sequence[A] =
      @tailrec
      def _reverse(rest: Sequence[A], acc: Sequence[A]): Sequence[A] =
        rest match
          case Nil() => acc
          case Cons(h, t) => _reverse(t, Cons(h, acc))
      _reverse(s, Nil())

    /*
     * Map the elements of the sequence to a new sequence and flatten the result
     * E.g., [10, 20, 30], calling with mapper(v => [v, v + 1]) returns [10, 11, 20, 21, 30, 31]
     * E.g., [10, 20, 30], calling with mapper(v => [v]) returns [10, 20, 30]
     * E.g., [10, 20, 30], calling with mapper(v => Nil()) returns []
     */
    def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = s match
      case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
      case _ => Nil()

    /*
     * Get the minimum element in the sequence
     * E.g., [30, 20, 10] => 10
     * E.g., [10, 1, 30] => 1
     */
    def min(s: Sequence[Int]): Optional[Int] = s match
    case Nil() => Empty()
    case Cons(h, t) =>
      @tailrec
      def _min(s: Sequence[Int], minimum: Int): Optional[Int] = s match
        case Nil() => Just(minimum)
        case Cons(h2, t2) if h2 < minimum => _min(t2, h2)
        case Cons(_, t2) => _min(t2, minimum)
      _min(t, h)

    /*
     * Get the elements at even indices
     * E.g., [10, 20, 30] => [10, 30]
     * E.g., [10, 20, 30, 40] => [10, 30]
     */
    def evenIndices[A](s: Sequence[A]): Sequence[A] =
        @tailrec
        def _even(s: Sequence[A], count: Int, acc: Sequence[A]): Sequence[A] = s match
          case Nil() => reverse(acc)
          case Cons(h, t) if count % 2 == 0 => _even(t, count + 1, Cons(h, acc))
          case Cons(_, t) => _even(t, count + 1, acc)
        _even(s, 0, Nil())

    /*
     * Check if the sequence contains the element
     * E.g., [10, 20, 30] => true if elem is 20
     * E.g., [10, 20, 30] => false if elem is 40
     */
    @tailrec
    def contains[A](s: Sequence[A])(elem: A): Boolean = s match
      case Cons(h, t) if h == elem => true
      case Cons(_, t) => contains(t)(elem)
      case Nil() => false

    /*
     * Remove duplicates from the sequence
     * E.g., [10, 20, 10, 30] => [10, 20, 30]
     * E.g., [10, 20, 30] => [10, 20, 30]
     */
    def distinct[A](s: Sequence[A]): Sequence[A] =
      @tailrec
      def _distinct(s: Sequence[A], acc: Sequence[A]): Sequence[A] = s match
        case Nil() => reverse(acc)
        case Cons(h, t) if !contains(acc)(h) => _distinct(t, Cons(h, acc))
        case Cons(_, t) => _distinct(t, acc)
      _distinct(s, Nil())

    /*
     * Group contiguous elements in the sequence
     * E.g., [10, 10, 20, 30] => [[10, 10], [20], [30]]
     * E.g., [10, 20, 30] => [[10], [20], [30]]
     * E.g., [10, 20, 20, 30] => [[10], [20, 20], [30]]
     */
    def group[A](s: Sequence[A]): Sequence[Sequence[A]] =
      @tailrec
      def _group(s: Sequence[A], current: Sequence[A], acc: Sequence[Sequence[A]]): Sequence[Sequence[A]] = s match
        case Nil() =>
          if current == Nil() then acc else Cons(current, acc)
        case Cons(h, t) => current match
          case Nil() => _group(t, Cons(h, Nil()), acc)
          case Cons(ch, ct) if ch == h => _group(t, Cons(h, current), acc)
          case _ => _group(s, Nil(), Cons(current, acc))
      reverse(_group(s, Nil(), Nil()))

    /*
     * Partition the sequence into two sequences based on the predicate
     * E.g., [10, 20, 30] => ([10], [20, 30]) if pred is (_ < 20)
     * E.g., [11, 20, 31] => ([20], [11, 31]) if pred is (_ % 2 == 0)
     */
    def partition[A](s: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) =
      @tailrec
      def _partition(s: Sequence[A], matching: Sequence[A], notMatching: Sequence[A]): (Sequence[A], Sequence[A]) = s match
        case Nil() => (reverse(matching), reverse(notMatching))
        case Cons(h, t) =>
          if pred(h) then _partition(t, Cons(h, matching), notMatching)
          else _partition(t, matching, Cons(h, notMatching))
      _partition(s, Nil(), Nil())

    /*
     * Fold left: accumulate elements through a binary operator
     * E.g., [3, 7, 1, 5], init=0, op=(+) => (((0 + 3) + 7) + 1) + 5 = 16
     * E.g., [3, 7, 1, 5], init=1, op=(*) => (((1 * 3) * 7) * 1) * 5 = 105
     */
    def foldLeft[A, B](s: Sequence[A])(init: B)(op: (B, A) => B): B =
      @tailrec
      def _foldLeft(s: Sequence[A], acc: B): B = s match
        case Cons(h, t) => _foldLeft(t, op(acc, h))
        case Nil() => acc
      _foldLeft(s, init)


@main def trySequences =
  import Sequences.* 
  val l = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(l)) // 30

  import Sequence.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
  val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
  println(foldLeft(lst)(0)(_ - _)) // -16

