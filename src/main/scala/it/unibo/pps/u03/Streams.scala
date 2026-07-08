package u03

import u03.Streams.Stream.{cons, fib, takeWhile}

object Streams extends App :

  import Sequences.*

  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def toList[A](stream: Stream[A]): Sequence[A] = stream match
      case Cons(h, t) => Sequence.Cons(h(), toList(t()))
      case _ => Sequence.Nil()
      
    def fromList[A](list: List[A]): Stream[A] = list match
      case h :: t => cons(h, fromList(t))
      case _ => Empty()

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
      case Cons(head, tail) => filter(tail())(pred)
      case _ => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def takeWhile[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if pred(head()) => cons(head(), takeWhile(tail())(pred))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    def fill[A](n: Int)(k: A): Stream[A] = n match
      case n if n > 0 => cons(k, fill(n - 1)(k))
      case _ => Empty()

    def fib(a: Int, b: Int): Stream[Int] =
      cons(a, fib(b, a+b))

    def interleave[A](stream1: Stream[A], stream2: Stream[A]): Stream[A] = (stream1, stream2) match
      case (Cons(head, tail), _) => cons(head(), interleave(stream2, tail()))
      case (_, Cons(head, tail)) => cons(head(), interleave(stream1, tail()))
      case _ => Empty()

  end Stream

@main def tryStreams =
  import Streams.* 

  val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
  val str2 = Stream.map(str1)(_ + 1) // {1,2,3,4,..}
  val str3 = Stream.filter(str2)(x => (x < 3 || x > 20)) // {1,2,21,22,..}
  val str4 = Stream.take(str3)(10) // {1,2,21,22,..,28}
  println(Stream.toList(str4)) // [1,2,21,22,..,28]

  lazy val corec: Stream[Int] = Stream.cons(1, corec) // {1,1,1,..}
  println(Stream.toList(Stream.take(corec)(10))) // [1,1,..,1]

  val stream = Stream.iterate(0)(_ + 1)
  println(Stream.toList(takeWhile(stream)(_ < 5))) // Cons (0 , Cons (1 , Cons (2 , Cons (3 , Cons (4 , Nil ())))))
   
  println(Stream.toList(Stream.fill(3)("a"))) // Cons(a, Cons(a, Cons(a, Nil())))

  val fibonacci: Stream[Int] = fib(0, 1)
  println(Stream.toList(Stream.take(fibonacci)(5))) // Cons (0 , Cons (1 , Cons (1 , Cons (2 , Cons (3 , Nil ()))))

  val s1 = Stream.fromList(List(1, 3, 5))
  val s2 = Stream.fromList(List(2, 4, 6, 8, 10))
  println(Stream.toList(Stream.interleave(s1, s2)))
// Expected output : Cons (1 , Cons (2 , Cons (3 , Cons (4 , Cons (5 , Cons (6 , Cons (8 , Cons (10 , Nil ()))))))))