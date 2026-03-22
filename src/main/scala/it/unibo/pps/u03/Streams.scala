package u03

object Streams extends App:

  import u03.Sequences.*
  import Sequences.Sequence.*

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

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if pred(head()) => cons(head(), filter(tail())(pred))
      case Cons(_, tail) => filter(tail())(pred)
      case _ => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    // Utile per task opzionali
    def fromSequence[A](seq: Sequence[A]): Stream[A] = seq match
      case Sequence.Cons(h, t) => cons(h, fromSequence(t))
      case Sequence.Nil() => empty()

    /*
     * 6. Restituisce il prefisso massimo dello stream
     * i cui elementi soddisfano pred.
     */
    def takeWhile[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if pred(head()) =>
        cons(head(), takeWhile(tail())(pred))
      case _ =>
        Empty()

    /*
     * 7. Crea uno stream finito di n elementi tutti uguali a k.
     */
    def fill[A](n: Int)(k: => A): Stream[A] =
      if n <= 0 then empty()
      else cons(k, fill(n - 1)(k))

    /*
     * 8. Stream infinito di Fibonacci.
     * 0, 1, 1, 2, 3, 5, ...
     */
    val fibonacci: Stream[Int] =
      def fib(a: Int, b: Int): Stream[Int] =
        cons(a, fib(b, a + b))
      fib(0, 1)

    /*
     * 9. Alterna gli elementi di due stream.
     * Quando uno finisce, continua con l'altro.
     */
    def interleave[A](s1: Stream[A], s2: Stream[A]): Stream[A] = s1 match
      case Cons(head, tail) =>
        cons(head(), interleave(s2, tail()))
      case _ =>
        s2

    /*
     * 10. Crea uno stream infinito ciclato su una Sequence finita.
     * Se la lista è vuota, restituisce stream vuoto.
     */
    def cycle[A](lst: Sequence[A]): Stream[A] =
      def loop(current: Sequence[A]): Stream[A] = current match
        case Sequence.Cons(h, t) => cons(h, loop(t))
        case Sequence.Nil() => loop(lst)

      lst match
        case Sequence.Nil() => empty()
        case _ => loop(lst)

  end Stream

@main def tryStreams =
  import Streams.*
  import Stream.*
  import Sequences.Sequence.*

  val str1 = Stream.iterate(0)(_ + 1)
  val str2 = Stream.map(str1)(_ + 1)
  val str3 = Stream.filter(str2)(x => x < 3 || x > 20)
  val str4 = Stream.take(str3)(10)
  println(Stream.toList(str4))

  lazy val corec: Stream[Int] = Stream.cons(1, corec)
  println(Stream.toList(Stream.take(corec)(10)))

  println(Stream.toList(Stream.takeWhile(Stream.iterate(0)(_ + 1))(_ < 5)))
  println(Stream.toList(Stream.fill(3)("a")))
  println(Stream.toList(Stream.take(Stream.fibonacci)(5)))

  val s1 = Stream.fromSequence(Cons(1, Cons(3, Cons(5, Nil()))))
  val s2 = Stream.fromSequence(Cons(2, Cons(4, Cons(6, Cons(8, Cons(10, Nil()))))))
  println(Stream.toList(Stream.interleave(s1, s2)))

  val repeat = Stream.cycle(Cons('a', Cons('b', Cons('c', Nil()))))
  println(Stream.toList(Stream.take(repeat)(5)))