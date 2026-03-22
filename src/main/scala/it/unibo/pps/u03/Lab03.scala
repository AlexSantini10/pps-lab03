package u03

import u03.Optionals.Optional
import u03.Optionals.Optional.*
import u03.Person
import u03.Person.*

object Lab03:

  // =========================
  // Task 1 - Sequences
  // =========================

  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()

    def filter[A](l: Sequence[A])(pred: A => Boolean): Sequence[A] = l match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()

    def skip[A](s: Sequence[A])(n: Int): Sequence[A] = s match
      case Cons(_, t) if n > 0 => skip(t)(n - 1)
      case _ => s

    def zip[A, B](s1: Sequence[A], s2: Sequence[B]): Sequence[(A, B)] =
      (s1, s2) match
        case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))
        case _ => Nil()

    def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = s1 match
      case Cons(h, t) => Cons(h, concat(t, s2))
      case Nil() => s2

    def reverse[A](s: Sequence[A]): Sequence[A] =
      def loop(s: Sequence[A], acc: Sequence[A]): Sequence[A] = s match
        case Cons(h, t) => loop(t, Cons(h, acc))
        case Nil() => acc
      loop(s, Nil())

    def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = s match
      case Nil() => Nil()
      case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))

    def min(s: Sequence[Int]): Optional[Int] = s match
      case Nil() => Empty()
      case Cons(h, t) => min(t) match
        case Empty() => Just(h)
        case Just(m) => Just(if h < m then h else m)

    def evenIndices[A](s: Sequence[A]): Sequence[A] = s match
      case Cons(h, Cons(_, t)) => Cons(h, evenIndices(t))
      case Cons(h, Nil()) => Cons(h, Nil())
      case Nil() => Nil()

    def contains[A](s: Sequence[A])(elem: A): Boolean = s match
      case Cons(h, _) if h == elem => true
      case Cons(_, t) => contains(t)(elem)
      case Nil() => false

    def distinct[A](s: Sequence[A]): Sequence[A] =
      def loop(s: Sequence[A], acc: Sequence[A]): Sequence[A] = s match
        case Nil() => reverse(acc)
        case Cons(h, t) if contains(acc)(h) => loop(t, acc)
        case Cons(h, t) => loop(t, Cons(h, acc))
      loop(s, Nil())

    def group[A](s: Sequence[A]): Sequence[Sequence[A]] =
      def takeWhile(s: Sequence[A])(p: A => Boolean): Sequence[A] = s match
        case Cons(h, t) if p(h) => Cons(h, takeWhile(t)(p))
        case _ => Nil()

      def dropWhile(s: Sequence[A])(p: A => Boolean): Sequence[A] = s match
        case Cons(h, t) if p(h) => dropWhile(t)(p)
        case _ => s

      s match
        case Nil() => Nil()
        case Cons(h, _) =>
          val g = takeWhile(s)(_ == h)
          val rest = dropWhile(s)(_ == h)
          Cons(g, group(rest))

    def partition[A](s: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) = s match
      case Nil() => (Nil(), Nil())
      case Cons(h, t) =>
        val (yes, no) = partition(t)(pred)
        if pred(h) then (Cons(h, yes), no)
        else (yes, Cons(h, no))

    // =========================
    // Task 2 - Person + fold
    // =========================

    def teacherCourses(s: Sequence[Person]): Sequence[String] =
      flatMap(s):
        case Teacher(_, c) => Cons(c, Nil())
        case _ => Nil()

    def foldLeft[A, B](s: Sequence[A])(v: B)(f: (B, A) => B): B = s match
      case Cons(h, t) => foldLeft(t)(f(v, h))(f)
      case Nil() => v

    def differentCourses(s: Sequence[Person]): Int =
      foldLeft(distinct(teacherCourses(s)))(0)((acc, _) => acc + 1)

  // =========================
  // Task 3 - Streams
  // =========================

  enum Stream[A]:
    case Empty()
    case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def map[A, B](s: Stream[A])(f: A => B): Stream[B] = s match
      case Cons(h, t) => cons(f(h()), map(t())(f))
      case _ => Empty()

    def filter[A](s: Stream[A])(p: A => Boolean): Stream[A] = s match
      case Cons(h, t) if p(h()) => cons(h(), filter(t())(p))
      case Cons(_, t) => filter(t())(p)
      case _ => Empty()

    def take[A](s: Stream[A])(n: Int): Stream[A] = (s, n) match
      case (Cons(h, t), n) if n > 0 => cons(h(), take(t())(n - 1))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    def fromList[A](lst: List[A]): Stream[A] = lst match
      case h :: t => cons(h, fromList(t))
      case Nil => empty()

    def takeWhile[A](s: Stream[A])(p: A => Boolean): Stream[A] = s match
      case Cons(h, t) =>
        val v = h()
        if p(v) then cons(v, takeWhile(t())(p))
        else Empty()
      case _ => Empty()

    def fill[A](n: Int)(k: => A): Stream[A] =
      if n <= 0 then empty()
      else cons(k, fill(n - 1)(k))

    val fibonacci: Stream[Int] =
      def loop(a: Int, b: Int): Stream[Int] =
        cons(a, loop(b, a + b))
      loop(0, 1)

    def interleave[A](s1: Stream[A], s2: Stream[A]): Stream[A] = s1 match
      case Cons(h, t) => cons(h(), interleave(s2, t()))
      case _ => s2

    def cycle[A](lst: Sequence[A]): Stream[A] =
      def loop(cur: Sequence[A]): Stream[A] = cur match
        case Sequence.Cons(h, t) => cons(h, loop(t))
        case Sequence.Nil() => loop(lst)

      lst match
        case Sequence.Nil() => empty()
        case _ => loop(lst)