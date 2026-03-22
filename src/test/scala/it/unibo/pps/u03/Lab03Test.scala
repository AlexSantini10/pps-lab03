package u03

import org.junit.Test
import org.junit.Assert.*
import u03.Lab03.*
import u03.Lab03.Sequence.*
import u03.Lab03.Stream
import u03.Person.*
import u03.Optionals.Optional.*

class Lab03Test:

  // =========================
  // Helpers
  // =========================

  private def seqOf[A](values: A*): Sequence[A] =
    values.foldRight(Nil(): Sequence[A])(Cons(_, _))

  private def streamToSeq[A](s: Stream[A]): Sequence[A] =
    def loop(stream: Stream[A]): Sequence[A] = stream match
      case Stream.Cons(h, t) => Sequence.Cons(h(), loop(t()))
      case Stream.Empty() => Sequence.Nil()
    loop(s)

  // =========================
  // Task 1 - Sequences
  // =========================

  @Test def skipShouldDropFirstNElements(): Unit =
    assertEquals(seqOf(30), Sequence.skip(seqOf(10, 20, 30))(2))

  @Test def skipShouldReturnEmptyWhenNDropsWholeSequence(): Unit =
    assertEquals(seqOf(), Sequence.skip(seqOf(10, 20, 30))(3))

  @Test def skipShouldReturnOriginalSequenceWhenNIsZero(): Unit =
    assertEquals(seqOf(10, 20, 30), Sequence.skip(seqOf(10, 20, 30))(0))

  @Test def zipShouldCombineElementsUntilShortestEnds(): Unit =
    assertEquals(
      seqOf((10, 40), (20, 50)),
      Sequence.zip(seqOf(10, 20, 30), seqOf(40, 50))
    )

  @Test def zipShouldReturnEmptyIfOneSequenceIsEmpty(): Unit =
    assertEquals(seqOf(), Sequence.zip(seqOf(10), seqOf()))

  @Test def concatShouldAppendSecondSequenceToFirst(): Unit =
    assertEquals(
      seqOf(10, 20, 30, 40, 50),
      Sequence.concat(seqOf(10, 20, 30), seqOf(40, 50))
    )

  @Test def reverseShouldReverseSequence(): Unit =
    assertEquals(seqOf(30, 20, 10), Sequence.reverse(seqOf(10, 20, 30)))

  @Test def reverseShouldWorkOnEmptySequence(): Unit =
    assertEquals(seqOf(), Sequence.reverse(seqOf()))

  @Test def flatMapShouldMapAndFlatten(): Unit =
    assertEquals(
      seqOf(10, 11, 20, 21, 30, 31),
      Sequence.flatMap(seqOf(10, 20, 30))(v => seqOf(v, v + 1))
    )

  @Test def flatMapShouldWorkWithIdentityLikeMapper(): Unit =
    assertEquals(
      seqOf(10, 20, 30),
      Sequence.flatMap(seqOf(10, 20, 30))(v => seqOf(v))
    )

  @Test def flatMapShouldReturnEmptyWhenMapperAlwaysReturnsEmpty(): Unit =
    assertEquals(
      seqOf(),
      Sequence.flatMap(seqOf(10, 20, 30))(_ => seqOf())
    )

  @Test def minShouldReturnEmptyOnEmptySequence(): Unit =
    assertEquals(Empty(), Sequence.min(seqOf()))

  @Test def minShouldReturnMinimumElement(): Unit =
    assertEquals(Just(1), Sequence.min(seqOf(10, 1, 30)))

  @Test def evenIndicesShouldKeepElementsAtEvenPositions(): Unit =
    assertEquals(seqOf(10, 30), Sequence.evenIndices(seqOf(10, 20, 30, 40)))

  @Test def containsShouldReturnTrueWhenElementExists(): Unit =
    assertTrue(Sequence.contains(seqOf(10, 20, 30))(20))

  @Test def containsShouldReturnFalseWhenElementDoesNotExist(): Unit =
    assertFalse(Sequence.contains(seqOf(10, 20, 30))(40))

  @Test def distinctShouldRemoveDuplicatesKeepingFirstOccurrenceOrder(): Unit =
    assertEquals(seqOf(10, 20, 30), Sequence.distinct(seqOf(10, 20, 10, 30, 20)))

  @Test def groupShouldGroupContiguousEqualElements(): Unit =
    assertEquals(
      seqOf(seqOf(10), seqOf(20, 20), seqOf(30)),
      Sequence.group(seqOf(10, 20, 20, 30))
    )

  @Test def partitionShouldSplitSequenceByPredicate(): Unit =
    assertEquals(
      (seqOf(20), seqOf(11, 31)),
      Sequence.partition(seqOf(11, 20, 31))(_ % 2 == 0)
    )

  // =========================
  // Task 2 - Person + fold
  // =========================

  @Test def teacherCoursesShouldReturnOnlyTeacherCourses(): Unit =
    val people =
      seqOf(
        Student("Alice", 2024),
        Teacher("Viroli", "PPS"),
        Student("Bob", 2023),
        Teacher("Ricci", "PCD")
      )

    assertEquals(seqOf("PPS", "PCD"), Sequence.teacherCourses(people))

  @Test def teacherCoursesShouldReturnEmptyWhenNoTeachersExist(): Unit =
    val people = seqOf(Student("Alice", 2024), Student("Bob", 2023))
    assertEquals(seqOf(), Sequence.teacherCourses(people))

  @Test def foldLeftShouldAccumulateFromLeft(): Unit =
    assertEquals(-16, Sequence.foldLeft(seqOf(3, 7, 1, 5))(0)(_ - _))

  @Test def foldLeftShouldSupportDifferentAccumulatorType(): Unit =
    assertEquals("123", Sequence.foldLeft(seqOf(1, 2, 3))("")(_ + _.toString))

  @Test def differentCoursesShouldCountDistinctTeacherCourses(): Unit =
    val people =
      seqOf(
        Teacher("Viroli", "PPS"),
        Teacher("Aguzzi", "PPS"),
        Teacher("Ricci", "PCD"),
        Student("Alice", 2024)
      )

    assertEquals(2, Sequence.differentCourses(people))

  @Test def differentCoursesShouldReturnZeroWhenNoTeachersExist(): Unit =
    val people = seqOf(Student("Alice", 2024), Student("Bob", 2023))
    assertEquals(0, Sequence.differentCourses(people))

  // =========================
  // Task 3 - Streams
  // =========================

  @Test def takeWhileShouldReturnLongestPrefixMatchingPredicate(): Unit =
    val stream = Stream.iterate(0)(_ + 1)
    assertEquals(seqOf(0, 1, 2, 3, 4), streamToSeq(Stream.takeWhile(stream)(_ < 5)))

  @Test def fillShouldCreateFiniteStreamOfRepeatedElements(): Unit =
    assertEquals(seqOf("a", "a", "a"), streamToSeq(Stream.fill(3)("a")))

  @Test def fibonacciShouldProduceCorrectFirstFiveValues(): Unit =
    assertEquals(seqOf(0, 1, 1, 2, 3), streamToSeq(Stream.take(Stream.fibonacci)(5)))

  @Test def interleaveShouldAlternateElementsAndAppendRemainingTail(): Unit =
    val s1 = Stream.fromList(List(1, 3, 5))
    val s2 = Stream.fromList(List(2, 4, 6, 8, 10))

    assertEquals(
      seqOf(1, 2, 3, 4, 5, 6, 8, 10),
      streamToSeq(Stream.interleave(s1, s2))
    )

  @Test def cycleShouldRepeatFiniteSequenceIndefinitely(): Unit =
    val repeated = Stream.cycle(seqOf('a', 'b', 'c'))
    assertEquals(seqOf('a', 'b', 'c', 'a', 'b'), streamToSeq(Stream.take(repeated)(5)))

  @Test def cycleShouldReturnEmptyOnEmptySequence(): Unit =
    assertEquals(seqOf(), streamToSeq(Stream.take(Stream.cycle(seqOf[Char]()))(5)))