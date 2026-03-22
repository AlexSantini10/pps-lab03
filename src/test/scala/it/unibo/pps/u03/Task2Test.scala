package it.unibo.pps.u03

import org.junit.Test
import org.junit.Assert.*

import u03.Sequences.*
import u03.Sequences.Sequence.*
import u03.Person
import u03.Person.*

class Task2Test:

  @Test
  def teacherCoursesEmpty(): Unit =
    // Arrange
    val input: Sequence[Person] = Nil()

    // Act
    val result = Task2.teacherCourses(input)

    // Assert
    assertEquals(Nil(), result)

  @Test
  def teacherCoursesMixed(): Unit =
    // Arrange
    val input: Sequence[Person] =
      Cons(
        Teacher("Viroli", "PPS"),
        Cons(
          Student("Mario", 2024),
          Cons(Teacher("Ricci", "PCD"), Nil())
        )
      )

    // Act
    val result = Task2.teacherCourses(input)

    // Assert
    assertEquals(
      Cons("PPS", Cons("PCD", Nil())),
      result
    )

  @Test
  def teacherCoursesOnlyStudents(): Unit =
    // Arrange
    val input: Sequence[Person] =
      Cons(
        Student("Mario", 2024),
        Cons(Student("Luigi", 2023), Nil())
      )

    // Act
    val result = Task2.teacherCourses(input)

    // Assert
    assertEquals(Nil(), result)

  @Test
  def foldLeftEmpty(): Unit =
    // Arrange
    val input: Sequence[Int] = Nil()

    // Act
    val result = Task2.foldLeft(input)(10)(_ + _)

    // Assert
    assertEquals(10, result)

  @Test
  def foldLeftSubtractExample(): Unit =
    // Arrange
    val input =
      Cons(
        3,
        Cons(
          7,
          Cons(
            1,
            Cons(5, Nil())
          )
        )
      )

    // Act
    val result = Task2.foldLeft(input)(0)(_ - _)

    // Assert
    assertEquals(-16, result)

  @Test
  def foldLeftSum(): Unit =
    // Arrange
    val input =
      Cons(
        3,
        Cons(
          7,
          Cons(
            1,
            Cons(5, Nil())
          )
        )
      )

    // Act
    val result = Task2.foldLeft(input)(0)(_ + _)

    // Assert
    assertEquals(16, result)

  @Test
  def differentCoursesEmpty(): Unit =
    // Arrange
    val input: Sequence[Person] = Nil()

    // Act
    val result = Task2.differentCourses(input)

    // Assert
    assertEquals(0, result)

  @Test
  def differentCoursesOnlyStudents(): Unit =
    // Arrange
    val input: Sequence[Person] =
      Cons(
        Student("Mario", 2024),
        Cons(Student("Luigi", 2023), Nil())
      )

    // Act
    val result = Task2.differentCourses(input)

    // Assert
    assertEquals(0, result)

  @Test
  def differentCoursesCountsDistinctTeacherCourses(): Unit =
    // Arrange
    val input: Sequence[Person] =
      Cons(
        Teacher("Viroli", "PPS"),
        Cons(
          Student("Mario", 2024),
          Cons(
            Teacher("Ricci", "PCD"),
            Cons(
              Teacher("Bianchi", "PPS"),
              Cons(Teacher("Rossi", "AI"), Nil())
            )
          )
        )
      )

    // Act
    val result = Task2.differentCourses(input)

    // Assert
    assertEquals(3, result)

  @Test
  def differentCoursesCountsOneWhenAllTeachersHaveSameCourse(): Unit =
    // Arrange
    val input: Sequence[Person] =
      Cons(
        Teacher("Viroli", "PPS"),
        Cons(
          Teacher("Bianchi", "PPS"),
          Cons(Teacher("Rossi", "PPS"), Nil())
        )
      )

    // Act
    val result = Task2.differentCourses(input)

    // Assert
    assertEquals(1, result)