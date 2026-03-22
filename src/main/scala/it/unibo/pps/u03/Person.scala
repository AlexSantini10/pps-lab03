package u03

enum Person:
  case Student(name: String, year: Int)
  case Teacher(name: String, course: String)