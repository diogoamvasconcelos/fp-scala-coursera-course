case class Book(title: String, authors: List[String])

val books: List[Book] = List(
    Book(title = "Structure and Interpretation of Computer Programs",
        authors = List("Abelson, Harald", "Sussman, Gerald J.")),
    Book(title = "Introduction to Functional Programming",
        authors = List("Bird, Richard", "Wadler, Phil")),
    Book(title = "Introduction to Functional Programming 2",
        authors = List("Some, Guy", "Bird, Richard")),
    Book(title = "Introduction to Functional Programming 3",
        authors = List("Some other, Guy", "Bird, Richard")),
    Book(title = "Programming in Scala",
        authors = List("Odersky, Martin", "Spoon, Lex", "Venners, Bill")))


//find all title of books whose authors name is "Bird"
for (b <- books; a <- b.authors if a startsWith "Bird")
    yield b.title

// find the names of all authors who have written at least two books
// this solution has the problem of returning duplicated results
for {
    b1 <- books
    b2 <- books
    if b1 != b2
    a1 <- b1.authors
    a2 <- b2.authors
    if (a1 == a2)
} yield a1


//fix, make sure you dont check the same pairs by adding a order condition (on the title)
//this wont work if author has published 3 books (will print 3 times) (works if you comment intro to FP3)
for {
    b1 <- books
    b2 <- books
    if b1.title < b2.title
    a1 <- b1.authors
    a2 <- b2.authors
    if (a1 == a2)
} yield a1

//use distinct function (can be applied to Seq) to remove duplicates
//Or use Sets instead!! (event the Database should be a Set and not a List
{
    for
        {
        b1 <- books
        b2 <- books
        if b1.title < b2.title
        a1 <- b1.authors
        a2 <- b2.authors
        if (a1 == a2)
    } yield a1
}.distinct


// translate the first query using high order functions (map, flatMap, filter)

books.flatMap(b =>
        b.authors.withFilter(_ startsWith "Bird").map (a => b.title))

