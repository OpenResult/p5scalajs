package lexographicpermutations



import utest._


object LPSuite extends TestSuite:
  private def permutations(head: String, tail: String): LazyList[String] =
    if tail.isEmpty() then
      LazyList(head)
    else
      tail
        .toCharArray()
        .to(LazyList)
        .flatMap(c =>
        permutations(head + c, tail.filterNot(_ == c))  
      )
  def permutations(string: String): Seq[String] =
    val sorted = string.sorted
    permutations("", sorted)

  val tests = Tests {
    test("example") {
      val first = permutations("120").take(6)
      first ==> Seq("012", "021", "102", "120", "201", "210")
    }
    test("millionth") {
      val millionth = permutations("1234567890")
        .drop(999999)
        .take(1)
        .mkString

      millionth ==> "2783915460"
    }
  }
