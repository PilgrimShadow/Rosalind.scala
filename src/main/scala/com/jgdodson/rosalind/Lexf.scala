package com.jgdodson.rosalind

object Lexf {

  def main(args: Array[String]): Unit = {

    val lines = io.Source.fromFile(args(0)).mkString.split("\n")
    val alpha = lines(0).split(" ").map(a => a(0))
    val order = makeOrdering(alpha)
    val len = Integer.parseInt(lines(1).trim())

    // Loosen up types to prevent these conversions (all we need is an iterable, right?)
    val words = enumerateFixed(len, alpha.toSet)
    val sorted = words.toVector.sorted(order)
    println(sorted.mkString("\n"))
  }

  /**
   * Returns a set containing every string of given length from the given alphabet.
   *
   * @param len The string length.
   * @param alphabet The string representation of these items form the alphabet.
   * @tparam T The type of items in the alphabet.
   * @return
   */
  def enumerateFixed[T](len: Int, alphabet: Set[T]): Set[String] = {

    assert(len >= 0)

    // Return a set with the given piece prepended to all strings.
    def prependAll(n: T, ss: Set[String]): Set[String] = {
      ss.map(item => n.toString + item)
    }

    if (len <= 0) Set[String]()
    else if (len == 1) alphabet.map(_.toString)
    else {
      // avoid vicious re-computation
      val subSolution = enumerateFixed(len - 1, alphabet)

      alphabet.foldLeft(Set[String]())((acc, next) => acc ++ prependAll(next, subSolution))
    }
  }

  def makeOrdering(alphabet: Seq[Char]): Ordering[String] = new Ordering[String] {

    // Throw error if strings are not from the alphabet?

    def compare(xs: String, ys: String): Int = {

      // Could throw an error if strings are not upper/lower case instead of casefolding.

      val xLen = xs.length
      val yLen = ys.length
      val lim = math.min(xLen, yLen)

      for (i <- 0 until lim) {

        if (alphabet.indexOf(xs(i)) < alphabet.indexOf(ys(i))) return -1
        else if (alphabet.indexOf(xs(i)) > alphabet.indexOf(ys(i))) return 1
      }

      // If the intersection of the strings was equal, we compare the lengths.
      if (xLen < yLen) -1
      else if (yLen < xLen) 1
      else 0
    }
  }

}
