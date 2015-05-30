package com.jgdodson.rosalind

object Subs {

  def main(args: Array[String]): Unit = {
    val strings = io.Source.fromFile(args(0)).mkString.split("\n")
    println(findSubs(strings(1), strings(0)).mkString(" "))
  }

  // write up notes on looping and foldLeft
  def findSubs(sub: String, str: String): Vector[Int] = {

    val subLen = sub.length

    val strLen = str.length

    (0 to strLen - subLen).foldLeft(Vector[Int]()) { (indices, i) =>
      if (str.substring(i, i + subLen) == sub) indices :+ (i + 1)
      else indices
    }
  }

}
