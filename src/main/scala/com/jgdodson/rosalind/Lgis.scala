package com.jgdodson.rosalind

import utils.Utils.readLines

object Lgis {

  def main(args: Array[String]): Unit = {

    val seq = readLines(args(0)).tail.flatMap(_.split(" ")).map(_.toInt)

    println(longestMonotonic2(seq)(_ > _).mkString(" "))
    println(longestMonotonic2(seq)(_ < _).mkString(" "))
  }

  // Only need one subsequence of a particular length at any point in the computation!!!
  // The one with the smallest last element is the one that survives. Use a map from length to seq.
  def longestMonotonic1(seq: Vector[Int])(cmp: (Int, Int) => Boolean): Vector[Int] = {

    assert(seq.nonEmpty)

    seq.tail.foldLeft(Set(Vector(seq.head))) { (acc, next) =>

      var flag = false

      val tmp = acc.foldLeft(Set[Vector[Int]]()) { (acc, subseq) =>
        if (cmp(next, subseq.last)) {
          flag = true
          (acc + (subseq :+ next)) + subseq
        }
        else acc + subseq
      }

      if (flag) tmp
      else tmp + Vector(next)
    } maxBy (_.length)
  }

  // Need to traverse map by decreasing key to prevent masking (use a range)
  def longestMonotonic2(seq: Vector[Int])(cmp: (Int, Int) => Boolean): Vector[Int] = {

    assert(seq.nonEmpty)

    (seq.tail.foldLeft(Map[Int, Vector[Int]](1 -> Vector(seq.head))) { (acc, i) =>

      (acc.size to 1 by -1).foldLeft(acc) { (acc, len) =>

        if (cmp(i, acc(len).last)) {

          if (acc.isDefinedAt(len + 1)) {

            if (!cmp(i, acc(len + 1).last)) {
              acc + ((len + 1) -> (acc(len) :+ i))
            } else {
              acc
            }
          } else {
            acc + ((len + 1) -> (acc(len) :+ i))
          }
        } else if (len == 1) {
          acc + (1 -> Vector(i))
        } else {
          acc
        }
      }
    } maxBy (_._2.length))._2
  }
}
