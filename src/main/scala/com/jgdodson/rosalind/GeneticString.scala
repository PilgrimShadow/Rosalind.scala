package com.jgdodson.rosalind

// It's weired how the types work out just right... (Curiously Recursive Template Pattern)
abstract class GeneticString[T <: GeneticString[T]] {

  val seq: String

  // The order of the characters is significant as it is used as a default
  // ordering when manipulating sequences.
  def alphabet: Seq[Char]

  def length: Int = seq.length

  def masses: Map[Char, Double]

  def mass: Double = seq.foldLeft(0.0)(_ + masses(_))

  def reverse: GeneticString[T]

  // TODO: convert this into an explicit motif finding function
  def failureArray: Vector[Int] = {

    (1 until seq.length).foldLeft((Vector[Int](0), Set[Int](0))) { (acc, next) =>
      val updated = acc._2.filter(i => seq(next) == seq(i)).map(i => i + 1) + 0
      (acc._1 :+ updated.max, updated)
    }._1
  }

  def kmerComposition(k: Int): Seq[Int] = {

    val indices = Lexf.enumerateFixed(k, alphabet).zipWithIndex.toMap

    (0 to length - k).foldLeft(for (_ <- Seq.range(0, indices.size)) yield 0) { (acc, next) =>
      val index = indices(seq.substring(next, next + k))
      acc.updated(index, acc(index) + 1)
    }
  }

  def hammingDistance(other: T): Int = {
    val min = Math.min(length, other.length)
    val max = Math.max(length, other.length)

    (0 until min).count(i => seq(i) != other.seq(i)) + (max - min)
  }

  def findSplicedMotif(motif: T): Option[Vector[Int]] = {

    val res = (0 until seq.length).foldLeft(motif.seq, Vector[Int]()) { (acc, i) =>
      if (acc._1.isEmpty) acc
      else if (seq(i) == acc._1.head) (acc._1.tail, acc._2 :+ i)
      else acc
    }

    if (res._1.isEmpty) Some(res._2)
    else None
  }
}
