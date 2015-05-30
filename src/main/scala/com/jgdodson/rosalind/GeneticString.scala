package com.jgdodson.rosalind

abstract class GeneticString {

  val seq: String

  // The order of the characters is significant as it is used as a default
  // ordering when manipulating sequences.
  def alphabet: Seq[Char]

  def length: Int = seq.length

  def masses: Map[Char, Double]

  def mass: Double = seq.foldLeft(0.0)(_ + masses(_))

  def reverse: GeneticString

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
}
