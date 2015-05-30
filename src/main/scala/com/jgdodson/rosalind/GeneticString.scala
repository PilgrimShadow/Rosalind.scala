package com.jgdodson.rosalind

abstract class GeneticString {

  val seq: String

  def alphabet: Seq[Char]

  def masses: Map[Char, Double]

  def mass: Double = seq.foldLeft(0.0)(_ + masses(_))

  def reverse: GeneticString

  def failureArray: Vector[Int] = {

    (1 until seq.length).foldLeft((Vector[Int](0), Set[Int](0))) { (acc, next) =>
      val updated = acc._2.filter(i => seq(next) == seq(i)).map(i => i + 1) + 0
      (acc._1 :+ updated.max, updated)
    }._1
  }

}
