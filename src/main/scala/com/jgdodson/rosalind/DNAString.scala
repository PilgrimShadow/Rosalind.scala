package com.jgdodson.rosalind

case class DNAString(seq: String) extends GeneticString {

  // Error checking during initialization.
  if (seq.exists(ch => !alphabet.contains(ch))) {
    throw new Error("DNA contains non-ACGT character")
  }

  def alphabet: Seq[Char] = DNAString.alphabet

  val length = seq.length

  def masses = DNAString.masses

  def toRNAString: RNAString = RNAString(seq.replace('T', 'U'))

  def complement: DNAString = DNAString(seq.map(DNAString.complements))

  def reverse: DNAString = DNAString(seq.reverse)

  def reverseComplement: DNAString = DNAString(seq.reverseMap(DNAString.complements))

  def restrictionSites: Set[(Int, Int)] = {

    (0 until seq.length).foldLeft(Set[(Int, Int)]()) { (acc, i) =>

      val res = (4 to 12 by 2).filter { j =>

        val end = i + j

        if (end > seq.length) false
        else {
          val test = seq.substring(i, end)
          test.reverseMap(DNAString.complements) == test
        }
      }

      // Convert to one-based indexing
      acc ++ res.map(entry => (i + 1, entry)).toSet
    }

  }

}

object DNAString {

  val alphabet: Seq[Char] = Seq('A', 'C', 'G', 'T')

  // Might need to change these
  val masses = Map(
    'A' -> 135.054489,
    'C' -> 111.043259,
    'G' -> 151.049408,
    'T' -> 126.042931)

  val complements = Map(
    'A' -> 'T',
    'C' -> 'G',
    'G' -> 'C',
    'T' -> 'A'
  )

}
