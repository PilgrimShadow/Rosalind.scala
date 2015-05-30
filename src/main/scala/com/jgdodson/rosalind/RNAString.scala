package com.jgdodson.rosalind

case class RNAString(seq: String) extends GeneticString {

  // Error checking during initialization.
  if (seq.exists(ch => !alphabet.contains(ch))) {
    throw new Error("DNA contains non-ACGT character")
  }

  val alphabet = RNAString.alphabet

  val length = seq.length

  def masses = RNAString.masses

  def toDNAString: DNAString = DNAString(seq.replace('U', 'T'))

  def toProteinString: ProteinString = ProteinString(seq.grouped(3).map(RNAString.rnaCodonTable).toString)

  def splice(introns: Seq[RNAString]): RNAString = {
    RNAString(seq.split(introns.map(_.seq).mkString("|")).mkString(""))
  }

  def candidateProteins: Set[(Int, Int)] = {

    def isStartCodon(codon: String): Boolean = {
      RNAString.rnaCodonTable(codon) == 'M'
    }

    def isStopCodon(codon: String): Boolean = {
      RNAString.rnaCodonTable(codon) == 'X'
    }

    def oneway(seq: String) = {
      (1 to seq.length - 3).foldLeft((for (_ <- 0 to 2) yield Set[(Int, Int)](), Set[(Int, Int)]())) { (acc, i) =>
        val binNum = i % 3
        val codon = seq.substring(i, i + 3)
        if (isStartCodon(codon)) {
          (acc._1.updated(binNum, acc._1(binNum).map(p => (p._1, p._2 + 1)) + ((i, i))), acc._2)
        } else if (isStopCodon(codon)) {
          (acc._1.updated(binNum, Set[(Int, Int)]()), acc._2 ++ acc._1(binNum))
        } else {
          (acc._1.updated(binNum, acc._1(binNum).map(p => (p._1, p._2 + 1))), acc._2)
        }
      }._2
    }

    oneway(seq) ++ oneway(seq.reverse)
  }

  def reverse: RNAString = RNAString(seq.reverse)


}

object RNAString {

  val alphabet = Seq('A', 'C', 'G', 'U')

  val masses = Map(
    'A' -> 135.054489,
    'C' -> 111.043259,
    'G' -> 151.049408,
    'U' -> 112.027275
  )

  // X is used for stop codons
  val rnaCodonTable: Map[String, Char] =
    Map(
      "UUU" -> 'F', "CUU" -> 'L', "AUU" -> 'I', "GUU" -> 'V',
      "UUC" -> 'F', "CUC" -> 'L', "AUC" -> 'I', "GUC" -> 'V',
      "UUA" -> 'L', "CUA" -> 'L', "AUA" -> 'I', "GUA" -> 'V',
      "UUG" -> 'L', "CUG" -> 'L', "AUG" -> 'M', "GUG" -> 'V',
      "UCU" -> 'S', "CCU" -> 'P', "ACU" -> 'T', "GCU" -> 'A',
      "UCC" -> 'S', "CCC" -> 'P', "ACC" -> 'T', "GCC" -> 'A',
      "UCA" -> 'S', "CCA" -> 'P', "ACA" -> 'T', "GCA" -> 'A',
      "UCG" -> 'S', "CCG" -> 'P', "ACG" -> 'T', "GCG" -> 'A',
      "UAU" -> 'Y', "CAU" -> 'H', "AAU" -> 'N', "GAU" -> 'D',
      "UAC" -> 'Y', "CAC" -> 'H', "AAC" -> 'N', "GAC" -> 'D',
      "UAA" -> 'X', "CAA" -> 'Q', "AAA" -> 'K', "GAA" -> 'E',
      "UAG" -> 'X', "CAG" -> 'Q', "AAG" -> 'K', "GAG" -> 'E',
      "UGU" -> 'C', "CGU" -> 'R', "AGU" -> 'S', "GGU" -> 'G',
      "UGC" -> 'C', "CGC" -> 'R', "AGC" -> 'S', "GGC" -> 'G',
      "UGA" -> 'X', "CGA" -> 'R', "AGA" -> 'R', "GGA" -> 'G',
      "UGG" -> 'W', "CGG" -> 'R', "AGG" -> 'R', "GGG" -> 'G'
    )

}