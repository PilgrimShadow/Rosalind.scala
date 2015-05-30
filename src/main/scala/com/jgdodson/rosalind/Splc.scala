package com.jgdodson.rosalind

import utils.Utils

object Splc {

  def main(args: Array[String]): Unit = {
    val strings = Utils.readFastaFile(args(0)).map(_._2)
    val rawDNA = strings.head
    val introns = strings.tail
    val codingDNA = splice(rawDNA, introns)
    val mRNA = Rna.transcribe(codingDNA)
    val aminos = Prot.translate(mRNA)
    println(aminos)
  }

  def splice(rna: String, introns: Vector[String]): String = {
    rna.split(introns.mkString("|")).mkString("")
  }

}
