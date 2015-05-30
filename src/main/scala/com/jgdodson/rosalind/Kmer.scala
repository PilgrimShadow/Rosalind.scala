package com.jgdodson.rosalind

import utils.Utils.readFastaFile

object Kmer {

  def main(args: Array[String]): Unit = {

    val seq = readFastaFile(args(0)).head._2
    val ans = composition(seq, 4)
    println(ans.mkString(" "))
  }


  def composition(seq: String, k: Int): Vector[Int] = {

    val alphabet = Vector("A", "C", "G", "T")

    def count(kmer: String, seq: String): Int = {

      val klen = kmer.length

      (0 to seq.length - klen).foldLeft(0) { (acc, i) =>
        if (seq.substring(i, i + klen) == kmer) acc + 1
        else acc
      }
    }

    (2 to k).foldLeft(alphabet) { (acc, next) =>
      acc.flatMap(item => alphabet.map(item + _))
    } map (kmer => count(kmer, seq))
  }


}
