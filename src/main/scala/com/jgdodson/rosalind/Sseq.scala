package com.jgdodson.rosalind

import utils.Utils.readFastaFile

object Sseq {

  def main(args: Array[String]): Unit = {

    val strings = readFastaFile(args(0)).map(_._2)
    val res = getIndices(strings(0), strings(1)).map(i => i + 1).mkString(" ")
    println(res)
  }

  def getIndices(seq: String, subSeq: String): Vector[Int] = {

    (0 until seq.length).foldLeft(subSeq, Vector[Int]()) { (acc, i) =>
      if (acc._1.isEmpty) acc
      else if (seq(i) == acc._1.head) (acc._1.tail, acc._2 :+ i)
      else acc
    }._2

  }

}
