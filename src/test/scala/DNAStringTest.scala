import com.jgdodson.rosalind.DNAString
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen

object DNAStringTest extends Properties("DNAString") {

  val ACGTGenerator: Gen[String] = Gen.containerOf[Vector, Char](Gen.oneOf('A', 'C', 'G', 'T')).map(_.mkString(""))

  val DNAGenerator: Gen[DNAString] = ACGTGenerator.map(DNAString(_))

  property("length") = forAll(ACGTGenerator) { raw =>
    DNAString(raw).length == raw.length
  }

  property("reverse-length") = forAll(DNAGenerator) { dna =>
    dna.length == dna.reverse.length
  }

  property("complement-length") = forAll(DNAGenerator) { dna =>
    dna.length == dna.complement.length
  }

  property("reverseComplement-length") = forAll(DNAGenerator) { dna =>
    dna.length == dna.reverseComplement.length
  }

  property("complement-gcContent") = forAll(DNAGenerator) { dna =>
    dna.gcContent == dna.complement.gcContent
  }

  property("complement-atContent") = forAll(DNAGenerator) { dna =>
    dna.atContent == dna.complement.atContent
  }

  property("sum-AT/GC") = forAll(DNAGenerator) { dna =>
    val sum = dna.gcContent + dna.atContent
    if (dna.length > 0) sum == 1
    else sum == 0
  }

}
