package Rosalind

import org.scalatest.FunSuite
import scala.io.Source

/**
 * Created with IntelliJ IDEA.
 * User: andreabirotti
 * Date: 21/07/2013
 * Time: 15:24
 * To change this template use File | Settings | File Templates.
 */
class StrongholdTest extends FunSuite{

  ignore("count DNA nucleotides") {
    val sequence = "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"
    assert(Stronghold.DNA(sequence) === "20 12 17 21")
  }

  ignore("transcribe DNA into RNA") {
    val sequence = "GATGGAACTTGACTACGTAAATT"
    assert(Stronghold.RNA(sequence) === "GAUGGAACUUGACUACGUAAAUU")
  }

  ignore("(reverse) complement of a strand of DNA") {
    val sequence = "AAAACCCGGT"
    println(Stronghold.REVC(sequence))
    assert(Stronghold.REVC(sequence) === "ACCGGGTTTT")
  }

  ignore("fibonacci rabbits with a twist") {
    assert(Stronghold.FIB(5,3)===19)
  }

  test("Counting GC Content") {


    assert("Rosalind_0808\n60.919540" === Stronghold.GC(Stronghold.readFastaIntoMap))
  }
}
