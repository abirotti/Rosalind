package Rosalind

import scala.collection.mutable

/**
 * Created with IntelliJ IDEA.
 * User: andreabirotti
 * Date: 21/07/2013
 * Time: 01:41
 * To change this template use File | Settings | File Templates.
 */
object Stronghold {

  /**
   * Implementation a bit involved, to only go through the input array once. It performs in linear time.
   * @param sequence
   * @return
   */
  def DNA(sequence: String): String = {
    val nucs: mutable.Map[Char, Int] = new mutable.HashMap[Char, Int]()
    for( c <- sequence.toUpperCase.toList) {
      val occ = nucs.get(c)
      nucs.put(c, occ.getOrElse(0) + 1)
    }
    nucs.get('A').getOrElse(0)+" "+nucs.get('C').getOrElse(0)+" "+nucs.get('G').getOrElse(0)+" "+nucs.get('T').getOrElse(0)
  }

  def RNA(sequence: String): String = sequence replace('T', 'U')

  def REVC(sequence: String): String = {
    def complement(c: Char) = c match {
      case 'A' => 'T'
      case 'C' => 'G'
      case 'G' => 'C'
      case 'T' => 'A'
      case _ =>
    }
    sequence.toList.map(c => complement(c)).mkString.reverse
  }



}