package Rosalind

import scala.collection.mutable
import scala.io.Source

/**
 * Created with IntelliJ IDEA.
 * User: andreabirotti
 * Date: 21/07/2013
 * Time: 01:41
 * To change this template use File | Settings | File Templates.
 */
object Stronghold {

  def readFastaIntoMap(): Map[String, String] ={
    val seqs: scala.collection.mutable.Map[String, String] = scala.collection.mutable.Map.empty
    var key: String = ""
    for(line<-Source.fromFile("GC.fasta").getLines().toList)
      line.charAt(0) match {
        case '>' => seqs += line.substring(1) -> ""; key = line.substring(1) ;
        case _ => seqs.put(key, seqs(key)+line)
      }
    seqs.toMap
  }
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

  /**
   * Improve by applying while reversing with a fold left
   *
   * @param sequence
   * @return
   */
  //TODO: improve with :\
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

  /**
   * memoized fibonacci with a twist
   * @param n
   * @param k
   * @return
   */
  //TODO: use streams?
  def FIB(n: Int, k: Int): Long = {
    val memo = new Array[Long](n)
    def fib(x: Int): Long = x match {
      case 1 => {memo{0}=1; 1}
      case 2 => {memo(1)=1; 1}
      case _ => {
        if(memo(x-1) != 0)
          memo(x-1)
        else {
          memo(x-1) = k*fib(x-2) + fib(x-1)
          memo(x-1)
        }
      }
    }
    fib (n)
  }

  def gcRatio(ss: String):Float = ss.toCharArray.count(Set('C','G')).toFloat*100 / ss.size.toFloat

  def GC(mapz: Map[String, String]): String = {
    val values = mapz.mapValues(x => gcRatio(x)).maxBy(_._2)
    values._1+"\n"+values._2
  }

}
