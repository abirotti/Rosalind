package Rosalind

import scala.collection.mutable
import scala.io.Source
import scala.annotation.tailrec

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

  def HAMM(seq1: String, seq2: String): Int = {
    var occu = 0
    for((c1, c2) <- seq1 zip seq2) {
      if (c1 != c2)
        occu += 1
    }
    occu
  }

  def HAMM2(s1: String, s2: String):Int =
    ((s1 zip s2) foldLeft 0 )((ac, elm) => if(elm._1 !=elm._2) ac + 1 else ac)

  /**
   * Calculates the probability of an outcome possessing a dominant allele.
   * @param k Number of elements in a population of homozygous dominant for a factor
   * @param m Number of elements in a population of heterozygous
   * @param n Number of elements in a population of homozygous recessive
   * @return
   */
  def IPRB(k: Int, m: Int, n: Int):String = {
    ""
  }

  /**
   * Returns the indices at which t is contained in s.
   * @param s A DNA sequence to run comparisons on
   * @param t A DNA sequence that is contained within s
   * @return the indices at which a full t is contained in s
   */
  def SUBS(s: String, t:String): List[Int] = {
    @tailrec
    def i_SUBS(acc:List[Int], st:String, base:Int): List[Int] =
      st.indexOf(t) match {
        case -1 => acc.reverse
        case x => i_SUBS((x + base)::acc, st.substring(x + 1), base + x + 1)
      }
    i_SUBS(Nil, s, 1)
  }

  val codonTable = Map(
       "UUU" -> "F"      ,"CUU" -> "L"      ,"AUU" -> "I"      ,"GUU" -> "V"
      ,"UUC" -> "F"      ,"CUC" -> "L"      ,"AUC" -> "I"      ,"GUC" ->"V"
      ,"UUA" -> "L"      ,"CUA" -> "L"      ,"AUA" -> "I"      ,"GUA" -> "V"
      ,"UUG" -> "L"      ,"CUG" -> "L"      ,"AUG" -> "M"      ,"GUG" -> "V"
      ,"UCU" -> "S"      ,"CCU" -> "P"      ,"ACU" -> "T"      ,"GCU" -> "A"
      ,"UCC" -> "S"      ,"CCC" -> "P"      ,"ACC" -> "T"      ,"GCC" -> "A"
      ,"UCA" -> "S"      ,"CCA" -> "P"      ,"ACA" -> "T"      ,"GCA" -> "A"
      ,"UCG" -> "S"      ,"CCG" -> "P"      ,"ACG" -> "T"      ,"GCG" -> "A"
      ,"UAU" -> "Y"      ,"CAU" -> "H"      ,"AAU" -> "N"      ,"GAU" -> "D"
      ,"UAC" -> "Y"      ,"CAC" -> "H"      ,"AAC" -> "N"      ,"GAC" -> "D"
      ,"UAA" -> "Stop"   ,"CAA" -> "Q"      ,"AAA" -> "K"      ,"GAA" -> "E"
      ,"UAG" -> "Stop"   ,"CAG" -> "Q"      ,"AAG" -> "K"      ,"GAG" -> "E"
      ,"UGU" -> "C"      ,"CGU" -> "R"      ,"AGU" -> "S"      ,"GGU" -> "G"
      ,"UGC" -> "C"      ,"CGC" -> "R"      ,"AGC" -> "S"      ,"GGC" -> "G"
      ,"UGA" -> "Stop"   ,"CGA" -> "R"      ,"AGA" -> "R"      ,"GGA" -> "G"
      ,"UGG" -> "W"      ,"CGG" -> "R"      ,"AGG" -> "R"      ,"GGG" -> "G"
  )

  val monoisotopicMass = Map(
  'A ->  71.03711,
  'C ->  103.00919,
  'D ->  115.02694,
  'E ->  129.04259,
  'F ->  147.06841,
  'G ->  57.02146,
  'H ->  137.05891,
  'I ->  113.08406,
  'K ->  128.09496,
  'L ->  113.08406,
  'M ->  131.04049,
  'N ->  114.04293,
  'P ->  97.05276,
  'Q ->  128.05858,
  'R ->  156.10111,
  'S ->  87.03203,
  'T ->  101.04768,
  'V ->  99.06841,
  'W ->  186.07931,
  'Y ->  163.06333
  )

  val waterMass = 18.01056

  /**
   * Returns the protein string encoded by the provided mRNA
   * @param s mRNA to translate
   */
  def PROT(s: String) = {
    val prot = (s grouped(3) toList) map(x => codonTable(x))
    val lastIndex = (prot size) - 1
    if (prot (lastIndex) == "Stop")
      prot dropRight 1 mkString
    else ""
  }

  /**
   * Returns the number of mRNA strings that are possible generators for
   * the given protein
   * @param protein
   * @return
   */
  def MRNA(protein: String): Int = {
    val stops = codonTable.values.filter( _.toLowerCase  == "stop") size
    val mil: Int = 1000000
    //    var res = 1
//    for( split <- (protein grouped(1) toList)) {
//      res = res * (codonTable.values.filter(_ == split) size)
//    }
    val res = ((protein grouped 1 toList) foldLeft 1) ((ac, elm) => ac%mil * ((codonTable.values.filter(_ == elm) size)%mil) )
    //println ( "ASD: "+res * stops % mil)

    (res * stops) % mil
  }

  def PRTM(protein: String): Double = {
    val res = ((protein grouped 1 toList) foldLeft 0d) ((acc, element) => acc + monoisotopicMass(Symbol(element)))
    println(res)

    BigDecimal(res).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
  }
}
