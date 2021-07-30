package crc

import java.lang._

object CRC {

  /** Converts binary string to array of integers
  *
  *  @param binString binary string that should be converted to array of integers
  */
  def convertBinStringToIntArray(binString: String): Seq[Int] = {
    var intArray: Seq[Int] = Seq()
    for (i <- 0 until binString.length) {
      intArray = intArray :+ Integer.parseInt(String.valueOf(binString.charAt(i)))
    }
//     for (j <-0 until binString.length) {
//       System.out.println(intArray(j));
//     }
    intArray
  }

  /** Converts both negative and positive numbers to their binary representation
  *
  *  @param x integer that should be converted to binary string
  *  @param len number of bits used to represent x
  */
  def toBinaryInternal(x: Int, len: Int): String = {
    val result = new StringBuilder();
    for (i <- len - 1 to 0 by -1)
    {
      val mask = 1 << i
      result.append(if ((x & mask) != 0) 1 else 0)
    }
    result.toString()
  }

  def serialCRC(poly: String, inBit: Int = 1, oldCRC: Int): Int = {
    val count = poly.toCharArray.length
    var returnVal: Int = 0
    val polySeq: Seq[Int] = convertBinStringToIntArray(poly.reverse.tail) // to get 32 bits

    val crcSeq: Seq[Int]  = convertBinStringToIntArray(toBinaryInternal(oldCRC, count-1).reverse)
    var newCrc: Seq[Int] = Seq()
    newCrc = newCrc :+ (inBit ^ crcSeq.last)
    for (i <- 1 until (count - 1)) {
      if (polySeq(i-1) == 1) {
        newCrc = newCrc :+ (crcSeq(i-1) ^ inBit ^ crcSeq.last)
      }
      else {
        //println(crcSeq(i-1))
        newCrc = newCrc :+ crcSeq(i-1)
      }
    }
    val binStringCRC: String = newCrc.map(
                                  c => if (c == 0) "0" else "1").fold("")((acc, bit) => bit + acc)
    if (binStringCRC.last == 1) {
      returnVal = (Long.parseLong(binStringCRC, 2) - (1 << count).toLong).toInt
    }
    else {
      returnVal = (Long.parseLong(binStringCRC, 2)).toInt
    }
    returnVal
  }

  def parallelCRC(poly: String, inBits: Int, inDataWidth: Int, oldCRC: Int, msbFirst: Boolean) : Int = {
    val inBitsSeq: Seq[Int] = convertBinStringToIntArray(toBinaryInternal(inBits, inDataWidth))
    var newCrc = oldCRC
    for (i <-0 until inDataWidth) {
      if (msbFirst) {
        newCrc = serialCRC(poly, inBitsSeq(inDataWidth - 1 - i), newCrc)
      }
      else {
        newCrc = serialCRC(poly, inBitsSeq(i), newCrc)
      }
    }
    newCrc
  }

  /*
  def serialCRCwithMap(poly: String, in_bit: Int = 1, crc: Int): Seq[Int] = {
  }

  def parallelCRCwithMap(poly: String, in_bit: Int = 1, crc: Int): Seq[Int] = {
  }*/
}

object TestCRC extends App {

  //0x4c11db7
  val poly = "100000100110000010001110110110111"
  val inDataWidth = 8
  val in = 15
  val crcInit = -1 // ffffffff
  val msbFirst = false

  val serialCRCresult1 = CRC.serialCRC(poly, 0, -1)
  println(serialCRCresult1)
  val serialCRCresult2 = CRC.serialCRC(poly, 0, serialCRCresult1)
  println(serialCRCresult2)
  val parallelCRCresult = CRC.parallelCRC(poly, in, inDataWidth, crcInit, msbFirst)
  println(parallelCRCresult)
}
