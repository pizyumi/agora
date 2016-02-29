package Common

import java.io.BufferedReader
import java.io.BufferedWriter
import java.io.ByteArrayOutputStream
import java.math.BigInteger
import java.nio.ByteBuffer
import java.nio.charset.Charset
import java.nio.file.Files
import java.nio.file.FileSystem
import java.nio.file.FileSystems
import java.nio.file.Path
import java.security.MessageDigest
import java.text.SimpleDateFormat
import java.util.Date
import javax.xml.bind.DatatypeConverter

import scala.collection.mutable.ListBuffer
import scala.util.Random
import scala.util.control.Exception

object __ {
  lazy val utf8: String = "UTF-8"
  lazy val sha256: String = "SHA-256"

  lazy val mdsha256: MessageDigest = MessageDigest.getInstance(sha256)

  lazy val fs: FileSystem = FileSystems.getDefault
  lazy val extensionHTML: String = ".htm"
  lazy val extensionTXT: String = ".txt"
  lazy val extensionDOT: String = ".dot"
  lazy val extensionSVG: String = ".svg"

  lazy val mimetypeHTML: String = "text/html"
  lazy val mimetypeSVG: String = "image/svg+xml"

  lazy val millisecond: String = "ms"

  lazy val colorRed: String = "red"

  def getBytes(in: Int): Array[Byte] = {
    val buf: ByteBuffer = ByteBuffer.allocate(4)
    buf.putInt(in)
    buf.array()
  }
  def getBytes(in: Long): Array[Byte] = {
    val buf: ByteBuffer = ByteBuffer.allocate(8)
    buf.putLong(in)
    buf.array()
  }
  def getBytes(in: String): Array[Byte] = in.getBytes(utf8)
  def getBytes(in: Traversable[Array[Byte]]): Array[Byte] = {
    val stream: ByteArrayOutputStream = new ByteArrayOutputStream()
    for (i <- in) {
      stream.write(i)
    }
    stream.toByteArray
  }

  def tryToInt(in: String) = Exception.catching(classOf[NumberFormatException]) opt in.toInt
  def tryToLong(in: String) = Exception.catching(classOf[NumberFormatException]) opt in.toLong

  def getSha256(in: Array[Byte]): Array[Byte] = mdsha256.digest(in)

  def toHexString(in: Array[Byte]): String = DatatypeConverter.printHexBinary(in)
  def fromHexString(in: String): Array[Byte] = in.replaceAll("[^0-9A-Fa-f]", "").sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)

  lazy val dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss")

  def fromStringToDate(in: String): Date = dateFormat.parse(in)

  lazy val rand: Random = new Random()

  def getRandomBytes(length: Int): Traversable[Byte] = {
    val bytes: Array[Byte] = new Array[Byte](length)
    rand.nextBytes(bytes)
    bytes
  }
  def getRandomBoolean: Boolean = rand.nextBoolean()
  def getRandomInt: Int = rand.nextInt()
  def getRandomInt(n: Int): Int = rand.nextInt(n)
  def getRandomLong: Long = rand.nextLong()
  def getRandomPrintableChar: Char = rand.nextPrintableChar()
  def getRandomPrintableString(length: Int): String = {
    var str: String = __.emptyString
    for (i <- 0 until length) {
      str += rand.nextPrintableChar()
    }
    str
  }
  def getShuffledInts(n: Int): Traversable[Int] = rand.shuffle(0 to n - 1)

  lazy val consoleDef: String = "\033[0m"
  lazy val consoleBold: String = "\033[1m"
  lazy val consoleUnderline: String = "\033[4m"
  lazy val consoleTenmetsu: String = "\033[5m"
  lazy val consoleHanten: String = "\033[7m"
  lazy val consoleForeBlack: String = "\033[30m"
  lazy val consoleForeRed: String = "\033[31m"
  lazy val consoleForeGreen: String = "\033[32m"
  lazy val consoleForeYellow: String = "\033[33m"
  lazy val consoleForeBlue: String = "\033[34m"
  lazy val consoleForePurple: String = "\033[35m"
  lazy val consoleForeAqua: String = "\033[36m"
  lazy val consoleForeWhite: String = "\033[37m"
  lazy val consoleForeDef: String = "\033[39m"
  lazy val consoleBackBlack: String = "\033[40m"
  lazy val consoleBackRed: String = "\033[41m"
  lazy val consoleBackGreen: String = "\033[42m"
  lazy val consoleBackYellow: String = "\033[43m"
  lazy val consoleBackBlue: String = "\033[44m"
  lazy val consoleBackPurple: String = "\033[45m"
  lazy val consoleBackAqua: String = "\033[46m"
  lazy val consoleBackWhite: String = "\033[47m"
  lazy val consoleBackDef: String = "\033[49m"

  def toRedConsoleText(in: String): String = consoleForeRed + in + consoleDef
  def toBlueConsoleText(in: String): String = consoleForeBlue + in + consoleDef

  lazy val emptyString: String = ""
  lazy val newlineString: String = "\n"
  lazy val nullString: String = "<null>"

  lazy val testPrefix: String = "test - "
  lazy val errorPrefix: String = "error - "

  def toErrorMessage(message: String): String = toRedConsoleText(errorPrefix + message)
  def toErrorMessageHTML(message: String): String = errorPrefix + message

  def toKeyValueString(key: String, value: String): String = key + ": " + value
  def toMultilineString(lines: Traversable[String]): String = lines.mkString(newlineString)
  def toMultilineStringHTML(lines: Traversable[String]): String = lines.mkString(HTML.br)

  def getFromListBuffer[T](lb: ListBuffer[T], index: Int): Option[T] = if (lb.isDefinedAt(index)) Some(lb(index)) else None

  lazy val defaultCharset: Charset = Charset.defaultCharset()

  def writeFile(path: String, content: String) : Unit = writeFile(fs.getPath(path), content)
  def writeFile(path: Path, content: String) : Unit = {
    val bw: BufferedWriter = Files.newBufferedWriter(path, defaultCharset)
    bw.write(content)
    bw.close()
  }
  def readFile(path: String): String = readFile(fs.getPath(path))
  def readFile(path: Path): String = {
    val br: BufferedReader = Files.newBufferedReader(path, defaultCharset)
    val lines: ListBuffer[String] = ListBuffer()
    var line: String = br.readLine()
    while (line != null) {
      lines += line
      line = br.readLine()
    }
    br.close()
    lines.mkString(newlineString)
  }

  def bigIntegerToBytes(in: BigInteger, numBytes: Int): Array[Byte] = {
    val bytes: Array[Byte] = new Array[Byte](numBytes)
    val biBytes: Array[Byte] = in.toByteArray
    val start: Int = if (biBytes.length == numBytes + 1) 1 else 0
    val length: Int = Math.min(biBytes.length, numBytes)
    System.arraycopy(biBytes, start, bytes, numBytes - length, length)
    bytes
  }

  def positiveBigIntegerToBytes(in: BigInteger, numBytes: Int): Array[Byte] = {
    if (in.compareTo(BigInteger.ZERO) < 0) {
      throw new Exception("negative value")
    }

    val bytes: Array[Byte] = new Array[Byte](numBytes)
    val biBytes: Array[Byte] = in.toByteArray
    for (i <- bytes.indices) {
      if (i < biBytes.length) {
        bytes(bytes.length - i - 1) = biBytes(biBytes.length - i - 1)
      }
      else {
        bytes(bytes.length - i - 1) = UByte.__(0)
      }
    }
    bytes
  }

  def bytesToPositiveBigInteger(in: Array[Byte]): BigInteger = {
    val bytes: Array[Byte] = new Array[Byte](in.length + 1)
    for (i <- bytes.indices) {
      if (i == 0) {
        bytes(i) = UByte.__(0)
      }
      else {
        bytes(i) = in(i - 1)
      }
    }
    new BigInteger(bytes)
  }

  def getMinBytes(n: Int): Array[Byte] = {
    val bytes: Array[Byte] = new Array[Byte](n)
    for (i <- 0 until n) {
      bytes(i) = UByte.__(0)
    }
    bytes
  }

  def getMaxBytes(n: Int): Array[Byte] = {
    val bytes: Array[Byte] = new Array[Byte](n)
    for (i <- 0 until n) {
      bytes(i) = UByte.__(255)
    }
    bytes
  }

  def parseInts(str: String, n: Int): Option[Array[Int]] = {
    val args: Array[String] = str.split(' ')
    if (args.length < n) {
      None
    }
    else {
      val ints: Array[Int] = new Array[Int](n)
      var i: Int = 0
      var flag: Boolean = true
      while (flag && i < n) {
        __.tryToInt(args(i)) match {
          case Some(int) => ints(i) = int
          case None => flag = false
        }
        i += 1
      }
      if (flag) {
        Some(ints)
      }
      else {
        None
      }
    }
  }
}