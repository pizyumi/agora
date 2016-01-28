package Common

import java.io.{BufferedReader, BufferedWriter, ByteArrayOutputStream}
import java.math.BigInteger
import java.nio.ByteBuffer
import java.nio.file.{FileSystem, FileSystems, Files, Path}
import java.security.MessageDigest
import javax.xml.bind.DatatypeConverter

import _root_.Graph.Graphviz

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random
import scala.util.control.Exception

object Common {
  //木
  trait ITree[T] {
    //値を取得する
    def getValue: T
    //子を取得する
    def getChildren: Traversable[ITree[T]]
    //親を取得する
    def getParent: Option[ITree[T]]

    //子を追加する
    def addChild(child: ITree[T]): Unit
    //子を削除する
    def removeChild(child: ITree[T]): Unit

    //子孫の値を列挙する
    def descendants(): Traversable[T] = {
      //全ての子を一時的に格納する
      //複数の子を一度に処理することはできないため、子は1つずつ処理しなければならない
      val inStack = new mutable.Stack[ITree[T]]()
      //全ての子の値を最終的に格納する
      val outQueue = new mutable.Queue[T]()
      //自身を格納する
      inStack.push(this)
      //子が存在する限り
      while (inStack.nonEmpty) {
        //一時的に格納した子を取得する
        val tree = inStack.pop()
        //子の値を取得し格納する
        outQueue.enqueue(tree.getValue)
        //子の子を取得する
        for (child <- tree.getChildren) {
          //子の子を格納する
          inStack.push(child)
        }
      }
      //全ての子の値を返す
      outQueue
    }

    //子孫を列挙する
    def descendantTrees(): Traversable[ITree[T]] = {
      //全ての子を一時的に格納する
      //複数の子を一度に処理することはできないため、子は1つずつ処理しなければならない
      val inStack = new mutable.Stack[ITree[T]]()
      //全ての子を最終的に格納する
      val outQueue = new mutable.Queue[ITree[T]]()
      //自身を格納する
      inStack.push(this)
      //子が存在する限り
      while (inStack.nonEmpty) {
        //一時的に格納した子を格納する
        val tree = inStack.pop()
        //子を格納する
        outQueue.enqueue(tree)
        //子の子を取得する
        for (child <- tree.getChildren) {
          //子の子を格納する
          inStack.push(child)
        }
      }
      //全ての子を返す
      outQueue
    }

//    //子孫を列挙する
//    //値と派生値から成る組の木を作成する関数を受け取る
//    def descendantTrees[S](base: T => ITree[(T, S)], rec: (T, ITree[(T, S)]) => ITree[(T, S)]): Traversable[ITree[(T, S)]] = {
//      //全ての子を一時的に格納する
//      //複数の子を一度に処理することはできないため、子は1つずつ処理しなければならない
//      val inStack = new mutable.Stack[ITree[T]]()
//      val inStackCalc = new mutable.Stack[ITree[(T, S)]]()
//      //全ての子を最終的に格納する
//      val outQueue = new mutable.Queue[ITree[(T, S)]]()
//      //自身を格納する
//      inStack.push(this)
//      inStackCalc.push(base(this.getValue))
//      //子が存在する限り
//      while (inStack.nonEmpty) {
//        //一時的に格納した子を格納する
//        val tree: ITree[T] = inStack.pop()
//        val treeCalc: ITree[(T, S)] = inStackCalc.pop()
//        //子を格納する
//        outQueue.enqueue(treeCalc)
//        //子の子を取得する
//        for (child <- tree.getChildren) {
//          //子の子を格納する
//          inStack.push(child)
//          inStackCalc.push(rec(child.getValue, treeCalc))
//        }
//      }
//      //全ての子を返す
//      outQueue
//    }

    //グラフの名称
    lazy val graphName: String = "tree"

//    //DOT形式のグラフを作成する
//    //値を文字列に変換する関数を受け取る
//    def toDotGraph(valueToString: T => String): String = toDotGraphIn(descendantTrees(), valueToString)
//
//    //DOT形式のグラフを作成する
//    //値と派生値から成る組の木を作成する関数を受け取る
//    //値を文字列に変換する関数を受け取る
//    def toDotGraph[S](base: T => ITree[(T, S)], rec: (T, ITree[(T, S)]) => ITree[(T, S)], valueToString: ((T, S)) => String): String = toDotGraphIn[(T, S)](descendantTrees(base, rec), valueToString)

    //DOT形式のグラフを作成する
    //値を文字列に変換する関数を受け取る
    def toDotGraph(valueToString: T => String): String = toDotGraph(valueToString, (_) => __.emptyString, (_, _) => __.emptyString)

    //DOT形式のグラフを作成する
    //値を文字列に変換する関数を受け取る
    //値を設定に変換する関数を受け取る
    def toDotGraph(valueToString: T => String, valueToNodeSettings: T => String, valueToEdgeStrrings: (T, T) => String): String = {
      val descendants: Traversable[ITree[T]] = descendantTrees()
      //子孫が存在しない場合には空のグラフを作成する
      if (descendants.isEmpty) {
        Graphviz.createDigraph(graphName, __.emptyString)
      }
      //子孫が存在する場合
      else {
        //グラフの全ての節を格納する
        val nodes: ListBuffer[String] = ListBuffer()
        //グラフの全ての辺を格納する
        val edges: ListBuffer[String] = ListBuffer()
        //値を取得し、文字列に変換し、節を作成し、追加する
        nodes += Graphviz.createNode(valueToString(descendants.head.getValue), valueToNodeSettings(descendants.head.getValue))
        //全ての子孫に対して
        for (d <- descendants) {
          //値を取得し、文字列に変換する
          val pValue: String = valueToString(d.getValue)
          //全ての子に対して
          for (c <- d.getChildren) {
            //値を取得し、文字列に変換する
            val cValue: String = valueToString(c.getValue)
            //節を作成し、追加する
            nodes += Graphviz.createNode(cValue, valueToNodeSettings(c.getValue))
            //辺を作成し、追加する
            edges += Graphviz.createEdge(pValue, cValue, valueToEdgeStrrings(d.getValue, c.getValue))
          }
        }
        //グラフを作成する
        Graphviz.createDigraph(graphName, nodes.mkString(__.newlineString) + __.newlineString + edges.mkString(__.newlineString))
      }
    }
  }

  //値を直接保持する木
  class ValueTree[T](value: T, var children: ListBuffer[ITree[T]], parent: Option[ITree[T]]) extends ITree[T] {
    //値を取得する
    def getValue: T = value
    //子を取得する
    def getChildren: Traversable[ITree[T]] = children
    //親を取得する
    def getParent: Option[ITree[T]] = parent

    //子を追加する
    def addChild(child: ITree[T]): Unit = children += child
    //子を削除する
    def removeChild(child: ITree[T]): Unit = children -= child
  }

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

    def writeFile(path: String, content: String) : Unit = writeFile(fs.getPath(path), content)
    def writeFile(path: Path, content: String) : Unit = {
      val bw: BufferedWriter = Files.newBufferedWriter(path)
      bw.write(content)
      bw.close()
    }
    def readFile(path: String): String = readFile(fs.getPath(path))
    def readFile(path: Path): String = {
      val br: BufferedReader = Files.newBufferedReader(path)
      val lines: ListBuffer[String] = ListBuffer()
      var line: String = br.readLine()
      while (line != null) {
        lines += line
        line = br.readLine()
      }
      br.close()
      lines.mkString(newlineString)
    }

    def bigIntegerToBytes(in: BigInteger, numBytes: Int) = {
      val bytes: Array[Byte] = new Array[Byte](numBytes)
      val biBytes: Array[Byte] = in.toByteArray
      val start: Int = if (biBytes.length == numBytes + 1) 1 else 0
      val length: Int = Math.min(biBytes.length, numBytes)
      System.arraycopy(biBytes, start, bytes, numBytes - length, length)
      bytes
    }
  }

  trait TestInterface {
    def outputTitle(title: String, description: Option[String]): Unit
    def outputMessage(message: String): Unit
    def outputItem(name: String, description: Option[String], isOk: Boolean): Unit
    def outputDot(name: String, dot: String): Unit

    def outputReport(name: String): Unit
  }

  class TestCLI() extends TestInterface {
    protected lazy val ok: String = "ok"
    protected lazy val ng: String = "ng"
    protected lazy val testResultOk: String = __.toBlueConsoleText(ok)
    protected lazy val testResultNg: String = __.toRedConsoleText(ng)

    def outputTitle(title: String, description: Option[String]): Unit = println(toTitle(title, description))
    def outputMessage(message: String): Unit = println(message)
    def outputItem(name: String, description: Option[String], isOk: Boolean): Unit = {
      println(toItemHeader(name, description))
      println(if (isOk) testResultOk else testResultNg)
    }
    def outputDot(name: String, dot: String): Unit = println(__.toErrorMessage("outputting file is not supported"))

    def outputReport(name: String): Unit = println(__.toErrorMessage("outputting report is not supported"))

    protected def toTitle(title: String, description: Option[String]): String = __.testPrefix + description.map((c) => __.toKeyValueString(title, c)).getOrElse(title)
    protected def toItemHeader(name: String, description: Option[String]): String = __.testPrefix + description.map((c) => __.toKeyValueString(name, c)).getOrElse(name)
  }

  class TestCLIWithHTMLReport(reportFolder: String, dotExeFile: String) extends TestCLI() {
    protected lazy val testResultOkHTML: String = HTML.createSpan(ok)
    protected lazy val testResultNgHTML: String = HTML.createSpan(ng)

    protected val outputs: ListBuffer[String] = ListBuffer()

    override def outputTitle(title: String, description: Option[String]): Unit = {
      super.outputTitle(title, description)

      outputs += HTML.createHeader(toTitle(title, description), 1)
    }
    override def outputMessage(message: String): Unit = {
      super.outputMessage(message)

      outputs += HTML.createParagraph(message)
    }
    override def outputItem(name: String, description: Option[String], isOk: Boolean): Unit = {
      super.outputItem(name, description, isOk)

      outputs += HTML.createParagraph(toItemHeader(name, description) + HTML.br + (if (isOk) testResultOkHTML else testResultNgHTML))
    }

    override def outputDot(name: String, dot: String): Unit = {
      val outPath: Path = Graphviz.execute(dot, dotExeFile, reportFolder, name)

      outputs += HTML.createImage(outPath.getFileName.toString)

      println("dot file was outputted successfully")
    }

    override def outputReport(name: String): Unit = {
      __.writeFile(__.fs.getPath(reportFolder, name + __.extensionHTML).toString, HTML.createHTML(name, outputs.mkString(__.newlineString)))

      println("report was outputted successfully")

      outputs.clear()
    }
  }

  trait SampleInterface {
    def outputTitle(title: String, description: Option[String]): Unit
    def outputMessage(message: String): Unit
  }

  class SampleCLI() extends SampleInterface {
    lazy val samplePrefix: String = "sample - "

    def outputTitle(title: String, description: Option[String]): Unit = println(samplePrefix + description.map((c) => __.toKeyValueString(title, c)).getOrElse(title))
    def outputMessage(message: String): Unit = println(message)
  }

  object HTML {
    lazy val br: String = "<br/>"

    def createHTML(title: String, content: String): String = {
      var html: String = ""
      html += "<!DOCTYPE html>" + __.newlineString
      html += "<html>" + __.newlineString
      html += "<head>" + __.newlineString
      html += "<meta charset=\"utf-8\" />" + __.newlineString
      html += "<title>" + title + "</title>" + __.newlineString
      html += "</head>" + __.newlineString
      html += "<body>" + __.newlineString
      html += content + __.newlineString
      html += "</body>" + __.newlineString
      html += "</html>" + __.newlineString
      html
    }
    def createHeader(header: String, level: Int): String = "<h" + level.toString + ">" + header + "</h" + level.toString + ">"
    def createParagraph(paragraph: String): String = "<p>" + paragraph + "</p>"
    def createSpan(text: String): String = "<span>" + text + "</span>"
    def createImage(href: String): String = "<img src=\"" + href + "\"/>"
    def createForm(action: String, content: String): String = "<form method=\"post\" action=\"" + action + "\">" + content + "</form>"
    def createTextbox(name: String, size: Int): String = "<input type=\"text\" name=\"" + name + "\" size=\"" + size.toString + "\"/>"
    def createButton(name: String): String = "<input type=\"submit\" value=\"" + name + "\"/>"
  }
}