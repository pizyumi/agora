import java.nio.file.{Files, Path}

import scala.sys.process.Process

object Graph {
  import Common._

  object Graphviz {
    lazy val keywordDigraph = "digraph "
    lazy val keywordGraph = "graph "
    lazy val keywordNode = "node "
    lazy val keywordEdge = "edge "

    def createDigraph(name: String, content: String): String = keywordDigraph + name + " {" + __.newlineString + content + __.newlineString + "}"
    def createGraphSettings(settings: String): String = keywordGraph + "[" + __.newlineString + settings + __.newlineString + "];"
    def createNodeSettings(settings: String): String = keywordNode + "[" + __.newlineString + settings + __.newlineString + "];"
    def createEdgeSettings(settings: String): String = keywordEdge + "[" + __.newlineString + settings + __.newlineString + "];"
    def createNode(name: String, settings: String): String = "\"" + name + "\"" + "[" + __.newlineString + settings + __.newlineString + "];"
    def createEdge(name1: String, name2: String, settings: String): String = "\"" + name1 + "\"" + " -> " + "\"" + name2 + "\"" + "[" + __.newlineString + settings + __.newlineString + "];"
    def createSettings(settingsTraversable: Traversable[String]): String = settingsTraversable.mkString(", " + __.newlineString)
    def createSetting(name: String, value: String): String = name + " = " + value

    def createCommandLine(dotExePath: String, layoutEngine: String, format: String, inPath: String, outPath: String) = dotExePath + " -K" + layoutEngine + " -T" + format + " " + inPath + " -o" + outPath

    lazy val defaultLayoutEngine: String = "dot"
    lazy val defaultFormat: String = "svg"

    def execute(dotExeFile: String, inPath: String, outPath: String): Unit = Process(createCommandLine(dotExeFile, defaultLayoutEngine, defaultFormat, inPath, outPath)).run()

    def execute(dot: String, dotExeFile: String, folder: String, inFile: String, outFile: String): Path = {
      val inPath: Path = __.fs.getPath(folder, inFile)
      val outPath: Path = __.fs.getPath(folder, outFile)
      val inPathStr: String = inPath.toString
      val outPathStr: String = outPath.toString

      if (Files.exists(inPath)) {
        Files.delete(inPath)
      }
      if (Files.exists(outPath)) {
        Files.delete(outPath)
      }

      __.writeFile(inPathStr, dot)

      execute(dotExeFile, inPathStr, outPathStr)

      outPath
    }

    def execute(dot: String, dotExeFile: String, folder: String, filebase: String): Path = execute(dot, dotExeFile, folder, filebase + __.extensionDOT, filebase + __.extensionSVG)
  }

  class GraphvizSampleCLI(dotExeFile: String, workFolder: String) {
    protected lazy val sampleInterface: SampleInterface = new SampleCLI()

    protected lazy val sample: String = "sample graphviz 1"

    def executeCommand(command: String): Boolean = {
      if (command.startsWith(sample)) {
        doSample1()
        true
      }
      else {
        false
      }
    }

    protected lazy val sample1Dot: String = "digraph sample {" +
      "  node [shape=box, width=1];" +
      "  a [label=\"Node A\"];" +
      "  b [label=\"Node B\"];" +
      "  c [label=\"Node C\"];" +
      "  d [label=\"Node D\"];" +
      "  a -> b [label=\" A-B \"];" +
      "  b -> c [label=\" B-C \"];" +
      "  d -> c [label=\" C-D \", dir=back];" +
      "  a -> d [style=dashed];" +
      "  {rank=same; a; b}" +
      "  {rank=same; c; d}" +
      "}"
    protected lazy val sample1InFileName: String = "sample1.dot"
    protected lazy val sample1OutFileName: String = "sample1.svg"

    protected def doSample1(): Unit = {
      sampleInterface.outputTitle("graphviz sample 1", None)

      val outPath: Path = Graphviz.execute(sample1Dot, dotExeFile, workFolder, sample1InFileName, sample1OutFileName)

      sampleInterface.outputMessage("generated sample svg!!")
      sampleInterface.outputMessage(__.toKeyValueString("sample svg", __.readFile(outPath)))
    }
  }
}