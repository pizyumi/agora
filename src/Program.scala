import java.nio.file.Files
import java.nio.file.Path

import Common.Common
import skinny.micro.WebApp
import skinny.micro.WebServer

object Program {
  import Common._
  import Crypto._
  import Graph._
  import StandardImplemantation._
  import StandardImplementationCLI._

  lazy val exit: String = "exit"

  lazy val graphvizFolder: String = "C:\\work\\graphviz"
  lazy val reportFolder: String = "C:\\work\\report"

  lazy val dotExeFile: String = "C:\\work\\graphviz-2.38\\release\\bin\\dot.exe"

  lazy val webServerPort: Int = 7777

  def main (args: Array[String]) {
    val graphvizFolderPath: Path = __.fs.getPath(graphvizFolder)
    val reportFolderPath: Path = __.fs.getPath(reportFolder)

    if (!Files.exists(graphvizFolderPath)) {
      Files.createDirectory(graphvizFolderPath)
    }
    if (!Files.exists(reportFolderPath)) {
      Files.createDirectory(reportFolderPath)
    }

    //WebServer.mount(new BlockchainApp(dotExeFile, graphvizFolder)).port(webServerPort).start()

    val cli: CLI = new CLI(StandardCLIFactory, new BusinessLogic(StandardBusinessLogicFactory))
    val secp256k1CLI: Secp256k1TestCLI = new Secp256k1TestCLI()
    val graphvizCLI: GraphvizSampleCLI = new GraphvizSampleCLI(dotExeFile, graphvizFolder)
    val linkedListCLI: LinkedListTestCLI = new LinkedListTestCLI(reportFolder, dotExeFile)
    val binaryTreeCLI: BinaryTreeTestCLI = new BinaryTreeTestCLI(reportFolder, dotExeFile)
    val standardImplTestCLI: StandardImplementationTestCLI = new StandardImplementationTestCLI()

    var f: Boolean = true
    while (f) {
      val command = scala.io.StdIn.readLine()
      if (command == exit) {
        f = false
      }
      else {
        if (!cli.executeCommand(command)) {
          if (!secp256k1CLI.executeCommand(command)) {
            if (!binaryTreeCLI.executeCommand(command)) {
              if (!graphvizCLI.executeCommand(command)) {
                if (!linkedListCLI.executeCommand(command)) {
                  if (!standardImplTestCLI.executeCommand(command)) {
                    println(__.toErrorMessage("the command is not supported"))
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  class BlockchainApp(dotExeFile: String, workFolder: String) extends WebApp {
    val webCLI: WebCLI = new WebCLI(dotExeFile, workFolder)

    get("/say-hello") {
      s"Hello, ${params.getOrElse("name", "Anonymous")}!\n"
    }

    get(webCLI.blockchainURL) {
      contentType = __.mimetypeHTML
      webCLI.blockchainGet()
    }

    post(webCLI.blockchainURL) {
      contentType = __.mimetypeHTML
      webCLI.blockchainPost(params.getOrElse("command", __.emptyString))
    }

    get(webCLI.blockchainGraphURL) {
      contentType = __.mimetypeSVG
      val svgSource: Option[String] = webCLI.blockchainGraphGet()
      svgSource match {
        case Some(svg) => svg
        case None => halt(404)
      }
    }
  }
}