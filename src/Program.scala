import java.math.BigInteger
import java.nio.file.Files
import java.nio.file.Path

import skinny.micro.{WebServer, WebApp}

object Program {
  import Common._
  import Crypto._
  import Blockchain._

  lazy val exit: String = "exit"
  lazy val test: String = "test"

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

    val perfLogicsMap: Map[String, IPerformanceBusinessLogic] = Map(
      "blocktree" -> new PerformanceBusinessLogic(StandardBusinessLogicFactory),
      "indexedblocktree" -> new PerformanceBusinessLogic(StandardBusinessLogicFactoryIndexed)
    )

    val cli: Common.CLI = new Common.CLI()
    cli.register(new Blockchain.CLI(StandardCLIFactory, new BusinessLogic(StandardBusinessLogicFactory), new CreateBlockBusinessLogic(BlockchainSettings.defaultSettings), perfLogicsMap))
    cli.register(new Secp256k1TestCLI())
    cli.register(new GraphvizSampleCLI(dotExeFile, graphvizFolder))
    cli.register(new LinkedListTestCLI(reportFolder, dotExeFile))
    cli.register(new BinaryTreeTestCLI(reportFolder, dotExeFile))
    cli.register(new StandardImplementationTestCLI())

    val testing: Testing = new Testing()
    testing.register(new Secp256k1TestCLI())
    testing.register(new LinkedListTestCLI(reportFolder, dotExeFile))
    testing.register(new BinaryTreeTestCLI(reportFolder, dotExeFile))
    testing.register(new StandardImplementationTestCLI())

    var f: Boolean = true
    while (f) {
      val command = scala.io.StdIn.readLine()
      if (command == exit) {
        f = false
      }
      else if (command == test) {
        testing.executeTest()
      }
      else {
        if (!cli.executeCommand(command)) {
          println(__.toErrorMessage("the command is not supported"))
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