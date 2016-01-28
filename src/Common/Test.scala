package Common

import java.nio.file.Path

import scala.collection.mutable.ListBuffer

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