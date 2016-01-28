package Common

trait SampleInterface {
  def outputTitle(title: String, description: Option[String]): Unit
  def outputMessage(message: String): Unit
}

class SampleCLI() extends SampleInterface {
  lazy val samplePrefix: String = "sample - "

  def outputTitle(title: String, description: Option[String]): Unit = println(samplePrefix + description.map((c) => __.toKeyValueString(title, c)).getOrElse(title))
  def outputMessage(message: String): Unit = println(message)
}