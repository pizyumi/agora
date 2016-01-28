package Common

import scala.collection.mutable.ListBuffer

class Command(commandBodyIn: String, actionIn: String => Unit) {
  val commandBody: String = commandBodyIn
  val action: String => Unit = actionIn
}

trait ICLIComponent {
  def getCommands: Traversable[Command]
}

class CLI() {
  private val components: ListBuffer[ICLIComponent] = ListBuffer()

  def register(component: ICLIComponent): Unit = {
    if (!components.contains(component)) {
      components += component
    }
  }

  def unregister(component: ICLIComponent): Unit = {
    if (components.contains(component)) {
      components -= component
    }
  }

  //TODO: コマンドの順番を考慮すべき
  def executeCommand(commandStr: String): Boolean = {
    components.flatMap((c) => c.getCommands).find((c) => commandStr.startsWith(c.commandBody)) match {
      case Some(c) =>
        c.action(commandStr.substring(c.commandBody.length).trim)
        true
      case None => false
    }
  }
}