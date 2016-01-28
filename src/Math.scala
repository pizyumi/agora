//import scala.collection.mutable.Stack
//import scala.collection.mutable.Queue
//
//
//object RBT {
//  def empty[K, V](): RBT[K, V] = Leaf[K, V]()
//  def turnB[K, V](t: Node[K, V]) = Node[K, V](B(), t.left, t.value, t.right)
//
//}
//sealed trait RBT[K, V] {}
//case class Leaf[K, V]() extends RBT[K, V]
//case class Node[K, V](color: Color, left: RBT[K, V], value: (K , V), right: RBT[K, V]) extends RBT[K, V]


import Common._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait IValueStore[T <: IIdentifiable[TId], TId <: ICompareOrder] {
  def find(id: TId): Option[T]
  def insert(elem: T): Unit
  def remove(elem: T): Unit
  def delete(id: TId): Unit
  def enumerate(): Traversable[T]
  def enumerateInSort(): Traversable[T]

  def toDotGraph(valueToString: T => String): String = throw new UnsupportedOperationException()
}

class LinkedListNode[T](vIn : T) {
  var v: T = vIn
  var next: LinkedListNode[T] = null
}

class LinkedList[T <: IIdentifiable[TId], TId <: ICompareOrder]() extends IValueStore[T, TId] {
  protected var root: LinkedListNode[T] = null

  protected def findNode(id: TId): LinkedListNode[T] = {
    var n: LinkedListNode[T] = root
    var ret: LinkedListNode[T] = null
    while (n != null && ret == null) {
      if (n.v.getComparableId.isGreat(id)) {
        n = n.next
      }
      else if (n.v.getComparableId.isLess(id)) {
        n = n.next
      }
      else {
        ret = n
      }
    }
    ret
  }

  protected def insertNode(node: LinkedListNode[T]): Unit = {
    node.next = root
    root = node
  }

  protected def removeNode(id: TId): Unit = {
    if (root != null) {
      if (root.v.getComparableId.isSame(id)) {
        root = root.next
      }
      else {
        var n: LinkedListNode[T] = root
        var ret: LinkedListNode[T] = null
        while (n.next != null && ret == null) {
          if (n.next.v.getComparableId.isSame(id)) {
            ret = n
          }
          else {
            n = n.next
          }
        }
        if (ret != null) {
          ret.next = ret.next.next
        }
      }
    }
  }

  protected def enumarateInList(list: ListBuffer[T]): Unit = {
    var n: LinkedListNode[T] = root
    while (n != null) {
      list += n.v
      n = n.next
    }
  }

  def find(id: TId): Option[T] = Option(findNode(id).v)
  def insert(elem: T): Unit = insertNode(new LinkedListNode[T](elem))
  def remove(elem: T): Unit = removeNode(elem.getComparableId)
  def delete(id: TId): Unit = removeNode(id)
  def enumerate(): Traversable[T] = {
    val list: ListBuffer[T] = ListBuffer()
    enumarateInList(list)
    list.toList
  }
  def enumerateInSort(): Traversable[T] = {
    val list: ListBuffer[T] = ListBuffer()
    enumarateInList(list)
    list.toList.sortWith((elem1, elem2) => elem1.getComparableId.isLess(elem2.getComparableId))
  }

  lazy val graphName: String = "list"

  override def toDotGraph(valueToString: T => String): String = {
    if (root == null) {
      Graphviz.createDigraph(graphName, __.emptyString)
    }
    else {
      val rankdir: String = Graphviz.createSetting("rankdir", "LR")
      val graphSettings: String = Graphviz.createGraphSettings(rankdir)

      val nodes: ListBuffer[String] = ListBuffer()
      val edges: ListBuffer[String] = ListBuffer()
      var n: LinkedListNode[T] = root
      var currentValue = valueToString(root.v)
      nodes += Graphviz.createNode(currentValue, __.emptyString)
      while (n.next != null) {
        val nextValue = valueToString(n.next.v)
        nodes += Graphviz.createNode(nextValue, __.emptyString)
        edges += Graphviz.createEdge(currentValue, nextValue, __.emptyString)
        currentValue = nextValue
        n = n.next
      }
      Graphviz.createDigraph(graphName, graphSettings + __.newlineString + nodes.mkString(__.newlineString) + __.newlineString + edges.mkString(__.newlineString))
    }
  }
}

class BinaryTreeNode[T](vIn : T) {
  var v: T = vIn
  var left: BinaryTreeNode[T] = null
  var right: BinaryTreeNode[T] = null
  var parent: BinaryTreeNode[T] = null
}

class BinaryTree[T <: IIdentifiable[TId], TId <: ICompareOrder]() extends IValueStore[T, TId] {
  protected var root: BinaryTreeNode[T] = null

  protected def findNode(id: TId): BinaryTreeNode[T] = {
    var n: BinaryTreeNode[T] = root
    var ret: BinaryTreeNode[T] = null
    while (n != null && ret == null) {
      if (n.v.getComparableId.isGreat(id)) {
        n = n.left
      }
      else if (n.v.getComparableId.isLess(id)) {
        n = n.right
      }
      else {
        ret = n
      }
    }
    ret
  }

  protected def insertNode(node: BinaryTreeNode[T]): Unit = {
    if (root == null) {
      root = node
    }
    else {
      var n: BinaryTreeNode[T] = root
      var p: BinaryTreeNode[T] = null
      while (n != null) {
        p = n
        if (n.v.getComparableId.isGreat(node.v.getComparableId)) {
          n = n.left
        }
        else {
          n = n.right
        }
      }

      node.parent = p
      if (p.v.getComparableId.isGreat(node.v.getComparableId)) {
        p.left = node
      }
      else {
        p.right = node
      }
    }
  }

  protected def removeNode(node: BinaryTreeNode[T]): Unit = {
    if (node.left == null) {
      replace(node, node.right)
    }
    else if (node.right == null) {
      replace(node, node.left)
    }
    else {
      val r: BinaryTreeNode[T] = min(node.right)
      node.v = r.v
      replace(r, r.right)
    }
  }

  protected def replace(reped: BinaryTreeNode[T], reping: BinaryTreeNode[T]): Unit = {
    val p: BinaryTreeNode[T] = reped.parent
    if (reping != null) {
      reping.parent = p
    }
    if (reped == root) {
      root = reping
    }
    else if (reped == p.left) {
      p.left = reping
    }
    else {
      p.right = reping
    }
  }

  protected def min(node: BinaryTreeNode[T]): BinaryTreeNode[T] = {
    var n: BinaryTreeNode[T] = node
    while (n.left != null) {
      n = n.left
    }
    n
  }

  protected def max(node: BinaryTreeNode[T]): BinaryTreeNode[T] = {
    var n: BinaryTreeNode[T] = node
    while (n.right != null) {
      n = n.right
    }
    n
  }

  protected def enumarateNodeInList(node: BinaryTreeNode[T], list: ListBuffer[T]): Unit = {
    val stack: mutable.Stack[BinaryTreeNode[T]] = mutable.Stack[BinaryTreeNode[T]]()
    var n: BinaryTreeNode[T] = node
    var f: Boolean = true
    while (n != null) {
      if (f && n.left != null) {
        stack.push(n)
        n = n.left
      }
      else {
        list += n.v
        if (n.right != null) {
          n = n.right
          f = true
        }
        else {
          if (stack.nonEmpty) {
            n = stack.pop()
          }
          else {
            n = null
          }
          f = false
        }
      }
    }
  }

  def find(id: TId): Option[T] = Option(findNode(id).v)
  def insert(elem: T): Unit = insertNode(new BinaryTreeNode[T](elem))
  def remove(elem: T): Unit = {
    val n: BinaryTreeNode[T] = findNode(elem.getComparableId)
    if (n != null) {
      removeNode(n)
    }
  }
  def delete(id: TId): Unit = {
    val n: BinaryTreeNode[T] = findNode(id)
    if (n != null) {
      removeNode(n)
    }
  }
  def enumerate(): Traversable[T] = {
    val list: ListBuffer[T] = ListBuffer()
    enumarateNodeInList(root, list)
    list.toList
  }
  def enumerateInSort(): Traversable[T] = enumerate()

  override def toDotGraph(valueToString: T => String): String = {
    ""
  }
}

class AVLTreeNode[T](vIn : T) {
  var v: T = vIn
  var left: AVLTreeNode[T] = null
  var right: AVLTreeNode[T] = null
  var parent: AVLTreeNode[T] = null
  var factor: Int = 0
}

//class AVLTree[T <: IIdentifiable[TId], TId <: ICompareOrder]() extends IValueStore[T, TId] {
//  protected var root: AVLTreeNode[T] = null
//
//  protected def rotateRight(node: AVLTreeNode[T]): Unit = {
//    val nodeLeft: AVLTreeNode[T] = node.left
//    if (node.parent == null) {
//      root = nodeLeft
//      nodeLeft.parent = null
//    }
//    else {
//      if (node.parent.left == node) {
//        node.parent.left = nodeLeft
//      }
//      else {
//        node.parent.right = nodeLeft
//      }
//      nodeLeft.parent = node.parent
//    }
//    node.left = nodeLeft.right
//    nodeLeft.right.parent = node
//    nodeLeft.right = node
//    node.parent = nodeLeft
//  }
//}

object RedBlackTreeColor {
  lazy val red: RedBlackTreeColor = Red()
  lazy val black: RedBlackTreeColor = Black()
}
sealed trait RedBlackTreeColor {}
case class Red() extends RedBlackTreeColor {}
case class Black() extends RedBlackTreeColor {}

class RedBlackTreeNode[T](vIn : T) {
  var v: T = vIn
  var left: RedBlackTreeNode[T] = null
  var right: RedBlackTreeNode[T] = null
  var parent: RedBlackTreeNode[T] = null
  var color: RedBlackTreeColor = RedBlackTreeColor.red
}

//class RedBlackTree[T <: IIdentifiable[TId], TId <: ICompareOrder]() extends IValueStore[T, TId] {
//  protected var root: RedBlackTreeNode[T] = null
//
//  protected def insertNode(node: RedBlackTreeNode[T]): Unit = {
//    if (root == null) {
//      root = node
//    }
//    else {
//      var n: RedBlackTreeNode[T] = root
//      var p: RedBlackTreeNode[T] = null
//      while (n != null) {
//        p = n
//        if (n.v.getComparableId.isGreat(node.v.getComparableId)) {
//          n = n.left
//        }
//        else {
//          n = n.right
//        }
//      }
//
//      node.parent = p
//      if (p.v.getComparableId.isGreat(node.v.getComparableId)) {
//        p.left = node
//      }
//      else {
//        p.right = node
//      }
//    }
//    node.color = RedBlackTreeColor.red
//
//    insertNodeCase1(node)
//  }
//
//  protected def insertNodeCase1(node: RedBlackTreeNode[T]): Unit = {
//    if (node.parent == null) {
//      node.color = RedBlackTreeColor.black
//    }
//    else {
//      insertNodeCase2(node)
//    }
//  }
//
//  protected def insertNodeCase2(node: RedBlackTreeNode[T]): Unit = {
//    if (node.parent.color != RedBlackTreeColor.black) {
//      insertNodeCase3(node)
//    }
//  }
//
//  protected def insertNodeCase3(node: RedBlackTreeNode[T]): Unit = {
//    val g: RedBlackTreeNode[T] = getGrandparent(node)
//    val u: RedBlackTreeNode[T] = getUncle(node, g)
//    if (u != null && u.color == RedBlackTreeColor.red) {
//      node.parent.color = RedBlackTreeColor.black
//      u.color = RedBlackTreeColor.black
//      g.color = RedBlackTreeColor.red
//      insertNodeCase1(g)
//    }
//    else {
//      insertNodeCase4(node, g)
//    }
//  }
//
//  protected def insertNodeCase4(node: RedBlackTreeNode[T], g: RedBlackTreeNode[T]): Unit = {
//    if (node == node.parent.right && node.parent == g.left) {
//      val p: RedBlackTreeNode[T] = g.left
//      val nLeft: RedBlackTreeNode[T] = node.left
//      g.left = node
//      node.left = p
//      p.right = nLeft
//
//      insertNodeCase5(node.left, g)
//    }
//    else if (node == node.parent.left && node.parent == g.right) {
//      val p: RedBlackTreeNode[T] = g.right
//      val nRight: RedBlackTreeNode[T] = node.right
//      g.right = node
//      node.right = p
//      p.left = nRight
//
//      insertNodeCase5(node.right, g)
//    }
//    else {
//      insertNodeCase5(node, g)
//    }
//  }
//
//  protected def insertNodeCase5(node: RedBlackTreeNode[T], g: RedBlackTreeNode[T]): Unit = {
//    node.parent.color = RedBlackTreeColor.black
//    g.color = RedBlackTreeColor.red
//    val gp: RedBlackTreeNode[T] = g.parent
//    if (node == node.parent.left) {
//      val p: RedBlackTreeNode[T] = g.left
//      val pRight: RedBlackTreeNode[T] = p.right
//      if (gp == null) {
//        root = p
//      }
//      else if (gp.left == g){
//        gp.left = p
//      }
//      else {
//        gp.right = p
//      }
//      p.right = g
//      g.left = pRight
//    }
//    else {
//      val p: RedBlackTreeNode[T] = g.right
//      val pLeft: RedBlackTreeNode[T] = p.left
//      if (gp == null) {
//        root = p
//      }
//      else if (gp.left == g){
//        gp.left = p
//      }
//      else {
//        gp.right = p
//      }
//      p.left = g
//      g.right = pLeft
//    }
//  }
//
//  protected def getGrandparent(node: RedBlackTreeNode[T]): RedBlackTreeNode[T] = {
//    if (node != null && node.parent != null) {
//      node.parent.parent
//    }
//    else {
//      null
//    }
//  }
//
//  protected def getUncle(node: RedBlackTreeNode[T]): RedBlackTreeNode[T] = getUncle(node, getGrandparent(node))
//  protected def getUncle(node: RedBlackTreeNode[T], g: RedBlackTreeNode[T]): RedBlackTreeNode[T] = {
//    if (g == null) {
//      null
//    }
//    else if (node.parent == g.left) {
//      g.right
//    }
//    else {
//      g.left
//    }
//  }
//}

class TestableInt(intIn: Int) extends IIdentifiable[ICompareOrder] with ICompareOrder {
  val int: Int = intIn

  protected override def specCompare(r: ICompareOrder): Ordering = {
    r.asInstanceOf[TestableInt] match {
      case ras if int > ras.int => Great
      case ras if int == ras.int => Equal
      case ras if int < ras.int => Less
    }
  }
  protected override def specHashCode: Int = int

  protected def specGetComparableId: ICompareOrder = this
}

abstract class ValueStoreTestCLI(reportFolder: String, dotExeFile: String) extends ICLIComponent {
  protected lazy val testInterface: TestInterface = new TestCLIWithHTMLReport(reportFolder, dotExeFile)

  protected lazy val valueToString: TestableInt => String = (elem) => elem.int.toString

  protected val valueStoreName: String

  protected val test1: String
  protected val test2: String

  def getCommands: Traversable[Command] = {
    Array(
      new Command(test1, doTest1),
      new Command(test2, doTest2)
    )
  }

  protected def createValueStore(): IValueStore[TestableInt, ICompareOrder]

  protected def doTest1(args: String): Unit = {
    val iteration: Int = 32

    testInterface.outputTitle(valueStoreName + " test 1", None)

    testInterface.outputMessage("generating random integers...")
    val data: Array[Int] = __.getShuffledInts(iteration).toArray
    testInterface.outputMessage(__.toKeyValueString("addition data", data.mkString(", ")))
    testInterface.outputMessage("generating another random integers...")
    val data2: Array[Int] = __.getShuffledInts(iteration).toArray
    testInterface.outputMessage(__.toKeyValueString("deletion data", data2.mkString(", ")))
    var trueState: mutable.ListBuffer[Int] = mutable.ListBuffer()
    val tree: IValueStore[TestableInt, ICompareOrder] = createValueStore()

    for(i <- 0 until iteration) {
      val testId1: String = "1." + (i + 1) + ".1"

      testInterface.outputMessage("adding data element (" + data(i) + ") to true state...")
      trueState += data(i)
      trueState = trueState.sorted
      testInterface.outputMessage(__.toKeyValueString("true state", trueState.mkString(", ")))
      testInterface.outputMessage("adding data element (" + data(i) + ") to value store ( " + valueStoreName + " )...")
      tree.insert(new TestableInt(data(i)))
      testInterface.outputMessage(__.toKeyValueString("value store state", tree.enumerateInSort().map((elem) => elem.int).mkString(", ")))
      testInterface.outputItem(testId1, Some("both enumerations are same"), trueState.toArray[Int].sameElements(tree.enumerateInSort().map((elem) => elem.int).toArray[Int]))
      testInterface.outputDot(testId1, tree.toDotGraph(valueToString))

      val testId2: String = "1." + (i + 1) + ".2"

      testInterface.outputMessage("removing data element (" + data2(i) + ") from true state...")
      trueState -= data2(i)
      testInterface.outputMessage(__.toKeyValueString("true state", trueState.mkString(", ")))
      testInterface.outputMessage("removing data element (" + data2(i) + ") from value store ( " + valueStoreName + " )...")
      tree.remove(new TestableInt(data2(i)))
      testInterface.outputMessage(__.toKeyValueString("value store state", tree.enumerateInSort().map((elem) => elem.int).mkString(", ")))
      testInterface.outputItem(testId2, Some("both enumerations are same"), trueState.toArray[Int].sameElements(tree.enumerateInSort().map((elem) => elem.int).toArray[Int]))
      testInterface.outputDot(testId2, tree.toDotGraph(valueToString))
    }

    testInterface.outputReport(valueStoreName + " 1")
  }

  protected def doTest2(args: String): Unit = {
    val iteration: Int = 32

    testInterface.outputTitle(valueStoreName + " test 2", None)

    testInterface.outputMessage("generating random integers...")
    val data: Array[Int] = __.getShuffledInts(iteration).toArray
    testInterface.outputMessage(__.toKeyValueString("addition data", data.mkString(", ")))
    testInterface.outputMessage("generating another random integers...")
    val data2: Array[Int] = __.getShuffledInts(iteration).toArray
    testInterface.outputMessage(__.toKeyValueString("deletion data", data2.mkString(", ")))
    var trueState: mutable.ListBuffer[Int] = mutable.ListBuffer()
    val tree: BinaryTree[TestableInt, ICompareOrder] = new BinaryTree[TestableInt, ICompareOrder]()

    for(i <- 0 until iteration) {
      val testId1: String = "2." + (i + 1) + ".1"

      testInterface.outputMessage("adding data element (" + data(i) + ") to true state...")
      trueState += data(i)
      trueState = trueState.sorted
      testInterface.outputMessage(__.toKeyValueString("true state", trueState.mkString(", ")))
      testInterface.outputMessage("adding data element (" + data(i) + ") to value store ( " + valueStoreName + " )...")
      tree.insert(new TestableInt(data(i)))
      testInterface.outputMessage(__.toKeyValueString("value store state", tree.enumerateInSort().map((elem) => elem.int).mkString(", ")))
      testInterface.outputItem(testId1, Some("both enumerations are same"), trueState.toArray[Int].sameElements(tree.enumerateInSort().map((elem) => elem.int).toArray[Int]))
      testInterface.outputDot(testId1, tree.toDotGraph(valueToString))

      val testId2: String = "2." + (i + 1) + ".2"

      testInterface.outputMessage("removing data element (" + data2(i) + ") from true state...")
      trueState -= data2(i)
      testInterface.outputMessage(__.toKeyValueString("true state", trueState.mkString(", ")))
      testInterface.outputMessage("deleting data element (" + data2(i) + ") from value store ( " + valueStoreName + " )...")
      tree.delete(new TestableInt(data2(i)))
      testInterface.outputMessage(__.toKeyValueString("value store state", tree.enumerateInSort().map((elem) => elem.int).mkString(", ")))
      testInterface.outputItem(testId2, Some("both enumerations are same"), trueState.toArray[Int].sameElements(tree.enumerateInSort().map((elem) => elem.int).toArray[Int]))
      testInterface.outputDot(testId2, tree.toDotGraph(valueToString))
    }

    testInterface.outputReport(valueStoreName + " 2")
  }
}

class LinkedListTestCLI(reportFolder: String, dotExeFile: String) extends ValueStoreTestCLI(reportFolder, dotExeFile) {
  protected override lazy val valueStoreName: String = "linked list"

  protected override lazy val test1: String = "test linked list 1"
  protected override lazy val test2: String = "test linked list 2"

  protected override def createValueStore() = new LinkedList[TestableInt, ICompareOrder]()
}

class BinaryTreeTestCLI(reportFolder: String, dotExeFile: String) extends ValueStoreTestCLI(reportFolder, dotExeFile) {
  protected override lazy val valueStoreName: String = "binary tree"

  protected override lazy val test1: String = "test binary tree 1"
  protected override lazy val test2: String = "test binary tree 2"

  protected override def createValueStore() = new BinaryTree[TestableInt, ICompareOrder]()
}