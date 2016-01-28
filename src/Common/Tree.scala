package Common

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import _root_.Graph.Graphviz

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