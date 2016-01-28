package Common

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import _root_.Graph.Graphviz

//��
trait ITree[T] {
  //�l���擾����
  def getValue: T
  //�q���擾����
  def getChildren: Traversable[ITree[T]]
  //�e���擾����
  def getParent: Option[ITree[T]]

  //�q��ǉ�����
  def addChild(child: ITree[T]): Unit
  //�q���폜����
  def removeChild(child: ITree[T]): Unit

  //�q���̒l��񋓂���
  def descendants(): Traversable[T] = {
    //�S�Ă̎q���ꎞ�I�Ɋi�[����
    //�����̎q����x�ɏ������邱�Ƃ͂ł��Ȃ����߁A�q��1���������Ȃ���΂Ȃ�Ȃ�
    val inStack = new mutable.Stack[ITree[T]]()
    //�S�Ă̎q�̒l���ŏI�I�Ɋi�[����
    val outQueue = new mutable.Queue[T]()
    //���g���i�[����
    inStack.push(this)
    //�q�����݂������
    while (inStack.nonEmpty) {
      //�ꎞ�I�Ɋi�[�����q���擾����
      val tree = inStack.pop()
      //�q�̒l���擾���i�[����
      outQueue.enqueue(tree.getValue)
      //�q�̎q���擾����
      for (child <- tree.getChildren) {
        //�q�̎q���i�[����
        inStack.push(child)
      }
    }
    //�S�Ă̎q�̒l��Ԃ�
    outQueue
  }

  //�q����񋓂���
  def descendantTrees(): Traversable[ITree[T]] = {
    //�S�Ă̎q���ꎞ�I�Ɋi�[����
    //�����̎q����x�ɏ������邱�Ƃ͂ł��Ȃ����߁A�q��1���������Ȃ���΂Ȃ�Ȃ�
    val inStack = new mutable.Stack[ITree[T]]()
    //�S�Ă̎q���ŏI�I�Ɋi�[����
    val outQueue = new mutable.Queue[ITree[T]]()
    //���g���i�[����
    inStack.push(this)
    //�q�����݂������
    while (inStack.nonEmpty) {
      //�ꎞ�I�Ɋi�[�����q���i�[����
      val tree = inStack.pop()
      //�q���i�[����
      outQueue.enqueue(tree)
      //�q�̎q���擾����
      for (child <- tree.getChildren) {
        //�q�̎q���i�[����
        inStack.push(child)
      }
    }
    //�S�Ă̎q��Ԃ�
    outQueue
  }

  //    //�q����񋓂���
  //    //�l�Ɣh���l���琬��g�̖؂��쐬����֐����󂯎��
  //    def descendantTrees[S](base: T => ITree[(T, S)], rec: (T, ITree[(T, S)]) => ITree[(T, S)]): Traversable[ITree[(T, S)]] = {
  //      //�S�Ă̎q���ꎞ�I�Ɋi�[����
  //      //�����̎q����x�ɏ������邱�Ƃ͂ł��Ȃ����߁A�q��1���������Ȃ���΂Ȃ�Ȃ�
  //      val inStack = new mutable.Stack[ITree[T]]()
  //      val inStackCalc = new mutable.Stack[ITree[(T, S)]]()
  //      //�S�Ă̎q���ŏI�I�Ɋi�[����
  //      val outQueue = new mutable.Queue[ITree[(T, S)]]()
  //      //���g���i�[����
  //      inStack.push(this)
  //      inStackCalc.push(base(this.getValue))
  //      //�q�����݂������
  //      while (inStack.nonEmpty) {
  //        //�ꎞ�I�Ɋi�[�����q���i�[����
  //        val tree: ITree[T] = inStack.pop()
  //        val treeCalc: ITree[(T, S)] = inStackCalc.pop()
  //        //�q���i�[����
  //        outQueue.enqueue(treeCalc)
  //        //�q�̎q���擾����
  //        for (child <- tree.getChildren) {
  //          //�q�̎q���i�[����
  //          inStack.push(child)
  //          inStackCalc.push(rec(child.getValue, treeCalc))
  //        }
  //      }
  //      //�S�Ă̎q��Ԃ�
  //      outQueue
  //    }

  //�O���t�̖���
  lazy val graphName: String = "tree"

  //    //DOT�`���̃O���t���쐬����
  //    //�l�𕶎���ɕϊ�����֐����󂯎��
  //    def toDotGraph(valueToString: T => String): String = toDotGraphIn(descendantTrees(), valueToString)
  //
  //    //DOT�`���̃O���t���쐬����
  //    //�l�Ɣh���l���琬��g�̖؂��쐬����֐����󂯎��
  //    //�l�𕶎���ɕϊ�����֐����󂯎��
  //    def toDotGraph[S](base: T => ITree[(T, S)], rec: (T, ITree[(T, S)]) => ITree[(T, S)], valueToString: ((T, S)) => String): String = toDotGraphIn[(T, S)](descendantTrees(base, rec), valueToString)

  //DOT�`���̃O���t���쐬����
  //�l�𕶎���ɕϊ�����֐����󂯎��
  def toDotGraph(valueToString: T => String): String = toDotGraph(valueToString, (_) => __.emptyString, (_, _) => __.emptyString)

  //DOT�`���̃O���t���쐬����
  //�l�𕶎���ɕϊ�����֐����󂯎��
  //�l��ݒ�ɕϊ�����֐����󂯎��
  def toDotGraph(valueToString: T => String, valueToNodeSettings: T => String, valueToEdgeStrrings: (T, T) => String): String = {
    val descendants: Traversable[ITree[T]] = descendantTrees()
    //�q�������݂��Ȃ��ꍇ�ɂ͋�̃O���t���쐬����
    if (descendants.isEmpty) {
      Graphviz.createDigraph(graphName, __.emptyString)
    }
    //�q�������݂���ꍇ
    else {
      //�O���t�̑S�Ă̐߂��i�[����
      val nodes: ListBuffer[String] = ListBuffer()
      //�O���t�̑S�Ă̕ӂ��i�[����
      val edges: ListBuffer[String] = ListBuffer()
      //�l���擾���A������ɕϊ����A�߂��쐬���A�ǉ�����
      nodes += Graphviz.createNode(valueToString(descendants.head.getValue), valueToNodeSettings(descendants.head.getValue))
      //�S�Ă̎q���ɑ΂���
      for (d <- descendants) {
        //�l���擾���A������ɕϊ�����
        val pValue: String = valueToString(d.getValue)
        //�S�Ă̎q�ɑ΂���
        for (c <- d.getChildren) {
          //�l���擾���A������ɕϊ�����
          val cValue: String = valueToString(c.getValue)
          //�߂��쐬���A�ǉ�����
          nodes += Graphviz.createNode(cValue, valueToNodeSettings(c.getValue))
          //�ӂ��쐬���A�ǉ�����
          edges += Graphviz.createEdge(pValue, cValue, valueToEdgeStrrings(d.getValue, c.getValue))
        }
      }
      //�O���t���쐬����
      Graphviz.createDigraph(graphName, nodes.mkString(__.newlineString) + __.newlineString + edges.mkString(__.newlineString))
    }
  }
}

//�l�𒼐ڕێ������
class ValueTree[T](value: T, var children: ListBuffer[ITree[T]], parent: Option[ITree[T]]) extends ITree[T] {
  //�l���擾����
  def getValue: T = value
  //�q���擾����
  def getChildren: Traversable[ITree[T]] = children
  //�e���擾����
  def getParent: Option[ITree[T]] = parent

  //�q��ǉ�����
  def addChild(child: ITree[T]): Unit = children += child
  //�q���폜����
  def removeChild(child: ITree[T]): Unit = children -= child
}