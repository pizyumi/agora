package Blockchain

import scala.collection.mutable.ListBuffer

import Common._

trait BlockTreeBase extends IBlockChain {
  //ブロック木
  protected val blockTree: ValueTree[IBlock]
  //有効な先頭ブロック
  protected var activeHead: ValueTree[IBlock] = blockTree

  //指定した識別子のブロック木が含まれているか
  protected def isContainBlockTree(id: IId): Boolean
  //指定した識別子のブロック木を取得する
  protected def getBlockTree(id: IId): Option[ITree[IBlock]]
  //指定した番号の有効なブロック木を取得する
  protected def getActiveBlockTree(index: IIndex): Option[ITree[IBlock]]
  //指定した番号の有効なブロック木を変更する
  protected def changeActiveBlockTree(index: IIndex, blockTree: Option[ITree[IBlock]]): Unit
  //親ブロック木にブロック木を追加する
  protected def addBlockTree(pt: ITree[IBlock], bt: ITree[IBlock]): Unit
  //親ブロック木からブロック木を削除する
  protected def removeBlockTree(pt: ITree[IBlock], bt: ITree[IBlock]): Unit

  //ブロックを追加する
  def addBlock(block: IBlock): Either[Unit, String] = {
    if (block == null) {
      Right("the block is null")
    }
    else {
      block.parentId match {
        case Some(pId) =>
          getBlockTree(pId) match {
            case Some(pt) =>
              if (pt.getValue.index.moveForward(1) != block.index) {
                Right("the block's index is wrong")
              }
              else {
                pt.getChildren.find((t) => t.getValue.id == block.id) match {
                  case Some(_) => Right("the block is already in the blockchain")
                  case None =>
                    val bt: ValueTree[IBlock] = new ValueTree(block, ListBuffer(), Some(pt))
                    addBlockTree(pt, bt)
                    challenge(bt)
                    Left()
                }
              }
            case None => Right("the block's parent block is not in the blockchain")
          }
        case None => Right("the block's parent id is not specified")
      }
    }
  }

  //有効な先頭ブロックを更新する（ブロック追加時）
  private def challenge(blockTree: ValueTree[IBlock]): Unit = {
    var cumulativeTrustworthiness: ITrustworthiness = blockTree.getValue.trustworthiness
    var activeCumulativeTrustworthiness: ITrustworthiness = activeHead.getValue.trustworthiness

    var bt: ITree[IBlock] = blockTree
    var f: Boolean = true
    while (bt.getValue.index.isGreat(activeHead.getValue.index) && f) {
      bt.getParent match {
        case Some(pt) =>
          cumulativeTrustworthiness = cumulativeTrustworthiness.add(pt.getValue.trustworthiness).asInstanceOf[ITrustworthiness]
          bt = pt
        case None => f = false
      }
    }

    if (f) {
      if (bt == activeHead) {
        activeHead = blockTree
        changeActiveBlockTree(blockTree.getValue.index, Some(blockTree))
      }
      else {
        var activebt: ITree[IBlock] = activeHead
        var f2: Boolean = true
        while (activebt.getValue.index.isGreat(bt.getValue.index) && f2) {
          activebt.getParent match {
            case Some(pt) =>
              activeCumulativeTrustworthiness = activeCumulativeTrustworthiness.add(pt.getValue.trustworthiness).asInstanceOf[ITrustworthiness]
              activebt = pt
            case None => f2 = false
          }
        }

        if (f2) {
          var f3: Boolean = true
          while (bt != activebt && f3) {
            bt.getParent match {
              case Some(pt) =>
                cumulativeTrustworthiness = cumulativeTrustworthiness.add(pt.getValue.trustworthiness).asInstanceOf[ITrustworthiness]
                bt = pt
              case None => f3 = false
            }
            activebt.getParent match {
              case Some(pt) =>
                activeCumulativeTrustworthiness = activeCumulativeTrustworthiness.add(pt.getValue.trustworthiness).asInstanceOf[ITrustworthiness]
                activebt = pt
              case None => f3 = false
            }
          }

          if (f3) {
            if (cumulativeTrustworthiness.isGreat(activeCumulativeTrustworthiness)) {
              changeActiveBlockchain(blockTree)
            }
            else if (cumulativeTrustworthiness.isSame(activeCumulativeTrustworthiness)) {
              if (__.getRandomBoolean) {
                changeActiveBlockchain(blockTree)
              }
            }
          }
        }
      }
    }
  }

  //ブロックを削除する
  def deleteBlock(id: IId): Either[Unit, String] = {
    if (id == null) {
      Right("the id is null")
    }
    else {
      getBlockTree(id) match {
        case Some(bt) =>
          if (bt.getChildren.toArray.length == 0) {
            bt.getParent match {
              case Some(pt) =>
                removeBlockTree(pt, bt)
                if (bt == activeHead) {
                  dechallenge()
                }
                Left()
              case None => Right("the block's parent block does not exist (the genesis block can not be deleted)")
            }
          }
          else {
            Right("the block has child block")
          }
        case None => Right("the block does not exist")
      }
    }
  }

  //有効な先頭ブロックを更新する（ブロック削除時）
  private def dechallenge(): Unit = {
    var head: ValueTree[IBlock] = blockTree
    var maxCumulativeTrustworthiness: ITrustworthiness = blockTree.getValue.trustworthiness
    for ((bt, d) <- blockTree.descendantDeriveTrees[ITrustworthiness]((b) => b.trustworthiness, (pd, b) => pd.add(b.trustworthiness).asInstanceOf[ITrustworthiness])) {
      if (bt.getChildren.isEmpty && d.isGreat(maxCumulativeTrustworthiness)) {
        head = bt.asInstanceOf[ValueTree[IBlock]]
        maxCumulativeTrustworthiness = d
      }
    }
    changeActiveBlockchain(head)
  }

  //有効なブロック鎖の一部を変更する
  private def changeActiveBlockchain(head: ValueTree[IBlock]) = {
    var ablockTree: ITree[IBlock] = activeHead
    var f: Boolean = true
    while (ablockTree.getValue.index.isGreat(head.getValue.index) && f) {
      changeActiveBlockTree(ablockTree.getValue.index, None)
      ablockTree.getParent match {
        case Some(pt) => ablockTree = pt
        case None => f = false
      }
    }
    activeHead = head
    ablockTree = head
    var f2 = true
    while (getActiveBlockTree(ablockTree.getValue.index).get != ablockTree && f2) {
      changeActiveBlockTree(ablockTree.getValue.index, Some(ablockTree))
      ablockTree.getParent match {
        case Some(pt) => ablockTree = pt
        case None => f2 = false
      }
    }
  }

  //ブロックを削除する
  def deleteBlock(block: IBlock): Either[Unit, String] = {
    if (block == null) {
      Right("the block is null")
    }
    else {
      deleteBlock(block.id)
    }
  }

  //ブロックを取得する
  def getBlock(id: IId): Option[IBlock] = {
    if (id == null) {
      None
    }
    else {
      getBlockTree(id).map((bt) => bt.getValue)
    }
  }

  //親ブロックを取得する
  def getParentBlock(block: IBlock): Option[IBlock] = {
    if (block == null) {
      None
    }
    else {
      getBlockTree(block.id).flatMap((bt) => bt.getParent).map((pt) => pt.getValue)
    }
  }

  //子ブロックを取得する
  def getChildBlocks(block: IBlock): Option[Traversable[IBlock]] = {
    if (block == null) {
      None
    }
    else {
      getBlockTree(block.id).map((bt) => bt.getChildren.map((ct) => ct.getValue))
    }
  }

  //ブロックが含まれている場合にはtrueを返し、含まれていない場合にはfalseを返す
  def isContain(block: IBlock): Boolean = {
    if (block == null) {
      false
    }
    else {
      isContainBlockTree(block.id)
    }
  }

  //ブロックが含まれている場合にはtrueを返し、含まれていない場合にはfalseを返す
  def isContain(id: IId): Boolean = {
    if (id == null) {
      false
    }
    else {
      isContainBlockTree(id)
    }
  }

  //先頭ブロックを取得する
  def getHeadBlock: IBlock = activeHead.getValue

  //指定した番号の有効なブロックを取得する
  def getActiveBlock(index: IIndex): Option[IBlock] = {
    if (index == null) {
      None
    }
    else {
      getActiveBlockTree(index).map((abt) => abt.getValue)
    }
  }

  //先頭ブロックから指定した個数分の有効なブロックを取得する
  def getBlockchain(n: Int): Traversable[IBlock] = getPath(activeHead, n)

  //指定した番号から指定した個数分の有効なブロックを取得する
  def getBlockchain(index: IIndex, n: Int): Option[Traversable[IBlock]] = {
    if (index == null) {
      None
    }
    else {
      getActiveBlockTree(index).map((ab) => getPath(ab, n))
    }
  }

  //指定したブロックから指定した個数分のブロックを取得する
  private def getPath(block: ITree[IBlock], n: Int): Traversable[IBlock] = {
    var ablock: ITree[IBlock] = block
    var i: Int = n
    var f: Boolean = true
    val lb: ListBuffer[IBlock] = ListBuffer()
    while (i > 0 && f) {
      lb += ablock.getValue
      ablock.getParent match {
        case Some(p) => ablock = p
        case None => f = false
      }
      i -= 1
    }
    lb
  }

  //ブロックが有効な場合にはtrueを返し、有効でない場合にはfalseを返す
  def isActive(block: IBlock): Boolean = {
    if (block == null) {
      false
    }
    else {
      getActiveBlockTree(block.index) match {
        case Some(b) => b.getValue == block
        case None => false
      }
    }
  }

  //ブロックが有効な場合にはtrueを返し、有効でない場合にはfalseを返す
  def isActive(id: IId): Boolean = {
    if (id == null) {
      false
    }
    else {
      getPath(activeHead, Int.MaxValue).exists((ab) => ab.id == id)
    }
  }

  //グラフの節の設定
  lazy val nodeSettings: String = Graphviz.createSettings(Array(Graphviz.createSetting(Graphviz.keywordColor, __.colorRed)))

  //DOT形式のグラフを作成する
  override def toDotGraph(valueToString: IBlock => String): String = blockTree.toDotGraph(valueToString, (b) => if (isActive(b)) nodeSettings else __.emptyString, (p, b) => __.emptyString)
}

//ブロック鎖のある高さのブロックの集まり
class IndexBlockTrees() {
  //ブロック鎖のある高さの全てのブロック
  val lb: ListBuffer[ITree[IBlock]] = ListBuffer()
  //ブロック鎖のある高さの有効なブロック
  var activeBlockTree: Option[ITree[IBlock]] = None
}

//インデックス付きのブロック鎖
class IndexedBlockTree(genesis: IGenesisBlock) extends BlockTreeBase {
  //ブロック木
  protected val blockTree: ValueTree[IBlock] = new ValueTree(genesis, ListBuffer(), None)
  //ブロックの識別子からブロックへの参照
  protected val mapIdToBlockTree: scala.collection.mutable.Map[IId, ITree[IBlock]] = scala.collection.mutable.Map()
  //ブロックの番号からブロックの集まりへの参照
  protected val mapIndexToBlockTrees: scala.collection.mutable.Map[IIndex, IndexBlockTrees] = scala.collection.mutable.Map()

  //指定した識別子のブロック木が含まれているか
  protected def isContainBlockTree(id: IId): Boolean = mapIdToBlockTree.contains(id)
  //指定した識別子のブロック木を取得する
  protected def getBlockTree(id: IId): Option[ITree[IBlock]] = mapIdToBlockTree.get(id)
  //指定した番号の有効なブロック木を取得する
  protected def getActiveBlockTree(index: IIndex): Option[ITree[IBlock]] = {
    if (index.isGreat(activeHead.getValue.index)) {
      None
    }
    else {
      mapIndexToBlockTrees(index).activeBlockTree
    }
  }
  //指定した番号の有効なブロック木を変更する
  protected def changeActiveBlockTree(index: IIndex, blockTree: Option[ITree[IBlock]]): Unit = mapIndexToBlockTrees(index).activeBlockTree = blockTree
  //親ブロック木にブロック木を追加する
  protected def addBlockTree(pt: ITree[IBlock], bt: ITree[IBlock]): Unit = {
    pt.addChild(bt)
    mapIdToBlockTree.put(bt.getValue.id, bt)
    if (mapIndexToBlockTrees.contains(bt.getValue.index)) {
      mapIndexToBlockTrees(bt.getValue.index).lb += bt
    }
    else {
      val indexBlockTrees = new IndexBlockTrees()
      indexBlockTrees.lb += bt
      mapIndexToBlockTrees.put(bt.getValue.index, indexBlockTrees)
    }
  }
  //親ブロック木からブロック木を削除する
  protected def removeBlockTree(pt: ITree[IBlock], bt: ITree[IBlock]): Unit = {
    pt.removeChild(bt)
    mapIdToBlockTree.remove(bt.getValue.id)
    val indexBlockTrees = mapIndexToBlockTrees(bt.getValue.index)
    indexBlockTrees.lb -= bt
    if (indexBlockTrees.lb.isEmpty) {
      mapIndexToBlockTrees.remove(bt.getValue.index)
    }
  }
}

//ブロック鎖
class BlockTree(genesis: IGenesisBlock) extends BlockTreeBase {
  //ブロック木
  protected val blockTree: ValueTree[IBlock] = new ValueTree(genesis, ListBuffer(), None)

  //指定した識別子のブロック木が含まれているか
  protected def isContainBlockTree(id: IId): Boolean = blockTree.descendants().exists((b) => b.id == id)
  //指定した識別子のブロック木を取得する
  protected def getBlockTree(id: IId): Option[ITree[IBlock]] = blockTree.descendantTrees().find((t) => t.getValue.id == id)
  //指定した番号の有効なブロック木を取得する
  protected def getActiveBlockTree(index: IIndex): Option[ITree[IBlock]] = {
    if (index.isGreat(activeHead.getValue.index)) {
      None
    }
    else {
      var ablock: ITree[IBlock] = activeHead
      var f: Boolean = true
      while (ablock.getValue.index != index && f) {
        ablock.getParent match {
          case Some(p) => ablock = p
          case None => f = false
        }
      }
      if (!f) {
        None
      }
      else {
        Some(ablock)
      }
    }
  }
  //指定した番号の有効なブロック木を変更する
  protected def changeActiveBlockTree(index: IIndex, blockTree: Option[ITree[IBlock]]): Unit = {}
  //親ブロック木にブロック木を追加する
  protected def addBlockTree(pt: ITree[IBlock], bt: ITree[IBlock]): Unit = pt.addChild(bt)
  //親ブロック木からブロック木を削除する
  protected def removeBlockTree(pt: ITree[IBlock], bt: ITree[IBlock]): Unit = pt.removeChild(bt)
}