package Blockchain

import java.math.BigInteger

import scala.collection.mutable.ListBuffer

import Common._

//ブロックの識別子の標準実装
class IdV1(idIn: Array[Byte]) extends IId {
  //ブロックの識別子の長さの最大値
  val maxIdLength: Int = 32

  //ブロックの識別子は32バイト以下のバイト配列である
  val id: Array[Byte] = {
    if (idIn.length > maxIdLength) {
      throw new IllegalArgumentException("id is too long")
    }
    idIn
  }

  //ブロックの識別子のバイト配列が同等である場合、ブロックの識別子は同一である
  protected override def specIsSame(r: ICompare): Boolean = id.sameElements(r.asInstanceOf[IdV1].id)
  //ハッシュコードを返す関数
  protected override def specHashCode: Int = {
    if (id.length > 4) {
      ((id(id.length - 1) * 256 + id(id.length - 2)) * 256 + id(id.length - 3)) * 256 + id(id.length - 4)
    }
    else if (id.length == 3) {
      (id(id.length - 1) * 256 + id(id.length - 2)) * 256 + id(id.length - 3)
    }
    else if (id.length == 2) {
      id(id.length - 1) * 256 + id(id.length - 2)
    }
    else if (id.length == 1) {
      id(id.length - 1)
    }
    else {
      0
    }
  }
  //ブロックの識別子をバイト配列に変換したものはブロックの識別子のバイト配列そのものである
  protected override def specToBytes: Array[Byte] = id
}

//ブロックの番号の標準実装
class IndexV1(indexIn: Long) extends IIndex {
  //ブロックの番号の最小値
  val minIndex: Long = 0

  //ブロックの番号は0以上の64ビット整数である
  val index: Long = {
    if (indexIn < minIndex) {
      throw new IllegalArgumentException("index is out of bound")
    }
    indexIn
  }

  //ブロックの番号を比較する
  protected override def specCompare(r: ICompareOrder): Ordering = {
    r.asInstanceOf[IndexV1] match {
      case ras if index > ras.index => Great
      case ras if index == ras.index => Equal
      case ras if index < ras.index => Less
    }
  }
  //ハッシュコードを返す関数
  protected override def specHashCode: Int = (index % Int.MaxValue).toInt
  //ブロックの番号を移動する
  protected override def specMove(delta: Long) = new IndexV1(index + delta)
  //ブロックの番号をバイト配列に変換する
  protected override def specToBytes: Array[Byte] = __.getBytes(index)
}

//ブロックの信用度の標準実装
class TrustworthinessV1(trustworthinessIn: BigInteger) extends ITrustworthiness {
  //ブロックの信用度
  val trustworthiness: BigInteger = trustworthinessIn

  //ブロックの信用度を比較する
  protected override def specCompare(r: ICompareOrder): Ordering = {
    val comp: Int = trustworthiness.compareTo(r.asInstanceOf[TrustworthinessV1].trustworthiness)
    if (comp > 0) {
      Great
    }
    else if (comp == 0) {
      Equal
    }
    else {
      Less
    }
  }
  //ハッシュコードを返す関数
  protected override def specHashCode: Int = trustworthiness.mod(BigInteger.valueOf(Int.MaxValue)).intValue()
  //ブロックの信用度を加算する
  protected override def specAdd(r: IAddition): IAddition = new TrustworthinessV1(trustworthiness.add(r.asInstanceOf[TrustworthinessV1].trustworthiness))
  //ブロックの信用度を減算する
  protected override def specSubtract(r: IAddition): IAddition = new TrustworthinessV1(trustworthiness.subtract(r.asInstanceOf[TrustworthinessV1].trustworthiness))
}

//ブロックの標準実装
trait BlockBaseV1 extends IBlock {
  lazy val id: IdV1 = new IdV1(toIdSha256)

  protected def toIdBytesIngredient: Array[Array[Byte]] = Array(index.toBytes, parentId.map((v) => v.toBytes).getOrElse(Array.emptyByteArray))
  private def toIdBytes: Array[Byte] = __.getBytes(toIdBytesIngredient)
  private def toIdSha256: Array[Byte] = __.getSha256(toIdBytes)

  protected override def specIsSame(r: ICompare): Boolean = id.id.sameElements(r.asInstanceOf[BlockBaseV1].id.id)
  //ハッシュコードを返す関数
  protected override def specHashCode: Int = id.hashCode
}

class GenesisBlockTest1(seedIn: String) extends BlockBaseV1 with IGenesisBlock {
  val maxSeedLength: Int = 1024

  lazy val index: IndexV1 = new IndexV1(0)
  lazy val trustworthiness: TrustworthinessV1 = new TrustworthinessV1(BigInteger.ZERO)

  val seed: String = {
    if (seedIn.length > maxSeedLength) {
      throw new IllegalArgumentException("seed is too long")
    }
    seedIn
  }

  protected override def toIdBytesIngredient: Array[Array[Byte]] = super.toIdBytesIngredient ++ Array(__.getBytes(seed))
}

class NormalBlockTest1(indexIn: IndexV1, parentIdIn: IdV1, trustworthinessIn: TrustworthinessV1, dataIn: Array[Byte]) extends BlockBaseV1 {
  val maxDataLength: Int = 1024

  val index: IndexV1 = indexIn
  val parentId: Option[IdV1] = Some(parentIdIn)
  val trustworthiness: TrustworthinessV1 = trustworthinessIn

  val data: Array[Byte] = {
    if (dataIn.length > maxDataLength) {
      throw new IllegalArgumentException("data is too long")
    }
    dataIn
  }

  protected override def toIdBytesIngredient: Array[Array[Byte]] = super.toIdBytesIngredient ++ Array(data, trustworthiness.trustworthiness.toByteArray)
}

class BlockTree(genesis: IGenesisBlock) extends IBlockChain {
  //ブロック木
  private val blockTree: ValueTree[IBlock] = new ValueTree(genesis, ListBuffer(), None)
  //有効な先頭ブロック
  private var activeHead: ValueTree[IBlock] = blockTree

  //ブロックを追加する
  //O(n)
  def addBlock(block: IBlock): Either[Unit, String] = {
    if (block == null) {
      Right("the block is null")
    }
    else {
      block.parentId match {
        case Some(pId) =>
          blockTree.descendantTrees().find((t) => t.getValue.id == pId) match {
            case Some(p) =>
              if (p.getValue.index.moveForward(1) != block.index) {
                Right("the block's index is wrong")
              }
              else {
                p.getChildren.find((t) => t.getValue.id == block.id) match {
                  case Some(_) => Right("the block is already in the blockchain")
                  case None =>
                    val bt: ValueTree[IBlock] = new ValueTree(block, ListBuffer(), Some(p))
                    p.addChild(bt)
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

  //有効な先頭ブロックを更新する
  //O(n)程度？
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
              activeHead = blockTree
            }
            else if (cumulativeTrustworthiness.isSame(activeCumulativeTrustworthiness)) {
              if (__.getRandomBoolean) {
                activeHead = blockTree
              }
            }
          }
        }
      }
    }
  }

  //ブロックを取得する
  //O(n)
  def getBlock(id: IId): Option[IBlock] = {
    if (id == null) {
      None
    }
    else {
      blockTree.descendants().find((b) => b.id == id)
    }
  }

  //親ブロックを取得する
  //O(n)
  def getParentBlock(block: IBlock): Option[IBlock] = {
    if (block == null) {
      None
    }
    else {
      blockTree.descendantTrees().find((t) => t.getValue == block).flatMap((t) => t.getParent).map((t) => t.getValue)
    }
  }

  //子ブロックを取得する
  //O(n)
  def getChildBlocks(block: IBlock): Option[Traversable[IBlock]] = {
    if (block == null) {
      None
    }
    else {
      blockTree.descendantTrees().find((t) => t.getValue == block).map((t) => t.getChildren.map((elem) => elem.getValue))
    }
  }

  //ブロックが含まれている場合にはtrueを返し、含まれていない場合にはfalseを返す
  //O(n)
  def isContain(block: IBlock): Boolean = {
    if (block == null) {
      false
    }
    else {
      blockTree.descendants().exists((b) => block.id == b.id)
    }
  }

  //ブロックが含まれている場合にはtrueを返し、含まれていない場合にはfalseを返す
  //O(n)
  def isContain(id: IId): Boolean = {
    if (id == null) {
      false
    }
    else {
      blockTree.descendants().exists((b) => b.id == id)
    }
  }

  //先頭ブロックを取得する
  //O(1)
  def getHeadBlock: IBlock = activeHead.getValue

  //指定した番号の有効なブロックを取得する
  //O(n)程度
  def getActiveBlock(index: IIndex): Option[IBlock] = {
    if (index == null) {
      None
    }
    else {
      getActiveBlockTree(index).map((abt) => abt.getValue)
    }
  }

  //指定した番号の有効なブロック木を取得する
  //O(n)程度
  private def getActiveBlockTree(index: IIndex): Option[ITree[IBlock]] = {
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

  //先頭ブロックから指定した個数分の有効なブロックを取得する
  //O(n)程度
  def getBlockchain(n: Int): Traversable[IBlock] = getPath(activeHead, n)

  //指定した番号から指定した個数分の有効なブロックを取得する
  //O(n)程度
  def getBlockchain(index: IIndex, n: Int): Option[Traversable[IBlock]] = {
    if (index == null) {
      None
    }
    else {
      getActiveBlockTree(index).map((ab) => getPath(ab, n))
    }
  }

  //指定したブロックから指定した個数分のブロックを取得する
  //O(n)程度
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
  //O(n)程度
  def isActive(block: IBlock): Boolean = {
    if (block == null) {
      false
    }
    else {
      getActiveBlock(block.index) match {
        case Some(b) => b == block
        case None => false
      }
    }
  }

  //ブロックが有効な場合にはtrueを返し、有効でない場合にはfalseを返す
  //O(n^2)程度
  def isActive(id: IId): Boolean = {
    if (id == null) {
      false
    }
    else {
      getBlock(id).map((b) => isActive(b)) match {
        case Some(f) => f
        case None => false
      }
    }
  }

  //DOT形式のグラフを作成する
  override def toDotGraph: String = blockTree.toDotGraph((b) => __.toHexString(b.id.toBytes.slice(0, 8)))
}

object StandardUtil {
  def idToString(id: IdV1): String = __.toKeyValueString("id", __.toHexString(id.id))
  def indexToString(index: IndexV1): String = __.toKeyValueString("index", index.index.toString)
  def trustworthinessToString(trustworthiness: TrustworthinessV1): String = __.toKeyValueString("trustworthiness", trustworthiness.trustworthiness.toString)
  def genesisBlockToString(gblock: GenesisBlockTest1): String = {
    __.toMultilineString(Array(
      indexToString(gblock.index),
      idToString(gblock.id),
      trustworthinessToString(gblock.trustworthiness),
      __.toKeyValueString("seed", gblock.seed)
    ))
  }
  def normalBlockToString(nblock: NormalBlockTest1): String = {
    __.toMultilineString(Array(
      indexToString(nblock.index),
      idToString(nblock.id),
      trustworthinessToString(nblock.trustworthiness),
      __.toKeyValueString("parent id", nblock.parentId.map((t) => __.toHexString(t.id)).getOrElse(__.nullString)),
      __.toKeyValueString("data", __.toHexString(nblock.data))
    ))
  }

  def genesisBlockToHTML(gblock: GenesisBlockTest1): String = {
    __.toMultilineStringHTML(Array(
      indexToString(gblock.index),
      idToString(gblock.id),
      trustworthinessToString(gblock.trustworthiness),
      __.toKeyValueString("seed", gblock.seed)
    ))
  }
  def normalBlockToHTML(nblock: NormalBlockTest1): String = {
    __.toMultilineStringHTML(Array(
      indexToString(nblock.index),
      idToString(nblock.id),
      trustworthinessToString(nblock.trustworthiness),
      __.toKeyValueString("parent id", nblock.parentId.map((t) => __.toHexString(t.id)).getOrElse(__.nullString)),
      __.toKeyValueString("data", __.toHexString(nblock.data))
    ))
  }
}