import java.math.BigInteger
import java.nio.file.Files
import java.nio.file.Path

import scala.collection.mutable.ListBuffer

object Interface {
  import Common.Common._

  //ブロックの識別子を表す
  //同一比較可能であり、バイト配列に変換可能である
  trait IId extends ICompare with IBytes {}
  //ブロックの高さを表す
  //大小比較可能であり、移動可能であり、バイト配列に変換可能である
  trait IIndex extends ICompareOrder with ISequence with IBytes {}
  //ブロックの信用度を表す
  //大小比較可能であり、加減算可能である
  trait ITrustworthiness extends ICompareOrder with IAddition {}
  //ブロックを表す
  //同一比較可能である
  trait IBlock extends ICompare {
    //高さ
    val index: IIndex
    //識別子
    val id: IId
    //親ブロックの識別子
    val parentId: Option[IId]
    //信用度
    val trustworthiness: ITrustworthiness
  }
  //起源ブロックを表す
  trait IGenesisBlock extends IBlock {
    //起源ブロックの親ブロックの識別子は空である
    lazy val parentId: Option[IId] = None
  }

  //ブロック鎖を表す
  trait IBlockChain {
    //ブロックを追加する
    def addBlock(block: IBlock): Either[Unit, String]
    //ブロックを削除する
    //def deleteBlock(id: IId): Either[Unit, String]
    //def deleteBlock(block: IBlock): Either[Unit, String]
    //ブロックを取得する
    def getBlock(id: IId): Option[IBlock]
    //親ブロックを取得する
    def getParentBlock(block: IBlock): Option[IBlock]
    //子ブロックを取得する
    def getChildBlocks(block: IBlock): Option[Traversable[IBlock]]
    //ブロックが含まれている場合にはtrueを返し、含まれていない場合にはfalseを返す
    def isContain(block: IBlock): Boolean
    def isContain(id: IId): Boolean

    //先頭ブロックを取得する
    def getHeadBlock: IBlock
    //指定した番号の有効なブロックを取得する
    def getActiveBlock(index: IIndex): Option[IBlock]
    //先頭ブロックから指定した個数分の有効なブロックを取得する
    def getBlockchain(n: Int): Traversable[IBlock]
    //指定した番号から指定した個数分の有効なブロックを取得する
    def getBlockchain(index: IIndex, n: Int): Option[Traversable[IBlock]]
    //ブロックが有効な場合にはtrueを返し、有効でない場合にはfalseを返す
    def isActive(block: IBlock): Boolean
    def isActive(id: IId): Boolean

    //DOT形式のグラフを作成する
    def toDotGraph: String = throw new UnsupportedOperationException()
  }
}

object StandardImplemantation {
  import Common.Common._
  import Interface._

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
      val cumulativeTrustworthiness: ITrustworthiness = blockTree.getValue.trustworthiness
      val activeCumulativeTrustworthiness: ITrustworthiness = activeHead.getValue.trustworthiness

      var bt: ITree[IBlock] = blockTree
      var f: Boolean = true
      while (bt.getValue.index.isGreat(activeHead.getValue.index) && f) {
        bt.getParent match {
          case Some(pt) =>
            cumulativeTrustworthiness.add(pt.getValue.trustworthiness)
            bt = pt
          case None => f = false
        }
      }

      if (!f) {
        if (bt == activeHead) {
          activeHead = blockTree
        }
        else {
          var activebt: ITree[IBlock] = activeHead
          var f2: Boolean = true
          while (activebt.getValue.index.isGreat(bt.getValue.index) && f2) {
            activebt.getParent match {
              case Some(pt) =>
                activeCumulativeTrustworthiness.add(pt.getValue.trustworthiness)
                activebt = pt
              case None => f2 = false
            }
          }

          if (!f2) {
            var f3: Boolean = true
            while (bt != activebt && f3) {
              bt.getParent match {
                case Some(pt) =>
                  cumulativeTrustworthiness.add(pt.getValue.trustworthiness)
                  bt = pt
                case None => f3 = false
              }
              activebt.getParent match {
                case Some(pt) =>
                  activeCumulativeTrustworthiness.add(pt.getValue.trustworthiness)
                  activebt = pt
                case None => f3 = false
              }
            }

            if (!f3) {
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

  class StandardImplementationTestCLI() {
    protected lazy val testInterface: TestInterface = new TestCLI()

    protected lazy val testId: String = "test id"
    protected lazy val testIndex: String = "test index"
    protected lazy val testGenesisBlock: String = "test genesis block"
    protected lazy val testNormalBlock: String = "test normal block"
    protected lazy val testBlockTree: String = "test block tree"

    def executeCommand(command: String): Boolean = {
      if (command.startsWith(testId)) {
        doTestId()
        true
      }
      else if (command.startsWith(testIndex)) {
        doTestIndex()
        true
      }
      else if (command.startsWith(testGenesisBlock)) {
        doTestGenesisBlock()
        true
      }
      else if (command.startsWith(testNormalBlock)) {
        doTestNormalBlock()
        true
      }
      else if (command.startsWith(testBlockTree)) {
        doTestBlockTree()
        true
      }
      else {
        false
      }
    }

    protected def doTestId(): Unit = {
      testInterface.outputTitle("id test", None)

      testInterface.outputMessage("generating id...")
      val id: IdV1 = new IdV1(__.getRandomBytes(32).toArray)
      testInterface.outputMessage(StandardUtil.idToString(id))
      testInterface.outputMessage("copying id...")
      val id2: IdV1 = new IdV1(id.id.clone())
      testInterface.outputMessage(StandardUtil.idToString(id2))
      testInterface.outputItem("1.1", Some("both ids are same"), id == id2)
      testInterface.outputMessage("generating another id...")
      val id3: IdV1 = new IdV1(__.getRandomBytes(32).toArray)
      testInterface.outputMessage(StandardUtil.idToString(id3))
      testInterface.outputItem("1.2", Some("both ids are not same"), id != id3)
    }

    protected def doTestIndex(): Unit = {
      testInterface.outputTitle("index test", None)

      testInterface.outputMessage("generating index...")
      val index: IndexV1 = new IndexV1(__.getRandomLong)
      testInterface.outputMessage(StandardUtil.indexToString(index))
      testInterface.outputMessage("copying index...")
      val index2: IndexV1 = new IndexV1(index.index)
      testInterface.outputMessage(StandardUtil.indexToString(index2))
      testInterface.outputItem("2.1", Some("both indexes are same"), index == index2)
      testInterface.outputMessage("generating another index...")
      val index3: IndexV1 = new IndexV1(__.getRandomLong)
      testInterface.outputMessage(StandardUtil.indexToString(index3))
      testInterface.outputItem("2.2", Some("both indexes are not same"), index != index3)
    }

    protected def doTestGenesisBlock(): Unit = {
      testInterface.outputTitle("genesis block test", None)

      testInterface.outputMessage("generating genesis block...")
      val gblock: GenesisBlockTest1 = new GenesisBlockTest1(__.getRandomPrintableString(32))
      testInterface.outputMessage(StandardUtil.genesisBlockToString(gblock))
      testInterface.outputMessage("copying genesis block...")
      val gblock2: GenesisBlockTest1 = new GenesisBlockTest1(gblock.seed)
      testInterface.outputMessage(StandardUtil.genesisBlockToString(gblock2))
      testInterface.outputItem("3.1", Some("both genesis blocks are same"), gblock == gblock2)
      testInterface.outputMessage("generating another genesis block...")
      val gblock3: GenesisBlockTest1 = new GenesisBlockTest1(__.getRandomPrintableString(32))
      testInterface.outputMessage(StandardUtil.genesisBlockToString(gblock3))
      testInterface.outputItem("3.2", Some("both genesis blocks are not same"), gblock != gblock3)
    }

    protected def doTestNormalBlock(): Unit = {
      val trustworthiness: TrustworthinessV1 = new TrustworthinessV1(BigInteger.ZERO)

      testInterface.outputTitle("normal block test", None)

      testInterface.outputMessage("generating normal block...")
      val nblock: NormalBlockTest1 = new NormalBlockTest1(new IndexV1(__.getRandomLong), new IdV1(__.getRandomBytes(32).toArray), trustworthiness, __.getRandomBytes(32).toArray)
      testInterface.outputMessage(StandardUtil.normalBlockToString(nblock))
      testInterface.outputMessage("copying normal block...")
      val nblock2: NormalBlockTest1 = new NormalBlockTest1(nblock.index, nblock.parentId.get, trustworthiness, nblock.data)
      testInterface.outputMessage(StandardUtil.normalBlockToString(nblock2))
      testInterface.outputItem("4.1", Some("both normal blocks are same"), nblock == nblock2)
      testInterface.outputMessage("generating another normal block...")
      val nblock3: NormalBlockTest1 = new NormalBlockTest1(nblock.index, new IdV1(__.getRandomBytes(32).toArray), trustworthiness, __.getRandomBytes(32).toArray)
      testInterface.outputMessage(StandardUtil.normalBlockToString(nblock3))
      testInterface.outputItem("4.2", Some("both normal blocks are not same"), nblock != nblock3)
      testInterface.outputMessage("generating yet another normal block...")
      val nblock4: NormalBlockTest1 = new NormalBlockTest1(new IndexV1(__.getRandomLong), nblock.parentId.get, trustworthiness, __.getRandomBytes(32).toArray)
      testInterface.outputMessage(StandardUtil.normalBlockToString(nblock4))
      testInterface.outputItem("4.3", Some("both normal blocks are not same"), nblock != nblock4)
    }

    protected def doTestBlockTree(): Unit = {
      val trustworthiness: TrustworthinessV1 = new TrustworthinessV1(BigInteger.ZERO)

      testInterface.outputTitle("block tree test", None)

      testInterface.outputMessage("generating genesis block...")
      val gblock: GenesisBlockTest1 = new GenesisBlockTest1(__.getRandomPrintableString(32))
      testInterface.outputMessage(StandardUtil.genesisBlockToString(gblock))
      testInterface.outputMessage("initializing block tree, providing genesis block...")
      val blocktree: BlockTree = new BlockTree(gblock)
      testInterface.outputItem("5.1", Some("genesis block retrieved from block tree is same as original genesis block"), blocktree.getBlock(gblock.id).get == gblock)
      testInterface.outputItem("5.2", Some("genesis block is contained in block tree"), blocktree.isContain(gblock.id))
      testInterface.outputItem("5.3", Some("genesis block is contained in block tree, again"), blocktree.isContain(gblock))
      testInterface.outputItem("5.4", Some("genesis block's parent block cannot be retrieved from block tree"), blocktree.getParentBlock(gblock).isEmpty)
      testInterface.outputMessage("generating normal block...")
      val nblock: NormalBlockTest1 = new NormalBlockTest1(new IndexV1(__.getRandomLong), new IdV1(__.getRandomBytes(32).toArray), trustworthiness, __.getRandomBytes(32).toArray)
      testInterface.outputMessage(StandardUtil.normalBlockToString(nblock))
      testInterface.outputItem("5.5", Some("normal block cannot be retrieved from block tree"), blocktree.getBlock(nblock.id).isEmpty)
      testInterface.outputItem("5.6", Some("normal block is not contained in block tree"), !blocktree.isContain(nblock.id))
      testInterface.outputItem("5.7", Some("normal block is not contained in block tree, again"), !blocktree.isContain(nblock))
      testInterface.outputItem("5.8", Some("normal block's parent block cannot be retrieved from block tree"), blocktree.getParentBlock(nblock).isEmpty)
      testInterface.outputItem("5.9", Some("normal block's child blocks cannot be retrieved from block tree"), blocktree.getChildBlocks(nblock).isEmpty)
      testInterface.outputMessage("trying to add genesis block to block tree...")
      val e1: Either[Unit, String] = blocktree.addBlock(gblock)
      testInterface.outputMessage(e1.right.get)
      testInterface.outputItem("5.10", Some("genesis block cannot be added to block tree"), e1.isRight)
      testInterface.outputMessage("trying to add normal block to block tree...")
      val e2: Either[Unit, String] = blocktree.addBlock(nblock)
      testInterface.outputMessage(e2.right.get)
      testInterface.outputItem("5.11", Some("normal block cannot be added to block tree"), e2.isRight)
      testInterface.outputMessage("generating another normal block...")
      val nblock2: NormalBlockTest1 = new NormalBlockTest1(new IndexV1(1), gblock.id, trustworthiness, __.getRandomBytes(32).toArray)
      testInterface.outputMessage(StandardUtil.normalBlockToString(nblock2))
      testInterface.outputItem("5.12", Some("normal block was added to block tree"), blocktree.addBlock(nblock2).isLeft)
      testInterface.outputMessage("trying to add normal block to block tree again...")
      val e3: Either[Unit, String] = blocktree.addBlock(nblock2)
      testInterface.outputMessage(e3.right.get)
      testInterface.outputItem("5.13", Some("normal block cannot be added to block tree twice"), e3.isRight)
      testInterface.outputMessage("generating yet another normal block...")
      val nblock3: NormalBlockTest1 = new NormalBlockTest1(new IndexV1(__.getRandomLong), gblock.id, trustworthiness, __.getRandomBytes(32).toArray)
      testInterface.outputMessage(StandardUtil.normalBlockToString(nblock3))
      testInterface.outputMessage("trying to add normal block to block tree...")
      val e4: Either[Unit, String] = blocktree.addBlock(nblock3)
      testInterface.outputMessage(e4.right.get)
      testInterface.outputItem("5.14", Some("normal block cannot be added to block tree"), e4.isRight)
      testInterface.outputItem("5.15", Some("normal block's parent block was retrieved from block tree"), blocktree.getParentBlock(nblock2).get == gblock)
      val children: Array[IBlock] = blocktree.getChildBlocks(gblock).get.toArray
      testInterface.outputItem("5.16", Some("normal block's child blocks was retrieved from block tree"), children.length == 1 && children(0) == nblock2)
    }
  }

  object StandardUtil {
    def idToString(id: IdV1): String = __.toKeyValueString("id", __.toHexString(id.id))
    def indexToString(index: IndexV1): String = __.toKeyValueString("index", index.index.toString)
    def genesisBlockToString(gblock: GenesisBlockTest1): String = {
      __.toMultilineString(Array(
        indexToString(gblock.index),
        idToString(gblock.id),
        __.toKeyValueString("seed", gblock.seed)
      ))
    }
    def normalBlockToString(nblock: NormalBlockTest1): String = {
      __.toMultilineString(Array(
        indexToString(nblock.index),
        idToString(nblock.id),
        __.toKeyValueString("parent id", nblock.parentId.map((t) => __.toHexString(t.id)).getOrElse(__.nullString)),
        __.toKeyValueString("data", __.toHexString(nblock.data))
      ))
    }

    def genesisBlockToHTML(gblock: GenesisBlockTest1): String = {
      __.toMultilineStringHTML(Array(
        indexToString(gblock.index),
        idToString(gblock.id),
        __.toKeyValueString("seed", gblock.seed)
      ))
    }
    def normalBlockToHTML(nblock: NormalBlockTest1): String = {
      __.toMultilineStringHTML(Array(
        indexToString(nblock.index),
        idToString(nblock.id),
        __.toKeyValueString("parent id", nblock.parentId.map((t) => __.toHexString(t.id)).getOrElse(__.nullString)),
        __.toKeyValueString("data", __.toHexString(nblock.data))
      ))
    }
  }
}

object StandardImplementationCLI {
  import Common.Common._
  import Graph._
  import Interface._
  import StandardImplemantation._

  trait IBusinessLogicFactory {
    def createGenesisBlock(seed: String): GenesisBlockTest1
    def createNormalBlock(index: IndexV1, parentId: IdV1, trustworthiness: TrustworthinessV1, data: Array[Byte]): NormalBlockTest1
    def createBlockchain(gblock: GenesisBlockTest1): IBlockChain
  }

  object StandardBusinessLogicFactory extends IBusinessLogicFactory {
    def createGenesisBlock(seed: String): GenesisBlockTest1 = new GenesisBlockTest1(seed)
    def createNormalBlock(index: IndexV1, parentId: IdV1, trustworthiness: TrustworthinessV1, data: Array[Byte]): NormalBlockTest1 = new NormalBlockTest1(index, parentId, trustworthiness, data)
    def createBlockchain(gblock: GenesisBlockTest1) = new BlockTree(gblock)
  }

  trait ICLIFactory {
    def toStringGenesisBlock(gblock: GenesisBlockTest1): String
    def toStringNormalBlock(nblock: NormalBlockTest1): String
  }

  object StandardCLIFactory extends ICLIFactory {
    def toStringGenesisBlock(gblock: GenesisBlockTest1): String = StandardUtil.genesisBlockToString(gblock)
    def toStringNormalBlock(nblock: NormalBlockTest1): String = StandardUtil.normalBlockToString(nblock)
  }

  object HTMLCLIFactory extends ICLIFactory {
    def toStringGenesisBlock(gblock: GenesisBlockTest1): String = StandardUtil.genesisBlockToHTML(gblock)
    def toStringNormalBlock(nblock: NormalBlockTest1): String = StandardUtil.normalBlockToHTML(nblock)
  }

  trait IBusinessLogic {
    def doNewBlockchain(seed: String): Either[GenesisBlockTest1, String]
    def doAddBlock(index: Int, sequence: Int): Either[NormalBlockTest1, String]
    def doAddBlocksRandom(n: Int): Either[Traversable[NormalBlockTest1], String]
  }

  class BusinessLogic(factory: IBusinessLogicFactory) extends IBusinessLogic {
    var blockchain: Option[IBlockChain] = None
    private var blocksMap: ListBuffer[ListBuffer[BlockBaseV1]] = null

    def doNewBlockchain(seed: String): Either[GenesisBlockTest1, String] = {
      val gblock: GenesisBlockTest1 = factory.createGenesisBlock(seed)
      blockchain = Some(factory.createBlockchain(gblock))
      blocksMap = ListBuffer(ListBuffer(gblock))
      Left(gblock)
    }

    private def doAddBlock(bc: IBlockChain, blocks: ListBuffer[BlockBaseV1], index: Int, sequence: Int): Either[NormalBlockTest1, String] = {
      val parent: Option[BlockBaseV1] = __.getFromListBuffer(blocks, sequence)
      parent match {
        case Some(p) =>
          val cIndex: Int = index + 1
          val nblock: NormalBlockTest1 = factory.createNormalBlock(new IndexV1(cIndex), p.id, new TrustworthinessV1(BigInteger.valueOf(__.getRandomInt)), __.getRandomBytes(32).toArray)
          bc.addBlock(nblock)
          __.getFromListBuffer(blocksMap, cIndex) match {
            case Some(cblocks) => cblocks += nblock
            case None => blocksMap += ListBuffer(nblock)
          }
          Left(nblock)
        case None => Right("the block does't exist")
      }
    }

    def doAddBlock(index: Int, sequence: Int): Either[NormalBlockTest1, String] = {
      blockchain match {
        case Some(bc) =>
          val blocks: Option[ListBuffer[BlockBaseV1]] = __.getFromListBuffer(blocksMap, index)
          blocks match {
            case Some(bs) => doAddBlock(bc, bs, index, sequence)
            case None => Right("the block does't exist")
          }
        case None => Right("the blockchain does't exist")
      }
    }

    private def doAddBlockRandom(bc: IBlockChain): Either[NormalBlockTest1, String] = {
      val index: Int = __.getRandomInt(blocksMap.length)
      val blocks: Option[ListBuffer[BlockBaseV1]] = __.getFromListBuffer(blocksMap, index)
      blocks match {
        case Some(bs) => doAddBlock(bc, bs, index, __.getRandomInt(bs.length))
        case None => Right("the block does't exist")
      }
    }

    def doAddBlockRandom(): Either[NormalBlockTest1, String] = {
      blockchain match {
        case Some(bc) => doAddBlockRandom(bc)
        case None => Right("the blockchain does't exist")
      }
    }

    def doAddBlocksRandom(n: Int): Either[Traversable[NormalBlockTest1], String] = {
      blockchain match {
        case Some(bc) =>
          if (n < 1) {
            Right("parameter is invalid")
          }
          else {
            val nblocks: ListBuffer[NormalBlockTest1] = ListBuffer()
            var message: String = __.emptyString
            for (i <- 0 until n) {
              val nblock: Either[NormalBlockTest1, String] = doAddBlockRandom(bc)
              nblock match {
                case Left(nb) => nblocks += nb
                case Right(msg) => message = msg
              }
            }
            if (nblocks.nonEmpty) {
              Left(nblocks)
            }
            else {
              Right(message)
            }
          }
        case None => Right("the blockchain does't exist")
      }
    }

    def toDotGraph: Either[String, String] = {
      blockchain match {
        case Some(bc) => Left(bc.toDotGraph)
        case None => Right("the blockchain does't exist")
      }
    }
  }

  class CLI(factory: ICLIFactory, logic: IBusinessLogic) {
    lazy val newBlockchain: String = "new blockchain"
    lazy val addBlock: String = "add block"

    def executeCommand(command: String): Boolean = {
      if (command.startsWith(newBlockchain)) {
        var seed: String = command.substring(newBlockchain.length).trim
        while (seed.isEmpty) {
          seed = scala.io.StdIn.readLine("enter a seed of genesis block to be generated: ")
        }
        val gblock: Either[GenesisBlockTest1, String] = logic.doNewBlockchain(seed)
        gblock match {
          case Left(gb) => println(factory.toStringGenesisBlock(gb))
          case Right(message) => println(__.toErrorMessage(message))
        }
        true
      }
      else if (command.startsWith(addBlock)) {
        var blockIndicator: Option[(Int, Int)] = parseBlockIndicator(command.substring(addBlock.length).trim)
        while (blockIndicator.isEmpty) {
          blockIndicator = parseBlockIndicator(scala.io.StdIn.readLine("enter the index and sequence of the parent block in the blockchain: "))
        }
        blockIndicator match {
          case Some(bi) =>
            val index: Int = bi._1
            val sequence: Int = bi._2

            val nblock: Either[NormalBlockTest1, String] = logic.doAddBlock(index, sequence)
            nblock match {
              case Left(nb) => println(factory.toStringNormalBlock(nb))
              case Right(message) => println(__.toErrorMessage(message))
            }
          case None =>
        }
        true
      }
      else {
        false
      }
    }

    private def parseBlockIndicator(str: String): Option[(Int, Int)] = {
      val args: Array[String] = str.split(' ')
      if (args.length < 2) {
        None
      }
      else {
        val index: Option[Int] = __.tryToInt(args(0))
        val sequence: Option[Int] = __.tryToInt(args(1))
        index.flatMap((i) => sequence.map((s) => (i, s)))
      }
    }
  }

  class TextInterface(factory: ICLIFactory, logic: IBusinessLogic) {
    lazy val newBlockchain: String = "new blockchain"
    lazy val addBlock: String = "add block"
    lazy val addBlockRandom: String = "add block random"

    def executeCommand(command: String): String = {
      if (command.startsWith(newBlockchain)) {
        val seed: String = command.substring(newBlockchain.length).trim
        if (seed.isEmpty) {
          __.toErrorMessageHTML("invalid argument")
        }
        else {
          val gblock: Either[GenesisBlockTest1, String] = logic.doNewBlockchain(seed)
          gblock match {
            case Left(gb) => factory.toStringGenesisBlock(gb)
            case Right(message) => __.toErrorMessageHTML(message)
          }
        }
      }
      else if (command.startsWith(addBlockRandom)) {
        parseN(command.substring(addBlockRandom.length).trim) match {
          case Some(n) =>
            logic.doAddBlocksRandom(n) match {
              case Left(nblocks) => __.toMultilineString(nblocks.map((nb) => factory.toStringNormalBlock(nb)))
              case Right(message) => __.toErrorMessageHTML(message)
            }
          case None => __.toErrorMessageHTML("invalid argument")
        }
      }
      else if (command.startsWith(addBlock)) {
        val blockIndicator: Option[(Int, Int)] = parseBlockIndicator(command.substring(addBlock.length).trim)
        blockIndicator match {
          case Some(bi) =>
            val index: Int = bi._1
            val sequence: Int = bi._2

            val nblock: Either[NormalBlockTest1, String] = logic.doAddBlock(index, sequence)
            nblock match {
              case Left(nb) => factory.toStringNormalBlock(nb)
              case Right(message) => __.toErrorMessageHTML(message)
            }
          case None => __.toErrorMessageHTML("invalid argument")
        }
      }
      else {
        __.toErrorMessageHTML("invalid command")
      }
    }

    private def parseBlockIndicator(str: String): Option[(Int, Int)] = {
      parseInts(str, 2).map((elem) => (elem(0), elem(1)))

//      val args: Array[String] = str.split(' ')
//      if (args.length < 2) {
//        None
//      }
//      else {
//        val index: Option[Int] = __.tryToInt(args(0))
//        val sequence: Option[Int] = __.tryToInt(args(1))
//        index.flatMap((i) => sequence.map((s) => (i, s)))
//      }
    }

    private def parseN(str: String): Option[Int] = {
      if (str.isEmpty) {
        Some(1)
      }
      else {
        parseInts(str, 1).map((elem) => elem(0))
      }
    }

    private def parseInts(str: String, n: Int): Option[Array[Int]] = {
      val args: Array[String] = str.split(' ')
      if (args.length < n) {
        None
      }
      else {
        val ints: Array[Int] = new Array[Int](n)
        var i: Int = 0
        var flag: Boolean = true
        while (flag && i < n) {
          __.tryToInt(args(i)) match {
            case Some(int) => ints(i) = int
            case None => flag = false
          }
          i += 1
        }
        if (flag) {
          Some(ints)
        }
        else {
          None
        }
      }
    }
  }

  class WebCLI(dotExeFile: String, workFolder: String) {
    lazy val blockchainURL: String = "/blockchain"
    lazy val blockchainGraphURLTip: String = "/blockchain.svg"
    lazy val blockchainGraphURL: String = blockchainURL + blockchainGraphURLTip

    private val businessLogic: BusinessLogic = new BusinessLogic(StandardBusinessLogicFactory)
    private val textInterface: TextInterface = new TextInterface(HTMLCLIFactory, businessLogic)
    private val messages: ListBuffer[String] = ListBuffer()

    def blockchainGet(): String = createblockchainHTML()

    def blockchainPost(command: String): String = {
      messages += HTML.createParagraph(textInterface.executeCommand(command))

      createblockchainHTML()
    }

    lazy val graphTmpFile: String = "graph"

    def blockchainGraphGet(): Option[String] = {
      val dotGraph: Either[String, String] = businessLogic.toDotGraph
      dotGraph match {
        case Left(graph) =>
          val time: Long = System.currentTimeMillis() + 5000
          val outPath: Path = Graphviz.execute(graph, dotExeFile, workFolder, graphTmpFile)
          while (!Files.exists(outPath) && time > System.currentTimeMillis()) {
            Thread.sleep(100)
          }
          if (Files.exists(outPath)) {
            Some(__.readFile(outPath))
          }
          else {
            None
          }
          Some(__.readFile(outPath))
        case Right(_) => None
      }
    }

    private def createblockchainHTML(): String = {
      val commandTextbox: String = HTML.createTextbox("command", 256)
      val executeButton: String = HTML.createButton("execute")
      val form: String = HTML.createForm(blockchainURL, commandTextbox + executeButton)

      val graph: String = HTML.createImage(blockchainGraphURL)

      HTML.createHTML("blockchain", __.toMultilineString(messages) + form + graph)
    }
  }
}