package Blockchain

import java.math.BigInteger

import Common._

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