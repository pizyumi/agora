package Blockchain

import java.math.BigInteger

import Common._

class StandardImplementationTestCLI() extends ICLIComponent with ITestComponent {
  protected lazy val testInterface: TestInterface = new TestCLI()

  protected lazy val testId: String = "test id"
  protected lazy val testIndex: String = "test index"
  protected lazy val testGenesisBlock: String = "test genesis block"
  protected lazy val testNormalBlock: String = "test normal block"
  protected lazy val testBlockTreeFork: String = "test block tree fork"
  protected lazy val testBlockTreeDelete: String = "test block tree delete"
  protected lazy val testBlockTree: String = "test block tree"

  def getCommands: Traversable[Command] = {
    Array(
      new Command(testId, (args) => doTestId()),
      new Command(testIndex, (args) => doTestIndex()),
      new Command(testGenesisBlock, (args) => doTestGenesisBlock()),
      new Command(testNormalBlock, (args) => doTestNormalBlock()),
      new Command(testBlockTreeFork, (args) => doTestBlockTreeFork()),
      new Command(testBlockTreeDelete, (args) => doTestBlockTreeDelete()),
      new Command(testBlockTree, (args) => doTestBlockTree())
    )
  }

  def getTests: Traversable[Test] = {
    Array(
      new Test(doTestId),
      new Test(doTestIndex),
      new Test(doTestGenesisBlock),
      new Test(doTestNormalBlock),
      new Test(doTestBlockTree)
    )
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
    val nblock: NormalBlockTest1 = new NormalBlockTest1(new IndexV1(__.getRandomInt(10000)), new IdV1(__.getRandomBytes(32).toArray), trustworthiness, __.getRandomBytes(32).toArray)
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
    val nblock3: NormalBlockTest1 = new NormalBlockTest1(new IndexV1(__.getRandomInt(10000)), gblock.id, trustworthiness, __.getRandomBytes(32).toArray)
    testInterface.outputMessage(StandardUtil.normalBlockToString(nblock3))
    testInterface.outputMessage("trying to add normal block to block tree...")
    val e4: Either[Unit, String] = blocktree.addBlock(nblock3)
    testInterface.outputMessage(e4.right.get)
    testInterface.outputItem("5.14", Some("normal block cannot be added to block tree"), e4.isRight)
    testInterface.outputItem("5.15", Some("normal block's parent block was retrieved from block tree"), blocktree.getParentBlock(nblock2).get == gblock)
    val children: Array[IBlock] = blocktree.getChildBlocks(gblock).get.toArray
    testInterface.outputItem("5.16", Some("normal block's child blocks was retrieved from block tree"), children.length == 1 && children(0) == nblock2)
  }

  private def addNewBlocks(blocktree: BlockTree, pblock: BlockBaseV1, n: Int): Array[NormalBlockTest1] = {
    testInterface.outputMessage("generating normal consecutive blocks...")
    val nblocks: Array[NormalBlockTest1] = new Array[NormalBlockTest1](n)
    for (i <- 0 until n) {
      if (i == 0) {
        nblocks(i) = new NormalBlockTest1(pblock.index.moveForward(i + 1).asInstanceOf[IndexV1], pblock.id, new TrustworthinessV1(BigInteger.valueOf(__.getRandomInt(10))), __.getRandomBytes(32).toArray)
      }
      else {
        nblocks(i) = new NormalBlockTest1(pblock.index.moveForward(i + 1).asInstanceOf[IndexV1], nblocks(i - 1).id, new TrustworthinessV1(BigInteger.valueOf(__.getRandomInt(10))), __.getRandomBytes(32).toArray)
      }
      testInterface.outputMessage(StandardUtil.normalBlockToString(nblocks(i)))
    }
    testInterface.outputMessage("adding normal blocks to block tree...")
    for (i <- 0 until n) {
      blocktree.addBlock(nblocks(i))
    }
    nblocks
  }

  protected def doTestBlockTreeFork(): Unit = {
    testInterface.outputTitle("block tree fork test", None)

    testInterface.outputMessage("generating genesis block...")
    val gblock: GenesisBlockTest1 = new GenesisBlockTest1(__.getRandomPrintableString(32))
    testInterface.outputMessage(StandardUtil.genesisBlockToString(gblock))
    testInterface.outputMessage("initializing block tree, providing genesis block...")
    val blocktree: BlockTree = new BlockTree(gblock)

    val paths: Array[Array[NormalBlockTest1]] = new Array[Array[NormalBlockTest1]](10)
    var maxCumulativeTrustworthiness: BigInteger = BigInteger.ZERO
    var maxIndex: Int = -1
    var f: Boolean = true
    for (i <- 0 until 10) {
      paths(i) = addNewBlocks(blocktree, gblock, 9)
      val cumulativeTrustworthiness: BigInteger = paths(i).map((b) => b.trustworthiness.trustworthiness).fold(BigInteger.ZERO)((x, acc) => acc.add(x))
      if (maxCumulativeTrustworthiness.compareTo(cumulativeTrustworthiness) < 0) {
        maxCumulativeTrustworthiness = cumulativeTrustworthiness
        maxIndex = i
        f = true
      }
      else if (maxCumulativeTrustworthiness.compareTo(cumulativeTrustworthiness) == 0) {
        f = false
      }

      testInterface.outputMessage("checking cumulative trustworthiness...")
      testInterface.outputMessage(__.toKeyValueString("cumulative trastworthiness", cumulativeTrustworthiness.toString))
      testInterface.outputMessage(__.toKeyValueString("max cumulative trastworthiness", maxCumulativeTrustworthiness.toString))
      testInterface.outputMessage("checking active blocks...")
      for (j <- 0 until 9) {
        testInterface.outputMessage(StandardUtil.normalBlockToString(blocktree.getActiveBlock(new IndexV1(j + 1)).get.asInstanceOf[NormalBlockTest1]))
      }

      if (f) {
        testInterface.outputItem("6.1", Some("head block"), blocktree.getHeadBlock == paths(maxIndex).last)
        for (j <- 0 until 9) {
          testInterface.outputItem("6.2", Some("active block"), blocktree.getActiveBlock(new IndexV1(j + 1)).get == paths(maxIndex)(j))
        }
        val n: Int = __.getRandomInt(9) + 1
        val bs: Traversable[IBlock] = blocktree.getBlockchain(n)
        for (b <- bs.toArray.zip(paths(maxIndex).reverse)) {
          testInterface.outputItem("6.3", Some("blockchain"), b._1 == b._2)
        }
        val m: Int = __.getRandomInt(9) + 1
        val bs2: Traversable[IBlock] = blocktree.getBlockchain(new IndexV1(m), m).get
        for (b <- bs2.toArray.zip(paths(maxIndex).reverse.drop(9 - m))) {
          testInterface.outputItem("6.4", Some("blockchain"), b._1 == b._2)
        }
      }
    }
    if (f) {
      for (i <- 0 until 10) {
        for (j <- 0 until 9) {
          if (i == maxIndex) {
            testInterface.outputItem("6.5", Some("is active"), blocktree.isActive(paths(i)(j)))
            testInterface.outputItem("6.6", Some("is active"), blocktree.isActive(paths(i)(j).id))
          }
          else {
            testInterface.outputItem("6.5", Some("is active"), !blocktree.isActive(paths(i)(j)))
            testInterface.outputItem("6.6", Some("is active"), !blocktree.isActive(paths(i)(j).id))
          }
        }
      }
    }
  }

  protected def doTestBlockTreeDelete(): Unit = {
    testInterface.outputTitle("block tree delete test", None)

    testInterface.outputMessage("generating genesis block...")
    val gblock: GenesisBlockTest1 = new GenesisBlockTest1(__.getRandomPrintableString(32))
    testInterface.outputMessage(StandardUtil.genesisBlockToString(gblock))
    testInterface.outputMessage("initializing block tree, providing genesis block...")
    val blocktree: BlockTree = new BlockTree(gblock)

    val paths: Array[Array[NormalBlockTest1]] = new Array[Array[NormalBlockTest1]](10)
    val heads: Array[Int] = new Array[Int](10)
    val cumulativeTrustworthinesses: Array[BigInteger] = new Array[BigInteger](10)
    for (i <- 0 until 10) {
      paths(i) = addNewBlocks(blocktree, gblock, 9)
      heads(i) = 8
      cumulativeTrustworthinesses(i) = paths(i).map((b) => b.trustworthiness.trustworthiness).fold(BigInteger.ZERO)((x, acc) => acc.add(x))
    }

    var remaining: Int = 10 * 9
    while (remaining > 0) {
      val i: Int = __.getRandomInt(10)
      if (heads(i) >= 0) {
        remaining = remaining - 1
        val delblock: NormalBlockTest1 = paths(i)(heads(i))
        testInterface.outputItem("7.1", Some("block exists before deletion"), blocktree.isContain(delblock))
        blocktree.deleteBlock(delblock)
        testInterface.outputItem("7.2", Some("block does not exist after deletion"), !blocktree.isContain(delblock))
        cumulativeTrustworthinesses(i) = cumulativeTrustworthinesses(i).subtract(delblock.trustworthiness.trustworthiness)
        heads(i) = heads(i) - 1
        var active: Int = 0
        var max: BigInteger = cumulativeTrustworthinesses(0)
        var f: Boolean = false
        for (m <- 1 until 10) {
          if (max.compareTo(cumulativeTrustworthinesses(m)) < 0) {
            active = m
            max = cumulativeTrustworthinesses(m)
            f = false
          }
          else if (max.compareTo(cumulativeTrustworthinesses(m)) == 0) {
            f = true
          }
        }
        if (!f) {
          if (heads(active) != -1) {
            testInterface.outputItem("7.3", Some("blockchain head after deletion"), blocktree.getHeadBlock == paths(active)(heads(active)))
          }
          else {
            testInterface.outputItem("7.3", Some("blockchain head after deletion"), blocktree.getHeadBlock == gblock)
          }
        }
      }
    }
    testInterface.outputItem("7.4", Some("blockchain head after all normal block deletion"), blocktree.getHeadBlock == gblock)
  }
}