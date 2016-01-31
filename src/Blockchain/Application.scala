package Blockchain

import java.math.BigInteger
import java.nio.file.Files
import java.nio.file.Path

import scala.collection.mutable.ListBuffer

import Common._

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

trait IPerformanceBusinessLogic {
  def doCheckBlockchainPerformance(n: Int, nBlock: Int): Either[Long, String]
}

class PerformanceBusinessLogic(factory: IBusinessLogicFactory) extends IPerformanceBusinessLogic {
  def doCheckBlockchainPerformance(n: Int, nBlock: Int): Either[Long, String] = {
    val blocks: ListBuffer[IBlock] = ListBuffer()
    val gblock: GenesisBlockTest1 = factory.createGenesisBlock(__.getRandomPrintableString(32))
    val blockchain: IBlockChain = factory.createBlockchain(gblock)
    var head: BlockBaseV1 = gblock
    blocks += gblock
    for (i <- 0 until nBlock) {
      val nblock: NormalBlockTest1 = factory.createNormalBlock(new IndexV1(head.index.index + 1), head.id, new TrustworthinessV1(BigInteger.valueOf(__.getRandomInt(10))), __.getRandomBytes(32).toArray)
      blockchain.addBlock(nblock)
      head = nblock
      blocks += nblock
    }
    val ids: Array[IId] = new Array[IId](n)
    for (i <- 0 until n) {
      ids(i) = blocks(__.getRandomInt(blocks.length)).id
    }
    val t1: Long = System.currentTimeMillis()
    for (i <- 0 until n) {
      blockchain.getBlock(ids(i))
    }
    val t2: Long = System.currentTimeMillis()
    Left(t2 - t1)
  }
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
        val nblock: NormalBlockTest1 = factory.createNormalBlock(new IndexV1(cIndex), p.id, new TrustworthinessV1(BigInteger.valueOf(__.getRandomInt(10))), __.getRandomBytes(32).toArray)
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

  def toDotGraph(valueToString: BlockBaseV1 => String): Either[String, String] = {
    blockchain match {
      case Some(bc) => Left(bc.toDotGraph((b) =>  valueToString(b.asInstanceOf[BlockBaseV1])))
      case None => Right("the blockchain does't exist")
    }
  }
}

class CLIBase() {
  lazy val newBlockchain: String = "new blockchain"
  lazy val addBlockRandom: String = "add block random"
  lazy val addBlock: String = "add block"
  lazy val checkBlockchainPerformance: String = "check blockchain performance"

  protected def parseBlockIndicator(str: String): Option[(Int, Int)] = __.parseInts(str, 2).map((elem) => (elem(0), elem(1)))

  protected def parseN(str: String): Option[Int] = {
    if (str.isEmpty) {
      Some(1)
    }
    else {
      __.parseInts(str, 1).map((elem) => elem(0))
    }
  }

  lazy val defaultNumOfAddition: Int = 10
  lazy val defaultNumOfBlocks: Int = 10

  protected def parsePerfParam(str: String, plogicsName: Array[String]): Either[(String, Int, Int), String] = {
    if (plogicsName.length == 0) {
      Right("the type of the blockchain does not exist")
    }
    else {
      val args: Array[String] = str.split(' ')
      if (args.length == 0) {
        Left(plogicsName(0), defaultNumOfAddition, defaultNumOfBlocks)
      }
      else {
        if (!plogicsName.contains(args(0))) {
          Right("the type of the blockchain does not mutch")
        }
        else {
          if (args.length == 1) {
            Left(args(0), defaultNumOfAddition, defaultNumOfBlocks)
          }
          else {
            __.tryToInt(args(1)) match {
              case Some(n) if n > 0 =>
                if (args.length == 2) {
                  Left(args(0), n, defaultNumOfBlocks)
                }
                else {
                  __.tryToInt(args(2)) match {
                    case Some(nBlock) if nBlock > 0 =>
                      if (args.length == 3) {
                        Left(args(0), n, nBlock)
                      }
                      else {
                        Right("the number of arguments must be three")
                      }
                    case None => Right("the number of blocks must be positive integer")
                  }
                }
              case None => Right("the number of addition must be positive integer")
            }
          }
        }
      }
    }
  }
}

class CLI(factory: ICLIFactory, logic: IBusinessLogic, plogics: Map[String, IPerformanceBusinessLogic]) extends CLIBase() with ICLIComponent {
  def getCommands: Traversable[Command] = {
    Array(
      new Command(newBlockchain, executeNewBlockchain),
      new Command(addBlockRandom, executeAddBlockRandom),
      new Command(addBlock, executeAddBlock),
      new Command(checkBlockchainPerformance, executeCheckBlockchainPerformance)
    )
  }

  private def executeNewBlockchain(args: String): Unit = {
    var seed: String = args
    while (seed.isEmpty) {
      seed = scala.io.StdIn.readLine("enter a seed of genesis block to be generated: ")
    }
    logic.doNewBlockchain(seed) match {
      case Left(gb) => println(factory.toStringGenesisBlock(gb))
      case Right(message) => println(__.toErrorMessage(message))
    }
  }

  private def executeAddBlockRandom(args: String): Unit = {
    var num: Option[Int] = parseN(args)
    while (num.isEmpty) {
      num = parseN(scala.io.StdIn.readLine("enter the num of the blocks to be added: "))
    }
    num match {
      case Some(n) =>
        logic.doAddBlocksRandom(n) match {
          case Left(nblocks) => println(__.toMultilineString(nblocks.map((nb) => factory.toStringNormalBlock(nb))))
          case Right(message) => println(__.toErrorMessage(message))
        }
      case None =>
    }
  }

  private def executeAddBlock(args: String): Unit = {
    var blockIndicator: Option[(Int, Int)] = parseBlockIndicator(args)
    while (blockIndicator.isEmpty) {
      blockIndicator = parseBlockIndicator(scala.io.StdIn.readLine("enter the index and sequence of the parent block in the blockchain: "))
    }
    blockIndicator match {
      case Some(bi) =>
        val index: Int = bi._1
        val sequence: Int = bi._2
        logic.doAddBlock(index, sequence) match {
          case Left(nb) => println(factory.toStringNormalBlock(nb))
          case Right(message) => println(__.toErrorMessage(message))
        }
      case None =>
    }
  }

  private def executeCheckBlockchainPerformance(args: String): Unit = {
    val plogicsKey: Array[String] = plogics.keys.toArray
    var params: Either[(String, Int, Int), String] = parsePerfParam(args, plogicsKey)
    while (params.isRight) {
      println(params.right)
      params = parsePerfParam(scala.io.StdIn.readLine("enter the index and sequence of the parent block in the blockchain: "), plogicsKey)
    }
    params match {
      case Left(p) =>
        plogics(p._1).doCheckBlockchainPerformance(p._2, p._3) match {
          case Left(msecond) => println(msecond.toString + __.millisecond)
          case Right(message) => println(__.toErrorMessage(message))
        }
      case Right(_) =>
    }
  }
}

class TextInterface(factory: ICLIFactory, logic: IBusinessLogic) extends CLIBase() {
  def executeCommand(command: String): String = {
    if (command.startsWith(newBlockchain)) {
      executeNewBlockchain(command.substring(newBlockchain.length).trim)
    }
    else if (command.startsWith(addBlockRandom)) {
      executeAddBlockRandom(command.substring(addBlockRandom.length).trim)
    }
    else if (command.startsWith(addBlock)) {
      executeAddBlock(command.substring(addBlock.length).trim)
    }
    else {
      __.toErrorMessageHTML("invalid command")
    }
  }

  private def executeNewBlockchain(args: String): String = {
    val seed: String = args
    if (seed.isEmpty) {
      __.toErrorMessageHTML("invalid argument")
    }
    else {
      logic.doNewBlockchain(seed) match {
        case Left(gb) => factory.toStringGenesisBlock(gb)
        case Right(message) => __.toErrorMessageHTML(message)
      }
    }
  }

  private def executeAddBlockRandom(args: String): String = {
    parseN(args) match {
      case Some(n) =>
        logic.doAddBlocksRandom(n) match {
          case Left(nblocks) => __.toMultilineString(nblocks.map((nb) => factory.toStringNormalBlock(nb)))
          case Right(message) => __.toErrorMessageHTML(message)
        }
      case None => __.toErrorMessageHTML("invalid argument")
    }
  }

  private def executeAddBlock(args: String): String = {
    parseBlockIndicator(args) match {
      case Some(bi) =>
        val index: Int = bi._1
        val sequence: Int = bi._2
        logic.doAddBlock(index, sequence) match {
          case Left(nb) => factory.toStringNormalBlock(nb)
          case Right(message) => __.toErrorMessageHTML(message)
        }
      case None => __.toErrorMessageHTML("invalid argument")
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
    val dotGraph: Either[String, String] = businessLogic.toDotGraph(toStringBlock)
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

  private def toStringBlock(gblock: BlockBaseV1): String = {
    __.toMultilineString(Array(
      __.toHexString(gblock.id.toBytes.slice(0, 8)),
      __.toKeyValueString("trustworthiness", gblock.trustworthiness.trustworthiness.toString)
    ))
  }

  private def createblockchainHTML(): String = {
    val commandTextbox: String = HTML.createTextbox("command", 256)
    val executeButton: String = HTML.createButton("execute")
    val form: String = HTML.createForm(blockchainURL, commandTextbox + executeButton)

    val graph: String = HTML.createImage(blockchainGraphURL)

    HTML.createHTML("blockchain", __.toMultilineString(messages) + form + graph)
  }
}