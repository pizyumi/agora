package Blockchain.Impl2

import java.math.BigInteger
import java.text.ParseException
import java.util.Date

import org.json4s.JsonAST.{JDouble, JInt, JValue}
import org.json4s._
import org.json4s.native.JsonMethods

import Blockchain.Interface._
import Blockchain.Impl._
import Common._

object BlockchainSettings {
  lazy val defaultSettings: BlockchainSettings = new BlockchainSettings(
    defaultHashAlgorithm,
    defaultBlockGenerationScheme,
    defaultSeed,
    defaultInitialTimestamp,
    defaultInitialTarget,
    defaultMaxLengthNonce,
    defaultRetargetBlock,
    defaultBlockGenerationInterval,
    defaultMaxRetargetChangeRate,
    defaultMinRetargetChangeRate
  )

  val bgsPOW: String = "pow"
  val bgsPOS: String = "pos"
  val bgsProperties: Map[String, Unit] = Map(bgsPOW -> Unit)

  val defaultHashAlgorithm: String = BlockchainSettingsBase.haSha256
  val defaultBlockGenerationScheme: String = bgsPOW
  val defaultSeed: String = "seed"
  val defaultInitialTimestamp: Long = 0
  val defaultInitialTarget: IdV1 = new IdV1(Array(UByte.__(0), UByte.__(127), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255)))
  val defaultMaxLengthNonce: Int = 8
  val defaultRetargetBlock: Int = 16
  val defaultBlockGenerationInterval: Int = 10
  val defaultMaxRetargetChangeRate: Double = 4.0
  val defaultMinRetargetChangeRate: Double = 0.25

  val itemHashAlgorithm: String = "hash algorithm"
  val itemBlockGenerationScheme: String = "block generation scheme"
  val itemSeed: String = "seed"
  val itemInitialTimestamp: String = "initial timestamp"
  val itemInitialTarget: String = "initial target"
  val itemMaxLengthNonce: String = "max nonce length"
  val itemRetargetBlock: String = "retarget block interval"
  val itemBlockGenerationInterval: String = "block generation interval"
  val itemMaxRetargetChangeRate: String = "max retarget change rate"
  val itemMinRetargetChangeRate: String = "min retarget change rate"

  def parseBlockchainSettings(jsonStr: String): BlockchainSettings = {
    try {
      val json: JValue = JsonMethods.parse(jsonStr)

      val hashAlgorithm: String = json \ itemHashAlgorithm match {
        case x: JString =>
          if (BlockchainSettingsBase.haProperties.contains(x.values)) {
            x.values
          }
          else {
            println("not supported hash algorithm. use default setting.")
            defaultHashAlgorithm
          }
        case _ =>
          println("doesn't parse hash algorithm. use default setting.")
          defaultHashAlgorithm
      }
      val blockGenerationScheme: String = json \ itemBlockGenerationScheme match {
        case x: JString =>
          if (bgsProperties.contains(x.values)) {
            x.values
          }
          else {
            println("not supported blockchain generation scheme. use default setting.")
            defaultBlockGenerationScheme
          }
        case _ =>
          println("doesn't parse blockchain generation scheme. use default setting.")
          defaultBlockGenerationScheme
      }
      val seed: String = json \ itemSeed match {
        case x: JString => x.values
        case _ =>
          println("doesn't parse seed. use default setting.")
          defaultSeed
      }
      val initialTimestamp: Long = json \ itemInitialTimestamp match {
        case x: JString =>
          try {
            __.fromStringToDate(x.values).getTime
          }
          catch {
            case _: ParseException =>
              println("doesn't parse initial timestamp. use default setting.")
              defaultInitialTimestamp
          }
        case _ =>
          println("doesn't parse initial timestamp. use default setting.")
          defaultInitialTimestamp
      }
      val initialTarget: IdV1 = json \ itemInitialTarget match {
        case x: JString =>
          if (__.isHexString(x.values, BlockchainSettingsBase.haProperties(hashAlgorithm).lengthByte)) {
            new IdV1(__.fromHexString(x.values))
          }
          else {
            println("doesn't parse initial target. use default setting.")
            defaultInitialTarget
          }
        case _ =>
          println("doesn't parse initial target. use default setting.")
          defaultInitialTarget
      }
      val maxLengthNonce: Int = json \ itemMaxLengthNonce match {
        case x: JString =>
          try {
            x.values.toInt
          }
          catch {
            case _: NumberFormatException =>
              println("doesn't parse max nonce length. use default setting.")
              defaultMaxLengthNonce
          }
        case x: JInt =>
          if (x.values.isValidInt) {
            x.values.toInt
          }
          else {
            println("too large max nonce length. use default setting.")
            defaultMaxLengthNonce
          }
        case _ =>
          println("doesn't parse max nonce length. use default setting.")
          defaultMaxLengthNonce
      }
      val retargetBlock: Int = json \ itemRetargetBlock match {
        case x: JString =>
          try {
            x.values.toInt
          }
          catch {
            case _: NumberFormatException =>
              println("doesn't parse retarget block interval. use default setting.")
              defaultRetargetBlock
          }
        case x: JInt =>
          if (x.values.isValidInt) {
            x.values.toInt
          }
          else {
            println("too large retarget block interval. use default setting.")
            defaultRetargetBlock
          }
        case _ =>
          println("doesn't parse retarget block interval. use default setting.")
          defaultRetargetBlock
      }
      val blockGenerationInterval: Int = json \ itemBlockGenerationInterval match {
        case x: JString =>
          try {
            x.values.toInt
          }
          catch {
            case _: NumberFormatException =>
              println("doesn't parse block generation interval. use default setting.")
              defaultBlockGenerationInterval
          }
        case x: JInt =>
          if (x.values.isValidInt) {
            x.values.toInt
          }
          else {
            println("too large block generation interval. use default setting.")
            defaultBlockGenerationInterval
          }
        case _ =>
          println("doesn't parse block generation interval. use default setting.")
          defaultBlockGenerationInterval
      }
      val maxRetargetChangeRate: Double = json \ itemMaxRetargetChangeRate match {
        case x: JString =>
          try {
            x.values.toDouble
          }
          catch {
            case _: NumberFormatException =>
              println("doesn't parse max retarget change rate. use default setting.")
              defaultMaxRetargetChangeRate
          }
        case x: JDouble => x.values
        case _ =>
          println("doesn't parse max retarget change rate. use default setting.")
          defaultMaxRetargetChangeRate
      }
      val minRetargetChangeRate: Double = json \ itemMinRetargetChangeRate match {
        case x: JString =>
          try {
            x.values.toDouble
          }
          catch {
            case _: NumberFormatException =>
              println("doesn't parse min retarget change rate. use default setting.")
              defaultMinRetargetChangeRate
          }
        case x: JDouble => x.values
        case _ =>
          println("doesn't parse min retarget change rate. use default setting.")
          defaultMinRetargetChangeRate
      }

      new BlockchainSettings(
        hashAlgorithm,
        blockGenerationScheme,
        seed,
        initialTimestamp,
        initialTarget,
        maxLengthNonce,
        retargetBlock,
        blockGenerationInterval,
        maxRetargetChangeRate,
        minRetargetChangeRate
      )
    }
    catch {
      case _: org.json4s.ParserUtil.ParseException =>
        println("doesn't parse json. use default setting.")
        defaultSettings
    }
  }
}
class BlockchainSettings(
                          hashAlgorithmIn: String,
                          blockGenerationSchemeIn: String,
                          seedIn: String,
                          initialTimestampIn: Long,
                          initialTargetIn: IdV1,
                          maxLengthNonceIn: Int,
                          retargetBlockIn: Int,
                          blockGenerationIntervalIn: Int,
                          maxRetargetChangeRateIn: Double,
                          minRetargetChangeRateIn: Double
                          ) {
  val hashAlgorithm: String = hashAlgorithmIn
  val hashAlgorithmProperty: HashAlgorithmProperty = BlockchainSettingsBase.haProperties(hashAlgorithmIn)
  val blockGenerationScheme: String = blockGenerationSchemeIn
  val seed: String = seedIn
  val initialTimestamp: Long = initialTimestampIn
  val initialTarget: IdV1 = initialTargetIn
  val maxLengthNonce: Int = maxLengthNonceIn
  val retargetBlock: Int = retargetBlockIn
  val blockGenerationInterval: Int = blockGenerationIntervalIn
  val retargetTime: Int = retargetBlockIn * blockGenerationIntervalIn
  val maxRetargetChangeRate: Double = maxRetargetChangeRateIn
  val minRetargetChangeRate: Double = minRetargetChangeRateIn
  val maxActualTime: Long = (retargetBlockIn * blockGenerationIntervalIn * maxRetargetChangeRateIn).toLong
  val minActualTime: Long = (retargetBlockIn * blockGenerationIntervalIn * minRetargetChangeRateIn).toLong

  lazy val diff1Target: IdV1 = {
    val array: Array[Byte] = new Array[Byte](hashAlgorithmProperty.lengthByte)
    for (i <- array.indices) {
      if (i < 4) {
        array(i) = UByte.__(0)
      }
      else {
        array(i) = UByte.__(255)
      }
    }
    new IdV1(array)
  }
}

abstract class GenesisBlockTest2(settings: BlockchainSettings) extends BlockBaseV1 with IGenesisBlock with IValidatableItems {
  val index: Long = 0
  val trustworthiness: TrustworthinessV1 = new TrustworthinessV1(BigInteger.ZERO)

  val seed: String = settings.seed

  protected override def toIdBytesIngredient: Array[Array[Byte]] = super.toIdBytesIngredient ++ Array(__.getBytes(seed))
}

abstract class NormalBlockTest2(indexIn: Long, parentIdIn: IdV1, dataIn: Array[Byte]) extends BlockBaseV1 with INormalBlock with IValidatableItems {
  val index: Long = indexIn
  val parentId: Option[IdV1] = Some(parentIdIn)

  val data: Array[Byte] = dataIn

  protected override def toIdBytesIngredient: Array[Array[Byte]] = super.toIdBytesIngredient ++ Array(data)
}

class POWGenesisBlockTest2(settings: BlockchainSettings) extends GenesisBlockTest2(settings) with POWBlockBaseV1 {
  val timestamp: Long = settings.initialTimestamp
  val target: IdV1 = settings.initialTarget
  val nonce: Array[Byte] = Array[Byte](0)

  protected val hashAlgorithm: String = settings.hashAlgorithm

  protected override def specValidatableItems: Map[String, () => Either[Unit, String]] = Map()

  protected override def toIdBytesIngredient: Array[Array[Byte]] = super.toIdBytesIngredient ++ Array(__.getBytes(timestamp), target.toBytes)
}

object POWNormalBlockTest2 {
  val validationNameId: String = "id"
  val validationNameNonce: String = "nonce"
}
class POWNormalBlockTest2(settings: BlockchainSettings, indexIn: Long, parentIdIn: IdV1, timestampIn: Long, targetIn: IdV1, nonceIn: Array[Byte], dataIn: Array[Byte]) extends NormalBlockTest2(indexIn, parentIdIn, dataIn) with POWBlockBaseV1 {
  lazy val trustworthiness: TrustworthinessV1 = toTrustworthiness

  val timestamp: Long = timestampIn
  val target: IdV1 = targetIn
  val nonce: Array[Byte] = nonceIn

  protected val hashAlgorithm: String = settings.hashAlgorithm

  protected def toTrustworthiness: TrustworthinessV1 = {
    val diff1TargetBigint: BigInteger = __.bytesToPositiveBigInteger(settings.diff1Target.id)
    val targetBigInt: BigInteger = __.bytesToPositiveBigInteger(target.id)
    new TrustworthinessV1(diff1TargetBigint.multiply(BigInteger.valueOf(100000000)).divide(targetBigInt))
  }

  protected def isValidId: Either[Unit, String] = {
    if (__.bytesToPositiveBigInteger(id.id).compareTo(__.bytesToPositiveBigInteger(target.id)) <= 0) {
      Left()
    }
    else {
      Right("id is too large")
    }
  }

  protected def isValidNonce: Either[Unit, String] = {
    if (nonce.length <= settings.maxLengthNonce) {
      Left()
    }
    else {
      Right("nonce is too long")
    }
  }

  protected override def specValidatableItems: Map[String, () => Either[Unit, String]] = Map(
    POWNormalBlockTest2.validationNameId -> (() => isValidId),
    POWNormalBlockTest2.validationNameNonce -> (() => isValidNonce)
  )

  protected override def toIdBytesIngredient: Array[Array[Byte]] = super.toIdBytesIngredient ++ Array(__.getBytes(timestamp), target.toBytes, nonce)
}

class POWBlockCreatorContext() {
  var f: Boolean = true
}

class POWBlockCreator(settings: BlockchainSettings) {
  def createBlock(index: Long, parentId: IdV1, timestamp: Long, target: IdV1, data: Array[Byte], context: POWBlockCreatorContext): Either[POWNormalBlockTest2, String] = {
    var bi: BigInteger = BigInteger.ZERO
    var nonce: Array[Byte] = bi.toByteArray
    var block: POWNormalBlockTest2 = new POWNormalBlockTest2(settings, index, parentId, timestamp, target, nonce, data)
    block.validatableItemsName.view.filter((name) => name != POWNormalBlockTest2.validationNameId && name != POWNormalBlockTest2.validationNameNonce).map((name) => block.isValidItemWithMessage(name)).find((r) => r.isRight) match {
      case Some(either) => Right(either.right.get)
      case None =>
        var f: Boolean = true
        while (f && context.f) {
          if (block.isValidItem(POWNormalBlockTest2.validationNameId)) {
            f = false
          }
          else {
            bi = bi.add(BigInteger.ONE)
            nonce = bi.toByteArray
            block = new POWNormalBlockTest2(settings, index, parentId, timestamp, target, nonce, data)
            if (!block.isValidItem(POWNormalBlockTest2.validationNameNonce)) {
              f = false
            }
          }
        }
        block.isValidWithMessage match {
          case Left(_) => Left(block)
          case Right(msg) => Right(msg)
        }
    }
  }
}

//POWブロック鎖
object POWBlockchain {
  val validationNameTarget: String = "target"
}
class POWBlockchain(settings: BlockchainSettings, genesis: IGenesisBlock) extends IndexedBlockTree(genesis) {
  //ブロックを検証する
  protected override def validateBlock(block: IBlock): Either[Unit, String] = {
    if (!block.isInstanceOf[POWNormalBlockTest2]) {
      Right("the block is not supported")
    }
    else {
      var validation: Either[Unit, String] = Left()
      block match {
        case _: IValidatableItems => validation = block.asInstanceOf[IValidatableItems].isValidWithMessage
        case _: IValidatable => validation = block.asInstanceOf[IValidatable].isValidWithMessage
      }
      validation match {
        case Left(_) => isConvalidWithMessage(block)
        case Right(_) => validation
      }
    }
  }

  def getTarget(block: IBlock): Option[IdV1] = getBlockTree(block.id).map((bt) => getTarget(bt))

  def getHeadTarget: IdV1 = getBlockTree(getHeadBlock.id).map((bt) => getTarget(bt)).get

  protected def getTarget(pt: ITree[IBlock]): IdV1 = {
    if (pt.getValue.index < settings.blockGenerationInterval) {
      settings.initialTarget
    }
    else {
      if (pt.getValue.index % settings.blockGenerationInterval == 0) {
        val timestamp1: Long = pt.getValue.asInstanceOf[POWBlockBaseV1].timestamp
        var t: ITree[IBlock] = pt
        for (i <- 0 until settings.blockGenerationInterval) {
          t.getParent match {
            case Some(b) => t = b
            case None =>
          }
        }
        val timestamp2: Long = t.getValue.asInstanceOf[POWBlockBaseV1].timestamp
        var timespan: Long = timestamp1 - timestamp2
        if (timespan < settings.minActualTime * 1000) {
          timespan = settings.minActualTime * 1000
        }
        if (timespan > settings.maxActualTime * 1000) {
          timespan = settings.maxActualTime * 1000
        }
        val rate: Double = timespan.toDouble / (settings.retargetTime * 1000).toDouble
        val ptTargetBigInt: BigInteger = __.bytesToPositiveBigInteger(pt.getValue.asInstanceOf[POWBlockBaseV1].target.id)
        var targetBigInt: BigInteger = ptTargetBigInt.multiply(BigInteger.valueOf((rate * 100000000).toLong)).divide(BigInteger.valueOf(100000000))
        if (targetBigInt.compareTo(settings.hashAlgorithmProperty.maxBigInt) > 0) {
          targetBigInt = settings.hashAlgorithmProperty.maxBigInt
        }
        new IdV1(__.positiveBigIntegerToBytes(targetBigInt, settings.hashAlgorithmProperty.lengthByte))
      }
      else {
        pt.getValue.asInstanceOf[POWBlockBaseV1].target
      }
    }
  }

  protected def isValidTarget(block: IBlock): Either[Unit, String] = {
    block.parentId match {
      case Some(pId) =>
        getBlockTree(pId) match {
          case Some(pt) =>
            if (getTarget(pt).isSame(block.asInstanceOf[POWBlockBaseV1].target)) {
              Left()
            }
            else {
              Right("target is wrong")
            }
          case None => Right("the block's parent block is not in the blockchain")
        }
      case None => Right("the block's parent id is not specified")
    }
  }

  protected override def specConvalidatableItems: Map[String, (IBlock) => Either[Unit, String]] = Map(
    POWBlockchain.validationNameTarget -> ((block) => isValidTarget(block))
  )
}