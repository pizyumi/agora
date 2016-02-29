package Blockchain

import java.math.BigInteger
import java.text.{ParseException, SimpleDateFormat, DateFormat}
import java.util.Date

import Common._
import org.json4s.JString
import org.json4s.JsonAST.{JDouble, JInt, JValue}
import org.json4s.native.JsonMethods

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

  val haSha256: String = "sha256"
  val haProperties: Map[String, HashAlgorithmProperty] = Map(haSha256 -> new HashAlgorithmProperty(32))

  val bgsPOW: String = "pow"
  val bgsPOS: String = "pos"
  val bgsProperties: Map[String, Unit] = Map(bgsPOW -> Unit)

  val defaultHashAlgorithm: String = haSha256
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
          if (haProperties.contains(x.values)) {
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
          if (__.isHexString(x.values, haProperties(hashAlgorithm).lengthByte)) {
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
  val hashAlgorithmProperty: HashAlgorithmProperty = BlockchainSettings.haProperties(hashAlgorithmIn)
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

class HashAlgorithmProperty(lengthByteIn: Int) {
  val lengthByte: Int = lengthByteIn

  val langthBit: Int = lengthByteIn * 8
  val minValue: Array[Byte] = __.getMinBytes(lengthByteIn)
  val minBigInt: BigInteger = __.bytesToPositiveBigInteger(__.getMinBytes(lengthByteIn))
  val maxValue: Array[Byte] = __.getMaxBytes(lengthByteIn)
  val maxBigInt: BigInteger = __.bytesToPositiveBigInteger(__.getMaxBytes(lengthByteIn))
}

//ブロックの標準実装
trait BlockBaseV1 extends IBlock {
  //信用度
  val trustworthiness: TrustworthinessV1
  //親ブロックの識別子
  //val parentId: Option[IdV1]

  //識別子
  lazy val id: IdV1 = new IdV1(toId)

  //ハッシュ関数
  protected val hashAlgorithm: String

  //識別子を計算する構成要素
  protected def toIdBytesIngredient: Array[Array[Byte]] = Array(__.getBytes(index), parentId.map((v) => v.toBytes).getOrElse(Array.emptyByteArray))
  //識別子を計算する素
  private def toIdBytes: Array[Byte] = __.getBytes(toIdBytesIngredient)
  //識別子を計算する
  //TODO: 他のハッシュ関数（ハッシュ関数の組み合わせも含む）への対応
  private def toId: Array[Byte] = {
    if (hashAlgorithm == BlockchainSettings.haSha256) {
      __.getSha256(toIdBytes)
    }
    else {
      null
    }
  }

  //ブロックの識別子が同等である場合、ブロックは同一である
  protected override def specIsSame(r: ICompare): Boolean = id.id.sameElements(r.asInstanceOf[BlockBaseV1].id.id)
  //ハッシュコードを返す関数
  protected override def specHashCode: Int = id.hashCode
}

//POWブロックの標準実装
trait POWBlockBaseV1 extends BlockBaseV1 with IPOW {
  val target: IdV1
}

class GenesisBlockTest1(seedIn: String) extends BlockBaseV1 with IGenesisBlock {
  val maxSeedLength: Int = 1024

  val index: Long = 0
  val trustworthiness: TrustworthinessV1 = new TrustworthinessV1(BigInteger.ZERO)

  val seed: String = {
    if (seedIn.length > maxSeedLength) {
      throw new IllegalArgumentException("seed is too long")
    }
    seedIn
  }

  protected val hashAlgorithm: String = BlockchainSettings.haSha256

  protected override def toIdBytesIngredient: Array[Array[Byte]] = super.toIdBytesIngredient ++ Array(__.getBytes(seed))
}

class NormalBlockTest1(indexIn: Long, parentIdIn: IdV1, trustworthinessIn: TrustworthinessV1, dataIn: Array[Byte]) extends BlockBaseV1 with INormalBlock {
  val maxDataLength: Int = 1024

  val index: Long = indexIn
  val parentId: Option[IdV1] = Some(parentIdIn)
  val trustworthiness: TrustworthinessV1 = trustworthinessIn

  val data: Array[Byte] = {
    if (dataIn.length > maxDataLength) {
      throw new IllegalArgumentException("data is too long")
    }
    dataIn
  }

  protected val hashAlgorithm: String = BlockchainSettings.haSha256

  protected override def toIdBytesIngredient: Array[Array[Byte]] = super.toIdBytesIngredient ++ Array(data, trustworthiness.trustworthiness.toByteArray)
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

object StandardUtil {
  def idToString(id: IdV1): String = __.toKeyValueString("id", __.toHexString(id.id))

  def block1ToStringElements(block: BlockBaseV1): Array[String] = {
    Array(
      __.toKeyValueString("index", block.index.toString),
      __.toKeyValueString("id", __.toHexString(block.id.id)),
      __.toKeyValueString("parent id", block.parentId.map((t) => __.toHexString(t.toBytes)).getOrElse(__.nullString)),
      __.toKeyValueString("trustworthiness", block.trustworthiness.trustworthiness.toString)
    )
  }

  def gblock1ToStringElements(gblock: GenesisBlockTest1): Array[String] = {
    Array(
      __.toKeyValueString("seed", gblock.seed)
    )
  }

  def nblock1ToStringElements(nblock: NormalBlockTest1): Array[String] = {
    Array(
      __.toKeyValueString("data", __.toHexString(nblock.data))
    )
  }

  def gblock2ToStringElements(gblock: GenesisBlockTest2): Array[String] = {
    Array(
      __.toKeyValueString("seed", gblock.seed)
    )
  }

  def nblock2ToStringElements(nblock: NormalBlockTest2): Array[String] = {
    Array(
      __.toKeyValueString("data", __.toHexString(nblock.data))
    )
  }

  def powblockToStringElements(powblock: POWBlockBaseV1): Array[String] = {
    Array(
      __.toKeyValueString("timestamp", new Date(powblock.timestamp).toString),
      __.toKeyValueString("target", __.toHexString(powblock.target.toBytes)),
      __.toKeyValueString("nonce", __.toHexString(powblock.nonce))
    )
  }

  def allGenesisBlockToString(gblock: IGenesisBlock): String = {
    gblock match {
      case b: GenesisBlockTest1 => genesisBlockToString(b)
      case b: POWGenesisBlockTest2 => powGenesisBlockToString(b)
      case _ => __.emptyString
    }
  }
  def allNormalBlockToString(nblock: INormalBlock): String = {
    nblock match {
      case b: NormalBlockTest1 => normalBlockToString(b)
      case b: POWNormalBlockTest2 => powNormalBlockToString(b)
      case _ => __.emptyString
    }
  }

  def genesisBlockToString(gblock: GenesisBlockTest1): String = __.toMultilineString(block1ToStringElements(gblock) ++ gblock1ToStringElements(gblock))
  def normalBlockToString(nblock: NormalBlockTest1): String = __.toMultilineString(block1ToStringElements(nblock) ++ nblock1ToStringElements(nblock))

  def powGenesisBlockToString(gblock: POWGenesisBlockTest2): String = __.toMultilineString(block1ToStringElements(gblock) ++ gblock2ToStringElements(gblock) ++ powblockToStringElements(gblock))
  def powNormalBlockToString(nblock: POWNormalBlockTest2): String = __.toMultilineString(block1ToStringElements(nblock) ++ nblock2ToStringElements(nblock) ++ powblockToStringElements(nblock))

  def allGenesisBlockToHTML(gblock: IGenesisBlock): String = {
    gblock match {
      case b: GenesisBlockTest1 => genesisBlockToHTML(b)
      case b: POWGenesisBlockTest2 => powGenesisBlockToHTML(b)
      case _ => __.emptyString
    }
  }
  def allNormalBlockToHTML(nblock: INormalBlock): String = {
    nblock match {
      case b: NormalBlockTest1 => normalBlockToHTML(b)
      case b: POWNormalBlockTest2 => powNormalBlockToHTML(b)
      case _ => __.emptyString
    }
  }

  def genesisBlockToHTML(gblock: GenesisBlockTest1): String = __.toMultilineStringHTML(block1ToStringElements(gblock) ++ gblock1ToStringElements(gblock))
  def normalBlockToHTML(nblock: NormalBlockTest1): String = __.toMultilineStringHTML(block1ToStringElements(nblock) ++ nblock1ToStringElements(nblock))

  def powGenesisBlockToHTML(gblock: POWGenesisBlockTest2): String = __.toMultilineStringHTML(block1ToStringElements(gblock) ++ gblock2ToStringElements(gblock) ++ powblockToStringElements(gblock))
  def powNormalBlockToHTML(nblock: POWNormalBlockTest2): String = __.toMultilineStringHTML(block1ToStringElements(nblock) ++ nblock2ToStringElements(nblock) ++ powblockToStringElements(nblock))
}