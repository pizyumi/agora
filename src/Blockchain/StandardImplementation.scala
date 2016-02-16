package Blockchain

import java.math.BigInteger

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

object BlockchainSettings {
  lazy val haSha256: String = "sha256"
  lazy val haProperties: Map[String, HashAlgorithmProperty] = Map(haSha256 -> new HashAlgorithmProperty(32))

  lazy val defaultSeed: String = "seed"
  lazy val defaultInitialTarget: IdV1 = new IdV1(Array(UByte.__(0), UByte.__(127), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255), UByte.__(255)))
}
class BlockchainSettings(hashAlgorithmIn: String, seedIn: String, initialTimestampIn: Long, initialTargetIn: IdV1) {
  val hashAlgorithm: String = hashAlgorithmIn
  val hashAlgorithmProperty: HashAlgorithmProperty = BlockchainSettings.haProperties(hashAlgorithmIn)
  val seed: String = seedIn
  val initialTimestamp: Long = initialTimestampIn
  val initialTarget: IdV1 = initialTargetIn
  val diff1Target: IdV1 = {
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

  lazy val langthBit: Int = lengthByte * 8
}

//ブロックの標準実装
trait BlockBaseV1 extends IBlock {
  //高さ
  val index: IndexV1
  //信用度
  val trustworthiness: TrustworthinessV1

  //識別子
  val id: IdV1 = new IdV1(toId)

  //ハッシュ関数
  protected val hashAlgorithm: String

  //識別子を計算する構成要素
  protected def toIdBytesIngredient: Array[Array[Byte]] = Array(index.toBytes, parentId.map((v) => v.toBytes).getOrElse(Array.emptyByteArray))
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
trait POWBlockBaseV1 extends BlockBaseV1 {
  val target: IdV1
}

class GenesisBlockTest1(seedIn: String) extends BlockBaseV1 with IGenesisBlock {
  val maxSeedLength: Int = 1024

  val index: IndexV1 = new IndexV1(0)
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

class NormalBlockTest1(indexIn: IndexV1, parentIdIn: IdV1, trustworthinessIn: TrustworthinessV1, dataIn: Array[Byte]) extends BlockBaseV1() {
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

  protected val hashAlgorithm: String = BlockchainSettings.haSha256

  protected override def toIdBytesIngredient: Array[Array[Byte]] = super.toIdBytesIngredient ++ Array(data, trustworthiness.trustworthiness.toByteArray)
}

abstract class GenesisBlockTest2(settings: BlockchainSettings) extends BlockBaseV1 with IGenesisBlock {
  val maxSeedLength: Int = 1024

  val index: IndexV1 = new IndexV1(0)
  val trustworthiness: TrustworthinessV1 = new TrustworthinessV1(BigInteger.ZERO)

  val seed: String = {
    if (settings.seed.length > maxSeedLength) {
      throw new IllegalArgumentException("seed is too long")
    }
    settings.seed
  }

  protected override def toIdBytesIngredient: Array[Array[Byte]] = super.toIdBytesIngredient ++ Array(__.getBytes(seed))
}

abstract class NormalBlockTest2(indexIn: IndexV1, parentIdIn: IdV1, dataIn: Array[Byte]) extends BlockBaseV1() {
  val maxDataLength: Int = 1024

  val index: IndexV1 = indexIn
  val parentId: Option[IdV1] = Some(parentIdIn)

  val data: Array[Byte] = {
    if (dataIn.length > maxDataLength) {
      throw new IllegalArgumentException("data is too long")
    }
    dataIn
  }

  protected override def toIdBytesIngredient: Array[Array[Byte]] = super.toIdBytesIngredient ++ Array(data)
}

class POWGenesisBlockTest2(settings: BlockchainSettings) extends GenesisBlockTest2(settings) with POWBlockBaseV1 {
  val timestamp: Long = settings.initialTimestamp
  val target: IdV1 = settings.initialTarget
  val nonce: Array[Byte] = Array[Byte](0)

  protected val hashAlgorithm: String = settings.hashAlgorithm

  protected override def toIdBytesIngredient: Array[Array[Byte]] = super.toIdBytesIngredient ++ Array(__.getBytes(timestamp), target.toBytes)
}

class POWNormalBlockTest2(settings: BlockchainSettings, indexIn: IndexV1, parentIdIn: IdV1, timestampIn: Long, targetIn: IdV1, nonceIn: Array[Byte], dataIn: Array[Byte]) extends NormalBlockTest2(indexIn, parentIdIn, dataIn) with POWBlockBaseV1 {
  val trustworthiness: TrustworthinessV1 = toTrustworthiness

  val timestamp: Long = timestampIn
  val target: IdV1 = targetIn
  val nonce: Array[Byte] = nonceIn

  protected val hashAlgorithm: String = settings.hashAlgorithm

  protected def toTrustworthiness: TrustworthinessV1 = {
    //val diff1TargetBigint: BigInteger = __.bigIntegerToBytes()
    throw new Exception()
  }

  protected override def toIdBytesIngredient: Array[Array[Byte]] = super.toIdBytesIngredient ++ Array(__.getBytes(timestamp), target.toBytes, nonce)
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