package Common

//同一比較可能
trait ICompare {
  //同一かどうか比較を行う関数
  //同一である場合にはtrueを返し、同一でない場合にはfalseを返す
  protected def specIsSame(r: ICompare): Boolean

  //ハッシュコードを返す関数
  protected def specHashCode: Int

  //同一である場合にはtrueを返し、同一でない場合にはfalseを返す
  def isSame(r: ICompare): Boolean = specIsSame(r)

  //同一である場合にはfalseを返し、同一でない場合にはtrueを返す
  def isNotSame(r: ICompare): Boolean = !specIsSame(r)

  //比較演算子などのためにequals関数を実装する
  override def equals(r: Any) = r match {
    case r: ICompare => isSame(r)
    case _ => false
  }

  //equals関数を実装する場合にはhashCode関数も実装しなければならない
  override def hashCode: Int = specHashCode
}

//大小を表す特質
sealed trait Ordering {}

//大きいことを表すオブジェクト
object Great extends Ordering {}

//同等であることを表すオブジェクト
object Equal extends Ordering {}

//小さいことを表すオブジェクト
object Less extends Ordering {}

//大小比較可能
//大小比較可能な場合、同一比較も可能である
trait ICompareOrder extends ICompare {
  //大小比較を行う関数
  //大小を表すオブジェクトの何れかを返す
  protected def specCompare(r: ICompareOrder): Ordering

  //同一かどうか比較を行う関数
  protected override def specIsSame(r: ICompare): Boolean = specCompare(r.asInstanceOf[ICompareOrder]) == Equal

  //大きい場合にはtrueを返し、小さいか同等である場合にはfalseを返す
  def isGreat(r: ICompareOrder): Boolean = specCompare(r) == Great

  //大きいか同等である場合にはtrueを返し、小さい場合にはfalseを返す
  def isGreatOrEqual(r: ICompareOrder): Boolean = {
    val ord: Ordering = specCompare(r)
    ord == Great || ord == Equal
  }

  //小さい場合にはtrueを返し、大きいか同等である場合にはfalseを返す
  def isLess(r: ICompareOrder): Boolean = specCompare(r) == Less

  //小さいか同等である場合にはtrueを返し、大きい場合にはfalseを返す
  def isLessOrEqual(r: ICompareOrder): Boolean = {
    val ord: Ordering = specCompare(r)
    ord == Less || ord == Equal
  }
}

//値を移動することができる
trait ISequence {
  //値を指定された分移動する
  protected def specMove(delta: Long): ISequence

  //値を指定された分前方に移動する
  def moveForward(delta: Long): ISequence = specMove(delta)

  //値を指定された分後方に移動する
  def moveBackward(delta: Long): ISequence = specMove(-delta)
}

//加減算可能
trait IAddition {
  //加算を行う関数
  protected def specAdd(r: IAddition): IAddition

  //減算を行う関数
  protected def specSubtract(r: IAddition): IAddition

  //加算した結果をそのまま返す
  def add(r: IAddition): IAddition = specAdd(r)

  //減算した結果をそのまま返す
  def subtract(r: IAddition): IAddition = specSubtract(r)

  //逆を返す
  def minus(): IAddition = specSubtract(this).specSubtract(this)
}

//バイト配列に変換可能
trait IBytes {
  //バイト変数に変換する関数
  protected def specToBytes: Array[Byte]

  //バイト配列に変換した結果のバイト配列をそのまま返す
  def toBytes: Array[Byte] = specToBytes
}

trait IIdentifiable[T <: ICompareOrder] {
  protected def specGetComparableId: T

  def getComparableId: T = specGetComparableId
}