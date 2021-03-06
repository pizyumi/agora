package Blockchain.Interface

import Common._

//ブロックの識別子を表す
//同一比較可能であり、バイト配列に変換可能である
trait IId extends ICompare with IBytes {}
//ブロックの信用度を表す
//大小比較可能であり、加減算可能である
trait ITrustworthiness extends ICompareOrder with IAddition {}
//ブロックを表す
//同一比較可能である
trait IBlock extends ICompare {
  //高さ
  val index: Long
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
  val parentId: Option[IId] = None
}
//通常ブロックを表す
trait INormalBlock extends IBlock {
}

//POWに必要な属性を表す
trait IPOW extends IValidatableItems {
  //時刻印
  val timestamp: Long
  //目標
  val target: IId
  //解
  val nonce: Array[Byte]
}

//ブロック鎖を表す
trait IBlockChain extends IConvalidatableItems[IBlock] {
  //ブロックを追加する
  def addBlock(block: IBlock): Either[Unit, String]
  //ブロックを削除する
  def deleteBlock(id: IId): Either[Unit, String]
  def deleteBlock(block: IBlock): Either[Unit, String]
  //ブロックを取得する
  def getBlock(id: IId): Option[IBlock]
  //親ブロックを取得する
  def getParentBlock(block: IBlock): Option[IBlock]
  //指定した個数分の親ブロックを取得する
  def getParentBlocks(block: IBlock, n: Int): Option[Traversable[IBlock]]
  //子ブロックを取得する
  def getChildBlocks(block: IBlock): Option[Traversable[IBlock]]
  //ブロックが含まれている場合にはtrueを返し、含まれていない場合にはfalseを返す
  def isContain(block: IBlock): Boolean
  def isContain(id: IId): Boolean

  //先頭ブロックを取得する
  def getHeadBlock: IBlock
  //指定した番号の有効なブロックを取得する
  def getActiveBlock(index: Long): Option[IBlock]
  //先頭ブロックから指定した個数分の有効なブロックを取得する
  def getBlockchain(n: Int): Traversable[IBlock]
  //指定した番号から指定した個数分の有効なブロックを取得する
  def getBlockchain(index: Long, n: Int): Option[Traversable[IBlock]]
  //ブロックが有効な場合にはtrueを返し、有効でない場合にはfalseを返す
  def isActive(block: IBlock): Boolean
  def isActive(id: IId): Boolean

  //DOT形式のグラフを作成する
  def toDotGraph(valueToString: IBlock => String): String = throw new UnsupportedOperationException()
}