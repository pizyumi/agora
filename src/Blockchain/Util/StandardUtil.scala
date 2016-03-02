package Blockchain.Util

import java.util.Date

import Blockchain.Interface._
import Blockchain.Impl._
import Blockchain.Impl1._
import Blockchain.Impl2._
import Common.__

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