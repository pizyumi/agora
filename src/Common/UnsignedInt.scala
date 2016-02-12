package Common

object UByte {
  def __(x: Int): Byte = {
    if (x < 0) {
      throw new Exception("out of range")
    }
    else if (x < 128) {
      x.asInstanceOf[Byte]
    }
    else if (x < 256) {
      (x - 256).asInstanceOf[Byte]
    }
    else {
      throw new Exception("out of range")
    }
  }
}