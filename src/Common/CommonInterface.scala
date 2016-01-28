package Common

//�����r�\
trait ICompare {
  //���ꂩ�ǂ�����r���s���֐�
  //����ł���ꍇ�ɂ�true��Ԃ��A����łȂ��ꍇ�ɂ�false��Ԃ�
  protected def specIsSame(r: ICompare): Boolean

  //�n�b�V���R�[�h��Ԃ��֐�
  protected def specHashCode: Int

  //����ł���ꍇ�ɂ�true��Ԃ��A����łȂ��ꍇ�ɂ�false��Ԃ�
  def isSame(r: ICompare): Boolean = specIsSame(r)

  //����ł���ꍇ�ɂ�false��Ԃ��A����łȂ��ꍇ�ɂ�true��Ԃ�
  def isNotSame(r: ICompare): Boolean = !specIsSame(r)

  //��r���Z�q�Ȃǂ̂��߂�equals�֐�����������
  override def equals(r: Any) = r match {
    case r: ICompare => isSame(r)
    case _ => false
  }

  //equals�֐�����������ꍇ�ɂ�hashCode�֐����������Ȃ���΂Ȃ�Ȃ�
  override def hashCode: Int = specHashCode
}

//�召��\������
sealed trait Ordering {}

//�傫�����Ƃ�\���I�u�W�F�N�g
object Great extends Ordering {}

//�����ł��邱�Ƃ�\���I�u�W�F�N�g
object Equal extends Ordering {}

//���������Ƃ�\���I�u�W�F�N�g
object Less extends Ordering {}

//�召��r�\
//�召��r�\�ȏꍇ�A�����r���\�ł���
trait ICompareOrder extends ICompare {
  //�召��r���s���֐�
  //�召��\���I�u�W�F�N�g�̉��ꂩ��Ԃ�
  protected def specCompare(r: ICompareOrder): Ordering

  //���ꂩ�ǂ�����r���s���֐�
  protected override def specIsSame(r: ICompare): Boolean = specCompare(r.asInstanceOf[ICompareOrder]) == Equal

  //�傫���ꍇ�ɂ�true��Ԃ��A�������������ł���ꍇ�ɂ�false��Ԃ�
  def isGreat(r: ICompareOrder): Boolean = specCompare(r) == Great

  //�傫���������ł���ꍇ�ɂ�true��Ԃ��A�������ꍇ�ɂ�false��Ԃ�
  def isGreatOrEqual(r: ICompareOrder): Boolean = {
    val ord: Ordering = specCompare(r)
    ord == Great || ord == Equal
  }

  //�������ꍇ�ɂ�true��Ԃ��A�傫���������ł���ꍇ�ɂ�false��Ԃ�
  def isLess(r: ICompareOrder): Boolean = specCompare(r) == Less

  //�������������ł���ꍇ�ɂ�true��Ԃ��A�傫���ꍇ�ɂ�false��Ԃ�
  def isLessOrEqual(r: ICompareOrder): Boolean = {
    val ord: Ordering = specCompare(r)
    ord == Less || ord == Equal
  }
}

//�l���ړ����邱�Ƃ��ł���
trait ISequence {
  //�l���w�肳�ꂽ���ړ�����
  protected def specMove(delta: Long): ISequence

  //�l���w�肳�ꂽ���O���Ɉړ�����
  def moveForward(delta: Long): ISequence = specMove(delta)

  //�l���w�肳�ꂽ������Ɉړ�����
  def moveBackward(delta: Long): ISequence = specMove(-delta)
}

//�����Z�\
trait IAddition {
  //���Z���s���֐�
  protected def specAdd(r: IAddition): IAddition

  //���Z���s���֐�
  protected def specSubtract(r: IAddition): IAddition

  //���Z�������ʂ����̂܂ܕԂ�
  def add(r: IAddition): IAddition = specAdd(r)

  //���Z�������ʂ����̂܂ܕԂ�
  def subtract(r: IAddition): IAddition = specSubtract(r)

  //�t��Ԃ�
  def minus(): IAddition = specSubtract(this).specSubtract(this)
}

//�o�C�g�z��ɕϊ��\
trait IBytes {
  //�o�C�g�ϐ��ɕϊ�����֐�
  protected def specToBytes: Array[Byte]

  //�o�C�g�z��ɕϊ��������ʂ̃o�C�g�z������̂܂ܕԂ�
  def toBytes: Array[Byte] = specToBytes
}

trait IIdentifiable[T <: ICompareOrder] {
  protected def specGetComparableId: T

  def getComparableId: T = specGetComparableId
}