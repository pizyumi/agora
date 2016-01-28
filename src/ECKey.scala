import java.io.ByteArrayOutputStream
import java.math.BigInteger
import java.security.SecureRandom

import org.spongycastle.asn1.ASN1InputStream
import org.spongycastle.asn1.ASN1Integer
import org.spongycastle.asn1.DERSequenceGenerator
import org.spongycastle.asn1.DLSequence
import org.spongycastle.asn1.x9.X9ECParameters
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.digests.SHA256Digest
import org.spongycastle.crypto.ec.CustomNamedCurves
import org.spongycastle.crypto.generators.ECKeyPairGenerator
import org.spongycastle.crypto.params.ECDomainParameters
import org.spongycastle.crypto.params.ECKeyGenerationParameters
import org.spongycastle.crypto.params.ECPrivateKeyParameters
import org.spongycastle.crypto.params.ECPublicKeyParameters
import org.spongycastle.crypto.signers.HMacDSAKCalculator
import org.spongycastle.crypto.signers.ECDSASigner
import org.spongycastle.math.ec.ECPoint
import org.spongycastle.math.ec.FixedPointUtil

object Crypto {
  import Common._

  class ECPubKey(pubIn: ECPoint) {
    val pub: ECPoint = pubIn

    def verify(bytes: Array[Byte])(sig: ECDSASignature): Boolean = ECKeyPairFactory.verify(bytes)(sig)(this)
    def getPubBytes: Array[Byte] = pub.getEncoded(true)
  }

  class ECKeyPair(privIn: BigInteger, pubIn: ECPoint) extends ECPubKey(pubIn) {
    val priv: BigInteger = privIn

    def sign(bytes: Array[Byte]): ECDSASignature = ECKeyPairFactory.sign(bytes)(this)
    def getPrivBytes: Array[Byte] = __.bigIntegerToBytes(priv, 32)
  }

  object ECKeyPairFactory {
    FixedPointUtil.precompute(curveParams.getG, 12)

    lazy val curveParams: X9ECParameters = CustomNamedCurves.getByName("secp256k1")
    lazy val curve: ECDomainParameters = new ECDomainParameters(curveParams.getCurve, curveParams.getG, curveParams.getN,
      curveParams.getH)
    lazy val halfCurveOrder: BigInteger = curveParams.getN.shiftRight(1)
    lazy val secureRandom: SecureRandom = new SecureRandom()

    def create(): ECKeyPair = {
      val g: ECKeyPairGenerator = new ECKeyPairGenerator()
      g.init(new ECKeyGenerationParameters(curve, secureRandom))
      val pair: AsymmetricCipherKeyPair = g.generateKeyPair()
      val privParams: ECPrivateKeyParameters = pair.getPrivate.asInstanceOf[ECPrivateKeyParameters]
      val pubParams: ECPublicKeyParameters = pair.getPublic.asInstanceOf[ECPublicKeyParameters]
      new ECKeyPair(privParams.getD, pubParams.getQ)
    }

    def createFromBytes(privBytes: Array[Byte])(pubBytes: Array[Byte]): ECKeyPair = {
      new ECKeyPair(new BigInteger(1, privBytes), curve.getCurve.decodePoint(pubBytes))
    }

    def createFromPubBytes(pubBytes: Array[Byte]): ECPubKey = {
      new ECPubKey(curve.getCurve.decodePoint(pubBytes))
    }

    def sign(bytes: Array[Byte])(pair: ECKeyPair): ECDSASignature = {
      val signer: ECDSASigner = new ECDSASigner(new HMacDSAKCalculator(new SHA256Digest()))
      signer.init(true, new ECPrivateKeyParameters(pair.priv, curve))
      val components: Array[BigInteger] = signer.generateSignature(bytes)
      new ECDSASignature(components(0), components(1)).toCanonicalized
    }

    def verify(bytes: Array[Byte])(sig: ECDSASignature)(pair: ECPubKey): Boolean = {
      val signer: ECDSASigner = new ECDSASigner()
      signer.init(false, new ECPublicKeyParameters(pair.pub, curve))
      signer.verifySignature(bytes, sig.r, sig.s)
    }
  }

  class ECDSASignature(rIn: BigInteger, sIn: BigInteger) {
    val r: BigInteger = rIn
    val s: BigInteger = sIn

    lazy val isCanonical: Boolean = s.compareTo(ECKeyPairFactory.halfCurveOrder) <= 0

    def toCanonicalized: ECDSASignature = {
      if (isCanonical) {
        this
      }
      else {
        new ECDSASignature(r, ECKeyPairFactory.curve.getN.subtract(s))
      }
    }

    def toDER: Array[Byte] = {
      val baos: ByteArrayOutputStream = new ByteArrayOutputStream(72)
      val seq: DERSequenceGenerator = new DERSequenceGenerator(baos)
      seq.addObject(new ASN1Integer(r))
      seq.addObject(new ASN1Integer(s))
      seq.close()
      baos.toByteArray
    }
  }

  object ECDSASignatureFactory {
    def createFromDER(bytes: Array[Byte]): ECDSASignature = {
      val seq: DLSequence = new ASN1InputStream(bytes).readObject().asInstanceOf
      new ECDSASignature(seq.getObjectAt(0).asInstanceOf[ASN1Integer].getPositiveValue, seq.getObjectAt(1).asInstanceOf[ASN1Integer].getPositiveValue)
    }
  }

  class Secp256k1TestCLI() extends ICLIComponent with ITestComponent {
    protected lazy val testInterface: TestInterface = new TestCLI()

    protected lazy val test: String = "test secp256k1"

    def getCommands: Traversable[Command] = {
      Array(
        new Command(test, (args) => doTest())
      )
    }

    def getTests: Traversable[Test] = {
      Array(
        new Test(doTest)
      )
    }

    protected def doTest(): Unit = {
      testInterface.outputTitle("secp256k1 ecdsa test 1", None)

      testInterface.outputMessage("generating ec key pair...")
      val pair: ECKeyPair = ECKeyPairFactory.create()
      testInterface.outputMessage(ecKeyPairToString(pair))
      testInterface.outputMessage("creating ec key pair...")
      val pair2: ECKeyPair = ECKeyPairFactory.createFromBytes(pair.getPrivBytes)(pair.getPubBytes)
      testInterface.outputMessage(ecKeyPairToString(pair2))
      testInterface.outputItem("1.1", Some("both key pairs are same"), pair.getPrivBytes.sameElements(pair2.getPrivBytes) && pair.getPubBytes.sameElements(pair2.getPubBytes))
      testInterface.outputMessage("generating random data...")
      val data: Int = __.getRandomInt
      testInterface.outputMessage(__.toKeyValueString("data", data.toString))
      testInterface.outputMessage("calclating sha256 hash...")
      val hash: Array[Byte] = __.getSha256(__.getBytes(data))
      testInterface.outputMessage(__.toKeyValueString("hash", __.toHexString(hash)))
      testInterface.outputMessage("generating signature...")
      val sig: ECDSASignature = pair.sign(hash)
      testInterface.outputMessage(ecdsaSignatureToString(sig))
      testInterface.outputItem("1.2", Some("signature is valid"), pair.verify(hash)(sig))
      testInterface.outputMessage("generating another random data...")
      val data2: Int = __.getRandomInt
      testInterface.outputMessage(__.toKeyValueString("data", data2.toString))
      testInterface.outputMessage("calclating sha256 hash...")
      val hash2: Array[Byte] = __.getSha256(__.getBytes(data2))
      testInterface.outputMessage(__.toKeyValueString("hash", __.toHexString(hash2)))
      testInterface.outputItem("1.3", Some("signature is invalid with irrelevant data"), !pair.verify(hash2)(sig))
      testInterface.outputMessage("generating another ec key pair...")
      val pair3: ECKeyPair = ECKeyPairFactory.create()
      testInterface.outputMessage(ecKeyPairToString(pair3))
      testInterface.outputItem("1.4", Some("signature is invalid with irrelevant public key"), !pair3.verify(hash)(sig))
    }

    protected def ecKeyPairToString(pair: ECKeyPair): String = {
      __.toMultilineString(Array(
        __.toKeyValueString("private key", __.toHexString(pair.getPrivBytes)),
        __.toKeyValueString("public key", __.toHexString(pair.getPubBytes))
      ))
    }
    protected def ecdsaSignatureToString(sig: ECDSASignature): String = {
      __.toMultilineString(Array(
        __.toKeyValueString("r", sig.r.toString),
        __.toKeyValueString("s", sig.s.toString)
      ))
    }
  }
}