file:///C:/Users/user/OneDrive/바탕%20화면/scala/bfae/src/main/scala/kuplrg/Implementation.scala
### java.lang.IndexOutOfBoundsException: 0

occurred in the presentation compiler.

presentation compiler configuration:
Scala version: 3.3.3
Classpath:
<HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala3-library_3\3.3.3\scala3-library_3-3.3.3.jar [exists ], <HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala-library\2.13.12\scala-library-2.13.12.jar [exists ]
Options:



action parameters:
offset: 1554
uri: file:///C:/Users/user/OneDrive/바탕%20화면/scala/bfae/src/main/scala/kuplrg/Implementation.scala
text:
```scala
package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*

  type BOp[T] = (T, T) => T
  def numBOp1(op: BOp[BigInt]): BOp[Value] = (_, _) match
    case (NumV(l), NumV(r))=> NumV(op(l, r))
    case _ => error("invalid operation")
  
  val numAdd: BOp[Value] = numBOp1(_ + _)
  val numMul: BOp[Value] = numBOp1(_ * _)

  def numBOp2(op: BOp[BigInt]): BOp[Value] = (_, _) match
    case (NumV(l), NumV(r)) if (NumV(r) != NumV(0))=> NumV(op(l, r))
    case _ => error("invalid operation")
  
  val numDiv: BOp[Value] = numBOp2(_ / _)
  val numMod: BOp[Value] = numBOp2(_ % _)

  def interp(expr: Expr, env: Env, mem: Mem): (Value, Mem) = expr match
      // number
  case Num(number: BigInt) => (NumV(number), mem)
  // addition
  case Add(left: Expr, right: Expr) =>
    val (lv, lmem) = interp(left, env, mem)
    val (rv, rmem) = interp(right, env, lmem)
    (numAdd(lv, rv), rmem)
  // multiplication
  case Mul(left: Expr, right: Expr) =>
    val (lv, lmem) = interp(left, env, mem)
    val (rv, rmem) = interp(right, env, lmem)
    (numMul(lv, rv), rmem)
  // identifier lookup
  case Id(name: String) => env.getOrElse(x, error("free identifier"))
  // anonymous (lambda) function
  case Fun(param: String, body: Expr) => (CloV(params, body, env), mem)
  // function application
  case App(fun: Expr, arg: Expr) => interp(fun, env, mem) match
    case (CloV(params, body, env), mem1) => 
      val (newEnv, mem2): (Env, Mem)= interp(arg, env, mem1)
      interp(body, env + @@)
  // box creation
  case NewBox(content: Expr)
  // box content getter
  case GetBox(box: Expr)
  // box content setter
  case SetBox(box: Expr, content: Expr)
  // sequence
  case Seq(left: Expr, right: Expr)
}

```



#### Error stacktrace:

```
scala.collection.LinearSeqOps.apply(LinearSeq.scala:131)
	scala.collection.LinearSeqOps.apply$(LinearSeq.scala:128)
	scala.collection.immutable.List.apply(List.scala:79)
	dotty.tools.dotc.util.Signatures$.countParams(Signatures.scala:501)
	dotty.tools.dotc.util.Signatures$.applyCallInfo(Signatures.scala:186)
	dotty.tools.dotc.util.Signatures$.computeSignatureHelp(Signatures.scala:94)
	dotty.tools.dotc.util.Signatures$.signatureHelp(Signatures.scala:63)
	scala.meta.internal.pc.MetalsSignatures$.signatures(MetalsSignatures.scala:17)
	scala.meta.internal.pc.SignatureHelpProvider$.signatureHelp(SignatureHelpProvider.scala:51)
	scala.meta.internal.pc.ScalaPresentationCompiler.signatureHelp$$anonfun$1(ScalaPresentationCompiler.scala:435)
```
#### Short summary: 

java.lang.IndexOutOfBoundsException: 0