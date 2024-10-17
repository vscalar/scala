file:///C:/Users/user/OneDrive/바탕%20화면/scala/rfae/src/main/scala/kuplrg/Implementation.scala
### java.lang.IndexOutOfBoundsException: 0

occurred in the presentation compiler.

presentation compiler configuration:
Scala version: 3.3.3
Classpath:
<HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala3-library_3\3.3.3\scala3-library_3-3.3.3.jar [exists ], <HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala-library\2.13.12\scala-library-2.13.12.jar [exists ]
Options:



action parameters:
offset: 2188
uri: file:///C:/Users/user/OneDrive/바탕%20화면/scala/rfae/src/main/scala/kuplrg/Implementation.scala
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

  type COp[T] = (T, T) => Boolean
  def numCOp(op: COp[BigInt]): BOp[Value] =
    case (NumV(l), NumV(r)) => BoolV(op(l, r))
    case _ => error("invalid operation")
  
  val numLt: BOp[Value] = numCOp(_ < _)
  def interp(expr: Expr, env: Env): Value = expr match
  // numbers
  case Num(number: BigInt) => NumV(number)
  // booleans
  case Bool(bool: Boolean) => BoolV(bool)
  // identifier lookups
  case Id(name: String) => env.getOrElse(x, error("free identifier"))
  // addition
  case Add(left: Expr, right: Expr) => numAdd(interp(l, env), interp(r, env))
  // multiplication
  case Mul(left: Expr, right: Expr) => numMul(interp(l, env), interp(r, env))
  // division
  case Div(left: Expr, right: Expr) => numDiv(interp(l, env), interp(r, env))
  // modulo
  case Mod(left: Expr, right: Expr) => numMod(interp(l, env), interp(r, env))
  // equal-to
  case Eq(left: Expr, right: Expr) => BoolV(eq(interp(l, env), interp(r, env)))
  // less-than
  case Lt(left: Expr, right: Expr) => numLt(interp(l, env), interp(r, env))
  // anonymous (lambda) functions
  case Fun(param: String, body: Expr) => CloV(params, body, () => env)
  // recursive functions
  case Rec(name: String, param: String, body: Expr, scope: Expr) => 
      lazy val newEnv: Env = env + (name -> CloV(param, body, () => newEnv))
      // println()
      // print(newEnv)
      interp(scope, newEnv)
  // function applications
  case App(fun: Expr, arg: Expr) => interp(fun, env) match
      case CloV(p, b, fenv) => 
        val newEnv: Env = fenv(@@) + 
        app(interp(fun, env), argsV)
      case _ => error("not a function")
  // conditional
  case If(cond: Expr, thenExpr: Expr, elseExpr: Expr) 
  

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