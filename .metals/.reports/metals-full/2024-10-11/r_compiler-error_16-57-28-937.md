file:///C:/Users/user/OneDrive/바탕%20화면/scala/cobalt/src/main/scala/kuplrg/Implementation.scala
### java.lang.IndexOutOfBoundsException: 0

occurred in the presentation compiler.

presentation compiler configuration:
Scala version: 3.3.3
Classpath:
<HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala3-library_3\3.3.3\scala3-library_3-3.3.3.jar [exists ], <HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala-library\2.13.12\scala-library-2.13.12.jar [exists ]
Options:



action parameters:
offset: 3475
uri: file:///C:/Users/user/OneDrive/바탕%20화면/scala/cobalt/src/main/scala/kuplrg/Implementation.scala
text:
```scala
package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*

  // ---------------------------------------------------------------------------
  // Problem #1
  // ---------------------------------------------------------------------------
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

  def interp(expr: Expr, env: Env): Value = 
    println()
    println(expr)
    expr match
    // numbers
    case ENum(n) => NumV(n)
    // booleans
    case EBool(b) => BoolV(b)
    // identifier lookups
    case EId(x) => env.getOrElse(x, error("free identifier"))
    // addition
    case EAdd(l, r) => numAdd(interp(l, env), interp(r, env))
    // multiplication
    case EMul(l, r) => numMul(interp(l, env), interp(r, env))
    // division
    case EDiv(l, r) => numDiv(interp(l, env), interp(r, env))
    // modulo
    case EMod(l, r) => numMod(interp(l, env), interp(r, env))
    // equal-to
    case EEq(l, r) => if (interp(l, env) == interp(r, env)) BoolV(true) else BoolV(false)
    // less-than
    case ELt(l, r) => numLt(interp(l, env), interp(r, env))
    // conditional
    case EIf(c, t, e) => interp(c, env) match
      case BoolV(true) => interp(t, env)
      case BoolV(false) => interp(e, env)
      case _ => error("not a boolean")
    // empty list
    case ENil => NilV
    // list cons
    case ECons(head: Expr, tail: Expr) => ConsV(interp(head, env), interp(tail, env))
    // list head
    case EHead(list: Expr) => list match
      case ECons(head, _) => interp(head, env)
      case ETail(l) => NumV(0)
      case ENil => error("empty list")
      case _ => error("not a list")
    // list tail
    case ETail(list: Expr) => list match
      case ECons(_, tail) => interp(tail, env)
      case ENil => error("empty list")
      case _ => error("not a list")
    // list map function
    case EMap(list: Expr, fun: Expr) => NumV(0)
    // list flatMap function
    case EFlatMap(list: Expr, fun: Expr) => NumV(0)
    // list filter function
    case EFilter(list: Expr, fun: Expr) => NumV(0)
    // list foldLeft function
    case EFoldLeft(list: Expr, init: Expr, fun: Expr) => NumV(0)
    // tuple
    case ETuple(exprs: List[Expr]) => 
      TupleV(for{
        e <- exprs
      } yield interp(e, env))
    // tuple projection
    case EProj(tuple: Expr, index: Int) => NumV(0)
    // variable definition
    case EVal(name: String, value: Expr, scope: Expr) => NumV(0)
    // lambda function
    case EFun(p, b) => CloV(p, b, () => env)
    // mutually recursive function
    case ERec(defs: List[FunDef], scope: Expr) => NumV(0)
    // function application
    case EApp(f, a) => interp(f, env) match
      case CloV(p, b, fenv) => interp(b, fenv(@@))
    

  

  def eq(left: Value, right: Value): Boolean = ???

  def map(list: Value, func: Value): Value = ???

  def join(list: Value): Value = ???

  def filter(list: Value, func: Value): Value = ???

  def foldLeft(list: Value, init: Value, func: Value): Value = ???

  def app(func: Value, args: List[Value]): Value = ???

  // ---------------------------------------------------------------------------
  // Problem #2
  // ---------------------------------------------------------------------------
  def subExpr1: String = ???

  def subExpr2: String = ???
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