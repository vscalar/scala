file:///C:/Users/user/OneDrive/바탕%20화면/scala/cobalt/src/main/scala/kuplrg/Implementation.scala
### java.lang.AssertionError: NoDenotation.owner

occurred in the presentation compiler.

presentation compiler configuration:
Scala version: 3.3.3
Classpath:
<HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala3-library_3\3.3.3\scala3-library_3-3.3.3.jar [exists ], <HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala-library\2.13.12\scala-library-2.13.12.jar [exists ]
Options:



action parameters:
offset: 655
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
    case (NumV(l), NumV(r)) if (NumV(r) != NumV(@@0)=> NumV(op(l, r))
    case _ => error("invalid operation")
  
  val numDiv: BOp[Value] = numBOp2(_ / _)
  val numMod: BOp[Value] = numBOp2(_ % _)

  def interp(expr: Expr, env: Env): Value = expr match
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
    case EEq(left: Expr, right: Expr) => NumV(0)
    // less-than
    case ELt(left: Expr, right: Expr) => NumV(0)
    // conditional
    case EIf(cond: Expr, thenExpr: Expr, elseExpr: Expr) => NumV(0)
    // empty list
    case ENil => NilV
    // list cons
    case ECons(head: Expr, tail: Expr) => NumV(0)
    // list head
    case EHead(list: Expr) => NumV(0)
    // list tail
    case ETail(list: Expr) => NumV(0)
    // list map function
    case EMap(list: Expr, fun: Expr) => NumV(0)
    // list flatMap function
    case EFlatMap(list: Expr, fun: Expr) => NumV(0)
    // list filter function
    case EFilter(list: Expr, fun: Expr) => NumV(0)
    // list foldLeft function
    case EFoldLeft(list: Expr, init: Expr, fun: Expr) => NumV(0)
    // tuple
    case ETuple(exprs: List[Expr]) => NumV(0)
    // tuple projection
    case EProj(tuple: Expr, index: Int) => NumV(0)
    // variable definition
    case EVal(name: String, value: Expr, scope: Expr) => NumV(0)
    // lambda function
    case EFun(params: List[String], body: Expr) => NumV(0)
    // mutually recursive function
    case ERec(defs: List[FunDef], scope: Expr) => NumV(0)
    // function application
    case EApp(fun: Expr, args: List[Expr]) => NumV(0)
  

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
dotty.tools.dotc.core.SymDenotations$NoDenotation$.owner(SymDenotations.scala:2607)
	scala.meta.internal.pc.SignatureHelpProvider$.isValid(SignatureHelpProvider.scala:83)
	scala.meta.internal.pc.SignatureHelpProvider$.notCurrentApply(SignatureHelpProvider.scala:94)
	scala.meta.internal.pc.SignatureHelpProvider$.$anonfun$1(SignatureHelpProvider.scala:48)
	scala.collection.StrictOptimizedLinearSeqOps.dropWhile(LinearSeq.scala:280)
	scala.collection.StrictOptimizedLinearSeqOps.dropWhile$(LinearSeq.scala:278)
	scala.collection.immutable.List.dropWhile(List.scala:79)
	scala.meta.internal.pc.SignatureHelpProvider$.signatureHelp(SignatureHelpProvider.scala:48)
	scala.meta.internal.pc.ScalaPresentationCompiler.signatureHelp$$anonfun$1(ScalaPresentationCompiler.scala:435)
```
#### Short summary: 

java.lang.AssertionError: NoDenotation.owner