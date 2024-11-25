file:///C:/Users/user/OneDrive/바탕%20화면/scala/atfae/src/main/scala/kuplrg/Implementation.scala
### java.lang.IndexOutOfBoundsException: -1

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
offset: 2987
uri: file:///C:/Users/user/OneDrive/바탕%20화면/scala/atfae/src/main/scala/kuplrg/Implementation.scala
text:
```scala
package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Type.*

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

  def mustSame(lty: Type, rty: Type): Unit =
    if (lty != rty) error(s"type mismatch: ${lty.str} != ${rty.str}")

  def mustValid(ty: Type, tenv: TypeEnv): Type = ty match
    case NumT => NumT
    case BoolT => BoolT
    case ArrowT(ptys, rty) => 
      ArrowT(ptys.map(_, tenv), mustValid(rty, tenv))
    case NameT(tn) =>
      if (!tenv.tys.contains(tn)) error(s"invalid type name: $tn")
      NameT(tn)
  
  def typeCheck(expr: Expr, tenv: TypeEnv): Type = expr match
    // numbers
    case Num(number: BigInt) =>
      NumT
    // booleans
    case Bool(bool: Boolean) =>
      BoolT
    // addition
    case Add(left: Expr, right: Expr) =>
      mustSame(typeCheck(left, tenv), typeCheck(right, tenv))
      mustSame(typeCheck(left, tenv), NumT)
      NumT
    // multiplication
    case Mul(left: Expr, right: Expr) =>
      mustSame(typeCheck(left, tenv), typeCheck(right, tenv))
      mustSame(typeCheck(left, tenv), NumT)
      NumT
    // division
    case Div(left: Expr, right: Expr) =>
      mustSame(typeCheck(left, tenv), typeCheck(right, tenv))
      mustSame(typeCheck(left, tenv), NumT)
      NumT
    // modulo
    case Mod(left: Expr, right: Expr) =>
      mustSame(typeCheck(left, tenv), typeCheck(right, tenv))
      mustSame(typeCheck(left, tenv), NumT)
      NumT
    // equal-to
    case Eq(left: Expr, right: Expr) => 
      mustSame(typeCheck(left, tenv), typeCheck(right, tenv))
      mustSame(typeCheck(left, tenv), NumT)
      BoolT
    // less-than
    case Lt(left: Expr, right: Expr) =>
      mustSame(typeCheck(left, tenv), typeCheck(right, tenv))
      mustSame(typeCheck(left, tenv), NumT)
      BoolT
    // immutable variable definition
    case Val(name: String, init: Expr, body: Expr) =>
      val initType = typeCheck(init, tenv)
      typeCheck(body, tenv + (name -> initType))
    // identifier lookups
    case Id(name: String) =>
      tenv.getOrElse(name, error("free identifier"))
    // anonymous (lambda) functions
    case Fun(params: List[Param], body: Expr) =>
      val ptys = params.map(_.ty)
      for (@@)
    // recursive functions
    case Rec(x: String, p: String, pty: Type, rty: Type, body: Expr, scope: Expr) =>
      val newTenv = tenv + (x -> ArrowT(pty, rty))
      mustSame(typeCheck(body, newTenv + (p -> pty)), rty)
      typeCheck(scope, newTenv)
    // function applications
    case App(fun: Expr, arg: Expr) => typeCheck(fun, tenv) match
      case ArrowT(pty, rty) =>
        mustSame(pty, typeCheck(arg, tenv))
        rty
      case ty => error(s"not a function type: ${ty.str}")
    // conditional
    case If(cond: Expr, thenExpr: Expr, elseExpr: Expr) =>
      mustSame(BoolT, typeCheck(cond, tenv))
      mustSame(typeCheck(thenExpr, tenv), typeCheck(elseExpr, tenv))
      typeCheck(thenExpr, tenv)
  

  def interp(expr: Expr, env: Env): Value = ???

}

```



#### Error stacktrace:

```
scala.collection.LinearSeqOps.apply(LinearSeq.scala:129)
	scala.collection.LinearSeqOps.apply$(LinearSeq.scala:128)
	scala.collection.immutable.List.apply(List.scala:79)
	dotty.tools.dotc.util.Signatures$.applyCallInfo(Signatures.scala:244)
	dotty.tools.dotc.util.Signatures$.computeSignatureHelp(Signatures.scala:101)
	dotty.tools.dotc.util.Signatures$.signatureHelp(Signatures.scala:88)
	dotty.tools.pc.SignatureHelpProvider$.signatureHelp(SignatureHelpProvider.scala:47)
	dotty.tools.pc.ScalaPresentationCompiler.signatureHelp$$anonfun$1(ScalaPresentationCompiler.scala:422)
```
#### Short summary: 

java.lang.IndexOutOfBoundsException: -1