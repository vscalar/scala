file:///C:/Users/user/OneDrive/바탕%20화면/scala/atfae/src/main/scala/kuplrg/Implementation.scala
### java.lang.IllegalArgumentException: Comparison method violates its general contract!

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
offset: 3789
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
      for (pty <- ptys) mustValid(pty, tenv)
      val rty = typeCheck(body, tenv.addVars(params.map(p => p.name -> p.ty)))
      ArrowT(ptys, rty)
    // recursive functions
    case Rec(x: String, params: List[Param], rty: Type, body: Expr, scope: Expr) =>
      val ptys = params.map(_.ty)
      for (pty <- ptys) mustValid(pty, tenv)
      mustValid(rty, tenv)
      val fty = ArrowT(ptys, rty)
      val bty = typeCheck(body, tenv.addVar(x -> fty).addVars(params.map(p => p.name -> p.ty)))
      mustSame(bty, rty)
      typeCheck(scope, tenv.addVar(x - fty))
    // function applications
    case App(fun: Expr, args: List[Expr]) =>
      case ArrowT(ptys, rty) =>
        if (ptys.length != args.length) error("arity mismatch")
        (pyts zip args).map((p, a) => mustSame(typeCheck(a, te@@)))
        mustSame(pty, typeCheck(arg, tenv))
        rty
      case ty => error(s"not a function type: ${ty.str}")
    // conditional
    case If(cond: Expr, thenExpr: Expr, elseExpr: Expr)
    // algebraic data type
    case TypeDef(name: String, varts: List[Variant], body: Expr)
    // pattern matching
    case Match(expr: Expr, mcases: List[MatchCase])
  

  def interp(expr: Expr, env: Env): Value = ???

}

```



#### Error stacktrace:

```
java.base/java.util.TimSort.mergeLo(TimSort.java:781)
	java.base/java.util.TimSort.mergeAt(TimSort.java:518)
	java.base/java.util.TimSort.mergeForceCollapse(TimSort.java:461)
	java.base/java.util.TimSort.sort(TimSort.java:254)
	java.base/java.util.Arrays.sort(Arrays.java:1233)
	scala.collection.SeqOps.sorted(Seq.scala:728)
	scala.collection.SeqOps.sorted$(Seq.scala:719)
	scala.collection.immutable.List.scala$collection$immutable$StrictOptimizedSeqOps$$super$sorted(List.scala:79)
	scala.collection.immutable.StrictOptimizedSeqOps.sorted(StrictOptimizedSeqOps.scala:75)
	scala.collection.immutable.StrictOptimizedSeqOps.sorted$(StrictOptimizedSeqOps.scala:75)
	scala.collection.immutable.List.sorted(List.scala:79)
	dotty.tools.pc.completions.Completions.completions(Completions.scala:143)
	dotty.tools.pc.completions.CompletionProvider.completions(CompletionProvider.scala:90)
	dotty.tools.pc.ScalaPresentationCompiler.complete$$anonfun$1(ScalaPresentationCompiler.scala:146)
```
#### Short summary: 

java.lang.IllegalArgumentException: Comparison method violates its general contract!