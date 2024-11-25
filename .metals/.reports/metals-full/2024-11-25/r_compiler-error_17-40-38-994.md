file:///C:/Users/user/OneDrive/바탕%20화면/scala/atfae/src/main/scala/kuplrg/Implementation.scala
### java.lang.IndexOutOfBoundsException: -1

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
offset: 6880
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
      ArrowT(ptys.map(mustValid(_, tenv)), mustValid(rty, tenv))
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
      typeCheck(body, tenv.addVar(name, initType))
    // identifier lookups
    case Id(name: String) =>
      tenv.vars.getOrElse(name, error("free identifier"))
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
      typeCheck(scope, tenv.addVar(x, fty))
    // function applications
    case App(fun: Expr, args: List[Expr]) => fun match
      case ArrowT(ptys, rty) =>
        if (ptys.length != args.length) error("arity mismatch")
        (ptys zip args).map((p, a) => mustSame(typeCheck(a, tenv), p))
        rty
      case ty => error(s"not a function type: ${ty.str}")
    // conditional
    case If(cond: Expr, thenExpr: Expr, elseExpr: Expr) =>
      mustSame(BoolT, typeCheck(cond, tenv))
      mustSame(typeCheck(thenExpr, tenv), typeCheck(elseExpr, tenv))
      typeCheck(thenExpr, tenv)
    // algebraic data type
    case TypeDef(name: String, varts: List[Variant], body: Expr) =>
      val newTEnv = tenv.addType(name, varts.map(w => w.name -> w.ptys).toMap)
      for (w <- varts; pty <- w.ptys) mustValid(pty, newTEnv)
      typeCheck(body, newTEnv.addVars(varts.map(w => w.name -> ArrowT(w.ptys, NameT(name)))))
    // pattern matching
    case Match(expr: Expr, mcases: List[MatchCase]) => typeCheck(expr, tenv) match
      case NameT(tn) =>
        val ts = tenv.tys.getOrElse(tn, error(s"unknown type $tn"))
        val xs = mcases.map(_.name).toSet
        if (ts.keySet != xs || xs.size != mcases.length) error("invalid case")
        mcases.map{case MatchCase(x, ps, b) =>
          typeCheck(b, tenv.addVars(ps zip ts(x)))
        }.reduce((lty, rty) => { mustSame(lty, rty); lty})
      case _ => error("not a variant")
    

  

  def interp(expr: Expr, env: Env): Value = expr match
    // numbers
    case Num(number: BigInt) => NumV(number)
    // booleans
    case Bool(bool: Boolean) => BoolV(bool)
    // addition
    case Add(left: Expr, right: Expr) => 
      numAdd(interp(left, env), interp(right, env))
    // multiplication
    case Mul(left: Expr, right: Expr) => 
      numMul(interp(left, env), interp(right, env))
    // division
    case Div(left: Expr, right: Expr) =>
      numDiv(interp(left, env), interp(right, env))
    // modulo
    case Mod(left: Expr, right: Expr) =>
      numMod(interp(left, env), interp(right, env))
    // equal-to
    case Eq(left: Expr, right: Expr) => (interp(left,env) , interp(right,env)) match
      case (NumV(n1), NumV(n2)) =>
        if (n1 == n2) BoolV(true) else BoolV(false)
      case _ => error("invalid operation")
    // less-than
    case Lt(left: Expr, right: Expr) => 
      numLt(interp(left, env), interp(right, env))
    // immutable variable definition
    case Val(name: String, init: Expr, body: Expr) =>
      val initVal = interp(init, env)
      interp(body, env + (name -> initVal))
    // identifier lookups
    case Id(name: String) =>
      env.getOrElse(name, error("free identifier"))
    // anonymous (lambda) functions
    case Fun(params: List[Param], body: Expr) =>
      CloV(params.map(_.name), body, () => env)
    // recursive functions
    case Rec(x: String, params: List[Param], rty: Type, body: Expr, scope: Expr) =>
      val funMapping = (x -> CloV(params.map(_.name), body, () => env))
      interp(scope, env + funMapping)
    // function applications
    case App(fun: Expr, args: List[Expr]) => fun match
      case CloV(param, body, fenv) => 
        val argVal = args.map(interp(_, env))
        val paramMap = (param zip argVal).toMap
        interp(body, fenv(@@) + paramMap)
      case ConstrV(name) => VariantV(name, args.map(interp(_, env)))
      case v => error(s"not a function: ${v.str}")
    // conditional
    case If(cond: Expr, thenExpr: Expr, elseExpr: Expr) => interp(cond, env) match
      case BoolV(true) =>
        interp(thenExpr, env)
      case BoolV(false) =>
        interp(elseExpr, env)
      case _ => error()
    // algebraic data type
    case TypeDef(name: String, varts: List[Variant], body: Expr) =>
      interp(body, env ++ varts.map(w => w.name -> ConstrV(w.name)))
    // pattern matching
    case Match(expr: Expr, mcases: List[MatchCase]) => interp(expr, env) match
      case VariantV(wname, vs) => mcases.find(_.name == wname) match
        case Some(MatchCase(_, ps, b)) =>
          if (ps.length != vs.length) error("arity mismatch")
          interp(b, env ++ (ps zip vs))
        case None => error(s"no such case: $wname")
      case v => error(s"not a variant: ${v.str}")
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