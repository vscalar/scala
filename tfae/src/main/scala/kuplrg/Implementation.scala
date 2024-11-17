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

  def typeCheck(expr: Expr, tenv: TypeEnv): Type = expr match
  // numbers
  case Num(number: BigInt) => NumT
  // additions
  case Add(left: Expr, right: Expr) =>
    mustSame(typeCheck(left, tenv), NumT)
    mustSame(typeCheck(right, tenv), NumT)
    NumT
  // multiplications
  case Mul(left: Expr, right: Expr) =>
    mustSame(typeCheck(left, tenv), NumT)
    mustSame(typeCheck(right, tenv), NumT)
    NumT
  // immutable variable definition
  case Val(name: String, init: Expr, body: Expr) => 
    val newTenv = tenv + (name -> typeCheck(init, tenv))
    typeCheck(body, newTenv)
  // identifier lookups
  case Id(name: String) => 
    tenv.getOrElse(name, error(s"free identifier: $name"))
  // anonymous (lambda) functions
  case Fun(param: String, ty: Type, body: Expr) =>
    val retTy = typeCheck(body, tenv + (param -> ty))
    ArrowT(ty, retTy)
  // function applications
  case App(fun: Expr, arg: Expr) => typeCheck(fun, tenv) match
    case ArrowT(paramTy, retTy) =>
      mustSame(typeCheck(arg, tenv), paramTy)
      retTy
    case ty => error(s"not a function type: ${ty.str}")
  
  def interp(expr: Expr, env: Env): Value = expr match
    // numbers
    case Num(number: BigInt) => NumV(number)
    // identifier lookups
    case Id(name: String) => env.getOrElse(name, error("free identifier"))
    // addition
    case Add(left: Expr, right: Expr) => numAdd(interp(left, env), interp(right, env))
    // multiplication
    case Mul(left: Expr, right: Expr) => numMul(interp(left, env), interp(right, env))
    // immutable variable definition
    case Val(name: String, init: Expr, body: Expr) =>
      val initVal = interp(init, env)
      interp(body, env + (name -> initVal))
    // division
    case Fun(param: String, ty: Type, body: Expr) => CloV(param, body, env)
    // function applications
    case App(fun: Expr, arg: Expr) => interp(fun, env) match
      case CloV(param, body, fenv) =>
        val argVal = interp(arg, env)
        interp(body, fenv + (param -> argVal))
    
      case _ => error()
    

  def mustSame(lty: Type, rty: Type): Unit =
    if (lty != rty) error(s"tyep mismatch: ${lty.str} != ${rty.str}")
}
