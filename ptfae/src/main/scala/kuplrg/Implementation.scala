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

  def mustSame(lty: Type, rty: Type): Unit =
    if(!isSame(lty,rty)) error(s"type mismatch: ${lty.str} != ${rty.str}")

  def mustValid(ty: Type, tenv: TypeEnv): Type = ty match
    case NumT => NumT
    case ArrowT(pty, rty) => 
      ArrowT(mustValid(pty, tenv), mustValid(rty, tenv))
    case VarT(name) =>
      if (!tenv.tys.contains(name)) error(s"unknown type $name")
      VarT(name)
    case PolyT(name, ty) =>
      PolyT(name, mustValid(ty, tenv.addType(name)))

  def isSame(lty : Type, rty : Type) : Boolean = (lty, rty) match{
    case (NumT, NumT) => true
    case (ArrowT(lpty, lrty),ArrowT(rpty, rrty))=>{
      isSame(lpty, rpty) && isSame(lrty, rrty)
    }
    case (VarT(lname),VarT(rname))=>lname == rname
    case(PolyT(lname, lty),PolyT(rname, rty))=>{
      isSame(lty, subst(rty, rname, VarT(lname)))
    }
    case _ => false
  }

  def subst(bodyTy:Type, name:String,ty:Type):Type =bodyTy match{
    case NumT => NumT
    case ArrowT(pty, rty) => ArrowT(subst(pty,name, ty),subst(rty,name,ty))
    case VarT(x)=>{
      if(name == x) ty 
      else VarT(x)
    }
    case PolyT(x,bodyTy)=>{
      if(name == x) PolyT(x,bodyTy)
      else PolyT(x,subst(bodyTy,name, ty))
    }
  }

  def typeCheck(expr: Expr, tenv: TypeEnv): Type = expr match
    // numbers
    case Num(number: BigInt) =>
      NumT
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
      val newTEnv = tenv.addVar(name, typeCheck(init, tenv))
      typeCheck(body, newTEnv)
    // identifier lookups
    case Id(name: String) =>
      tenv.vars.getOrElse(name, error("free identifier"))
    // anonymous (lambda) functions
    case Fun(param: String, ty: Type, body: Expr) =>
      mustValid(ty, tenv)
      val bodyTy = typeCheck(body, tenv.addVar(param, ty))
      ArrowT(ty, bodyTy)
    // function applications
    case App(fun: Expr, arg: Expr) => typeCheck(fun, tenv) match
      case ArrowT(pty, rty) =>
        mustSame(pty, typeCheck(arg, tenv))
        rty
      case ty => error(s"not a function type: ${ty.str}")
    // type abstraction 
    case TypeAbs(name: String, body: Expr) =>
      if(tenv.tys.contains(name)) error(s"already defined type: $name")
      PolyT(name, typeCheck(body, tenv.addType(name)))
    // type application
    case TypeApp(expr: Expr, ty: Type) => typeCheck(expr, tenv) match
      case PolyT(name, bodyTy) =>
        subst(bodyTy, name, mustValid(ty, tenv))
      case t => error(s"not a polymorphic type: ${t.str}")
      
  def interp(expr: Expr, env: Env): Value = expr match
      // numbers
    case Num(number: BigInt) =>
      NumV(number)
    // additions
    case Add(left: Expr, right: Expr) =>
      numAdd(interp(left, env), interp(right, env))
    // multiplications
    case Mul(left: Expr, right: Expr) =>
      numMul(interp(left, env), interp(right, env))
    // immutable variable definition
    case Val(name: String, init: Expr, body: Expr) =>
      val initVal = interp(init, env)
      interp(body, env + (name -> initVal))
    // identifier lookups
    case Id(name: String) =>
      env.getOrElse(name, error("free identifier"))
    // anonymous (lambda) functions
    case Fun(param: String, ty: Type, body: Expr) =>
      CloV(param, body, env)
    // function applications
    case App(fun: Expr, arg: Expr) => interp(fun, env) match
      case CloV(param, body, fenv) =>
        interp(body, fenv + (param -> interp(arg, env)))
      case _ => error("not a function")
    // type abstraction
    case TypeAbs(name: String, body: Expr) =>
      TypeAbsV(name, body, env)
    // type application
    case TypeApp(expr: Expr, ty: Type) => interp(expr, env) match
      case TypeAbsV(name, body, fenv) => 
        interp(body, fenv)
      case v => error(s"not a type abstraction: ${v.str}")
}
