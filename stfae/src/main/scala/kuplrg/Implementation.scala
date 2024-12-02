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

  def typeCheck(expr: Expr, tenv: TypeEnv): Type = 
    expr match
    // numbers
    case Num(number: BigInt) =>
      NumT
    // additions
    case Add(left: Expr, right: Expr) =>
      subtypeCheck(typeCheck(left, tenv), NumT)
      subtypeCheck(typeCheck(right, tenv), NumT)
      NumT
    // multiplications
    case Mul(left: Expr, right: Expr) =>
      subtypeCheck(typeCheck(left, tenv), NumT)
      subtypeCheck(typeCheck(right, tenv), NumT)
      NumT
    // immutable variable definition
    case Val(name: String, tyOpt: Option[Type], init: Expr, body: Expr) => tyOpt match
      case Some(ty) => 
        subtypeCheck(typeCheck(init, tenv), ty)
        typeCheck(body, tenv + (name -> ty))
      case None =>
        typeCheck(body, tenv + (name -> typeCheck(init, tenv)))
    // identifier lookups
    case Id(name: String) =>
      tenv.getOrElse(name, error("free identifier"))
    // anonymous (lambda) functions
    case Fun(param: String, ty: Type, body: Expr) =>
      ArrowT(ty, typeCheck(body, tenv + (param -> ty)))
    // function applications
    case App(fun: Expr, arg: Expr) => typeCheck(fun, tenv) match
      case ArrowT(pty, rty) =>
        subtypeCheck(typeCheck(arg, tenv), pty)
        rty
      case ty => error(s"not a function type: ${ty.str}")
    // records
    case Record(fields: List[(String, Expr)]) =>
      RecordT(fields.map{case(field, expr) => (field, typeCheck(expr, tenv))}.toMap)
    // field lookups
    case Access(record: Expr, field: String) => typeCheck(record, tenv) match
      case RecordT(fields) => fields.getOrElse(field, error("field not found"))
      case ty => error("not a record type")
    // exit
    case Exit => BotT

  def interp(expr: Expr, env: Env): Value = 
    expr match
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
    case Val(name: String, tyOpt: Option[Type], init: Expr, body: Expr) => 
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
        val argVal = interp(arg, env)
        interp(body, fenv + (param -> argVal))
      case _ => error(s"${fun.str} is not a function")
    // records
    case Record(fields: List[(String, Expr)]) =>
      RecordV(fields.map{case(field, expr) => (field, interp(expr, env))}.toMap)
    // field lookups
    case Access(record: Expr, field: String) => interp(record, env) match
      case RecordV(fields) =>
        fields.getOrElse(field, error("no field"))
      case v => error("not a record")
    // exit
    case Exit => error("exit")

  def subtype(lty: Type, rty: Type): Boolean = (lty, rty) match
    case (_, TopT) => true
    case (BotT, _) => true
    case (NumT, NumT) => true
    case (ArrowT(paramTy1, retTy1), ArrowT(paramTy2, retTy2)) =>
      subtype(paramTy2, paramTy1) && subtype(retTy1, retTy2)
    case (RecordT(fields1), RecordT(fields2)) =>
      fields2.forall{
        case (name, ty2) => fields1.get(name) match 
          case None => false
          case Some(ty1) => subtype(ty1, ty2)
      }
    case (_,_) => false
  
  def subtypeCheck(lty: Type, rty: Type): Type = subtype(lty: Type, rty: Type) match
    case true => rty
    case false => error("wrong type")
}