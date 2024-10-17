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
  case Id(name: String) => env.getOrElse(name, error("free identifier"))
  // addition
  case Add(left: Expr, right: Expr) => numAdd(interp(left, env), interp(right, env))
  // multiplication
  case Mul(left: Expr, right: Expr) => numMul(interp(left, env), interp(right, env))
  // division
  case Div(left: Expr, right: Expr) => numDiv(interp(left, env), interp(right, env))
  // modulo
  case Mod(left: Expr, right: Expr) => numMod(interp(left, env), interp(right, env))
  // equal-to
  case Eq(left: Expr, right: Expr) => BoolV(eq(interp(left, env), interp(right, env)))
  // less-than
  case Lt(left: Expr, right: Expr) => numLt(interp(left, env), interp(right, env))
  // anonymous (lambda) functions
  case Fun(param: String, body: Expr) => CloV(param, body, () => env)
  // recursive functions
  case Rec(name: String, param: String, body: Expr, scope: Expr) => 
      lazy val newEnv: Env = env + (name -> CloV(param, body, () => newEnv))
      // println()
      // print(newEnv)
      interp(scope, newEnv)
  // function applications
  case App(fun: Expr, arg: Expr) => interp(fun, env) match
      case CloV(p, b, fenv) => 
        val newEnv: Env = fenv() + (p -> interp(arg, env))
        interp(b, newEnv)
      case _ => error("not a function")
  // conditional
  case If(cond: Expr, thenExpr: Expr, elseExpr: Expr) => interp(cond, env) match
      case BoolV(true) => interp(thenExpr, env)
      case BoolV(false) => interp(elseExpr, env)
      case _ => error("not a boolean")
  
  def eq(left: Value, right: Value): Boolean = (left, right) match
    case (NumV(n1), NumV(n2)) => if (n1 == n2) true else false
    case _ => error("invalid operation")
}