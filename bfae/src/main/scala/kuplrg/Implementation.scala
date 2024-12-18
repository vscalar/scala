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

  def interp(expr: Expr, env: Env, mem: Mem): (Value, Mem) = expr match
      // number
  case Num(number: BigInt) => (NumV(number), mem)
  // addition
  case Add(left: Expr, right: Expr) =>
    val (lv, lmem) = interp(left, env, mem)
    val (rv, rmem) = interp(right, env, lmem)
    (numAdd(lv, rv), rmem)
  // multiplication
  case Mul(left: Expr, right: Expr) =>
    val (lv, lmem) = interp(left, env, mem)
    val (rv, rmem) = interp(right, env, lmem)
    (numMul(lv, rv), rmem)
  // identifier lookup
  case Id(name: String) => env.getOrElse(x, error("free identifier"))
  // anonymous (lambda) function
  case Fun(param: String, body: Expr) => (CloV(params, body, env), mem)
  // function application
  case App(fun: Expr, arg: Expr) => interp(fun, env, mem) match
    case (CloV(params, body, env), mem1) => 
      val (newEnv, mem2): (Env, Mem)= interp(arg, env, mem1)
      interp(body, env + newEnv, mem2)
    case _ => error("not a function")
  // box creation
  case NewBox(content: Expr)
  // box content getter
  case GetBox(box: Expr)
  // box content setter
  case SetBox(box: Expr, content: Expr)
  // sequence
  case Seq(left: Expr, right: Expr)
}
