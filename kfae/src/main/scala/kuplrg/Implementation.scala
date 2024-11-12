package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Cont.*

  type BOp[T] = (T, T) => T
  def numBOp1(op: BOp[BigInt]): BOp[Value] = (_, _) match
    case (NumV(l), NumV(r))=> NumV(op(l, r))
    case _ => error("invalid operation")
  
  val numAdd: BOp[Value] = numBOp1(_ + _)
  val numMul: BOp[Value] = numBOp1(_ * _)

  def reduce(cont: Cont, stack: Stack): (Cont, Stack) = 
    (cont, stack) match
    case (EvalK(env, expr, k), s) => expr match
      case Num(number: BigInt) => (k, NumV(number) :: s)
      case Add(left: Expr, right: Expr) => (EvalK(env, left, EvalK(env, right, AddK(k))), s)
      case Mul(left: Expr, right: Expr) => (EvalK(env, left, EvalK(env, right, MulK(k))), s)
      case Id(name: String) => (k, env.getOrElse(name, error("free identifier")) :: s)
      case Fun(param: String, body: Expr) => (k, CloV(param, body, env) :: s)
      case App(fun: Expr, arg: Expr) => (EvalK(env, fun, EvalK(env, arg, AppK(k))), s)
      case Vcc(name: String, body: Expr) => (EvalK(env + (name -> ContV(k, s)), body, k), s)

    case (AddK(k), n2 :: n1 :: s) => (k, numAdd(n1, n2) :: s)
    case (MulK(k), n2 :: n1 :: s) => (k, numMul(n1, n2) :: s)
    case (AppK(k), v2 :: f :: s) => f match
      case CloV(param, body, env) => (EvalK(env + (param -> v2), body, k), s)
      case ContV(k0, s0) => (k0, v2 :: s0)
      case _ => error("not a function")
    case _ => error("invalid operation")

}
