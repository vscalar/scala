package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Inst.*
  import Control.*

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
    case (NumV(l), NumV(r)) if (NumV(r) != NumV(0))=> NumV(op(l, r))
    case _ => error("invalid operation")
  
  val numDiv: BOp[Value] = numBOp2(_ / _)
  val numMod: BOp[Value] = numBOp2(_ % _)

  type COp[T] = (T, T) => Boolean
  def numCOp(op: COp[BigInt]): BOp[Value] =
    case (NumV(l), NumV(r)) => BoolV(op(l, r))
    case _ => error("invalid operation")
  
  val numLt: BOp[Value] = numCOp(_ < _)

  def reduce(st: State): State =
    val State(k, s, h, mem) = st
    (k, s, h, mem) match
      case (IEval(env: Env, expr: Expr) :: cont, s, h, mem) => expr match
        case EUndef => State(cont, UndefV::s,h,mem)
        case ENum(number: BigInt)=> State(cont, NumV(number)::s,h,mem)
        case EBool(bool: Boolean) => State(cont, BoolV(bool)::s,h,mem)
        case EAdd(left: Expr, right: Expr) => State(IEval(env,left)::IEval(env,right)::IAdd::cont,s,h,mem)
        case EMul(left: Expr, right: Expr) => State(IEval(env,left)::IEval(env,right)::IMul::cont,s,h,mem)
        case EDiv(left: Expr, right: Expr) => State(IEval(env,left)::IEval(env,right)::IDiv::cont,s,h,mem)
        case EMod(left: Expr, right: Expr) => State(IEval(env,left)::IEval(env,right)::IMod::cont,s,h,mem)
        case EEq(left: Expr, right: Expr) => State(IEval(env,left)::IEval(env,right)::IEq::cont,s,h,mem)
        case ELt(left: Expr, right: Expr) => State(IEval(env,left)::IEval(env,right)::ILt::cont,s,h,mem)
        case EVar(name: String, init: Expr, body: Expr) => State(IEval(env, init)::IDef(List(name), env, body)::cont,s,h,mem)
        case EId(name: String) => State(cont,mem(lookup(env, name))::s,h,mem)
        case EAssign(name: String, expr: Expr) => State(IEval(env, expr)::IWrite(lookup(env, name))::cont,s,h,mem)
        case ESeq(left: Expr, right: Expr) => State(IEval(env,left)::IPop::IEval(env,right)::cont,s,h,mem)
        case EIf(cond: Expr, thenExpr: Expr, elseExpr: Expr) => State(IEval(env,cond)::IJmpIf(KValue(IEval(env, thenExpr)::cont, s, h))::IEval(env,elseExpr)::cont,s,h,mem)
        case _ => State(List(),List(NumV(0)),Map(),Map())
        
      case (IAdd :: cont,n2::n1::s,h,mem) => State(cont, numAdd(n1, n2)::s, h,mem)
      case (IMul :: cont,n2::n1::s,h,mem) => State(cont, numMul(n1, n2)::s, h,mem)
      case (IDiv :: cont,n2::n1::s,h,mem) => State(cont, numDiv(n1, n2)::s, h,mem)
      case (IMod :: cont,n2::n1::s,h,mem) => State(cont, numMod(n1, n2)::s, h,mem)
      case (IEq :: cont,v2::v1::s,h,mem) => State(cont, BoolV(eq(v1, v2))::s, h,mem)
      case (ILt :: cont,n2::n1::s,h,mem) => State(cont, numLt(n1, n2)::s, h,mem)
      
      case _ => State(List(),List(NumV(0)),Map(),Map())

  // ---------------------------------------------------------------------------
  // Problem #2
  // ---------------------------------------------------------------------------
  def bodyOfSquares: String = ???

  // ---------------------------------------------------------------------------
  // Helper functions
  // ---------------------------------------------------------------------------
  def malloc(mem: Mem, n: Int): List[Addr] =
    val a = malloc(mem)
    (0 until n).toList.map(a + _)

  def malloc(mem: Mem): Addr = mem.keySet.maxOption.fold(0)(_ + 1)

  def lookup(env: Env, x: String): Addr =
    env.getOrElse(x, error(s"free identifier: $x"))

  def lookup(handler: Handler, x: Control): KValue =
    handler.getOrElse(x, error(s"invalid control operation: $x"))

  def eq(l: Value, r: Value): Boolean = (l, r) match
    case (UndefV, UndefV)                   => true
    case (NumV(l), NumV(r))                 => l == r
    case (BoolV(l), BoolV(r))               => l == r
    case (IterV(l), IterV(r))               => l == r
    case (ResultV(lv, ld), ResultV(rv, rd)) => eq(lv, rv) && ld == rd
    case _                                  => false
}
