file:///C:/Users/user/OneDrive/바탕%20화면/scala/magnet/src/main/scala/kuplrg/Implementation.scala
### java.lang.IndexOutOfBoundsException: -1

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
offset: 2909
uri: file:///C:/Users/user/OneDrive/바탕%20화면/scala/magnet/src/main/scala/kuplrg/Implementation.scala
text:
```scala
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
        case EWhile(cond: Expr, body: Expr) => 
          val kvBreak = KValue(cont, s, mem)
          val kvContinue = KValue(IPop::IEval(env, EWhile(cond, body))::cont, s, h)
          val hBody = h + (Continue -> kvContinue, @@)
          val kvBody
          State(IEval(env,cond)::IJmpIf()::cont,UndefV::s,h,mem)
        case EBreak => State(IJmp(Break)::Nil,UndefV::s,h,mem)
        case EContinue => State(IJmp(Continue)::Nil,UndefV::s,h,mem)
        case EFun(params: List[String], body: Expr) => State(cont, CloV(params, body, env)::s,h,mem)
        case EApp(fun: Expr, args: List[Expr]) =>
          //println()
          //println(args.map(x => IEval(env, x)))
          //println()
          val first = IEval(env,fun)::args.map(x => IEval(env, x))
          val last = ICall(args.length)::cont
          //println(first:::last)
          State(first:::last,s,h,mem)

        case EReturn(expr: Expr) => State(IEval(env, expr)::IReturn::cont,s,h,mem)
        case ETry(body: Expr, catchParam: String, catchExpr: Expr) => State(List(),List(NumV(0)),Map(),Map())//State(IEval(env, expr)::IReturn::cont,s,h,mem)
        case EThrow(expr: Expr) => State(IEval(env, expr)::IJmp(Throw)::k,s,h,mem)
        case EGen(params: List[String], body: Expr) => State(cont,GenV(params, body, env)::s,h,mem)
        case EIterNext(iter: Expr, arg: Option[Expr]) => arg match
          case Some(expr) => State(IEval(env, iter)::IEval(env, expr)::INext::cont,s,h,mem)
          case _ => State(IEval(env, iter)::IEval(env, EUndef)::INext::cont,s,h,mem)
        
        case EYield(expr: Expr) => State(IEval(env, expr)::IYield::Nil,BoolV(false)::ContV(KValue(cont,s,h))::s,h,mem)
        case EValueField(result: Expr) => State(IEval(env,result)::IValueField::cont,s,h,mem)
        case EDoneField(result: Expr) => State(IEval(env,result)::IDoneField::cont,s,h,mem)
        
      case (IAdd::cont,n2::n1::s,h,mem) => State(cont, numAdd(n1, n2)::s, h,mem)
      case (IMul::cont,n2::n1::s,h,mem) => State(cont, numMul(n1, n2)::s, h,mem)
      case (IDiv::cont,n2::n1::s,h,mem) => State(cont, numDiv(n1, n2)::s, h,mem)
      case (IMod::cont,n2::n1::s,h,mem) => State(cont, numMod(n1, n2)::s, h,mem)
      case (IEq::cont,v2::v1::s,h,mem) => State(cont, BoolV(eq(v1, v2))::s, h,mem)
      case (ILt::cont,n2::n1::s,h,mem) => State(cont, numLt(n1, n2)::s, h,mem)
      //case (IDef(xs: List[String], env: Env, body: Expr)::cont, ::s, h,mem)
      case (IWrite(addr: Addr)::cont,v::s,h,mem) => State(cont, v::s, h,mem + (addr -> v))
      case (IPop::cont,v::s,h,mem) => State(cont, s, h,mem)
      case (IJmpIf(KValue(cont,s,h))::_,BoolV(true)::_,_,mem) => State(cont, s, h,mem)
      case (IJmpIf(_)::cont,BoolV(false)::s,h,mem) => State(cont, s, h,mem)
      case (IJmp(c)::cont,v::s,h,mem) => 
        val kv = lookup(h, c)
        kv match
          case KValue(cont1, s1, h1) => 
            val h2 = if (h.contains(Yield)) h1 + (Yield -> lookup(h, Yield)) else h1
            State(cont1,v::s,h2,mem)
        
      case (IReturn::cont,v::s,h,mem) => 
        val kvDone = ContV(KValue(IReturn::Nil, Nil, Map()))
        if (h.contains(Yield)) State(IYield::Nil, v::BoolV(true)::kvDone::s, h, mem) 
        else State(IJmp(Return)::Nil, v::Nil, h, mem)

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