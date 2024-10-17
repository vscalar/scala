package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*

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

  def interp(expr: Expr, env: Env): Value = 
    // println()
    // println(expr)
    // println(env.keys)
    expr match
    // numbers
    case ENum(n) => NumV(n)
    // booleans
    case EBool(b) => BoolV(b)
    // identifier lookups
    case EId(x) => env.getOrElse(x, error("free identifier"))
    // addition
    case EAdd(l, r) => numAdd(interp(l, env), interp(r, env))
    // multiplication
    case EMul(l, r) => numMul(interp(l, env), interp(r, env))
    // division
    case EDiv(l, r) => numDiv(interp(l, env), interp(r, env))
    // modulo
    case EMod(l, r) => numMod(interp(l, env), interp(r, env))
    // equal-to
    case EEq(l, r) => BoolV(eq(interp(l, env), interp(r, env)))
    // less-than
    case ELt(l, r) => numLt(interp(l, env), interp(r, env))
    // conditional
    case EIf(c, t, e) => interp(c, env) match
      case BoolV(true) => interp(t, env)
      case BoolV(false) => interp(e, env)
      case _ => error("not a boolean")
    // empty list
    case ENil => NilV
    // list cons
    case ECons(head: Expr, tail: Expr) => ConsV(interp(head, env), interp(tail, env))
    // list head
    case EHead(list: Expr) => list match
      case EId(x) => interp(EId(x), env) match
        case ConsV(head, tail) => head
        case _ => error("not a list")
      case ECons(head, _) => interp(head, env)
      case ETail(l) => interp(EHead(tails(l)), env)
      case ENil => error("empty list")
      case _ => error("not a list")
    // list tail
    case ETail(list: Expr) => list match
      case EId(x) => interp(EId(x), env) match
        case ConsV(head, tail) => tail
        case _ => error("not a list")
      case ECons(_, tail) => interp(tail, env)
      case ENil => error("empty list")
      case _ => error("not a list")
    // list map function
    case EMap(list: Expr, fun: Expr) => interp(fun, env) match
      case CloV(p, b, fenv) => 
        map(interp(list, env), interp(fun, env))
      case _ => error("not a function")
    // list flatMap function
    case EFlatMap(list: Expr, fun: Expr) => interp(fun, env) match
      case CloV(p, b, fenv) => 
        join(map(interp(list, env), interp(fun, env)))
      case _ => error("not a function")
    // list filter function
    case EFilter(list: Expr, fun: Expr) => interp(fun, env) match
      case CloV(p, b, fenv) => 
        filter(interp(list, env), interp(fun, env))
      case _ => error("not a function")
    // list foldLeft function
    case EFoldLeft(list: Expr, init: Expr, fun: Expr) => interp(fun, env) match
      case CloV(p, b, fenv) => 
        foldLeft(interp(list, env), interp(init, env), interp(fun, env))
      case _ => error("not a function")
    // tuple
    case ETuple(exprs: List[Expr]) => 
      TupleV(for{
        e <- exprs
      } yield interp(e, env))
    // tuple projection
    
    case EProj(tuple: Expr, index: Int) => tuple match
      case ETuple(exprs) => 
        if (exprs.length < index) error("out of bounds")
        else interp(exprs(index-1), env)
      //case EId(x) => interp(EProj(env.getOrElse(x, error("free identifier")), index), env)
      case _ => error("not a tuple")
    
    // variable definition
    case EVal(name: String, value: Expr, scope: Expr) => interp(scope, env + (name -> interp(value, env)))
    // lambda function
    case EFun(params: List[String], body: Expr) => CloV(params, body, () => env)
    // mutually recursive function
    case ERec(defs: List[FunDef], scope: Expr) => 
      lazy val newEnv: Env = recEnv(defs, env)
      // println()
      // print(newEnv)
      interp(scope, newEnv)
    // function application
    case EApp(fun: Expr, args: List[Expr]) => interp(fun, env) match
      case CloV(p, b, fenv) => 
        val argsV: List[Value] = for{
          arg <- args
        } yield interp(arg, env)
        app(interp(fun, env), argsV)
      case _ => error("not a function")
      

  def eq(left: Value, right: Value): Boolean = (left, right) match
    case (NumV(n1), NumV(n2)) => if (n1 == n2) true else false
    case (BoolV(b1), BoolV(b2)) => if (b1 == b2) true else false
    case (NilV, NilV) => true
    case (NilV, ConsV(_,_)) => false
    case (ConsV(_,_), NilV) => false
    case (ConsV(h1,t1), ConsV(h2,t2)) => eq(h1, h2) && eq(t1, t2)
    case _ => false

  def map(list: Value, func: Value): Value = list match
    case NilV => NilV
    case ConsV(head, tail) => ConsV(app(func, List(head)), map(tail, func))
    case _ => error("not a list")
  
  def join(list: Value): Value = list match
    case NilV => NilV
    case ConsV(NilV, tail) => join(tail)
    case ConsV(ConsV(h1,t1), t2) => ConsV(h1, join(ConsV(t1, t2)))
    case _ => error("not a list")

  def filter(list: Value, func: Value): Value = list match
    case ConsV(head, tail) => app(func, List(head)) match
      case BoolV(true) => ConsV(head, filter(tail, func))
      case BoolV(false) => filter(tail, func)
      case _ => error("not a boolean")
    case NilV => NilV
    case _ => error("not a list")

  def foldLeft(list: Value, init: Value, func: Value): Value = func match
    case CloV(p, b, fenv) => list match
      case NilV => init
      case ConsV(head, tail) => 
        val newInit: Value = app(func, List(init, head))
        foldLeft(tail, newInit, func)
      case _ => error("not a list")
    case _ => error("not a function")
  
  def recEnv(defs: List[FunDef], env: Env): Env = 
    lazy val newEnv: Env = defs.foldLeft(env){
      (a, b) => b match
        case FunDef(name, params, body) =>
          a + (name -> CloV(params, body, () => newEnv))
    }
    newEnv

  def app(func: Value, args: List[Value]): Value = func match
    case CloV(p, b, fenv) if (p.length != args.length) => error("arity mismatch")
    case CloV(p, b, fenv) => 
      val newEnv = for{
        mapping <- p.zip(args)
      } yield (mapping(0) -> mapping(1))
      // println()
      // println("fenv")
      // println((fenv()).keys.toList)
      // println((fenv() ++ newEnv).keys.toList)
      interp(b, fenv() ++ newEnv)
    case _ => error("not a function")
  
    def tails(list: Expr): Expr = list match
      case ECons(head, tail) => tail
      case ETail(l) => tails(tails(l))
      case _ => error("not a list")
    

  // ---------------------------------------------------------------------------
  // Problem #2
  // ---------------------------------------------------------------------------
  def subExpr1: String = """
  y <- lists.flatMap(x => x);
"""
  def subExpr2: String = """
  if (pred(y) == true) y * y else 0
  """
}
