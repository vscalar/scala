file:///C:/Users/user/OneDrive/바탕%20화면/scala/cobalt/src/main/scala/kuplrg/Implementation.scala
### java.lang.IllegalArgumentException: Comparison method violates its general contract!

occurred in the presentation compiler.

presentation compiler configuration:
Scala version: 3.3.3
Classpath:
<HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala3-library_3\3.3.3\scala3-library_3-3.3.3.jar [exists ], <HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala-library\2.13.12\scala-library-2.13.12.jar [exists ]
Options:



action parameters:
offset: 2306
uri: file:///C:/Users/user/OneDrive/바탕%20화면/scala/cobalt/src/main/scala/kuplrg/Implementation.scala
text:
```scala
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
    println()
    println(expr)
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
    case EEq(l, r) => if (interp(l, env) == interp(r, env)) BoolV(true) else BoolV(false)
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
      case ECons(head, _) => interp(head, env)
      case ETail(l) => interp(rawStr(), e@@)
      case ENil => error("empty list")
      case _ => error("not a list")
    // list tail
    case ETail(list: Expr) => list match
      case ECons(_, tail) => interp(tail, env)
      case ENil => error("empty list")
      case _ => error("not a list")
    // list map function
    case EMap(list: Expr, fun: Expr) => interp(fun, env) match
      case CloV(p, b, fenv) => 
        map(interp(list, env), interp(fun, env))
      case _ => error("not a function")
    // list flatMap function
    case EFlatMap(list: Expr, fun: Expr) => NumV(0)
    // list filter function
    case EFilter(list: Expr, fun: Expr) => NumV(0)
    // list foldLeft function
    case EFoldLeft(list: Expr, init: Expr, fun: Expr) => NumV(0)
    // tuple
    case ETuple(exprs: List[Expr]) => 
      TupleV(for{
        e <- exprs
      } yield interp(e, env))
    // tuple projection
    case EProj(tuple: Expr, index: Int) => NumV(0)
    // variable definition
    case EVal(name: String, value: Expr, scope: Expr) => interp(scope, env + (name -> interp(value, env)))
    // lambda function
    case EFun(params: List[String], body: Expr) => CloV(params, body, () => env)
    // mutually recursive function
    case ERec(defs: List[FunDef], scope: Expr) => NumV(0)
    // function application
    case EApp(fun: Expr, args: List[Expr]) => interp(fun, env) match
      case CloV(p, b, fenv) => 
        val argsV: List[Value] = for{
          arg <- args
        } yield interp(arg, fenv())
        app(interp(fun, env), argsV)
      case _ => error("not a function")
      

  def eq(left: Value, right: Value): Boolean = ???

  def map(list: Value, func: Value): Value = list match
    case ConsV(head, ENil) => ConsV(func(head), NilV)
    case ConsV(head, tail) => ConsV(func(head), map(tail, func))
  

  def join(list: Value): Value = ???

  def filter(list: Value, func: Value): Value = ???

  def foldLeft(list: Value, init: Value, func: Value): Value = ???

  def app(func: Value, args: List[Value]): Value = func match
    case CloV(p, b, fenv) if (p.length != args.length) => error("arity mismatch")
    case CloV(p, b, fenv) => 
      val newEnv = for{l
        param <- p
        arg <- args
      } yield (param -> arg)
      interp(b, fenv() ++ newEnv)
    case _ => error("not a function")
  

  // ---------------------------------------------------------------------------
  // Problem #2
  // ---------------------------------------------------------------------------
  def subExpr1: String = ???

  def subExpr2: String = ???
}

```



#### Error stacktrace:

```
java.base/java.util.TimSort.mergeLo(TimSort.java:781)
	java.base/java.util.TimSort.mergeAt(TimSort.java:518)
	java.base/java.util.TimSort.mergeForceCollapse(TimSort.java:461)
	java.base/java.util.TimSort.sort(TimSort.java:254)
	java.base/java.util.Arrays.sort(Arrays.java:1233)
	scala.collection.SeqOps.sorted(Seq.scala:728)
	scala.collection.SeqOps.sorted$(Seq.scala:719)
	scala.collection.immutable.List.scala$collection$immutable$StrictOptimizedSeqOps$$super$sorted(List.scala:79)
	scala.collection.immutable.StrictOptimizedSeqOps.sorted(StrictOptimizedSeqOps.scala:78)
	scala.collection.immutable.StrictOptimizedSeqOps.sorted$(StrictOptimizedSeqOps.scala:78)
	scala.collection.immutable.List.sorted(List.scala:79)
	scala.meta.internal.pc.completions.Completions.completions(Completions.scala:211)
	scala.meta.internal.pc.completions.CompletionProvider.completions(CompletionProvider.scala:89)
	scala.meta.internal.pc.ScalaPresentationCompiler.complete$$anonfun$1(ScalaPresentationCompiler.scala:155)
```
#### Short summary: 

java.lang.IllegalArgumentException: Comparison method violates its general contract!