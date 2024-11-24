file:///C:/Users/user/OneDrive/바탕%20화면/scala/tfae/src/main/scala/kuplrg/Implementation.scala
### java.lang.IllegalArgumentException: Comparison method violates its general contract!

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
offset: 952
uri: file:///C:/Users/user/OneDrive/바탕%20화면/scala/tfae/src/main/scala/kuplrg/Implementation.scala
text:
```scala
package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Type.*

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
    tenv.getOrElse(name, error(s"free identifier: $x"))
  // anonymous (lambda) functions
  case Fun(param: String, ty: Type, body: Expr) =>
    ArrowT(ty, typeCheck(body, tenv@@))
  // function applications
  case App(fun: Expr, arg: Expr)

  def interp(expr: Expr, env: Env): Value = ???

  def mustSame(lty: Type, rty: Type): Unit =
    if (lty != rty) error(s"tyep mismatch: ${lty.str} != ${rty.str}")
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
	scala.collection.immutable.StrictOptimizedSeqOps.sorted(StrictOptimizedSeqOps.scala:75)
	scala.collection.immutable.StrictOptimizedSeqOps.sorted$(StrictOptimizedSeqOps.scala:75)
	scala.collection.immutable.List.sorted(List.scala:79)
	dotty.tools.pc.completions.Completions.completions(Completions.scala:143)
	dotty.tools.pc.completions.CompletionProvider.completions(CompletionProvider.scala:90)
	dotty.tools.pc.ScalaPresentationCompiler.complete$$anonfun$1(ScalaPresentationCompiler.scala:146)
```
#### Short summary: 

java.lang.IllegalArgumentException: Comparison method violates its general contract!