file:///C:/Users/user/OneDrive/바탕%20화면/scala/fae-cps/src/main/scala/kuplrg/Implementation.scala
### java.lang.IllegalArgumentException: Comparison method violates its general contract!

occurred in the presentation compiler.

presentation compiler configuration:
Scala version: 3.3.3
Classpath:
<HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala3-library_3\3.3.3\scala3-library_3-3.3.3.jar [exists ], <HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala-library\2.13.12\scala-library-2.13.12.jar [exists ]
Options:



action parameters:
offset: 361
uri: file:///C:/Users/user/OneDrive/바탕%20화면/scala/fae-cps/src/main/scala/kuplrg/Implementation.scala
text:
```scala
package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Cont.*

  def interpCPS(expr: Expr, env: Env, k: Value => Value): Value = expr match
  // numbers
  case Num(number: BigInt) => k(NumV(number))
  // additions
  case Add(left: Expr, right: Expr) => 
    interpCPS(left, env, lv => interpCPS(right, e@@)
      )
  // multiplications
  case Mul(left: Expr, right: Expr)
  // identifier lookups
  case Id(name: String)
  // anonymous (lambda) functions
  case Fun(param: String, body: Expr)
  // function applications
  case App(fun: Expr, arg: Expr)
  

  def reduce(k: Cont, s: Stack): (Cont, Stack) = ???

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