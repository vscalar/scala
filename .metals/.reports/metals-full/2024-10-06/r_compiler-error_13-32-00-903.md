file:///C:/Users/user/OneDrive/바탕%20화면/scala/fvae/src/main/scala/kuplrg/Implementation.scala
### java.lang.IllegalArgumentException: Comparison method violates its general contract!

occurred in the presentation compiler.

presentation compiler configuration:
Scala version: 3.3.3
Classpath:
<HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala3-library_3\3.3.3\scala3-library_3-3.3.3.jar [exists ], <HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala-library\2.13.12\scala-library-2.13.12.jar [exists ]
Options:



action parameters:
offset: 798
uri: file:///C:/Users/user/OneDrive/바탕%20화면/scala/fvae/src/main/scala/kuplrg/Implementation.scala
text:
```scala
package kuplrg

import javax.swing.text.html.HTML.Tag
import java.math.BigInteger

object Implementation extends Template {

  import Expr.*
  import Value.*

  type BOp[T] = (T, T) => T
  def numBOp(op: BOp[BigInt]): BOp[Value] = (_, _) match
    case (NumV(l), NumV(r)) => NumV(op(l, r))
    case _ => error("invalid operation")
  
  val numAdd: Bop[Value] = numBOp(_ + _)
  val numMul: BOp[Value] = numBOp(_ * _)
  
  def interp(expr: Expr, env: Env): Value = expr match
    case Num(n) => n
    case Add(l,r) => numAdd(interp(l, env), interp(r, env))
    case Mul(l,r) => numMul(interp(l, env), interp(r, env))
    case Val(x, e, b) => interp(b, env + (x -> interp(e, env)))
    case Id(x) => env.getOrElse(x, error("free identifier"))
    case App(f, e) => interp(f, en@@)
    case Fun(p, e) =>

  def interpDS(expr: Expr, env: Env): Value = ???
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