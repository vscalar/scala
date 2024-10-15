file:///C:/Users/user/OneDrive/바탕%20화면/scala/fvae/src/main/scala/kuplrg/Implementation.scala
### java.lang.AssertionError: NoDenotation.owner

occurred in the presentation compiler.

presentation compiler configuration:
Scala version: 3.3.3
Classpath:
<HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala3-library_3\3.3.3\scala3-library_3-3.3.3.jar [exists ], <HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala-library\2.13.12\scala-library-2.13.12.jar [exists ]
Options:



action parameters:
offset: 367
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
  
  val numAdd: Bop[@@]
  
  def interp(expr: Expr, env: Env): Value = expr match
    case Num(n) => n
    case Add(l,r) => interp(l, env) + interp(r, env = )
    case Mul(l,r) => interp(l, env) * interp(r, env)
    case Val(x, e, b) => interp(b, env + (x -> interp(e, env)))
    case Id(x) => env.getOrElse(x, error("free identifier"))
    case App(f, e) => 
    case Fun(p, e)

  def interpDS(expr: Expr, env: Env): Value = ???
}

```



#### Error stacktrace:

```
dotty.tools.dotc.core.SymDenotations$NoDenotation$.owner(SymDenotations.scala:2607)
	scala.meta.internal.pc.SignatureHelpProvider$.isValid(SignatureHelpProvider.scala:83)
	scala.meta.internal.pc.SignatureHelpProvider$.notCurrentApply(SignatureHelpProvider.scala:94)
	scala.meta.internal.pc.SignatureHelpProvider$.$anonfun$1(SignatureHelpProvider.scala:48)
	scala.collection.StrictOptimizedLinearSeqOps.dropWhile(LinearSeq.scala:280)
	scala.collection.StrictOptimizedLinearSeqOps.dropWhile$(LinearSeq.scala:278)
	scala.collection.immutable.List.dropWhile(List.scala:79)
	scala.meta.internal.pc.SignatureHelpProvider$.signatureHelp(SignatureHelpProvider.scala:48)
	scala.meta.internal.pc.ScalaPresentationCompiler.signatureHelp$$anonfun$1(ScalaPresentationCompiler.scala:435)
```
#### Short summary: 

java.lang.AssertionError: NoDenotation.owner