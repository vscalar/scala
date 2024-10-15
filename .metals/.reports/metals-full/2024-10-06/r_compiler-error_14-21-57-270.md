file:///C:/Users/user/OneDrive/바탕%20화면/scala/fvae/src/main/scala/kuplrg/Implementation.scala
### dotty.tools.dotc.core.TypeError$$anon$1: Toplevel definition Value is defined in
  C:/Users/user/OneDrive/바탕 화면/scala/f1vae/src/main/scala/kuplrg/F1VAE.scala
and also in
  C:/Users/user/OneDrive/바탕 화면/scala/fvae/src/main/scala/kuplrg/FVAE.scala
One of these files should be removed from the classpath.

occurred in the presentation compiler.

presentation compiler configuration:
Scala version: 3.3.3
Classpath:
<HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala3-library_3\3.3.3\scala3-library_3-3.3.3.jar [exists ], <HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala-library\2.13.12\scala-library-2.13.12.jar [exists ]
Options:



action parameters:
offset: 1013
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
  
  val numAdd: BOp[Value] = numBOp(_ + _)
  val numMul: BOp[Value] = numBOp(_ * _)
  
  def interp(expr: Expr, env: Env): Value = expr match
    case Num(n) => NumV(n)
    case Add(l,r) => numAdd(interp(l, env), interp(r, env))
    case Mul(l,r) => numMul(interp(l, env), interp(r, env))
    case Val(x, e, b) => interp(b, env + (x -> interp(e, env)))
    case Id(x) => env.getOrElse(x, error("free identifier"))
    case App(f, e) => interp(f, env) match
      case CloV(p, b, fenv) => interp(b, fenv + (p -> interp(e, env)))
      case _ => error("not a function")
    case Fun(p, e) => CloV(p, e, env)

  def interpDS(expr: Expr, env: Env): Value =@@ expr match
    case Num(n) => NumV(n)
    case Add(l,r) => numAdd(interp(l, env), interp(r, env))
    case Mul(l,r) => numMul(interp(l, env), interp(r, env))
    case Val(x, e, b) => interp(b, env + (x -> interp(e, env)))
    case Id(x) => env.getOrElse(x, error("free identifier"))
    case App(f, e) => interp(f, env) match
      case CloV(p, b, fenv) => interp(b, env + (p -> interp(e, env)))
      case _ => error("not a function")
    case Fun(p, e) => CloV(p, e, env)
}

```



#### Error stacktrace:

```

```
#### Short summary: 

dotty.tools.dotc.core.TypeError$$anon$1: Toplevel definition Value is defined in
  C:/Users/user/OneDrive/바탕 화면/scala/f1vae/src/main/scala/kuplrg/F1VAE.scala
and also in
  C:/Users/user/OneDrive/바탕 화면/scala/fvae/src/main/scala/kuplrg/FVAE.scala
One of these files should be removed from the classpath.