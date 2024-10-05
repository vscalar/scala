file:///C:/Users/user/OneDrive/바탕%20화면/scala/ae/src/main/scala/kuplrg/Implementation.scala
### dotty.tools.dotc.core.TypeError$$anon$1: Toplevel definition Value is defined in
  C:/Users/user/OneDrive/바탕 화면/scala/vae/src/main/scala/kuplrg/VAE.scala
and also in
  C:/Users/user/OneDrive/바탕 화면/scala/ae/src/main/scala/kuplrg/AE.scala
One of these files should be removed from the classpath.

occurred in the presentation compiler.

presentation compiler configuration:
Scala version: 3.3.3
Classpath:
<HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala3-library_3\3.3.3\scala3-library_3-3.3.3.jar [exists ], <HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala-library\2.13.12\scala-library-2.13.12.jar [exists ]
Options:



action parameters:
offset: 116
uri: file:///C:/Users/user/OneDrive/바탕%20화면/scala/ae/src/main/scala/kuplrg/Implementation.scala
text:
```scala
package kuplrg

object Implementation extends Template {

  import Expr.*

  def interp(expr: Expr): Value = e@@

  def countNums(expr: Expr): Int = ???
}

```



#### Error stacktrace:

```

```
#### Short summary: 

dotty.tools.dotc.core.TypeError$$anon$1: Toplevel definition Value is defined in
  C:/Users/user/OneDrive/바탕 화면/scala/vae/src/main/scala/kuplrg/VAE.scala
and also in
  C:/Users/user/OneDrive/바탕 화면/scala/ae/src/main/scala/kuplrg/AE.scala
One of these files should be removed from the classpath.