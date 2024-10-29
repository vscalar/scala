file:///C:/Users/user/OneDrive/바탕%20화면/scala/bfae/src/main/scala/kuplrg/Implementation.scala
### dotty.tools.dotc.core.TypeError$$anon$1: Toplevel definition Env is defined in
  C:/Users/user/OneDrive/바탕 화면/scala/rfae/src/main/scala/kuplrg/RFAE.scala
and also in
  C:/Users/user/OneDrive/바탕 화면/scala/cobalt/src/main/scala/kuplrg/COBALT.scala
One of these files should be removed from the classpath.

occurred in the presentation compiler.

presentation compiler configuration:
Scala version: 3.3.3
Classpath:
<HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala3-library_3\3.3.3\scala3-library_3-3.3.3.jar [exists ], <HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala-library\2.13.12\scala-library-2.13.12.jar [exists ]
Options:



action parameters:
offset: 161
uri: file:///C:/Users/user/OneDrive/바탕%20화면/scala/bfae/src/main/scala/kuplrg/Implementation.scala
text:
```scala
package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*

  def interp(expr: Expr, env: Env, mem: Mem): (Value, Mem) = e@@
      // number
  case Num(number: BigInt)
  // addition
  case Add(left: Expr, right: Expr)
  // multiplication
  case Mul(left: Expr, right: Expr)
  // identifier lookup
  case Id(name: String)
  // anonymous (lambda) function
  case Fun(param: String, body: Expr)
  // function application
  case App(fun: Expr, arg: Expr)
  // box creation
  case NewBox(content: Expr)
  // box content getter
  case GetBox(box: Expr)
  // box content setter
  case SetBox(box: Expr, content: Expr)
  // sequence
  case Seq(left: Expr, right: Expr)
}

```



#### Error stacktrace:

```

```
#### Short summary: 

dotty.tools.dotc.core.TypeError$$anon$1: Toplevel definition Env is defined in
  C:/Users/user/OneDrive/바탕 화면/scala/rfae/src/main/scala/kuplrg/RFAE.scala
and also in
  C:/Users/user/OneDrive/바탕 화면/scala/cobalt/src/main/scala/kuplrg/COBALT.scala
One of these files should be removed from the classpath.