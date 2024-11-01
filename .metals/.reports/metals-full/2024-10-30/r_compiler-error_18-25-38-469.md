file:///C:/Users/user/OneDrive/바탕%20화면/scala/fae-cps/src/main/scala/kuplrg/Implementation.scala
### java.lang.IndexOutOfBoundsException: -1

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
offset: 2090
uri: file:///C:/Users/user/OneDrive/바탕%20화면/scala/fae-cps/src/main/scala/kuplrg/Implementation.scala
text:
```scala
package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Cont.*

  type BOp[T] = (T, T) => T
  def numBOp1(op: BOp[BigInt]): BOp[Value] = (_, _) match
    case (NumV(l), NumV(r))=> NumV(op(l, r))
    case _ => error("invalid operation")
  
  val numAdd: BOp[Value] = numBOp1(_ + _)
  val numMul: BOp[Value] = numBOp1(_ * _)

  def interpCPS(expr: Expr, env: Env, k: Value => Value): Value = expr match
  // numbers
  case Num(number: BigInt) => k(NumV(number))
  // additions
  case Add(left: Expr, right: Expr) => 
    interpCPS(left, env, {
      lv => interpCPS(right, env, {
        rv => k(numAdd(lv, rv))
      })
    })
  // multiplications
  case Mul(left: Expr, right: Expr) => 
    interpCPS(left, env, {
      lv => interpCPS(right, env, rv => {
        k(numMul(lv, rv))
      })
    })
  // identifier lookups
  case Id(name: String) => k(env.getOrElse(name, error("free identifier")))
  // anonymous (lambda) functions
  case Fun(param: String, body: Expr) => k(CloV(param, body, env))
  // function applications
  case App(fun: Expr, arg: Expr) => interpCPS(fun, env, fv => fv match
    case CloV(param, body, fenv) => 
      interpCPS(arg, env, av => interpCPS(body, fenv + (param -> av), k))
    case _ => error("not a function")
    ) 
      
  def reduce(cont: Cont, stack: Stack): (Cont, Stack) = (cont, stack) match
    case (EvalK(env, expr, k), s) => expr match
      case Num(number: BigInt) => (k, NumV(number) :: s)
      case Add(left: Expr, right: Expr) => (EvalK(env, left, EvalK(env, right, AddK(k))), s)
      case Mul(left: Expr, right: Expr) => (EvalK(env, left, EvalK(env, right, MulK(k))), s)
      case Id(name: String) => (k, env.getOrElse(name, error("free identifier")) :: s)
      case Fun(param: String, body: Expr) => (k, CloV(param, body, env) :: s)
      case App(fun: Expr, arg: Expr) => (EvalK(env, fun, EvalK(env, arg, AppK(k))), s)

    case (AddK(k), n2 :: n1 :: s) => (k, numAdd(n1 + n2) :: s)
    case (MulK(k), n2 :: n1 :: s) => (k, (n1 * n2@@) :: s)
    case (AppK(k), v2 :: CloV(param, body, env) :: s) => (EvalK(env + (param -> v2), body, k), s)
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