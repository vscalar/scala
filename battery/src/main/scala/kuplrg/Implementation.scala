package kuplrg

object Implementation extends Template {

  import Expr.*
  import RecDef.*
  import Value.*
  import Type.*
  import TypeInfo.*

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

  type SOp[T] = (T, T) => T
  def strSOp(op: SOp[String]): SOp[Value] =
    case (StrV(l), StrV(r)) => StrV(op(l, r))
    case _ => error("invalid operation")

  val strCon: SOp[Value] = strSOp(_++_)

  def mustSame(lty: Type, rty: Type): Unit =
    if(!isSame(lty,rty)) error(s"type mismatch: ${lty.str} != ${rty.str}")
    // else println(s"type match: ${lty.str} == ${rty.str}")

  def mustValid(ty: Type, tenv: TypeEnv): Type = 
    // println()
    // println(ty)
    // println()
    // println(tenv)
    ty match
    // unit types
    case UnitT => UnitT
    // number types
    case NumT => NumT
    // boolean types
    case BoolT => BoolT
    // string types
    case StrT => StrT
    // parameterized type identifiers
    case IdT(name: String, tys: List[Type]) => tenv.tys.get(name) match
      case Some(TIAdt(_, _)) =>
        tys.map(mustValid(_, tenv))
        IdT(name, tys)
      case Some(TIVar) => IdT(name, tys)
      case _ => error("idt")
    // function (arrow) types
    case ArrowT(tvars: List[String], paramTys: List[Type], retTy: Type) =>
      val newTEnv: TypeEnv = tenv.addTypeVars(tvars)
      for (paramTy <- paramTys) mustValid(paramTy, newTEnv)
      mustValid(retTy, newTEnv)
      ArrowT(tvars, paramTys, retTy)

  def isSame(lty : Type, rty : Type) : Boolean = 
    println(lty)
    println(rty)
    println
    (lty, rty) match
    case (UnitT, UnitT) => true
    case (NumT, NumT) => true
    case (BoolT, BoolT) => true
    case (StrT, StrT) => true
    case (IdT(name1, tys1), IdT(name2, tys2)) => (tys1, tys2) match
      case (Nil, Nil) => true
      case (head1::types1, head2::types2) =>
        if (tys1.length == tys2.length) (tys1 zip tys2).map((ty1, ty2) => isSame(ty1, ty2)).foldLeft(true)(_&&_)
        else false
      case _ => false
    case (ArrowT(tvars1, paramTys1, retTy1), ArrowT(tvars2, paramTys2, retTy2)) =>
      if (tvars1.length == tvars2.length && paramTys1.length == paramTys2.length)
        val mapping = (tvars1 zip paramTys1).toMap
        (paramTys1 zip paramTys2).map((ty1, ty2) => isSame(ty1, subst(ty2, mapping))).foldLeft(true)(_&&_) && isSame(retTy1, subst(retTy2, mapping))
      else error("arity mismatch")
    case _ => false
        
  def subst(bodyTy:Type, mapping: Map[String, Type]):Type = bodyTy match{
    // unit types
    case UnitT => UnitT
    // number types
    case NumT => NumT
    // boolean types
    case BoolT => BoolT
    // string types
    case StrT => StrT
    // parameterized type identifiers
    case IdT(name: String, tys: List[Type]) => tys match
      case Nil => 
        mapping.getOrElse(name, IdT(name, tys))
      case tys => 
        IdT(name, tys.map(ty => subst(ty, mapping)))
    // function (arrow) types
    case ArrowT(tvars: List[String], paramTys: List[Type], retTy: Type) =>
      ArrowT(tvars, paramTys.map(pty => subst(pty, mapping)), subst(retTy, mapping))
  }
  
  def typeCheck(expr: Expr, tenv: TypeEnv): Type = 
    // println()
    // println(expr)
    // println()
    // println(tenv)    
    expr match
    // unit
      case EUnit => UnitT
      // numbers
      case ENum(number: BigInt) => NumT
      // booleans
      case EBool(bool: Boolean) => BoolT
      // strings
      case EStr(string: String) => StrT
      // identifier lookups
      case EId(name: String) =>
        tenv.vars.getOrElse(name, error(s"free identifier $name"))
      // addition
      case EAdd(left: Expr, right: Expr) =>
        mustSame(typeCheck(left, tenv), typeCheck(right, tenv))
        mustSame(typeCheck(left, tenv), NumT)
        NumT
      // multiplication
      case EMul(left: Expr, right: Expr) =>
        mustSame(typeCheck(left, tenv), typeCheck(right, tenv))
        mustSame(typeCheck(left, tenv), NumT)
        NumT
      // division
      case EDiv(left: Expr, right: Expr) =>
        mustSame(typeCheck(left, tenv), typeCheck(right, tenv))
        mustSame(typeCheck(left, tenv), NumT)
        NumT
      // modulo
      case EMod(left: Expr, right: Expr) =>
        mustSame(typeCheck(left, tenv), typeCheck(right, tenv))
        mustSame(typeCheck(left, tenv), NumT)
        NumT
      // string concatenation
      case EConcat(left: Expr, right: Expr) =>
        mustSame(typeCheck(left, tenv), typeCheck(right, tenv))
        mustSame(typeCheck(left, tenv), StrT)
        StrT
      // equal-to
      case EEq(left: Expr, right: Expr) =>
        mustSame(typeCheck(left, tenv), typeCheck(right, tenv))
        BoolT
      // less-than
      case ELt(left: Expr, right: Expr) =>
        mustSame(typeCheck(left, tenv), typeCheck(right, tenv))
        mustSame(typeCheck(left, tenv), NumT)
        BoolT
      // sequence
      case ESeq(left: Expr, right: Expr) =>
        typeCheck(left, tenv)
        typeCheck(right, tenv)
      // conditional
      case EIf(cond: Expr, thenExpr: Expr, elseExpr: Expr) =>
        mustSame(typeCheck(cond, tenv), BoolT)
        mustSame(typeCheck(thenExpr, tenv), typeCheck(elseExpr, tenv))
        typeCheck(thenExpr, tenv)
      // immutable variable definitions
      case EVal(x: String, tyOpt: Option[Type], expr: Expr, body: Expr) => tyOpt match
        case Some(ty) => 
          mustSame(typeCheck(expr, tenv), ty)
          typeCheck(body, tenv.addVar(x, ty))
        case None => 
          typeCheck(body, tenv.addVar(x, typeCheck(expr, tenv)))
      // anonymous (lambda) functions
      case EFun(params: List[Param], body: Expr) =>
        val ptys = params.map(_.ty)
        for (pty <- ptys) mustValid(pty, tenv)
        val rty = typeCheck(body, tenv.addVars(params.map(p => p.name -> p.ty)))
        ArrowT(Nil, ptys, rty)
      // function applications
      case EApp(fun: Expr, tys: List[Type], args: List[Expr]) =>
        for (ty <- tys) mustValid(ty, tenv)
        typeCheck(fun, tenv) match
          case ArrowT(tvars: List[String], paramTys: List[Type], retTy: Type) =>
            val argTys = args.map(arg => typeCheck(arg, tenv))
            val mapping = (tvars zip tys).toMap
            (argTys zip paramTys).map((argTy, ty) => mustSame(argTy, subst(ty, mapping)))
            subst(retTy, mapping)
          case _ => error("not a function")
      // mutually recursive definitions
      case ERecDefs(defs: List[RecDef], body: Expr) =>
        val nTEnv = defs.foldLeft(tenv)((currTEnv, d) => tenvRecUpdate(d, currTEnv))
        for (d <- defs) typingRec(d, nTEnv)
        mustValid(typeCheck(body, nTEnv), tenv)
      // pattern matching
      case EMatch(expr: Expr, mcases: List[MatchCase]) => typeCheck(expr, tenv) match
          case IdT(name, tys) => tenv.tys.getOrElse(name, error(s"$name not in type environment")) match
            case TIVar => error("not an ADT")
            case TIAdt(tvars, variants) =>
              val ts = variants.keySet
              val xs = mcases.map(_.name).toSet
              if (ts != xs || xs.size != mcases.length) error("invalid case")
              val mapping = (tvars zip tys).toMap
              mcases.map{ case MatchCase(x, ps, b) =>
                val pty = variants(x).map(_.ty)
                typeCheck(b, tenv.addVars(ps zip pty))
              }.reduce((lty, rty) => {mustSame(lty, rty); lty})
          case _ => error("ematch")

      // exit
      case EExit(ty: Type, expr: Expr) =>
        mustValid(ty, tenv)
        mustSame(typeCheck(expr, tenv), StrT)
        ty

  def tenvRecUpdate(recDef: RecDef, tenv: TypeEnv): TypeEnv = recDef match
    // immutable lazy variable definition
    case LazyVal(name: String, ty: Type, init: Expr) =>
      tenv.addVar((name, ty))
    // recursive function
    case RecFun(
      name: String,
      tvars: List[String],
      params: List[Param],
      rty: Type,
      body: Expr,
    ) =>
      tenv.addVar((name, ArrowT(tvars, params.map(_.ty), rty)))
    // polymorphic algebraic data type
    case TypeDef(
      name: String,
      tvars: List[String],
      varts: List[Variant],
    ) => 
      if (tenv.tys.contains(name)) error("already defined")
      val newTEnv = tenv.addTypeName(name, tvars, varts)
      varts.foldLeft(newTEnv)((nTEnv, vart) => 
        nTEnv.addVar((vart.name, ArrowT(tvars, vart.params.map(_.ty), IdT(name, tvars.map(IdT(_,Nil)))))))
  
  def typingRec(recDef: RecDef, tenv: TypeEnv): Unit = recDef match
    case LazyVal(name: String, ty: Type, init: Expr) =>
      mustSame(mustValid(ty, tenv), typeCheck(init, tenv))
    // recursive function
    case RecFun(
      name: String,
      tvars: List[String],
      params: List[Param],
      rty: Type,
      body: Expr,
    ) =>
      for (tvar <- tvars) if (tenv.tys.contains(tvar)) error("recfun")
      val newTEnv = tenv.addTypeVars(tvars)
      for (param <- params) mustValid(param.ty, newTEnv)
      mustValid(rty, newTEnv)
      val finalTEnv = newTEnv.addVars(params.map(param => (param.name, param.ty)))
      mustSame(typeCheck(body, finalTEnv), rty)
    // polymorphic algebraic data type
    case TypeDef(
      name: String,
      tvars: List[String],
      varts: List[Variant],
    ) => 
      for (tvar <- tvars) if (tenv.tys.contains(tvar)) error("typedef")
      val newTEnv = tenv.addTypeVars(tvars)
      for (vart <- varts) {
        for (param <- vart.params) mustValid(param.ty, newTEnv)
      }

  def eq(leftV: Value, rightV: Value): Boolean = (leftV, rightV) match
    case (UnitV, UnitV) => true
    case (NumV(num1), NumV(num2)) => num1 == num2
    case (BoolV(b1), BoolV(b2)) => b1 == b2
    case (StrV(str1), StrV(str2)) => str1 == str2
    case (VariantV(x1, val1), VariantV(x2, val2)) => 
      val1.zip(val2).map((value1, value2) => eq(value1, value2)).foldLeft(x1 == x2)(_ && _)
    case _ => false

  def interp(expr: Expr, env: Env): Value = 
    // println()
    // println(expr)
    // println()
    // println(env)
    expr match
    // unit
    case EUnit =>
      UnitV
    // numbers
    case ENum(number: BigInt) =>
      NumV(number)
    // booleans
    case EBool(bool: Boolean) =>
      BoolV(bool)
    // strings
    case EStr(string: String) =>
      StrV(string)
    // identifier lookups
    case EId(name: String) => env.getOrElse(name, error("free identifier")) match
      case ExprV(expr, fenv) =>
        interp(expr, fenv())
      case x => x
    // addition
    case EAdd(left: Expr, right: Expr) =>
      numAdd(interp(left, env), interp(right, env))
    // multiplication
    case EMul(left: Expr, right: Expr) =>
      numMul(interp(left, env), interp(right, env))
    // division
    case EDiv(left: Expr, right: Expr) =>
      numDiv(interp(left, env), interp(right, env))
    // modulo
    case EMod(left: Expr, right: Expr) =>
      numMod(interp(left, env), interp(right, env))
    // string concatenation
    case EConcat(left: Expr, right: Expr) =>
      strCon(interp(left, env), interp(right, env))
    // equal-to
    case EEq(left: Expr, right: Expr) => 
      BoolV(eq(interp(left, env), interp(right, env)))
    // less-than
    case ELt(left: Expr, right: Expr) =>
      numLt(interp(left, env), interp(right, env))
    // sequence
    case ESeq(left: Expr, right: Expr) =>
      interp(left, env)
      interp(right, env)
    // conditional
    case EIf(cond: Expr, thenExpr: Expr, elseExpr: Expr) => interp(cond, env) match
      case BoolV(true) => interp(thenExpr, env)
      case BoolV(false) => interp(elseExpr, env)
      case _ => error("eif")
    // immutable variable definitions
    case EVal(x: String, tyOpt: Option[Type], expr: Expr, body: Expr) => 
      interp(body, env + (x -> interp(expr, env)))
    // anonymous (lambda) functions
    case EFun(params: List[Param], body: Expr) => 
      CloV(params.map(_.name), body, () => env)
    // function applications
    case EApp(fun: Expr, tys: List[Type], args: List[Expr]) => interp(fun, env) match
      case CloV(params, body, fenv) =>
        val newEnv = fenv() ++ (params zip args.map(arg => interp(arg, env))).toMap
        interp(body, newEnv)
      case ConstrV(name) => VariantV(name, args.map(arg => interp(arg, env)))
      case _ => error("eapp")
    // mutually recursive definitions
    case ERecDefs(defs: List[RecDef], body: Expr) => 
      lazy val newEnv: Env = defs.foldLeft(env)((currEnv, d) => envRecUpdate(currEnv,() => newEnv, d))
      interp(body, newEnv)
    // pattern matching
    case EMatch(expr: Expr, mcases: List[MatchCase]) => interp(expr, env) match
      case VariantV(name, values) => mcases.find(_.name == name) match
        case Some(MatchCase(_, params, body)) =>
          if (params.length != values.length) error("arity mismatch")
          interp(body, env ++ (params zip values))
        case None => error(s"no match for $name")
      case v => error(s"${v.str} is not a variant")
    case EExit(ty: Type, expr: Expr) => error()
    
  
  def envRecUpdate(env:Env, fenv: () => Env, recDef: RecDef): Env = recDef match
    // immutable lazy variable definition
  case LazyVal(
    name: String,
    ty: Type,
    init: Expr,
  ) =>
    env + (name -> ExprV(init, fenv))
  // recursive function
  case RecFun(
    name: String,
    tvars: List[String],
    params: List[Param],
    rty: Type,
    body: Expr,
  ) =>
    env + (name -> CloV(params.map(_.name), body, fenv))
  // polymorphic algebraic data type
  case TypeDef(
    name: String,
    tvars: List[String],
    varts: List[Variant],
  ) =>
    env ++ varts.map(vart => (vart.name -> ConstrV(vart.name))).toMap
}
