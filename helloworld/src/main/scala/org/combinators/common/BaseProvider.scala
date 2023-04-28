package org.combinators.common

import org.combinators.ep.domain.abstractions.{DataType, DataTypeCase, TypeRep}
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}
import org.combinators.ep.generator.Command.{Generator, _}
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax._
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Arrays, Assertions, Console, Equality}
import org.combinators.ep.generator.paradigm.{AddImport, AnyParadigm, FindClass, ObjectOriented, ResolveImport, Templating}
import org.combinators.ep.generator.{NameProvider, Understands}


/** Attempt to provide a window provider -- perhaps can be abstracted.... */
trait BaseProvider {
  val paradigm: AnyParadigm
  val names: NameProvider[paradigm.syntax.Name]
  val console: Console.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val impParadigm: Imperative.WithBase[paradigm.MethodBodyContext,paradigm.type]
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Int]
  val ffiAssertions: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val array: Arrays.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val asserts: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val eqls: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]


  import paradigm._
  import syntax._

  /**
    * Default registration for findClass, which works with each registerTypeMapping for the different approaches.
    *
    * Sometimes the mapping is fixed for an EP approach, but sometimes it matters when a particular class is requested
    * in the evolution of the system over time.
    *
    * @param dtpe
    * @param canFindClass
    * @tparam Ctxt
    * @return
    */
  def domainTypeLookup[Ctxt](dtpe: DataType)(implicit canFindClass: Understands[Ctxt, FindClass[Name, Type]]): Generator[Ctxt, Type] = {
    FindClass(Seq(names.mangle(names.conceptNameOf(dtpe)))).interpret(canFindClass)
  }

  /** Provides meaningful default solution to find the base data type in many object-oriented approaches.
    *
    * This enables target-language classes to be retrieved from within the code generator in the Method, Class or Constructor contexts.
    */
  def registerTypeMapping(tpe: DataType): Generator[ProjectContext, Unit] = {
    import paradigm.projectCapabilities.addTypeLookupForMethods
    import ooParadigm.methodBodyCapabilities.canFindClassInMethod // must be present, regardless of IntelliJ
    import ooParadigm.projectCapabilities.addTypeLookupForClasses
    import ooParadigm.projectCapabilities.addTypeLookupForConstructors
    import ooParadigm.classCapabilities.canFindClassInClass // must be present, regardless of IntelliJ
    import ooParadigm.constructorCapabilities.canFindClassInConstructor // must be present, regardless of IntelliJ
    val dtpe = TypeRep.DataType(tpe)
    for {
      _ <- addTypeLookupForMethods(dtpe, domainTypeLookup(tpe))
      _ <- addTypeLookupForClasses(dtpe, domainTypeLookup(tpe))
      _ <- addTypeLookupForConstructors(dtpe, domainTypeLookup(tpe))
    } yield ()
  }


  def make_field_class_assignment(
                                      className: String,
                                      fieldName: String,
                                      constructorParams: Seq[Expression]
                                    ): Generator[MethodBodyContext, Option[paradigm.syntax.Expression]] = {
    import impParadigm.imperativeCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._
    for {

      selfRef <- selfReference()
      fieldVar <- getMember(selfRef, names.mangle(fieldName))
      classType <- findClass(names.mangle(className))
      obj <- instantiateObject(classType, constructorParams)
      expr <- assignVar(fieldVar, obj)
      _ <- addBlockDefinitions(Seq(expr))

    } yield None

  }

  def make_class_instantiation_floating(
                                typeName: String,
                                constructorParams: Seq[Expression],
                                addInline: Boolean = false
                              ): Generator[MethodBodyContext, paradigm.syntax.Expression] = {
    import impParadigm.imperativeCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._

    val myFunc: Expression => Generator[MethodBodyContext, paradigm.syntax.Expression] = {
      (stmt) => {
        if (addInline) {
          for {
            liftedStmt <- liftExpression(stmt)
            _ <- addBlockDefinitions(Seq(liftedStmt))
          } yield stmt
        } else {
          for {
            liftedStmt <- liftExpression(stmt)
          } yield stmt
        }
      }
    }

    for {
      classType <- findClass(names.mangle(typeName))
      stmt <- instantiateObject(classType, constructorParams)
      _ <- myFunc(stmt)
    } yield stmt

  }


  def make_class_instantiation(
                           typeName: String,
                           varName: String,
                           constructorParams: Seq[Expression]
                         ): Generator[MethodBodyContext, paradigm.syntax.Expression] = {
    import impParadigm.imperativeCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._

    for {
      classType <- findRawClass(names.mangle(typeName))
      _ <- resolveAndAddImport(classType)
      classObj <- instantiateObject(classType, constructorParams)
      classObjName <- freshName(names.mangle(varName))
      sceneObjVar <- declareVar(classObjName, classType, Some(classObj))
    } yield sceneObjVar
  }


//  def make_class_instantiation_in_constructor(
//                                typeName: String,
//                                varName: String,
//                                constructorParams: Seq[Expression]
//                              ): Generator[ooParadigm.ConstructorContext, paradigm.syntax.Expression] = {
//    import ooParadigm.constructorCapabilities._
//    //import impConstructorParadigm.imperativeCapabilities._
//    for {
//      classType <- findClass(names.mangle(typeName))
//      _ <- resolveAndAddImport(classType)
//      classObj <- instantiateObject(classType, constructorParams)
//      classObjName <- freshName(names.mangle(varName))
//      sceneObjVar <- declareVar(classObjName, classType, Some(classObj))
//    } yield sceneObjVar
//  }

  def make_field_assignment(
                                     expr: Expression,
                                     memberVarName: String
                                   ): Generator[paradigm.MethodBodyContext, Unit] = {
    import impParadigm.imperativeCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._

    for {
      self <- selfReference()
      member <- getMember(self, names.mangle(memberVarName))
      stmt <- assignVar(member, expr)
      _ <- addBlockDefinitions(Seq(stmt))
    } yield ()

  }

  def make_static_method_call(
                               obj: Type,
                               methodName: String,
                               args: Seq[Expression]
                           ): Generator[paradigm.MethodBodyContext, Unit] = {
    import impParadigm.imperativeCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._

    for {

      expr <- toStaticTypeExpression(obj)

      _ <- make_method_call(
        expr,
        methodName,
        args
      )

    } yield ()

  }



  def make_method_call(
                   obj: Expression,
                   methodName: String,
                   args: Seq[Expression],
                   isFloating: Boolean = false
                 ): Generator[paradigm.MethodBodyContext, Expression] = {
    import impParadigm.imperativeCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._

    for {

      alignFunc <- getMember(
        obj,
        names.mangle(methodName)
      )
      stmt <- apply(alignFunc, args)
      liftedStmt <- liftExpression(stmt)
      _ <- addBlockDefinitions(Seq(liftedStmt))

    } yield (stmt)
  }

  def make_chained_method_call(
                                obj: Expression,
                                method1Name: String,
                                method1Args: Seq[Expression],
                                method2Name: String,
                                method2Args: Seq[Expression],
                              ): Generator[paradigm.MethodBodyContext, Unit] = {

    import impParadigm.imperativeCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._

    for {
      method1 <- getMember(
        obj,
        names.mangle(method1Name)
      )
      method1Invoke <- apply(method1, method1Args)
      method1InvokeLifted <- liftExpression(method1Invoke)

      method2 <- getMember(
        method1Invoke,
        names.mangle(method2Name)
      )
      method2Invoke <- apply(method2, method2Args)
      method2InvokeLifted <- liftExpression(method2Invoke)
      _ <- addBlockDefinitions(Seq(method2InvokeLifted))

    } yield()

  }

  def make_method_call_with_ret(
                        obj: Expression,
                        methodName: String,
                        returnVarType: Type,
                        returnVarName: String,
                        args: Seq[Expression]
                      ): Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] = {
    import impParadigm.imperativeCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._

    for {

      //actualType <- toTargetLanguageType(returnVarType)
      alignFunc <- getMember(
        obj,
        names.mangle(methodName)
      )
      stmt <- apply(alignFunc, args)

      retVarName <- freshName(names.mangle(returnVarName))
      retVar <- declareVar(retVarName, returnVarType, Some(stmt))

      liftedStmt <- liftExpression(stmt)
      _ <- addBlockDefinitions(Seq(liftedStmt))

    } yield retVar
  }


  def print_message(msg: String): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
    import impParadigm.imperativeCapabilities._
    import paradigm.methodBodyCapabilities._

    for {
      // Generate print statement
      msgVal <- paradigm.methodBodyCapabilities.reify(
        TypeRep.String,
        msg
      )
      output <- console.consoleCapabilities.print(msgVal)
      le <- liftExpression(output)
      _ <- addBlockDefinitions(Seq(le))

    } yield Some(output)
  }

  /** Returns code to instantiate the given data type case, filling in `args` for its parameters. */
  def instantiate(baseTpe: DataType, tpeCase: DataTypeCase, args: Expression*): Generator[MethodBodyContext, Expression]



  /** Returns code to instantiate the given Scala model of a domain specific type. */
  def instantiate(baseType: DataType, inst: DataTypeInstance): Generator[MethodBodyContext, Expression] = {
    for {
      attributeInstances <- forEach (inst.attributeInstances) { ati => reify(ati) }
      result <- instantiate(baseType, inst.tpeCase, attributeInstances: _*)
    } yield result
  }

  /** Available in any Context that can ResolveImport and AddImport. */
  def resolveAndAddImport[Context, Elem](elem: Elem)
                                        (implicit
                                         canResolveImport: Understands[Context, ResolveImport[Import, Elem]],
                                         canAddImport: Understands[Context, AddImport[Import]]
                                        ) : Generator[Context, Unit] = {
    ResolveImport[Import, Elem](elem).interpret.flatMap(imp => imp.map(AddImport(_).interpret).getOrElse(skip))
  }

  /** Converts a Scala model of an instance of any representable type into code. */
  def reify(inst: InstanceRep): Generator[MethodBodyContext, Expression] = {
    (inst.tpe, inst.inst) match {
      case (TypeRep.DataType(baseTpe), domInst: DataTypeInstance) => instantiate(baseTpe, domInst)
      case (tpe, inst) =>
        import paradigm.methodBodyCapabilities._
        for {
          resTy <- toTargetLanguageType(tpe)
          _ <- resolveAndAddImport(resTy)
          res <- methodBodyCapabilities.reify[tpe.HostType](tpe, inst.asInstanceOf[tpe.HostType])
        } yield res
      case _ => throw new scala.NotImplementedError(s"No rule to compile instantiations of ${inst.tpe}.")
    }
  }

  /** Entry point into code generation. */
  def implement(): Generator[ProjectContext, Unit]

  /** Define standard test name. */
  def testCaseName:Name = {
    names.mangle("Test")
  }

}

