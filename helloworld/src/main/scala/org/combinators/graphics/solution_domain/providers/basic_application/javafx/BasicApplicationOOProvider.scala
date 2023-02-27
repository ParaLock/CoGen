package org.combinators.graphics.solution_domain.providers.basic_application.javafx

import org.combinators.ep.domain.abstractions.{DataType, DataTypeCase, TypeRep}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.ObjectOriented
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.{AbstractSyntax, Command, NameProvider, Understands}
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Arrays, Assertions, Console, Equality}
import org.combinators.ep.generator.paradigm.{AnyParadigm, FindClass, ObjectOriented}


trait BasicApplicationOOProvider extends BasicApplicationProvider {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val impParadigm: Imperative.WithBase[paradigm.MethodBodyContext,paradigm.type]
  val names: NameProvider[paradigm.syntax.Name]
  val ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Int]
  val ffiAssertions : Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val ffiEquality : Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val console: Console.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val array: Arrays.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val asserts: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val eqls: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]

  import paradigm._
  import syntax._
  import ooParadigm._

  lazy val initFuncName = names.mangle("init")
  lazy val startFuncName = names.mangle("start")
  lazy val stopFuncName = names.mangle("stop")
  lazy val mainFuncName = names.mangle("main")
  lazy val testWindowName = names.mangle("windowTest")

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

  def instantiate(baseTpe: DataType, tpeCase: DataTypeCase, args: Expression*): Generator[MethodBodyContext, Expression] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    for {
      rt <- findClass(names.mangle(names.conceptNameOf(tpeCase)))
      _ <- resolveAndAddImport(rt)

      res <- instantiateObject(rt, args)
    } yield res
  }

  def make_init(): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    for {

      intType <- toTargetLanguageType(TypeRep.Int)
      _ <- paradigm.methodBodyCapabilities.setReturnType(intType)

      sp <- superReference()
      init <- getMember(sp, initFuncName)
      result <- apply(init, Seq.empty)

    } yield Some(result)
  }

  def make_class_instance(typeName:String, varName: String, constructorParams: Seq[Expression]): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]]  = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import impParadigm.imperativeCapabilities._

    for {
      classType <- findClass(names.mangle(typeName))
      _ <- resolveAndAddImport(classType)
      classObj <- instantiateObject(classType, constructorParams)
      classObjName <- freshName(names.mangle(varName))
      sceneObjVar <- declareVar(classObjName, classType, Some(classObj))
    } yield Some(sceneObjVar)
  }

  def print_message(msg: String): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import impParadigm.imperativeCapabilities._

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


  def make_start_func(): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
      import paradigm.methodBodyCapabilities._
      import ooParadigm.methodBodyCapabilities._
      import impParadigm.imperativeCapabilities._
      for {

        // Make signature
        unitType <- toTargetLanguageType(TypeRep.Unit)
        _ <- paradigm.methodBodyCapabilities.setReturnType(unitType)
        stageType <- findClass(names.mangle("Stage"))
        _ <- resolveAndAddImport(stageType)
        _ <- setParameters(Seq((names.mangle("primaryStage"), stageType)))
        //----

        args <- getArguments()
        (name, tpe, primaryStage) = args.head

        _ <- print_message("Inside init() method! Perform necessary initializations here.")

        labelObjWrapper <- make_class_instance(
          "Label",
          "label",
          Seq.empty
        )

        labelObj = labelObjWrapper.get

        sceneObjWrapper <- make_class_instance("Scene", "scene", Seq(labelObj))
        sceneObj = sceneObjWrapper.get

        alignFunc <- getMember(
          sceneObj,
          names.mangle("setAlignment")
        )

        _ <- apply(alignFunc, Seq.empty)




        //        Label label = new Label("Hello World");
//        label.setAlignment(Pos.CENTER);
//        Scene scene = new Scene(label);
//
//        primaryStage.setTitle("Hello World Application");
//        primaryStage.setScene(scene);
//        primaryStage.show();


        // Add group object to scene
        // Loop over elements in domain model
        // Attach element to group object
        // Call setAlignment on each element according to style

        // Set primary stage title
        // Call set stage scene
        // Call stage show

      } yield Some(labelObj)
    }

    def make_stop_func(): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
      import paradigm.methodBodyCapabilities._
      import ooParadigm.methodBodyCapabilities._
      import impParadigm.imperativeCapabilities._
      for {

        // Make signatures
        intType <- toTargetLanguageType(TypeRep.Int)
        _ <- paradigm.methodBodyCapabilities.setReturnType(intType)

        _ <- print_message("Inside stop() method! Destroy resources. Perform Cleanup.")

        // call super.stop()
        sp <- superReference()
        stopFunc <- getMember(sp, stopFuncName)
        result <- apply(stopFunc, Seq.empty)

      } yield Some(result)
    }

//  def make_main_func(): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
//      import paradigm.methodBodyCapabilities._
//      import ooParadigm.methodBodyCapabilities._
//      import impParadigm.imperativeCapabilities._
//      for {
//
//        // Make signature
//        _ <- setStatic()
//        arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.String))
//        _ <- resolveAndAddImport(arrayType)
//        unitType <- toTargetLanguageType(TypeRep.Unit)
//        _ <- setReturnType(unitType)
//        _ <- setParameters(Seq((names.mangle("args"), arrayType)))
//        // --------------
//
//        selfRef <- selfReference()
//        launchRef <- getMember(selfRef, )
//        result <- apply(selfRef, Seq.empty)
//
//      } yield Some(result)
//    }
  def make_class(clazzName: String): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      val javafx_app_import = Seq(
        names.mangle("javafx"),
        names.mangle("application"),
        names.mangle("Application")
      )
      for {
        pt <- findClass(javafx_app_import : _ *)
        _ <- resolveAndAddImport(pt)
        _ <- addParent(pt)
        _ <- addMethod(initFuncName, make_init())
        _ <- addMethod(startFuncName, make_start_func())
        _ <- addMethod(stopFuncName, make_stop_func())

      } yield ()
    }

    addClassToProject(makeClass, names.mangle(clazzName))
  }

  def make_window_test(): Generator[paradigm.MethodBodyContext, Seq[paradigm.syntax.Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ffiEquality.equalityCapabilities._
    for {
      two <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 42)
    } yield Seq(two)
  }

  def make_test() : Generator[paradigm.TestContext, Unit] = {
    for {
      _ <- paradigm.testCapabilities.addTestCase(make_window_test(), testWindowName)
    } yield ()
  }

  def implement(): Generator[paradigm.ProjectContext, Unit] = {
    for {
      _ <- make_class("BasicApplicationJavaFX")
    } yield ()
  }

}

object BasicApplicationOOProvider {
  type WithParadigm[P <: AnyParadigm] = BasicApplicationOOProvider { val paradigm: P }
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

  def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
  (base: P)
  (nameProvider: NameProvider[base.syntax.Name],
   imp: Imperative.WithBase[base.MethodBodyContext, base.type],
   oo: ObjectOriented.WithBase[base.type],
   con: Console.WithBase[base.MethodBodyContext, base.type],
   arr: Arrays.WithBase[base.MethodBodyContext, base.type],
   assertsIn: Assertions.WithBase[base.MethodBodyContext, base.type],
   eqlsIn: Equality.WithBase[base.MethodBodyContext, base.type],
   ffiarith:  Arithmetic.WithBase[base.MethodBodyContext, base.type, Int],
   ffiassert: Assertions.WithBase[base.MethodBodyContext, base.type],
   ffiequal: Equality.WithBase[base.MethodBodyContext, base.type]
  )
  : BasicApplicationOOProvider.WithParadigm[base.type] =
    new BasicApplicationOOProvider {
      override val paradigm: base.type = base
      val impParadigm: imp.type = imp
      override val names: NameProvider[paradigm.syntax.Name] = nameProvider
      override val ooParadigm: ObjectOriented.WithBase[paradigm.type] = oo
      override val console: Console.WithBase[base.MethodBodyContext, paradigm.type] = con
      override val array: Arrays.WithBase[base.MethodBodyContext, paradigm.type] = arr
      override val asserts: Assertions.WithBase[base.MethodBodyContext, paradigm.type] = assertsIn
      override val eqls: Equality.WithBase[base.MethodBodyContext, paradigm.type] = eqlsIn
      override val ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Int] = ffiarith
      override val ffiAssertions: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type] = ffiassert
      override val ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type] = ffiequal
    }
}


