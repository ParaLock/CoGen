package org.combinators.gui.providers.javafx

import com.github.javaparser.ast.ImportDeclaration
import org.combinators.common._
import org.combinators.ep.domain.abstractions.{DataType, DataTypeCase, TypeRep}
import org.combinators.ep.generator.Command.{Generator, lift}
import org.combinators.ep.generator.paradigm.{AnyParadigm, FindClass, ObjectOriented, Templating}
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.{AbstractSyntax, Command, NameProvider, Understands}
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Arrays, Assertions, Console, Equality}
import org.combinators.graphics.GUIDomain
import org.combinators.gui.domain_model.GridLayout

import java.lang.CharSequence


trait JavaFXApplicationProvider extends BaseProvider {

  val domain: GUIDomain

  val templating: Templating.WithBase[ooParadigm.ClassContext, paradigm.MethodBodyContext, paradigm.type]

  import paradigm._
  import syntax._
  import ooParadigm._

  lazy val initFuncName = names.mangle("init")
  lazy val startFuncName = names.mangle("start")
  lazy val stopFuncName = names.mangle("stop")
  lazy val mainFuncName = names.mangle("main")
  lazy val testWindowName = names.mangle("windowTest")


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
    import impParadigm.imperativeCapabilities._
    for {

      // Call super
      sp <- superReference()
      init <- getMember(sp, initFuncName)
      result <- apply(init, Seq.empty)
      stmt <- liftExpression(result)
      _ <- addBlockDefinitions(Seq(stmt))

      // Print message
      _ <- print_message("Inside init() method! Perform necessary initializations here.")

      // Make signature
      unitType <- toTargetLanguageType(TypeRep.Unit)
      _ <- paradigm.methodBodyCapabilities.setReturnType(unitType)


    } yield None
  }

  def make_grid_layout(): Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] = {

    val layout: GridLayout = domain.layout

    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import ooParadigm.classCapabilities.addMethodFromFragment
    import impParadigm.imperativeCapabilities._

    for {

      unitType <- toTargetLanguageType(TypeRep.Unit)
      intType <- toTargetLanguageType(TypeRep.Int)

      zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)
      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
      one_hundred <- paradigm.methodBodyCapabilities.reify(TypeRep.Double, 100)


      // Get grid size
      numRows <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, layout.cols)
      numCols <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, layout.rows)

      // Constraint object initialization
      gridPane <- make_class_instantiation(
        "GridPane",
        "gridPane",
        Seq()
      )

      colConstraints <- make_class_instantiation(
        "ColumnConstraints",
        "colConstraints",
        Seq()
      )

      percentCols <- ffiArithmetic.arithmeticCapabilities.div(one_hundred, numCols)
      _ <- make_method_call(
        colConstraints,
        "setPercentWidth",
        Seq(percentCols)
      )


      rowConstraints <- make_class_instantiation(
        "RowConstraints",
        "rowConstraints",
        Seq()
      )

      percentRows <- ffiArithmetic.arithmeticCapabilities.div(one_hundred, numRows)
      _ <- make_method_call(
        rowConstraints,
        "setPercentHeight",
        Seq(percentRows)
      )

      // Row/Col loop -----------------------------

      iName <- freshName(names.mangle("i"))
      iType <- toTargetLanguageType(TypeRep.Int)

      iVar <- declareVar(iName, iType, Some(zero))
      compExpr <- ffiArithmetic.arithmeticCapabilities.lt(iVar, numRows)

//      for (int i = 0; i < 3; i ++) {
//        gridPane.getColumnConstraints().add(colConstraints);
//        gridPane.getRowConstraints().add(rowConstraints);
//          for (int j = 0; j < 3; j ++) {
//          Label label = new Label ("Label " + (i * 3 + j + 1));
//          label.setMaxSize (Double.MAX_VALUE, Double.MAX_VALUE);
//          label.setAlignment (Pos.CENTER);
//          GridPane.setConstraints (label, j, i);
//          gridPane.getChildren ().add (label);
//        }
//    }



      stmt <- whileLoop(compExpr, for {

          _ <- make_chained_method_call(
            gridPane,
            "getColumnConstraints",
            Seq(),
            "add",
            Seq(colConstraints)
          )

          _ <- make_chained_method_call(
            gridPane,
            "getRowConstraints",
            Seq(),
            "add",
            Seq(rowConstraints)
          )

          jName <- freshName(names.mangle("j"))
          jType <- toTargetLanguageType(TypeRep.Int)
          jVar <- declareVar(jName, jType, Some(zero))

          jCompExpr <- ffiArithmetic.arithmeticCapabilities.lt(jVar, numCols)

//          Label label = new Label("Label " + (i * 3 + j + 1));
//        label.setMaxSize(Double.MAX_VALUE, Double.MAX_VALUE);
//        label.setAlignment(Pos.CENTER);
//        GridPane.setConstraints(label, j, i);
//        gridPane.getChildren().add(label);

          innerLoop <- whileLoop(jCompExpr, for {

            _ <- print_message("test")

            } yield ()
          )

          inc <- ffiArithmetic.arithmeticCapabilities.add(iVar, one)
          incAssign <- assignVar(iVar, inc)

          _ <- addBlockDefinitions(Seq(innerLoop, incAssign))

        } yield ()
      )
      _ <- addBlockDefinitions(Seq(stmt))



    } yield gridPane
  }

  def make_start_func(): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
      import paradigm.methodBodyCapabilities._
      import ooParadigm.methodBodyCapabilities._
      import ooParadigm.classCapabilities.addMethodFromFragment
      import impParadigm.imperativeCapabilities._

      for {

        // Define types and literals
        unitType <- toTargetLanguageType(TypeRep.Unit)
        intType <- toTargetLanguageType(TypeRep.Int)

        zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)
        one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
        one_hundred <- paradigm.methodBodyCapabilities.reify(TypeRep.Double, 100)

        // Make signature
        _ <- paradigm.methodBodyCapabilities.setReturnType(unitType)
        stageType <- findClass(names.mangle("Stage"))
        _ <- resolveAndAddImport(stageType)
        _ <- setParameters(Seq((names.mangle("primaryStage"), stageType)))
        //----

        args <- getArguments()
        (name, tpe, primaryStage) = args.head

        gridObj <- make_grid_layout()

        sceneObj <- make_class_instantiation(
          "Scene",
          "scene",
          Seq(gridObj)
        )

        title <- paradigm.methodBodyCapabilities.reify(
          TypeRep.String,
          domain.window.title
        )

        _ <- make_method_call(
          primaryStage,
          "setTitle",
          Seq(title)
        )

        _ <- make_method_call(
          primaryStage,
          "setScene",
          Seq(sceneObj)
        )

      } yield None
    }

    def make_stop_func(): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
      import paradigm.methodBodyCapabilities._
      import ooParadigm.methodBodyCapabilities._
      import impParadigm.imperativeCapabilities._
      for {

        // Make signatures
        intType <- toTargetLanguageType(TypeRep.Int)
        _ <- paradigm.methodBodyCapabilities.setReturnType(intType)

        // call super.stop()
        sp <- superReference()
        init <- getMember(sp, stopFuncName)
        result <- apply(init, Seq.empty)

        stmt <- liftExpression(result)
        _ <- addBlockDefinitions(Seq(stmt))

        // Print message
        _ <- print_message("Inside stop() method! Destroy resources. Perform Cleanup.")

      } yield (None)
    }


//  def make_myfunc_statments(): Generator[paradigm.MethodBodyContext, Unit] = {
//    import paradigm.methodBodyCapabilities._
//    import ooParadigm.methodBodyCapabilities._
//    import impParadigm.imperativeCapabilities._
//    for {
//
//      // Make signatures
//      intType <- toTargetLanguageType(TypeRep.Int)
//      _ <- paradigm.methodBodyCapabilities.setReturnType(intType)
//
//      _ <- print_message("Inside stop() method! Destroy resources. Perform Cleanup.")
//
//      // call super.stop()
//      sp <- superReference()
//      stopFunc <- getMember(sp, stopFuncName)
//      result <- apply(stopFunc, Seq.empty)
//
//    } yield ()
//  }


  def make_main_func(): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
      import paradigm.methodBodyCapabilities._
      import ooParadigm.methodBodyCapabilities._
      import impParadigm.imperativeCapabilities._
      for {

        // Make signature
        _ <- setStatic()
        arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.String))
        _ <- resolveAndAddImport(arrayType)
        unitType <- toTargetLanguageType(TypeRep.Unit)
        _ <- setReturnType(unitType)
        _ <- setParameters(Seq((names.mangle("args"), arrayType)))
        // --------------


        args <- getArguments()
        (name, tpe, funcArgs) = args.head
        appClass <- findRawClass(names.mangle("Application"))
        _ <- make_static_method_call(
          appClass,
          "launch",
          Seq(funcArgs)
        )

      } yield None
    }
  def make_class(clazzName: String): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    import ooParadigm.methodBodyCapabilities._

    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._

      val app_import = Seq(
        names.mangle("Application"),
      )

      for {

        exceptionClass <- findRawClass(names.mangle("Exception"))
        pt <- findRawClass(app_import : _ *)
        _ <- resolveAndAddImport(pt)
        _ <- addParent(pt)
        _ <- addMethod(initFuncName, make_init(),true, Seq(exceptionClass))
        _ <- addMethod(startFuncName, make_start_func())
        _ <- addMethod(stopFuncName, make_stop_func(), true, Seq(exceptionClass))
        _ <- addMethod(mainFuncName, make_main_func())
      } yield ()
    }

    for {

        _ <- addClassToProject(makeClass, names.mangle(clazzName))

    } yield ()

  }

//  def make_class_template() : Generator[ClassContext, Unit] = {
//
//    import templating.templatingCapabilities._
//
//    for {
//
//      _ <- loadFragment(this.getClass.getResource("/GUI/JavaFX_Target/Fragment1.java"))
//
//      templateVar1 <- getTemplateVar[Unit](
//        "MyFuncStmts"
//      )
//
//      _ <- replace[Unit](templateVar1, make_myfunc_statments())
//
//    } yield ()
//
//  }


  def implement(): Generator[paradigm.ProjectContext, Unit] = {
    for {
      _ <- make_class("JavaFXBasicApplication")
    } yield ()
  }

}

object JavaFXApplicationProvider {
  type WithParadigm[P <: AnyParadigm] = JavaFXApplicationProvider { val paradigm: P }
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

  def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
  (base: P)
  (nameProvider: NameProvider[base.syntax.Name],
   imp: Imperative.WithBase[base.MethodBodyContext, base.type],
   //impConstructor: Imperative.WithBase[ObjectOriented.WithBase[base.type], base.type],
   oo: ObjectOriented.WithBase[base.type],
   con: Console.WithBase[base.MethodBodyContext, base.type],
   arr: Arrays.WithBase[base.MethodBodyContext, base.type],
   assertsIn: Assertions.WithBase[base.MethodBodyContext, base.type],
   eqlsIn: Equality.WithBase[base.MethodBodyContext, base.type],
   ffiarith:  Arithmetic.WithBase[base.MethodBodyContext, base.type, Int],
   ffiassert: Assertions.WithBase[base.MethodBodyContext, base.type],
   ffiequal: Equality.WithBase[base.MethodBodyContext, base.type],
  )
  (_templating: Templating.WithBase[oo.ClassContext, base.MethodBodyContext, base.type])(
    _domain: GUIDomain
  )
: JavaFXApplicationProvider.WithParadigm[base.type] =
    new JavaFXApplicationProvider {
      override val paradigm: base.type = base
      val impParadigm: imp.type = imp
      override val domain: GUIDomain = _domain
      override val names: NameProvider[paradigm.syntax.Name] = nameProvider
      override val ooParadigm: oo.type = oo
      override val console: Console.WithBase[base.MethodBodyContext, paradigm.type] = con
      override val array: Arrays.WithBase[base.MethodBodyContext, paradigm.type] = arr
      override val asserts: Assertions.WithBase[base.MethodBodyContext, paradigm.type] = assertsIn
      override val eqls: Equality.WithBase[base.MethodBodyContext, paradigm.type] = eqlsIn
      override val ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Int] = ffiarith
      override val ffiAssertions: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type] = ffiassert
      override val ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type] = ffiequal
      override val templating: Templating.WithBase[ooParadigm.ClassContext, paradigm.MethodBodyContext, paradigm.type] = _templating
    }
}


