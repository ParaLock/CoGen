package org.combinators.gui.providers.javafx

import com.github.javaparser.ast.ImportDeclaration
import org.combinators.common._
import org.combinators.ep.domain.abstractions.{DataType, DataTypeCase, TypeRep}
import org.combinators.ep.generator.Command.{Generator, lift}
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.{forEach, forEachWithIndex}
import org.combinators.ep.generator.paradigm.{AnyParadigm, FindClass, ObjectOriented, Templating}
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.{AbstractSyntax, Command, NameProvider, Understands}
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Arrays, Assertions, Console, Equality}
import org.combinators.ep.language.java.CodeGenerator
import org.combinators.graphics.GUIDomain
import org.combinators.gui.domain_model.GridLayout
import org.combinators.gui.domain_model.Text
import org.combinators.gui.domain_model.Element

import java.lang.CharSequence


trait JavaFXApplicationProvider extends BaseProvider {

  val domain: GUIDomain

  val impConstructorParadigm: Imperative.WithBase[ooParadigm.ConstructorContext, paradigm.type]
  val classTemplating: Templating.WithBase[ooParadigm.ClassContext, paradigm.MethodBodyContext, paradigm.type]
  val unitTemplating: Templating.WithBase[paradigm.CompilationUnitContext, paradigm.MethodBodyContext, paradigm.type]


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

      _ <- make_init_data_stmts()

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

          innerLoop <- whileLoop(jCompExpr, for {

            self <- selfReference()
            elements <- getMember(
              self,
              names.mangle("elements"),
            )

            rowTimesCol <- ffiArithmetic.arithmeticCapabilities.mult(
              numRows,
              jVar
            )

            index <- ffiArithmetic.arithmeticCapabilities.add(
              rowTimesCol,
              iVar
            )

            msgVal <- array.arrayCapabilities.get(
              elements,
              index
            )

            labelClass <- make_class_instantiation(
              "Label",
              "label",
              Seq(msgVal)
            )

            doubleClass <- findRawClass(names.mangle("Double"))
            doubleClassExpr <- toStaticTypeExpression(doubleClass)
            maxVal <- getMember(doubleClassExpr, names.mangle("MAX_VALUE"))

            posClass <- findRawClass(names.mangle("Pos"))
            posClassExpr <- toStaticTypeExpression(posClass)
            posCenter <- getMember(posClassExpr, names.mangle("CENTER"))

            _ <- make_method_call(
              labelClass,
              "setMaxSize",
              Seq(maxVal, maxVal)
            )

            _ <- make_method_call(
              labelClass,
              "setAlignment",
              Seq(posCenter)
            )

            gridPaneStaticClass <- findRawClass(names.mangle("GridPane"))

            _ <- make_static_method_call(
              gridPaneStaticClass,
              "setConstraints",
              Seq(labelClass, jVar, iVar)
            )

            _ <- make_chained_method_call(
              gridPane,
              "getChildren",
              Seq.empty,
              "add",
              Seq(labelClass)
            )

            inc <- ffiArithmetic.arithmeticCapabilities.add(jVar, one)
            incAssign <- assignVar(jVar, inc)
            _ <- addBlockDefinitions(Seq(incAssign))

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

  def make_text_element_class(): Generator[ClassContext, Unit] = {

    import ooParadigm.classCapabilities._

      def make_constructor(): Generator[ConstructorContext, Unit] = {
        import ooParadigm.constructorCapabilities._

        for {

          stringType <- toTargetLanguageType(TypeRep.String)

          _ <- setParameters(
            Seq((names.mangle("msg"),stringType))
          )

          args <- getArguments()
          (name, tpe, msgArg) = args.head

          self <- selfReference()

          msgField <- getMember(
            self,
            names.mangle("msg")
          )

          varAssign <- impConstructorParadigm.imperativeCapabilities.assignVar(
            msgField,
            msgArg
          )

          _ <- addBlockDefinitions(Seq(varAssign))

        } yield ()
      }

      def make_getter(): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
        import paradigm.methodBodyCapabilities._
        import ooParadigm.methodBodyCapabilities._
        import impParadigm.imperativeCapabilities._

        for {

          stringType <- toTargetLanguageType(TypeRep.String)
          _ <- setReturnType(stringType)

          selfRef <- selfReference()
          msg <- getMember(
            selfRef,
            names.mangle("msg")
          )

        } yield Some(msg)
      }

      for {

        stringType <- toTargetLanguageType(TypeRep.String)
        _ <- addConstructor(make_constructor)
        _ <- addField(names.mangle("msg"), stringType)
        _ <- addMethod(names.mangle("getMsg"), make_getter())

    } yield ()
  }

  def make_init_data_stmts(): Generator[MethodBodyContext, Option[Expression]] = {

    import array.arrayCapabilities._

    val layout: GridLayout = domain.layout

    for {

      stringType <- paradigm.methodBodyCapabilities.toTargetLanguageType(TypeRep.String)
      elementType <- ooParadigm.methodBodyCapabilities.findClass(
        names.mangle("TextElement")
      )

      self <- ooParadigm.methodBodyCapabilities.selfReference()

      elementArry <- array.arrayCapabilities.create(
        elementType,
        Seq.empty,
        Some(layout.cols * layout.rows)
      )

      elementsField <- ooParadigm.methodBodyCapabilities.getMember(
        self,
        names.mangle("elements")
      )

      varAssign <- impParadigm.imperativeCapabilities.assignVar(
        elementsField,
        elementArry
      )

      _ <- paradigm.methodBodyCapabilities.addBlockDefinitions(Seq(varAssign))

      _ <- forEachWithIndex(layout.elements) { (elem: Element, index: Int) =>
        for {

          msg <- paradigm.methodBodyCapabilities.reify(
            TypeRep.String,
            elem.asInstanceOf[Text].msg
          )

          textElementInst <- make_class_instantiation_floating(
            "TextElement",
            Seq(msg)
          )

          pos <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, index)

          arraySetExpr <- array.arrayCapabilities.set(
            elementsField,
            pos,
            textElementInst
          )

          arraySetExprLifted <- impParadigm.imperativeCapabilities.liftExpression(arraySetExpr)
          _ <- paradigm.methodBodyCapabilities.addBlockDefinitions(Seq(arraySetExprLifted))

        } yield (arraySetExpr)
      }
    } yield (None)
  }


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
  def make_application_class(clazzName: String): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    import ooParadigm.methodBodyCapabilities._

    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._

      for {
        exceptionClass <- findRawClass(names.mangle("Exception"))
        pt <- findRawClass(names.mangle("Application"))
        _ <- addParent(pt)
        _ <- addMethod(initFuncName, make_init(),true, Seq(exceptionClass))
        _ <- addMethod(startFuncName, make_start_func())
        _ <- addMethod(stopFuncName, make_stop_func(), true, Seq(exceptionClass))
        _ <- addMethod(mainFuncName, make_main_func())
      } yield ()
    }

    val makeUnit: Generator[CompilationUnitContext, Unit] = {

      for {

        _ <- unitTemplating.templatingCapabilities.loadFragment(
          this.getClass.getResource("/GUI/Target_JavaFX/Imports.java")
        )

        _ <- ooParadigm.compilationUnitCapabilities.addClass(
          names.mangle("JavaFXApplication"),
          makeClass
        )


      } yield()
    }

    for {

      _ <- addClassToProject(
        make_text_element_class,
        names.mangle("TextElement")
      )

      _ <- paradigm.projectCapabilities.addCompilationUnit(
        makeUnit
      )

    } yield ()

  }

  def implement(): Generator[paradigm.ProjectContext, Unit] = {
    for {
      _ <- make_application_class("JavaFXBasicApplication")
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
  (
    _classTemplating: Templating.WithBase[oo.ClassContext, base.MethodBodyContext, base.type],
    _unitTemplating: Templating.WithBase[base.CompilationUnitContext, base.MethodBodyContext, base.type]
  )(
    _domain: GUIDomain
  )(impConstructor: Imperative.WithBase[oo.ConstructorContext, base.type])
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
      override val impConstructorParadigm: Imperative.WithBase[ooParadigm.ConstructorContext, paradigm.type] = impConstructor
      override val ffiAssertions: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type] = ffiassert
      override val ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type] = ffiequal
      override val classTemplating: Templating.WithBase[ooParadigm.ClassContext, paradigm.MethodBodyContext, paradigm.type] = _classTemplating
      override val unitTemplating: Templating.WithBase[paradigm.CompilationUnitContext, paradigm.MethodBodyContext, paradigm.type] = _unitTemplating
    }
}


