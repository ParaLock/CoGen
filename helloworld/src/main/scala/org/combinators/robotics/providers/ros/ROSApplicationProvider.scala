package org.combinators.gui.providers.ros


import org.combinators.common._
import org.combinators.ep.domain.abstractions.{DataType, DataTypeCase, TypeRep}
import org.combinators.ep.generator.Command.{Generator, lift}
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.{forEach, forEachWithIndex}
import org.combinators.ep.generator.paradigm.{AnyParadigm, FindClass, ObjectOriented}
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.{AbstractSyntax, Command, NameProvider, Understands}
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Arrays, Assertions, Console, Equality, Exceptions}
import org.combinators.robotics.domain_model.ros.Node
import org.combinators.robotics.examples.RoboticsDomain
import org.combinators.robotics.domain_model.ros._


trait ROSApplicationProvider extends BaseProvider {

  import paradigm._
  import syntax._
  import ooParadigm._

  val domain: RoboticsDomain

  lazy val initFuncName = names.mangle("init")
  lazy val startFuncName = names.mangle("start")
  lazy val stopFuncName = names.mangle("stop")
  lazy val mainFuncName = names.mangle("main")
  lazy val testWindowName = names.mangle("windowTest")

  val exceptions: Exceptions.WithBase[paradigm.MethodBodyContext,paradigm.type]

  def instantiate(baseTpe: DataType, tpeCase: DataTypeCase, args: Expression*): Generator[MethodBodyContext, Expression] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    for {
      rt <- findClass(names.mangle(names.conceptNameOf(tpeCase)))
      _ <- resolveAndAddImport(rt)

      res <- instantiateObject(rt, args)
    } yield res
  }

  def make_ros_node_constructor(node: Node): Generator[ConstructorContext, Unit] = {

    import constructorCapabilities._

    for {

      self <- selfReference()

      _ <- make_method_call_in_constructor(self, "preinitCommon", Seq.empty)
      _ <- make_method_call_in_constructor(self, "preinitRole", Seq.empty)

    } yield(None)
  }

  def make_ros_node_name_getter(): Generator[ClassContext, Unit] = {

    import ooParadigm.classCapabilities._

    def make_method(): Generator[MethodBodyContext, Option[Expression]] = {
      import ooParadigm.methodBodyCapabilities._
      import impParadigm.imperativeCapabilities._
      import paradigm.methodBodyCapabilities._

      for {

        self <- selfReference()
        nodeNameVar <- getMember(self, names.mangle("nodeName"))

        graphNameCls <- findClass(names.mangle("GraphName"))
        _ <- setReturnType(graphNameCls)
        getNameFunc <- get_static_method_call(
          graphNameCls,
          "of",
          Seq(nodeNameVar)
        )

      } yield (Some(getNameFunc))

    }

    for {

      _ <- addMethod(names.mangle("getDefaultNodeName"), make_method())

    } yield (None)

//    public
//    final GraphName getDefaultNodeName () {
//      return GraphName.of(this.rosNodeName);
//    }

  }

  def make_ros_node_role_init(node: Node): Generator[MethodBodyContext, Unit] = {
      import ooParadigm.methodBodyCapabilities._
      import impParadigm.imperativeCapabilities._
      import paradigm.methodBodyCapabilities._

      val roleType = node.role.getClass.getSimpleName()

      for {

        _ <- roleType match {
          case "Client" => for {

            _ <- print_message("client stuff")

          } yield(None)
          case "Server" => for {


            _ <- print_message("server stuff")

          } yield (None)
          case "Publisher" => for {


            _ <- print_message("Publisher stuff")

          } yield (None)
          case "Subscriber" => for {


            _ <- print_message("Subscriber stuff")

          } yield (None)
          case _ => for {
            _ <- print_message("Unknown role type")
          } yield (None)
        }


      } yield (None)

  }

  def make_ros_node_onstart(node: Node): Generator[ClassContext, Unit] = {
    import ooParadigm.classCapabilities._

    def make_method(): Generator[MethodBodyContext, Option[Expression]] = {
      import ooParadigm.methodBodyCapabilities._
      import impParadigm.imperativeCapabilities._
      import paradigm.methodBodyCapabilities._

      for {

        self <- selfReference()
        unitType <- toTargetLanguageType(TypeRep.Unit)
        _ <- setReturnType(unitType)


        _ <- make_ros_node_role_init(node)
      } yield (None)

    }

    for {

      _ <- addMethod(names.mangle("onStart"), make_method())

    } yield (None)
  }

  def make_ros_node_common_preinit(node: Node): Generator[ClassContext, Unit] = {
    import ooParadigm.classCapabilities._

    def make_method(): Generator[MethodBodyContext, Option[Expression]] = {
      import ooParadigm.methodBodyCapabilities._
      import impParadigm.imperativeCapabilities._
      import paradigm.methodBodyCapabilities._

      for {

        self <- selfReference()

        unitType <- toTargetLanguageType(TypeRep.Unit)
        _ <- setReturnType(unitType)

        nodeName <-  paradigm.methodBodyCapabilities.reify(
          TypeRep.String,
          node.name
        )

        _ <- make_member_assignment(
          nodeName,
          "nodeName"
        )

        nodeNameVar <- getMember(self, names.mangle("nodeName"))

        preconditionsCls <- findClass(names.mangle("Preconditions"))
        stringUtlsCls <- findClass(names.mangle("StringUtils"))

        isNotBlankFunc <- get_static_method_call(
          stringUtlsCls,
          "isNotBlank",
          Seq(nodeNameVar)
        )

        _ <- make_static_method_call(
          preconditionsCls,
          "checkArgument",
          Seq(isNotBlankFunc)
        )


      } yield (None)

    }

    for {

      _ <- addMethod(names.mangle("preinitCommon"), make_method())

    } yield(None)
  }

  def make_ros_node_role_preinit(node: Node): Generator[ClassContext, Unit] = {
    import ooParadigm.classCapabilities._

    def make_method(): Generator[MethodBodyContext, Option[Expression]] = {
      import ooParadigm.methodBodyCapabilities._
      import impParadigm.imperativeCapabilities._
      import paradigm.methodBodyCapabilities._

      val roleType = node.role.getClass.getSimpleName()

      for {

        self <- selfReference()

        unitType <- toTargetLanguageType(TypeRep.Unit)
        _ <- setReturnType(unitType)

        varInfo <- roleType match {
          case "Client" =>     for { _ <- noop() } yield ("rosServiceName", node.role.asInstanceOf[roles.Client].serviceName)
          case "Server" =>     for { _ <- noop() } yield ("rosServiceName", node.role.asInstanceOf[roles.Server].serviceName)
          case "Publisher" =>  for { _ <- noop() } yield ("rosTopicName", node.role.asInstanceOf[roles.Publisher].topic)
          case "Subscriber" => for { _ <- noop() } yield ("rosTopicName",  node.role.asInstanceOf[roles.Subscriber].topic)
          case _ =>            for { _ <- noop() } yield ("placeholder", "placeholder")
        }

        varVal <- paradigm.methodBodyCapabilities.reify(
          TypeRep.String,
          varInfo._2
        )

        _ <- make_member_assignment(
          varVal,
          varInfo._1
        )

        varName <- getMember(self, names.mangle(varInfo._1))

        preconditionsCls <- findClass(names.mangle("Preconditions"))
        stringUtlsCls <- findClass(names.mangle("StringUtils"))

        isNotBlankFunc <- get_static_method_call(
          stringUtlsCls,
          "isNotBlank",
          Seq(varName)
        )

        _ <- make_static_method_call(
          preconditionsCls,
          "checkArgument",
          Seq(isNotBlankFunc)
        )


      } yield (None)

    }

    for {
      _ <- addMethod(names.mangle("preinitRole"), make_method())
    } yield (None)

  }

 def make_ros_node(node: Node): Generator[ClassContext, Unit] = {


   import classCapabilities._
   for {

     stringType <- toTargetLanguageType(TypeRep.String)

     appClass <- findClass(names.mangle("AbstractNodeMain"))
     _ <- addParent(appClass)

     _ <- addField(
       names.mangle("nodeName"),
       stringType,
     )

     _ <- addConstructor(make_ros_node_constructor(node))
     _ <- make_ros_node_common_preinit(node)
     _ <- make_ros_node_role_preinit(node)

     _ <- make_ros_node_onstart(node)
     _ <- make_ros_node_name_getter()


   } yield(None)

 }
//

//  def make_ros_node_subscriber(role: roles.Subscriber): Generator[ClassContext, Unit] = {
//
//  }
//
//  def make_ros_node_publisher(role: roles.Publisher): Generator[ClassContext, Unit] = {
//
//  }
//
//  def make_ros_node_client(role: roles.Client): Generator[ClassContext, Unit] = {
//
//  }
//
//  def make_ros_node_server(role: roles.Server): Generator[ClassContext, Unit] = {
//
//  }

  def make_main_func(): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import impParadigm.imperativeCapabilities._
    import exceptions.exceptionCapabilities._
    for {

      // Make signature
      _ <- setStatic()
      arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.String))
      _ <- resolveAndAddImport(arrayType)
      unitType <- toTargetLanguageType(TypeRep.Unit)
      intType <- toTargetLanguageType(TypeRep.Int)
      _ <- setReturnType(unitType)
      _ <- setParameters(Seq((names.mangle("args"), arrayType)))
      // --------------

      self <- selfReference()

      testCls <- findClass(names.mangle("test1"))
      testClsExpr <- toStaticTypeExpression(testCls)
      methodRef <- getMethodReference(self, names.mangle("callback"))

      myMethod <- getMember(self, names.mangle("myMethod1"))

      _ <- make_method_call(myMethod, Seq(methodRef))

      one_hundred <- paradigm.methodBodyCapabilities.reify(TypeRep.Double, 100)
      two_hundred <- paradigm.methodBodyCapabilities.reify(TypeRep.Double, 200)
      addExpr <- ffiArithmetic.arithmeticCapabilities.add(one_hundred, two_hundred)


      //      exceptionCls <- findClass(names.mangle("Exception1"))
//      exceptionCls2 <- findClass(names.mangle("Exception2"))
//
//      _ <- addExceptionHandler(
//        for {
//          _ <- print_message("in the try block")
//        } yield None,
//        Some(addExpr),
//        Seq(
//          (exceptionCls, names.mangle("e"),
//            for {
//              _ <- print_message("in the first catch")
//            } yield None
//          ),
//          (exceptionCls2, names.mangle("e"),
//            for {
//              _ <- print_message("in the second catch")
//            } yield None
//          )
//        ),
//        Some(
//          for {
//            _ <- print_message("in the finally block")
//          } yield None
//        )
//      )

    } yield None
  }

  def make_ros_nodes(): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    for {
      _ <- forEach(domain.nodes) { (node: Node) =>
        for {
          _ <- addClassToProject(
              make_ros_node(node),
              names.mangle("ROSNode" + node.name)
          )
        } yield ()
      }
    } yield ()
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
      _ <- make_ros_nodes()
    } yield ()
  }

}

object ROSApplicationProvider {
  type WithParadigm[P <: AnyParadigm] = ROSApplicationProvider { val paradigm: P }
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
   ffiequal: Equality.WithBase[base.MethodBodyContext, base.type]
  )
  (
    impConstructor: Imperative.WithBase[oo.ConstructorContext, base.type],
    _exceptions: Exceptions.WithBase[base.MethodBodyContext,base.type]
  )
  (_domain: RoboticsDomain)
  : ROSApplicationProvider.WithParadigm[base.type] =
    new ROSApplicationProvider {
      override val paradigm: base.type = base
      val impParadigm: imp.type = imp
      //val impConstructorParadigm: impConstructor.type = impConstructor
      override val names: NameProvider[paradigm.syntax.Name] = nameProvider
      override val ooParadigm: oo.type = oo
      override val impConstructorParadigm: Imperative.WithBase[ooParadigm.ConstructorContext, paradigm.type] = impConstructor
      override val console: Console.WithBase[base.MethodBodyContext, paradigm.type] = con
      override val array: Arrays.WithBase[base.MethodBodyContext, paradigm.type] = arr
      override val asserts: Assertions.WithBase[base.MethodBodyContext, paradigm.type] = assertsIn
      override val eqls: Equality.WithBase[base.MethodBodyContext, paradigm.type] = eqlsIn
      override val ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Int] = ffiarith
      override val ffiAssertions: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type] = ffiassert
      override val ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type] = ffiequal
      override val exceptions: Exceptions.WithBase[paradigm.MethodBodyContext,paradigm.type] = _exceptions
      override val domain: RoboticsDomain = _domain
    }
}


