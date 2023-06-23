package org.combinators.gui.providers.ros


import org.combinators.common._
import org.combinators.ep.domain.abstractions.{DataType, DataTypeCase, TypeRep}
import org.combinators.ep.generator.Command.{Generator, lift}
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.{forEach, forEachWithIndex}
import org.combinators.ep.generator.paradigm.{AnyParadigm, FindClass, Generics, ObjectOriented, ParametricPolymorphism, Templating}
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.{AbstractSyntax, Command, NameProvider, Understands}
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Arrays, Assertions, Console, Equality, Exceptions}
import org.combinators.robotics.domain_model.ros.Node
import org.combinators.robotics.examples.RoboticsDomain
import org.combinators.robotics.domain_model.ros._

import scala.reflect.runtime.universe._
import java.io.File
import java.nio.file.Paths


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
  val polymorphics: ParametricPolymorphism.WithBase[paradigm.type]
  val generics: Generics.WithBase[paradigm.type, ooParadigm.type, polymorphics.type]
  val classTemplating: Templating.WithBase[ooParadigm.ClassContext, paradigm.MethodBodyContext, paradigm.type]
  val unitTemplating: Templating.WithBase[paradigm.CompilationUnitContext, paradigm.MethodBodyContext, paradigm.type]

  def getClassName(cls: Class[_]): String = {
    val className = cls.getSimpleName
    className.stripSuffix("$")
  }

  def getBaseName(filepath: String): String = {
    val filename = Paths.get(filepath).getFileName.toString
    val parts = filename.split("\\.")
    if (parts.length > 1) parts.init.mkString(".") else filename
  }

  def scala_type_to_ros_type(tpe: String): String = {
    tpe match {
      case "String" => "string"
      case "long" => "int64"
    }
  }

  def get_payload_msg_name(requestCls: Class[_], responseCls: Class[_]): String = {

    responseCls.getSimpleName.split("Response").mkString.stripSuffix("$")
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

  //ROS_NODE_MISC *******************************************************************************************
  def make_ros_node_constructor(node: Node): Generator[ConstructorContext, Unit] = {

    import constructorCapabilities._

    for {

      self <- selfReference()
      _ <- make_method_call_in_constructor(self, "init", Seq.empty)

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
        getNameFunc <- make_static_method_call(
          graphNameCls,
          "of",
          Seq(nodeNameVar),
          false
        )

      } yield (Some(getNameFunc))

    }

    for {
      _ <- addMethod(names.mangle("getDefaultNodeName"), make_method())
    } yield (None)

  }
  //**********************************************************************************************************

  //ROS_NODE_ROLE_ONSTART ************************************************************************************
  def make_ros_node_client_role_onstart(node: Node): Generator[MethodBodyContext, Unit] = {

    import ooParadigm.methodBodyCapabilities._
    import impParadigm.imperativeCapabilities._
    import paradigm.methodBodyCapabilities._
    import exceptions.exceptionCapabilities._
    import generics.methodBodyCapabilities._

    var role = node.role.asInstanceOf[roles.Client]
    var excepName = names.mangle("serviceNotFoundException")

    var fields = get_role_fields(node)

    for {

      logCls <- findClass(names.mangle("Log"))

      args <- getArguments()
      (_, _, connectedNodeParam) = args.head

      getLogFunc <- make_method_call(
        connectedNodeParam,
        "getLog",
        Seq.empty,
        false
      )

      logVar <- declareVar(names.mangle("log"), logCls, Some(getLogFunc))

      exceptionCls <- findClass(names.mangle("ServiceNotFoundException"))

      _ <- addExceptionHandler(
        for {

          self <- selfReference()
          varName <- getMember(self, names.mangle(fields._1))

          serviceClientcls <- findClass(names.mangle("ServiceClient"))
          requestCls <- findClass(names.mangle(getClassName(role.requestMsgType)))
          responseCls <- findClass(names.mangle(getClassName(role.responseMsgType)))


          msgType <- findClass(
            names.mangle(get_payload_msg_name(role.responseMsgType, role.requestMsgType))
          )

          msgTypeType <- get_static_class_member(
            msgType,
            "_TYPE"
          )

          clientCreateExpr <- make_method_call(
            connectedNodeParam,
            "newServiceClient",
            Seq(varName, msgTypeType),
            false
          )
          serviceClientclsWithParams <- applyType(
            serviceClientcls,
            Seq(requestCls, responseCls)
          )

          serviceClientVar <- declareVar(
            names.mangle("serviceClient"),
            serviceClientclsWithParams,
            Some(clientCreateExpr)
          )

          _ <- make_method_call(
            serviceClientVar,
            "isConnected",
            Seq.empty,
            true
          )

          listenerCls <- findClass(names.mangle("ROSNode" + node.name + "Listener"))

          listenerVar <- make_class_instantiation(
            listenerCls,
            "responseListener",
            Seq(connectedNodeParam)

          )

          loopClsInst <- make_class_instantiation_floating(
            "ROSNode" + node.name + "Loop",
            Seq(serviceClientVar, logVar, listenerVar),
            false
          )

          executeCancellableLoopFunc <- getMember(
            connectedNodeParam,
            names.mangle("executeCancellableLoop")
          )

          _ <- make_method_call(
            executeCancellableLoopFunc,
            Seq(loopClsInst),
            true
          )

//          connectedNode.executeCancellableLoop(
//          new ROSJavaClientNodeLoop(serviceClient, log, responseListener)
//          );

        } yield None,
        None,
        Seq(
          (exceptionCls, excepName,
            for {

              excepName <- nameToExpression(excepName)
              exceptionUtlsCls <- findClass(names.mangle("ExceptionUtils"))

              getStackTraceFunc <- make_static_method_call(
                exceptionUtlsCls,
                "getStackTrace",
                Seq(excepName),
                false
              )

              _ <- make_method_call(
                logVar,
                "error",
                Seq(getStackTraceFunc),
                true
              )

              rosRuntimeExceptionCls <- findClass(names.mangle("RosRuntimeException"))
              _ <- resolveAndAddImport(rosRuntimeExceptionCls)

              excep <- exceptions.exceptionCapabilities.raise(
                excepName,
                Some(rosRuntimeExceptionCls)
              )

              _ <- addBlockDefinitions(Seq(excep))

            } yield None
          )
        )
      )


    } yield()


  }

  def make_ros_node_server_role_onstart(node: Node): Generator[MethodBodyContext, Unit] = {

    import ooParadigm.methodBodyCapabilities._
    import impParadigm.imperativeCapabilities._
    import paradigm.methodBodyCapabilities._
    import exceptions.exceptionCapabilities._
    import generics.methodBodyCapabilities._

    var role = node.role.asInstanceOf[roles.Server]
    var fields = get_role_fields(node)

    for {

      args <- getArguments()
      (_, _, connectedNodeParam) = args.head

      getLogFunc <- getMember(
        connectedNodeParam,
        names.mangle("getLog")
      )

      getLogCall <- make_method_call(
        getLogFunc,
        Seq.empty,
        false
      )

      _ <- make_field_assignment(
        getLogCall,
        "log"
      )

      self <- selfReference()
      varName <- getMember(self, names.mangle(fields._1))

      serviceServercls <- findClass(names.mangle("ServiceServer"))
      requestCls <- findClass(names.mangle(getClassName(role.requestMsgType)))
      responseCls <- findClass(names.mangle(getClassName(role.responseMsgType)))

      msgType <- findClass(
        names.mangle(get_payload_msg_name(role.responseMsgType, role.requestMsgType))
      )

      msgTypeType <- get_static_class_member(
        msgType,
        "_TYPE"
      )

      methodRef <- getMethodReference(
        self,
        names.mangle("onRequest")
      )

      clientCreateExpr <- make_method_call(
        connectedNodeParam,
        "newServiceServer",
        Seq(varName, msgTypeType, methodRef),
        false
      )

      serviceServerclsWithParams <- applyType(
        serviceServercls,
        Seq(requestCls, responseCls)
      )

      serviceServerVar <- declareVar(
        names.mangle("serviceServer"),
        serviceServerclsWithParams,
        Some(clientCreateExpr)
      )

      logVar <- getMember(
        self,
        names.mangle("log")
      )

      msg1 <- paradigm.methodBodyCapabilities.reify(
        TypeRep.String,
        "Created ROS Service Server["
      )

      msg2 <- paradigm.methodBodyCapabilities.reify(
        TypeRep.String,
        "] in URI:"
      )


      getNameFunc <- make_method_call(
        serviceServerVar,
        "getName",
        Seq.empty,
        false
      )

      getUriFunc <- make_method_call(
        serviceServerVar,
        "getUri",
        Seq.empty,
        false
      )

      msgExpr1 <- ffiArithmetic.arithmeticCapabilities.add(
        msg1,
        getNameFunc
      )

      msgExpr2 <- ffiArithmetic.arithmeticCapabilities.add(
        msgExpr1,
        msg2
      )

      msgExpr3 <- ffiArithmetic.arithmeticCapabilities.add(
        msgExpr2,
        getUriFunc
      )

      _ <- make_method_call(
        logVar,
        "info",
        Seq(msgExpr3),
        true
      )

    } yield (None)


  }

  def make_ros_node_publisher_role_onstart(node: Node): Generator[MethodBodyContext, Unit] = {
    import ooParadigm.methodBodyCapabilities._
    import impParadigm.imperativeCapabilities._
    import paradigm.methodBodyCapabilities._
    import exceptions.exceptionCapabilities._
    import generics.methodBodyCapabilities._

    var fields = get_role_fields(node)
    var role = node.role.asInstanceOf[roles.Publisher]
    for {

      self <- selfReference()

      logCls <- findClass(names.mangle("Log"))

      args <- getArguments()
      (_, _, connectedNodeParam) = args.head

      getLogFunc <- make_method_call(
        connectedNodeParam,
        "getLog",
        Seq.empty,
        false
      )

      logVar <- declareVar(names.mangle("log"), logCls, Some(getLogFunc))

      publisherCls <- findClass(
        names.mangle("Publisher")
      )

      msgType <- findClass(
        names.mangle(getClassName(role.msgType))
      )

      msgTypeType <- get_static_class_member(
        msgType,
        "_TYPE"
      )

      varName <- getMember(self, names.mangle(fields._1))

      publisherCreateExpr <- make_method_call(
        connectedNodeParam,
        "newPublisher",
        Seq(varName, msgTypeType),
        false
      )

      publisherClsWithParams <- applyType(
        publisherCls,
        Seq(msgType)
      )

      publisherVar <- declareVar(
        names.mangle("publisher"),
        publisherClsWithParams,
        Some(publisherCreateExpr)
      )

      loopInst <- make_class_instantiation_floating(
        "ROSNode" + node.name + "Loop",
        Seq(logVar, publisherVar),
        false
      )

      _ <- make_method_call(
        connectedNodeParam,
        "executeCancellableLoop",
        Seq(loopInst),
        true
      )

    } yield(None)

  }

  def make_ros_node_subscriber_role_onstart(node: Node): Generator[MethodBodyContext, Unit] = {
    import ooParadigm.methodBodyCapabilities._
    import impParadigm.imperativeCapabilities._
    import paradigm.methodBodyCapabilities._
    import exceptions.exceptionCapabilities._
    import generics.methodBodyCapabilities._

    var fields = get_role_fields(node)
    var role = node.role.asInstanceOf[roles.Subscriber]

    for {

      self <- selfReference()

      logCls <- findClass(names.mangle("Log"))

      args <- getArguments()
      (_, _, connectedNodeParam) = args.head

      getLogFunc <- make_method_call(
        connectedNodeParam,
        "getLog",
        Seq.empty,
        false
      )

      logVar <- declareVar(names.mangle("log"), logCls, Some(getLogFunc))

      subscriberCls <- findClass(
        names.mangle("Subscriber")
      )

      msgType <- findClass(
        names.mangle(getClassName(role.msgType))
      )

      msgTypeType <- get_static_class_member(
        msgType,
        "_TYPE"
      )

      varName <- getMember(self, names.mangle(fields._1))

      subscriberCreateExpr <- make_method_call(
        connectedNodeParam,
        "newSubscriber",
        Seq(varName, msgTypeType),
        false
      )

      subscriberClsWithParams <- applyType(
        subscriberCls,
        Seq(msgType)
      )

      subscriberVar <- declareVar(
        names.mangle("subscriber"),
        subscriberClsWithParams,
        Some(subscriberCreateExpr)
      )

      listenerInst <- make_class_instantiation_floating(
        "ROSNode" + node.name + "Listener",
        Seq(logVar),
        false
      )

      _ <- make_method_call(
        subscriberVar,
        "addMessageListener",
        Seq(listenerInst),
        true
      )

    } yield (None)

  }

  //
  def make_ros_node_role_onstart(node: Node): Generator[MethodBodyContext, Unit] = {
      import ooParadigm.methodBodyCapabilities._
      import impParadigm.imperativeCapabilities._
      import paradigm.methodBodyCapabilities._

      val roleType = node.role.getClass.getSimpleName()

      for {

        _ <- roleType match {
          case "Client" => for { _ <- make_ros_node_client_role_onstart(node) } yield(None)
          case "Server" => for { _ <- make_ros_node_server_role_onstart(node) } yield (None)
          case "Publisher" => for { _ <- make_ros_node_publisher_role_onstart(node) } yield (None)
          case "Subscriber" => for { _ <- make_ros_node_subscriber_role_onstart(node) } yield (None)
          case _ => for { _ <- print_message("Unknown role type") } yield (None)
        }

      } yield (None)

  }

  //**********************************************************************************************************
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

        connectedNodeCls <- findClass(names.mangle("ConnectedNode"))

        _ <- setParameters(
          Seq((names.mangle("connectedNode"), connectedNodeCls))
        )

        _ <- make_ros_node_role_onstart(node)
      } yield (None)

    }

    for {

      _ <- addMethod(names.mangle("onStart"), make_method())

    } yield (None)
  }

  //ROS_NODE_INIT *****************************************************************************************

  def make_ros_node_init(node: Node): Generator[ClassContext, Unit] = {
    import ooParadigm.classCapabilities._

    val fields = get_role_fields(node)

    def make_init_role_fields(): Generator[ClassContext, Option[Expression]] = {
      import ooParadigm.classCapabilities._
      import impParadigm.imperativeCapabilities._
      for {

        stringType <- toTargetLanguageType(TypeRep.String)
        _ <- addField(names.mangle(fields._1), stringType)

      } yield(None)
    }

    def make_init_common_fields(): Generator[ClassContext, Option[Expression]] = {
      import ooParadigm.classCapabilities._
      import impParadigm.imperativeCapabilities._

      for {
        stringType <- toTargetLanguageType(TypeRep.String)
        _ <- addField(
          names.mangle("nodeName"),
          stringType,
        )
      } yield(None)
    }

    def make_init_role_stmts(): Generator[MethodBodyContext, Option[Expression]] = {
      import ooParadigm.methodBodyCapabilities._
      import impParadigm.imperativeCapabilities._
      import paradigm.methodBodyCapabilities._

      for {

        self <- selfReference()

        unitType <- toTargetLanguageType(TypeRep.Unit)
        _ <- setReturnType(unitType)

        varVal <- paradigm.methodBodyCapabilities.reify(
          TypeRep.String,
          fields._2
        )

        _ <- make_member_assignment(
          varVal,
          fields._1
        )

        varName <- getMember(self, names.mangle(fields._1))

        preconditionsCls <- findClass(names.mangle("Preconditions"))
        stringUtlsCls <- findClass(names.mangle("StringUtils"))

        isNotBlankFunc <- make_static_method_call(
          stringUtlsCls,
          "isNotBlank",
          Seq(varName),
          false
        )

        _ <- make_static_method_call(
          preconditionsCls,
          "checkArgument",
          Seq(isNotBlankFunc),
          true
        )

      } yield(None)

    }

    def make_init_common_stmts(): Generator[MethodBodyContext, Option[Expression]] = {
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

        isNotBlankFunc <- make_static_method_call(
          stringUtlsCls,
          "isNotBlank",
          Seq(nodeNameVar),
          false
        )

        _ <- make_static_method_call(
          preconditionsCls,
          "checkArgument",
          Seq(isNotBlankFunc),
          true
        )


      } yield (None)

    }

    for {
      _ <- make_init_common_fields()
      _ <- make_init_role_fields()

      _ <- addMethod(
        names.mangle("init"),
        for {

          _ <- make_init_common_stmts()
          _ <- make_init_role_stmts()

        } yield(None)
      )

    } yield(None)
  }

  //**********************************************************************************************************

  //ROS_NODE_ROLE_FIELDS *************************************************************************************
  def get_role_fields(node: Node): Tuple2[String, String] = {

    val roleType = node.role.getClass.getSimpleName()

    roleType match {
      case "Client" => ("rosServiceName", node.role.asInstanceOf[roles.Client].serviceName)
      case "Server" => ("rosServiceName", node.role.asInstanceOf[roles.Server].serviceName)
      case "Publisher" => ("rosTopicName", node.role.asInstanceOf[roles.Publisher].topic)
      case "Subscriber" => ("rosTopicName", node.role.asInstanceOf[roles.Subscriber].topic)
      case _ => ("placeholder", "placeholder")
    }
  }

  def make_ros_node_publisher_fields(node: Node): Generator[ClassContext, Unit] = {
    import classCapabilities._
    for {
      _ <- noop()
    } yield(None)
  }

  def make_ros_node_subscriber_fields(node: Node): Generator[ClassContext, Unit] = {
    import classCapabilities._
    for {
      _ <- noop()
    } yield (None)

  }

  def make_ros_node_server_fields(node: Node): Generator[ClassContext, Unit] = {

    import classCapabilities._

    for {
      logType <- findClass(names.mangle("Log"))
      _ <- addField(names.mangle("log"), logType)
    } yield (None)

  }

  def make_ros_node_client_fields(node: Node): Generator[ClassContext, Unit] = {
    import classCapabilities._

    for {
      _ <- noop()
    } yield (None)
  }

  def make_ros_node_role_fields(node: Node): Generator[ClassContext, Unit] = {

    import classCapabilities._
    val roleType = node.role.getClass.getSimpleName()

    for {

      _ <- roleType match {
        case "Client"     => for { _ <- make_ros_node_client_fields(node)        } yield (None)
        case "Server"     => for { _ <- make_ros_node_server_fields(node)        } yield (None)
        case "Publisher"  => for { _ <- make_ros_node_publisher_fields(node)     } yield (None)
        case "Subscriber" => for { _ <- make_ros_node_subscriber_fields(node)    } yield (None)
        case _            => for { _ <- noop()                                   } yield (None)
      }

    } yield (None)

  }

  //**********************************************************************************************************

  //ROS_NODE_ROLE_METHODS ************************************************************************************
  def make_ros_node_server_methods(node: Node): Generator[ClassContext, Unit] = {
    import classCapabilities._

    val role: roles.Server = node.role.asInstanceOf[roles.Server]
    var onRequestFragName: String = role.onRequestFragment
    val fragmentFile = new File(onRequestFragName)
    val fragmentClassName = fragmentFile.getName.split("\\.").dropRight(1).mkString(".")

    def make_onrequest_method(): Generator[MethodBodyContext, Option[Expression]] = {
      import paradigm.methodBodyCapabilities._
      import ooParadigm.methodBodyCapabilities._
      import impParadigm.imperativeCapabilities._
      import exceptions.exceptionCapabilities._
      for {

        unitType <- toTargetLanguageType(TypeRep.Unit)
        _ <- setReturnType(unitType)

        self <- selfReference()

        requestCls <- findClass(names.mangle(getClassName(role.requestMsgType)))
        responseCls <- findClass(names.mangle(getClassName(role.responseMsgType)))

        _ <- setParameters(Seq(
          (names.mangle("request"), requestCls),
          (names.mangle("response"), responseCls)
        ))

        params <- getArguments()

        (_, _, requestVar) = params(0)
        (_, _, responseVar) = params(1)


        logVar <- getMember(self, names.mangle("log"))

        cls <- findClass(names.mangle(fragmentClassName))
        _ <- make_static_method_call(
          cls,
          "run",
          Seq(logVar, requestVar, responseVar),
          true
        )

      } yield(None)
    }

    for {
      _ <- addMethod(
        names.mangle("onRequest"),
        make_onrequest_method()
      )

    } yield (None)
  }

  def make_ros_node_client_methods(node: Node): Generator[ClassContext, Unit] = {
    import classCapabilities._

    for {
      _ <- noop()
    } yield (None)
  }

  def make_ros_node_subscriber_methods(node: Node): Generator[ClassContext, Unit] = {
    import classCapabilities._

    for {
      _ <- noop()
    } yield (None)
  }

  def make_ros_node_publisher_methods(node: Node): Generator[ClassContext, Unit] = {
    import classCapabilities._

    for {
      _ <- noop()
    } yield (None)
  }

  def make_ros_node_role_methods(node: Node): Generator[ClassContext, Unit] = {

    import classCapabilities._
    val roleType = node.role.getClass.getSimpleName()

    for {

      _ <- roleType match {
        case "Client" => for {_ <- make_ros_node_client_methods(node)} yield (None)
        case "Server" => for {_ <- make_ros_node_server_methods(node)} yield (None)
        case "Publisher" => for {_ <- make_ros_node_publisher_methods(node)} yield (None)
        case "Subscriber" => for {_ <- make_ros_node_subscriber_methods(node)} yield (None)
        case _ => for {_ <- noop()} yield (None)
      }

    } yield (None)

  }

  //**********************************************************************************************************

  //ROS_NODE_LISTENER ****************************************************************************************

  def make_ros_node_subscriber_listener(node: Node): Generator[ClassContext, Unit] = {
    import classCapabilities._
    import generics.classCapabilities._


    val role: roles.Subscriber = node.role.asInstanceOf[roles.Subscriber]
    val fragmentFile = new File(role.onMsgFragment)
    val fragmentClassName = fragmentFile.getName.split("\\.").dropRight(1).mkString(".")

    def make_constructor(logCls: Type): Generator[ConstructorContext, Unit] = {
      import ooParadigm.constructorCapabilities._
      import impConstructorParadigm.imperativeCapabilities._

      for {

        self <- selfReference()

        _ <- setParameters(Seq(
          (names.mangle("_log"), logCls)
        ))

        logVar <- getMember(self, names.mangle("log"))

        params <- getArguments()
        (_, _, logParam) = params(0)

        var1 <- assignVar(logVar, logParam)
        _ <- addBlockDefinitions(Seq(var1))


      } yield (None)
    }

    def make_onmessage(node: Node): Generator[MethodBodyContext, Option[Expression]] = {
      import paradigm.methodBodyCapabilities._
      import ooParadigm.methodBodyCapabilities._
      import impParadigm.imperativeCapabilities._
      import exceptions.exceptionCapabilities._
      for {

        self <- selfReference()

        logVar <- getMember(self, names.mangle("log"))

        fragCls <- findClass(names.mangle(fragmentClassName))

        unitType <- toTargetLanguageType(TypeRep.Unit)
        _ <- setReturnType(unitType)

        msgType <- findClass(names.mangle(getClassName(role.msgType)))

        _ <- setParameters(Seq(
          (names.mangle("message"), msgType)
        ))

        params <- getArguments()
        (_, _, msgVar) = params(0)

        _ <- make_static_method_call(
          fragCls,
          "run",
          Seq(logVar, msgVar),
          true
        )




      } yield (None)
    }

    for {

      msgListenerCls <- findClass(names.mangle("MessageListener"))
      msgCls <- findClass(names.mangle(getClassName(role.msgType)))
      logCls <- findClass(names.mangle("Log"))

      messageListenerWithParams <- applyType(
        msgListenerCls,
        Seq(msgCls)
      )
      _ <- addImplemented(messageListenerWithParams)

      _ <- addField(names.mangle("log"), logCls)
      _ <- addConstructor(make_constructor(logCls))
      _ <- addMethod(names.mangle("onNewMessage"), make_onmessage(node))

    } yield (None)

  }


  def make_ros_node_client_listener(node: Node): Generator[ClassContext, Unit] = {
    import classCapabilities._
    import generics.classCapabilities._


    val role: roles.Client = node.role.asInstanceOf[roles.Client]
    val fragmentFile = new File(role.onResponseFragment)
    val fragmentClassName = fragmentFile.getName.split("\\.").dropRight(1).mkString(".")

    def make_constructor(connectedNodeCls: Type): Generator[ConstructorContext, Unit] = {
      import ooParadigm.constructorCapabilities._
      import impConstructorParadigm.imperativeCapabilities._

      for {

        self <- selfReference()

        _ <- setParameters(Seq(
          (names.mangle("_connectedNode"), connectedNodeCls)
        ))

        connectedNodeMemberVar <- getMember(self, names.mangle("connectedNode"))

        params <- getArguments()
        (_, _, connectedNodeVar) = params(0)

        var1 <- assignVar(connectedNodeMemberVar, connectedNodeVar)
        _ <- addBlockDefinitions(Seq(var1))


      } yield(None)
    }

    def make_on_failure(node: Node): Generator[MethodBodyContext, Option[Expression]] = {
      import paradigm.methodBodyCapabilities._
      import ooParadigm.methodBodyCapabilities._
      import impParadigm.imperativeCapabilities._
      import exceptions.exceptionCapabilities._
      for {

        unitType <- toTargetLanguageType(TypeRep.Unit)
        _ <- setReturnType(unitType)

        self <- selfReference()

        remoteException <- findClass(names.mangle("RemoteException"))

        _ <- setParameters(Seq(
          (names.mangle("remoteException"), remoteException)
        ))

        params <- getArguments()
        (_, _, remoteExceptionVar) = params(0)

        connectedNodeMemberVar <- getMember(self, names.mangle("connectedNode"))

        exceptionUtilsCls <- findClass(names.mangle("ExceptionUtils"))

        stackTraceInvoke <- make_static_method_call(
          exceptionUtilsCls,
          "getStackTrace",
          Seq(remoteExceptionVar),
          false
        )

        _ <- make_chained_method_call(
          connectedNodeMemberVar,
          "getLog",
          Seq.empty,
          "error",
          Seq(stackTraceInvoke)
        )


      } yield (None)
    }

    def make_on_success(node: Node): Generator[MethodBodyContext, Option[Expression]] = {
      import paradigm.methodBodyCapabilities._
      import ooParadigm.methodBodyCapabilities._
      import impParadigm.imperativeCapabilities._
      import exceptions.exceptionCapabilities._
      for {

        unitType <- toTargetLanguageType(TypeRep.Unit)
        _ <- setReturnType(unitType)

        responseCls <- findClass(names.mangle(getClassName(role.responseMsgType)))
        fragmentCls <- findClass(names.mangle(fragmentClassName))

        _ <- setParameters(Seq(
          (names.mangle("response"), responseCls)
        ))

        params <- getArguments()
        (_, _, responseVar) = params(0)

        self <- selfReference()
        connectedNodeMemberVar <- getMember(self, names.mangle("connectedNode"))

        getLogFunc <- make_method_call(
          connectedNodeMemberVar,
          "getLog",
          Seq.empty,
          false
        )

        _ <- make_static_method_call(
          fragmentCls,
          "run",
          Seq(getLogFunc, responseVar),
          true
        )


      } yield (None)
    }

    for {

      responseListenerCls <- findClass(names.mangle("ServiceResponseListener"))
      responseCls <- findClass(names.mangle(getClassName(role.responseMsgType)))
      connectedNodeCls <- findClass(names.mangle("ConnectedNode"))

      serviceClientClsWithParams <- applyType(
        responseListenerCls,
        Seq(responseCls)
      )
      _ <- addImplemented(serviceClientClsWithParams)

      _ <- addField(names.mangle("connectedNode"), connectedNodeCls)

      _ <- addConstructor(make_constructor(connectedNodeCls))
      _ <- addMethod(names.mangle("onSuccess"), make_on_success(node), true)
      _ <- addMethod(names.mangle("onFailure"), make_on_failure(node), true)

    } yield(None)

  }

  def make_ros_node_listener(node: Node): Generator[ClassContext, Unit] = {
    import classCapabilities._
    val roleType = node.role.getClass.getSimpleName()

    for {

      _ <- roleType match {
        case "Client" => for {_ <- make_ros_node_client_listener(node)} yield (None)
        case "Server" => for {_ <- noop()} yield (None)
        case "Publisher" => for {_ <- noop()} yield (None)
        case "Subscriber" => for {_ <- make_ros_node_subscriber_listener(node)} yield (None)
        case _ => for {_ <- noop()} yield (None)
      }

    } yield (None)
  }

  def make_ros_node_listeners(): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    for {
      _ <- forEach(domain.nodes) { (node: Node) =>
        for {
          _ <- if (node.role.isInstanceOf[roles.Client] || node.role.isInstanceOf[roles.Subscriber]) {
            addClassToProject(
              make_ros_node_listener(node),
              names.mangle("ROSNode" + node.name + "Listener")
            )
          } else {
            paradigm.projectCapabilities.noop()
          }
        } yield ()
      }
    } yield ()
  }

  //**********************************************************************************************************

  //ROS_NODE_LOOP ********************************************************************************************

  def make_ros_node_client_loop(node: Node): Generator[ClassContext, Unit] = {

    import classCapabilities._
    import generics.classCapabilities._

    val role: roles.Client = node.role.asInstanceOf[roles.Client]

    var fragmentClassName: String = ""
    if(node.loopFragment.isDefined){
      fragmentClassName = getBaseName(node.loopFragment.get)
    }

    println(fragmentClassName)

    def make_loop_method(): Generator[MethodBodyContext, Option[Expression]] = {
      import paradigm.methodBodyCapabilities._
      import ooParadigm.methodBodyCapabilities._
      import impParadigm.imperativeCapabilities._
      import exceptions.exceptionCapabilities._

      for {

        self <- selfReference()

        unitType <- toTargetLanguageType(TypeRep.Unit)
        _ <- setReturnType(unitType)

        serviceClientVar <- getMember(self, names.mangle("serviceClient"))
        newMsgFunc <- getMember(serviceClientVar, names.mangle("newMessage"))

        newMsgInvoke <- make_method_call(newMsgFunc, Seq.empty, false)

        requestCls <- findClass(names.mangle(getClassName(role.requestMsgType)))

        requestVar <- declareVar(
          names.mangle("request"),
          requestCls,
          Some(newMsgInvoke)
        )

        logVar <- getMember(self, names.mangle("log"))

        _ <- if(node.loopFragment.isDefined) {
          for {
            loopCls <- findClass(names.mangle(fragmentClassName))
            _ <- make_static_method_call(
              loopCls,
              "run",
              Seq(requestVar, logVar),
              true
            )
          } yield()
        } else {
          for {
            _ <- noop()
          } yield()
        }

        responseListenerVar <- getMember(self, names.mangle("responseListener"))

        _ <- make_method_call(
          serviceClientVar,
          "call",
          Seq(requestVar, responseListenerVar),
          true
        )

      } yield(None)
    }

    def make_constructor(serviceClientCls: Type, logCls: Type, listenerCls: Type): Generator[ConstructorContext, Unit] = {
      import ooParadigm.constructorCapabilities._
      import impConstructorParadigm.imperativeCapabilities._

      for {

        _ <- setParameters(Seq(
          (names.mangle("_serviceClient"), serviceClientCls),
          (names.mangle("_log"), logCls),
          (names.mangle("_responseListener"), listenerCls),
        ))

        self <- selfReference()

        logMember <- getMember(self, names.mangle("log"))
        serviceMember <- getMember(self, names.mangle("serviceClient"))
        responseListenerMember <- getMember(self, names.mangle("responseListener"))

        params <- getArguments()
        (_, _, _serviceClientVar) = params(0)
        (_, _, _logVar) = params(1)
        (_, _, _responseListenerVar) = params(2)

        var1 <- assignVar(logMember, _logVar)
        var2 <- assignVar(serviceMember, _serviceClientVar)
        var3 <- assignVar(responseListenerMember, _responseListenerVar)

        _ <- addBlockDefinitions(Seq(var1, var2, var3))

      } yield(None)
    }

    for {

      cancelableLoopCls <- findClass(names.mangle("CancellableLoop"))

      _ <- addParent(cancelableLoopCls)

      serviceClientCls <- findClass(names.mangle("ServiceClient"))
      requestCls <- findClass(names.mangle(getClassName(role.requestMsgType)))
      responseCls <- findClass(names.mangle(getClassName(role.responseMsgType)))
      serviceClientClsWithParams <- applyType (
        serviceClientCls,
        Seq(requestCls, responseCls)
      )

      _ <- addField(names.mangle("serviceClient"), serviceClientClsWithParams)

      logCls <- findClass(names.mangle("Log"))
      _ <- addField(names.mangle("log"), logCls)

      serviceResponseListenerCls <- findClass(names.mangle("ServiceResponseListener"))
      serviceResponseListenerClsWithParams <- applyType(
        serviceResponseListenerCls,
        Seq(responseCls)
      )
      _ <- addField(names.mangle("responseListener"), serviceResponseListenerClsWithParams)

      _ <- addConstructor(
        make_constructor(
          serviceClientClsWithParams,
          logCls,
          serviceResponseListenerClsWithParams
        )
      )

      interruptableExcep <- findClass(names.mangle("InterruptedException"))

      _ <- addMethod(
        names.mangle("loop"),
        make_loop_method(),
        true,
        Seq(interruptableExcep)
      )


    } yield(None)

  }

  def make_ros_node_publisher_loop(node: Node): Generator[ClassContext, Unit] = {

    import classCapabilities._
    import generics.classCapabilities._

    val role: roles.Publisher = node.role.asInstanceOf[roles.Publisher]

    var fragmentClassName = ""
    if(node.loopFragment.isDefined) {
      fragmentClassName = getBaseName(node.loopFragment.get)
    }

    def make_loop_method(): Generator[MethodBodyContext, Option[Expression]] = {
      import paradigm.methodBodyCapabilities._
      import ooParadigm.methodBodyCapabilities._
      import impParadigm.imperativeCapabilities._
      import exceptions.exceptionCapabilities._

      for {

        self <- selfReference()

        unitType <- toTargetLanguageType(TypeRep.Unit)
        _ <- setReturnType(unitType)

        publisherVar <- getMember(self, names.mangle("publisher"))
        newMsgFunc <- getMember(publisherVar, names.mangle("newMessage"))

        newMsgInvoke <- make_method_call(newMsgFunc, Seq.empty, false)

        logVar <- getMember(self, names.mangle("log"))

        messageCls <- findClass(names.mangle(getClassName(role.msgType)))

        messageVar <- declareVar(
          names.mangle("message"),
          messageCls,
          Some(newMsgInvoke)
        )

        _ <- if(node.loopFragment.isDefined) {
          for {
            loopCls <- findClass(names.mangle(fragmentClassName))
            _ <- make_static_method_call(
              loopCls,
              "run",
              Seq(logVar, messageVar),
              true
            )
          } yield()
        } else {
          for {
            _ <- noop()
          } yield ()
        }




        _ <- make_method_call(
          publisherVar,
          "publish",
          Seq(messageVar),
          true
        )

      } yield (None)
    }

    def make_constructor(publisherCls: Type, logCls: Type): Generator[ConstructorContext, Unit] = {
      import ooParadigm.constructorCapabilities._
      import impConstructorParadigm.imperativeCapabilities._

      for {

        _ <- setParameters(Seq(
          (names.mangle("_publisher"), publisherCls),
          (names.mangle("_log"), logCls)
        ))

        self <- selfReference()

        logMember <- getMember(self, names.mangle("log"))
        publisherMember <- getMember(self, names.mangle("publisher"))

        params <- getArguments()
        (_, _, _publisherVar) = params(0)
        (_, _, _logVar) = params(1)

        var1 <- assignVar(logMember, _logVar)
        var2 <- assignVar(publisherMember, _publisherVar)

        _ <- addBlockDefinitions(Seq(var1, var2))

      } yield (None)
    }

    for {

      cancelableLoopCls <- findClass(names.mangle("CancellableLoop"))

      _ <- addParent(cancelableLoopCls)

      serviceClientCls <- findClass(names.mangle("Publisher"))
      messageCls <- findClass(names.mangle(getClassName(role.msgType)))
      publisherClsWithParams <- applyType(
        serviceClientCls,
        Seq(messageCls)
      )

      _ <- addField(names.mangle("publisher"), publisherClsWithParams)

      logCls <- findClass(names.mangle("Log"))
      _ <- addField(names.mangle("log"), logCls)

      _ <- addConstructor(
        make_constructor(
          publisherClsWithParams,
          logCls
        )
      )

      interruptableExcep <- findClass(names.mangle("InterruptedException"))

      _ <- addMethod(
        names.mangle("loop"),
        make_loop_method(),
        true,
        Seq(interruptableExcep)
      )


    } yield (None)

  }

  def make_ros_node_loop(node: Node): Generator[ClassContext, Unit] = {
    import classCapabilities._
    val roleType = node.role.getClass.getSimpleName()

    for {

      _ <- roleType match {
        case "Client" => for {_ <- make_ros_node_client_loop(node)} yield (None)
        case "Server" => for {_ <- noop()} yield (None)
        case "Publisher" => for {_ <- make_ros_node_publisher_loop(node)} yield (None)
        case "Subscriber" => for {_ <- noop()} yield (None)
        case _ => for {_ <- noop()} yield (None)
      }

    } yield (None)
  }
  def make_ros_node_loops(): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    for {
      _ <- forEach(domain.nodes) { (node: Node) =>
        for {
          _ <- if (node.role.isInstanceOf[roles.Client] || node.role.isInstanceOf[roles.Publisher]) {
            addClassToProject(
              make_ros_node_loop(node),
              names.mangle("ROSNode" + node.name + "Loop")
            )
          } else {
            paradigm.projectCapabilities.noop()
          }
        } yield ()
      }
    } yield ()
  }

  //**********************************************************************************************************

  def make_main_class(): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._

    def make_node_configuration_method(): Generator[MethodBodyContext, Option[Expression]] = {
      import paradigm.methodBodyCapabilities._
      import ooParadigm.methodBodyCapabilities._
      import impParadigm.imperativeCapabilities._
      for {


        nodeConfigurationCls <- findClass(names.mangle("NodeConfiguration"))
        stringCls <- findClass(names.mangle("String"))
        _ <- resolveAndAddImport(stringCls)

        uriCls <- findClass(names.mangle("URI"))

        _ <- setStatic()
        _ <- setReturnType(nodeConfigurationCls)

        _ <- setParameters(
          Seq(
            (names.mangle("rosHostIp"), stringCls),
            (names.mangle("nodeName"), stringCls),
            (names.mangle("rosMasterUri"), uriCls)
          )
        )

        args <- getArguments()

        (_, _, rosHostIpVar) = args(0)
        (_, _, nodeNameVar) = args(1)
        (_, _, rosMasterUri) = args(2)

        newPublicFunc <- make_static_method_call(
          nodeConfigurationCls,
          "newPublic",
          Seq(rosHostIpVar),
          false
        )

        nodeConfigurationVar <- declareVar(names.mangle("nodeConfiguration"), nodeConfigurationCls, Some(newPublicFunc))

        setNodeNameMethod <- getMember(
          nodeConfigurationVar,
          names.mangle("setNodeName")
        )

        _ <- make_method_call(
          setNodeNameMethod,
          Seq(nodeNameVar),
          true
        )

        setMasterUriMethod <- getMember(
          nodeConfigurationVar,
          names.mangle("setMasterUri")
        )

        _ <- make_method_call(
          setMasterUriMethod,
          Seq(rosMasterUri),
          true
        )

      } yield (Some(nodeConfigurationVar))
    }

    def make_main_method(): Generator[MethodBodyContext, Option[Expression]] = {
      import paradigm.methodBodyCapabilities._
      import ooParadigm.methodBodyCapabilities._
      import impParadigm.imperativeCapabilities._
      import exceptions.exceptionCapabilities._


      var excepName = names.mangle("exception")

      for {

        unit <- toTargetLanguageType(TypeRep.Unit)
        stringCls <- findClass(names.mangle("String"))
        _ <- resolveAndAddImport(stringCls)
        stringArrayExpr <- array.arrayCapabilities.create(stringCls, Seq.empty, None)
        arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.String))

        _ <- setStatic()
        _ <- setReturnType(unit)

        _ <- setParameters(
          Seq((names.mangle("args"), arrayType))
        )

        intType <- toTargetLanguageType(TypeRep.Int)

        ipStr <- paradigm.methodBodyCapabilities.reify(
          TypeRep.String,
          "127.0.0.1"
        )
        rosHostPort <- paradigm.methodBodyCapabilities.reify(
          TypeRep.Int,
          11311
        )

        rosHostPortStr <- paradigm.methodBodyCapabilities.reify(
          TypeRep.String,
          "11311"
        )

        _ <- declareVar(
          names.mangle("rosHostIp"),
          stringCls,
          Some(ipStr)
        )
        rosHostPortVar <- declareVar(
          names.mangle("rosHostPort"),
          intType,
          Some(rosHostPort)
        )

        rosCoreCls <- findClass(names.mangle("RosCore"))
        newPublicFunc <- make_static_method_call(
          rosCoreCls,
          "newPublic",
          Seq(rosHostPortVar),
          false
        )

        rosCoreVar <- declareVar(
          names.mangle("rosCore"),
          rosCoreCls,
          Some(newPublicFunc)
        )

        _ <- make_method_call(
          rosCoreVar,
          "start",
          Seq.empty,
          true
        )

        self <- selfReference()
        systemCls <- findClass(names.mangle("System"))
        exceptionCls <- findClass(names.mangle("Exception"))

        _ <- addExceptionHandler(
          for {

            str0 <- paradigm.methodBodyCapabilities.reify(
              TypeRep.String,
              ":"
            )
            str1 <- paradigm.methodBodyCapabilities.reify(
              TypeRep.String,
              "http://"
            )

            uriStr1 <- ffiArithmetic.arithmeticCapabilities.add(
              str1, ipStr
            )

            uriStr2 <- ffiArithmetic.arithmeticCapabilities.add(
              uriStr1, str0
            )

            uriStr3 <- ffiArithmetic.arithmeticCapabilities.add(
              uriStr2, rosHostPortStr
            )

            uriCls <- findClass(names.mangle("URI"))
            uriInst <- make_class_instantiation_floating(
              "URI",
              Seq(uriStr3)
            )
            rosMasterUriVar <- declareVar(names.mangle("rosMasterUri"), uriCls, Some(uriInst))


            timeUnit <- findClass(names.mangle("TimeUnit"))
            milli <- get_static_class_member(timeUnit,"MILLISECONDS")
            val200 <- paradigm.methodBodyCapabilities.reify(
              TypeRep.Int,
              2000
            )
            boolean <- toTargetLanguageType(TypeRep.Boolean)
            awaitStartFunc <- make_method_call(rosCoreVar, "awaitStart", Seq(val200, milli), false)
            startedVar <- declareVar(names.mangle("started"), boolean, Some(awaitStartFunc))


            ifStmt <- ifThenElse(
              startedVar,
              for {
                nodeMainExecutorCls <- findClass(names.mangle("NodeMainExecutor"))
                newDefaultMainExecutorCls <- findClass(names.mangle("DefaultNodeMainExecutor"))

                newDefaultMainExecutorFunc <- make_static_method_call(
                  newDefaultMainExecutorCls,
                  "newDefault",
                  Seq.empty,
                  false
                )

                nodeMainExecutorVar <- declareVar(
                  names.mangle("nodeMainExecutor"),
                  nodeMainExecutorCls,
                  Some(newDefaultMainExecutorFunc)
                )

                _ <- forEach(domain.nodes) { (node) =>
                  {

                    var roleSpecificName = node.role.getClass.getSimpleName() match  {
                      case "Client" => node.role.asInstanceOf[roles.Client].serviceName
                      case "Server" => node.role.asInstanceOf[roles.Server].serviceName
                      case "Publisher" => node.role.asInstanceOf[roles.Publisher].topic
                      case "Subscriber" => node.role.asInstanceOf[roles.Subscriber].topic
                      case _ =>  "ERROR"
                    }

                    for {

                      _ <- if (node.role.isInstanceOf[roles.Server]) {
                        for {

                          interruptedException <- findClass(names.mangle("InterruptedException"))

                           _ <- addExceptionHandler(
                             for {

                               threadCls <- findClass(names.mangle("Thread"))
                               val2000 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 2000)
                               _ <- make_static_method_call(
                                 threadCls,
                                 "sleep",
                                 Seq(val2000),
                                 true
                               )

                             } yield (),
                             None,
                             Seq((interruptedException, names.mangle("interruptedException"), for {
                               _ <- noop()
                             } yield ())),
                             None
                           )

                        } yield ()
                      } else {
                        for {
                          _ <- noop()
                        } yield ()
                      }

                      rosNodeVar <- make_class_instantiation(
                        "ROSNode" + node.name,
                        "instance" + node.name,
                        Seq.empty
                      )

                      roleSpecificNameStr <- paradigm.methodBodyCapabilities.reify(
                        TypeRep.String,
                        roleSpecificName
                      )
                      mainCls <- findClass(names.mangle("Main"))
                      nodeConfigurationFunc <- make_static_method_call(
                        mainCls,
                        "getNodeConfiguration",
                        Seq(rosHostPortVar, roleSpecificNameStr, rosMasterUriVar),
                        false
                      )

                      nodeConfigurationCls <- findClass(names.mangle("NodeConfiguration"))

                      nodeConfigurationVar <- declareVar(
                        names.mangle(node.name + "Config"),
                        nodeConfigurationCls,
                        Some(nodeConfigurationFunc)
                      )

                      _ <- make_method_call(
                        nodeMainExecutorVar,
                        "execute",
                        Seq(rosNodeVar, nodeConfigurationVar),
                        true
                      )

                    } yield ()
                  }
                }


                systemCls <- findClass(names.mangle("System"))
                scannerCls <- findClass(names.mangle("Scanner"))

                msg <- paradigm.methodBodyCapabilities.reify(
                  TypeRep.String,
                  "Press any key to exit."
                )

                scannerVar <- make_class_instantiation(
                  scannerCls,
                  "scanner",
                  Seq.empty
                )

                systemExpr <- toStaticTypeExpression(systemCls)
                outFunc <- getMember(systemExpr, names.mangle("out"))
                _ <- make_method_call(
                  outFunc,
                  "println",
                  Seq(msg),
                  true
                )

                _ <- make_method_call(scannerVar, "nextLine", Seq.empty, true)

                _ <- forEach(domain.nodes) {
                  (node) => {

                    for {

                      rosNodeVar <- nameToExpression(names.mangle("ROSNode" + node.name))
                      _ <- make_method_call(nodeMainExecutorVar, "shutdownNodeMain", Seq(rosNodeVar), true)

                    } yield()
                  }
                }

//                nodeMainExecutor.shutdownNodeMain(serviceClientNodeMain);
//                nodeMainExecutor.shutdownNodeMain(topicSubscriberNodeMain);
//                nodeMainExecutor.shutdownNodeMain(topicPublisherNodeMain);
//                nodeMainExecutor.shutdownNodeMain(serviceServerNodeMain);

                _ <- make_method_call(
                  nodeMainExecutorVar,
                  "shutdown",
                  Seq.empty,
                  true
                )

              } yield(None),
              Seq.empty,
              None
            )

            _ <- addBlockDefinitions(Seq(ifStmt))

          } yield(None),
          None,
          Seq((
              exceptionCls,
              names.mangle("exception"),
              for {

                excepName <- nameToExpression(excepName)
                exceptionUtlsCls <- findClass(names.mangle("ExceptionUtils"))

                getStackTraceFunc <- make_static_method_call(
                  exceptionUtlsCls,
                  "getStackTrace",
                  Seq(excepName),
                  false
                )

                systemExpr <- toStaticTypeExpression(systemCls)

                errFunc <- getMember(systemExpr, names.mangle("err"))

                _ <- make_method_call(
                  errFunc,
                  "println",
                  Seq(getStackTraceFunc),
                  true
                )

                exitVar <- nameToExpression(names.mangle("EXIT_ERROR"))
                _ <- make_static_method_call(systemCls, "exit", Seq(exitVar), true)

              } yield(None)
          )),
          Some(
            for {

              _ <- make_method_call(rosCoreVar, "shutdown", Seq.empty, true)

            } yield(None)
          )
        )


        exitVar <- nameToExpression(names.mangle("EXIT_OK"))
        _ <- make_static_method_call(systemCls, "exit", Seq(exitVar), true)

      } yield (None)
    }

    def make_class(): Generator[ClassContext, Unit] = {

      import ooParadigm.classCapabilities._

      for {

        intType <- toTargetLanguageType(TypeRep.Int)

        one <- ooParadigm.classCapabilities.reify(
          TypeRep.Int,
          1
        )
        zero <- ooParadigm.classCapabilities.reify(
          TypeRep.Int,
          0
        )
        _ <- addField(names.mangle("EXIT_ERROR"), intType, Some(one), true)
        _ <- addField(names.mangle("EXIT_OK"), intType, Some(zero), true)

        _ <- addMethod(names.mangle("getNodeConfiguration"), make_node_configuration_method())
        _ <- addMethod(names.mangle("main"), make_main_method())


      } yield(None)
    }

    for {

      _ <- addClassToProject(
        make_class(),
        names.mangle("Main")
      )

    } yield()


  }

  //ROS_MSGS **************************************************************************************************

  def scala_type_to_typerep(tpe: String): TypeRep = {

    tpe match {
      case "int" => TypeRep.Int
      case "String" => TypeRep.String
      case "Double" => TypeRep.Double
      case "long" => TypeRep.Long
      case "Unit" => TypeRep.Unit
    }
  }

  def make_message_getter(tpe: Type): Generator[MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._
    for {

      _ <- setReturnType(tpe)

    } yield (None)
  }

  def make_message_setter(tpe: Type): Generator[MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._
    for {

      unit <- toTargetLanguageType(TypeRep.Unit)
      _ <- setReturnType(unit)

      _ <- setParameters(
        Seq((names.mangle("val"), tpe))
      )

    } yield (None)
  }

  def make_message_getters_and_setters(msgCls: Class[_]): Generator[ClassContext, Unit] = {
    import classCapabilities._
    import generics.classCapabilities._
    for {

      _ <- forEach(msgCls.getDeclaredFields) { (field) =>
        for {

          convertedType <- toTargetLanguageType(scala_type_to_typerep(field.getType.getSimpleName))
          _ <- addAbstractMethod(
            names.mangle("get" + field.getName.capitalize),
            make_message_getter(convertedType),
            false
          )

          convertedType <- toTargetLanguageType(scala_type_to_typerep(field.getType.getSimpleName))
          _ <- addAbstractMethod(
            names.mangle("set" + field.getName.capitalize),
            make_message_setter(convertedType),
            false
          )

        } yield ()
      }

    } yield(None)
  }


  def make_message_type_field(msgCls: Class[_]): Generator[ClassContext, Unit] = {
    import ooParadigm.classCapabilities._
    for {

      stringType <- findClass(names.mangle("String"))

      defStr <- ooParadigm.classCapabilities.reify(
        TypeRep.String,
        "basic_ros_application/msgs/" + getClassName(msgCls)
      )

      _ <- addField(names.mangle("_TYPE"), stringType, Some(defStr))

    } yield (None)
  }

  def make_message_payload_type_field(requestCls: Class[_], responseCls: Class[_]): Generator[ClassContext, Unit] = {
    import ooParadigm.classCapabilities._
    for {

      stringType <- findClass(names.mangle("String"))

      defStr <- ooParadigm.classCapabilities.reify(
        TypeRep.String,
        "basic_ros_application/msgs/" + get_payload_msg_name(responseCls, requestCls)
      )

      _ <- addField(names.mangle("_TYPE"), stringType, Some(defStr))

    } yield (None)
  }


  def make_message_definition_field(msgCls: Class[_]): Generator[ClassContext, Unit] = {
    import ooParadigm.classCapabilities._

    def make_definition_str(): String = {

      var s: String = ""

      for(field <- msgCls.getDeclaredFields) {

        val rosTpe = scala_type_to_ros_type(
          field.getType.getSimpleName
        )

        s = s + rosTpe + " " + field.getName + '\n'

      }
      s
    }

    for {

      stringType <- findClass(names.mangle("String"))

      defStr <- ooParadigm.classCapabilities.reify(
        TypeRep.String,
        make_definition_str()
      )

      _ <- addField(names.mangle("_DEFINITION"), stringType, Some(defStr))

    } yield (None)

  }
  def make_message_payload_definition_field(requestCls: Class[_], responseCls: Class[_]): Generator[ClassContext, Unit] = {
      import ooParadigm.classCapabilities._

      def make_definition_str(): String = {

        var s: String = ""

        for (field <- requestCls.getDeclaredFields) {

          val rosTpe = scala_type_to_ros_type(
            field.getType.getSimpleName
          )
          s = s + rosTpe + " " + field.getName + '\n'
        }

        s += "--\n"

        for (field <- responseCls.getDeclaredFields) {

          val rosTpe = scala_type_to_ros_type(
            field.getType.getSimpleName
          )
          s = s + rosTpe + " " + field.getName + '\n'
        }

        s
      }

      for {

        stringType <- findClass(names.mangle("String"))

        defStr <- ooParadigm.classCapabilities.reify(
          TypeRep.String,
          make_definition_str()
        )

        _ <- addField(names.mangle("_DEFINITION"), stringType, Some(defStr))

      } yield (None)

    }

  def make_client_server_messages(node: Node, requestCls: Class[_], responseCls: Class[_]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._

    def make_payload_cls(requestCls: Class[_], responseCls: Class[_]): Generator[ClassContext, Unit] = {
      import classCapabilities._
      import generics.classCapabilities._
      for {
        rosMsgCls <- findClass(names.mangle("Message"))

        _ <- addParent(rosMsgCls)
        _ <- setInterface()

        _ <- make_message_payload_definition_field(
          requestCls,
          responseCls
        )

        _ <- make_message_payload_type_field(
          requestCls,
          responseCls
        )

      } yield(None)

    }

    def make_cls(cls: Class[_]): Generator[ClassContext, Unit] = {
      import classCapabilities._
      import generics.classCapabilities._

      for {
        rosMsgCls <- findClass(names.mangle("Message"))

        _ <- addParent(rosMsgCls)
        _ <- setInterface()

        _ <- make_message_getters_and_setters(cls)
        _ <- make_message_type_field(cls)
        _ <- make_message_definition_field(cls)

      } yield (None)

    }

    for {

      _ <- addClassToProject(
        make_cls(requestCls),
        names.mangle("msgs"),
        names.mangle(getClassName(requestCls))
      )

      _ <- addClassToProject(
        make_cls(responseCls),
        names.mangle("msgs"),
        names.mangle(getClassName(responseCls))
      )

      _ <- addClassToProject(
        make_payload_cls(requestCls, responseCls),
        names.mangle("msgs"),
        names.mangle(get_payload_msg_name(requestCls, responseCls))
      )


    } yield (None)
  }

  def make_pub_sub_message(node: Node, msgCls: Class[_]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._


    def make_class(): Generator[ClassContext, Unit] = {
      import classCapabilities._
      import generics.classCapabilities._
      for {

        rosMsgCls <- findClass(names.mangle("Message"))

        _ <- addParent(rosMsgCls)
        _ <- setInterface()

        _ <- make_message_getters_and_setters(msgCls)
        _ <- make_message_type_field(msgCls)
        _ <- make_message_definition_field(msgCls)

      } yield(None)

    }

    for {

      _ <- addClassToProject(
        make_class(),
        names.mangle("msgs"),
        names.mangle(getClassName(msgCls))
      )

    } yield(None)

  }

  def make_role_messages(node: Node): Generator[ProjectContext, Unit] = {

    import classCapabilities._
    val roleType = node.role.getClass.getSimpleName()

    for {

      _ <- roleType match {
        case "Client" => for {
          _ <- make_client_server_messages(
            node,
            node.role.asInstanceOf[roles.Client].requestMsgType,
            node.role.asInstanceOf[roles.Client].responseMsgType
          )
        } yield (None)
        case "Server" => for {
          _ <- make_client_server_messages(
            node,
            node.role.asInstanceOf[roles.Server].requestMsgType,
            node.role.asInstanceOf[roles.Server].responseMsgType
          )
        } yield (None)
        case "Publisher" => for {
          _ <- make_pub_sub_message(
            node,
            node.role.asInstanceOf[roles.Publisher].msgType
          )
        } yield (None)
        case "Subscriber" => for {
          _ <- make_pub_sub_message(
            node,
            node.role.asInstanceOf[roles.Subscriber].msgType
          )
        } yield (None)
        case _ => for {_ <- register_imports()} yield (None)
      }

    } yield (None)

  }


  def make_messages(): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    for {

      _ <- forEach(domain.nodes) { (node: Node) =>
        for {
          _ <- make_role_messages(node)
        } yield ()
      }
    } yield ()
  }

  //ROS_NODE **************************************************************************************************
  def make_ros_node(node: Node): Generator[ClassContext, Unit] = {

    import classCapabilities._
    for {

      appClass <- findClass(names.mangle("AbstractNodeMain"))
      _ <- addParent(appClass)

      _ <- addConstructor(make_ros_node_constructor(node))

      _ <- make_ros_node_init(node)

      _ <- make_ros_node_role_fields(node)
      _ <- make_ros_node_role_methods(node)
      _ <- make_ros_node_onstart(node)
      _ <- make_ros_node_name_getter()

    } yield (None)

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
  //**********************************************************************************************************

  //FRAGMENTS **************************************************************************************************
  def load_fragments(): Generator[paradigm.ProjectContext, Unit] = {

    import unitTemplating.templatingCapabilities._
    import paradigm.compilationUnitCapabilities._
    import paradigm.projectCapabilities._

    def get_role_fragment_name(node: Node): Option[String] = {
      val roleType = node.role.getClass.getSimpleName()

      roleType match {
        case "Client" => Some(node.role.asInstanceOf[roles.Client].onResponseFragment)
        case "Server" => Some(node.role.asInstanceOf[roles.Server].onRequestFragment)
        case "Publisher" => None
        case "Subscriber" => Some(node.role.asInstanceOf[roles.Subscriber].onMsgFragment)
      }
    }

    def make_fragment(fragPath: String): Generator[CompilationUnitContext, Unit] = {

      for {
        _ <- loadFragment(this.getClass.getResource(fragPath))
      } yield (None)
    }

    for {
      _ <- forEach(domain.nodes) { (node: Node) =>
        for {

          _ <- if(node.loopFragment.isDefined) {
            for {
              _ <- addCompilationUnit(
                    make_fragment(node.loopFragment.get),
                    names.mangle("logic"),
                    names.mangle( getBaseName(node.loopFragment.get))
                  )
            } yield(None)
          } else {
            for {
              _ <- paradigm.projectCapabilities.noop()
            } yield ()
          }

          _ <- if (get_role_fragment_name(node).isDefined) {
            for {
              _ <- addCompilationUnit(
                make_fragment(get_role_fragment_name(node).get),
                names.mangle("logic"),
                names.mangle( getBaseName(get_role_fragment_name(node).get))
              )
            } yield (None)
          } else {
            for {
              _ <- paradigm.projectCapabilities.noop()
            } yield ()
          }

          _ <- if (node.loopFragment.isDefined) {
            for {
              _ <- registerImportForName(
                getBaseName(node.loopFragment.get),
                Seq("logic/")
              )
            } yield (None)
          } else {
            for {
              _ <- paradigm.projectCapabilities.noop()
            } yield ()
          }


          _ <- if (get_role_fragment_name(node).isDefined) {
            for {
              _ <- registerImportForName(
                getBaseName(get_role_fragment_name(node).get),
                Seq("logic/")
              )
            } yield (None)
          } else {
            for {
              _ <- paradigm.projectCapabilities.noop()
            } yield ()
          }



        } yield ()
      }

    } yield(None)
  }

  def register_imports(): Generator[paradigm.ProjectContext, Unit] = {
    import paradigm.projectCapabilities._

    val importsList = Seq[Seq[String]](
      Seq("org","ros","exception","RosRuntimeException"),
      Seq("java","lang","String"),
      Seq("rosjava_test_msgs", "AddTwoIntsRequest"),
      Seq("rosjava_test_msgs", "AddTwoIntsResponse")
    )

    for {

      _ <- forEach(importsList) { (elem: Seq[String]) =>
        for {
          _ <- registerImportForName(
            elem.last,
            elem
          )
        } yield ()
      }

    } yield ()
  }

  def implement(): Generator[paradigm.ProjectContext, Unit] = {
    for {
      _ <- register_imports()
      _ <- load_fragments()
      _ <- make_main_class()
      _ <- make_ros_nodes()
      _ <- make_ros_node_listeners()
      _ <- make_ros_node_loops()
      _ <- make_messages()

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
    _exceptions: Exceptions.WithBase[base.MethodBodyContext,base.type],
    _parametricPolymorphism: ParametricPolymorphism.WithBase[base.type]
  )
  (_generics: Generics.WithBase[base.type, oo.type, _parametricPolymorphism.type])
  (
    _classTemplating: Templating.WithBase[oo.ClassContext, base.MethodBodyContext, base.type],
    _unitTemplating: Templating.WithBase[base.CompilationUnitContext, base.MethodBodyContext, base.type]
  )
  (_domain: RoboticsDomain)
  : ROSApplicationProvider.WithParadigm[base.type] =
    new ROSApplicationProvider {
      override val paradigm: base.type = base
      val impParadigm: imp.type = imp
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
      override val polymorphics: _parametricPolymorphism.type = _parametricPolymorphism
      override val generics: _generics.type = _generics
      override val classTemplating: Templating.WithBase[ooParadigm.ClassContext, paradigm.MethodBodyContext, paradigm.type] = _classTemplating
      override val unitTemplating: Templating.WithBase[paradigm.CompilationUnitContext, paradigm.MethodBodyContext, paradigm.type] = _unitTemplating
      override val domain: RoboticsDomain = _domain
    }
}


