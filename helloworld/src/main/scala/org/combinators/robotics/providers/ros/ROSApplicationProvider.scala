package org.combinators.gui.providers.ros


import org.combinators.common._
import org.combinators.ep.domain.abstractions.{DataType, DataTypeCase, TypeRep}
import org.combinators.ep.generator.Command.{Generator, lift}
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.{forEach, forEachWithIndex}
import org.combinators.ep.generator.paradigm.{AnyParadigm, FindClass, Generics, ObjectOriented, ParametricPolymorphism}
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.{AbstractSyntax, Command, NameProvider, Understands}
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Arrays, Assertions, Console, Equality, Exceptions}
import org.combinators.robotics.domain_model.ros.Node
import org.combinators.robotics.examples.RoboticsDomain
import org.combinators.robotics.domain_model.ros._

import java.io.File


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
          requestCls <- findClass(names.mangle(role.requestMsgType))
          responseCls <- findClass(names.mangle(role.responseMsgType))


          msgType <- findClass(
            names.mangle(role.payloadMsgType)
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
      requestCls <- findClass(names.mangle(role.requestMsgType))
      responseCls <- findClass(names.mangle(role.responseMsgType))

      msgType <- findClass(
        names.mangle(role.payloadMsgType)
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
        names.mangle(role.msgType)
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
        names.mangle(role.msgType)
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
    for {
      _ <- noop()
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
  def make_ros_node_listener(node: Node): Generator[ClassContext, Unit] = {
    import classCapabilities._
    val roleType = node.role.getClass.getSimpleName()

    for {

      _ <- roleType match {
        case "Client" => for {_ <- noop()} yield (None)
        case "Server" => for {_ <- noop()} yield (None)
        case "Publisher" => for {_ <- noop()} yield (None)
        case "Subscriber" => for {_ <- noop()} yield (None)
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
    var loopFragName: String = node.loopFragment

    val fragmentFile = new File(loopFragName)
    val fragmentClassName = fragmentFile.getName.split("\\.").dropRight(1).mkString(".")

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

        requestCls <- findClass(names.mangle(role.requestMsgType))

        requestVar <- declareVar(
          names.mangle("request"),
          requestCls,
          Some(newMsgInvoke)
        )

        loopCls <- findClass(names.mangle(fragmentClassName))
        logVar <- getMember(self, names.mangle("log"))


        _ <- make_static_method_call(
          loopCls,
          "run",
          Seq(requestVar, logVar),
          true
        )

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
      requestCls <- findClass(names.mangle(role.requestMsgType))
      responseCls <- findClass(names.mangle(role.responseMsgType))
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
        false,
        Seq(interruptableExcep)
      )


    } yield(None)

  }

//  def make_ros_node_publisher_loop(node: Node): Generator[ClassContext, Unit] = {
//
//
//  }

  def make_ros_node_loop(node: Node): Generator[ClassContext, Unit] = {
    import classCapabilities._
    val roleType = node.role.getClass.getSimpleName()

    for {

      _ <- roleType match {
        case "Client" => for {_ <- make_ros_node_client_loop(node)} yield (None)
        case "Server" => for {_ <- noop()} yield (None)
        case "Publisher" => for {_ <- noop()} yield (None)
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

      _ <- make_method_call(myMethod, Seq(methodRef), true)

      one_hundred <- paradigm.methodBodyCapabilities.reify(TypeRep.Double, 100)
      two_hundred <- paradigm.methodBodyCapabilities.reify(TypeRep.Double, 200)
      addExpr <- ffiArithmetic.arithmeticCapabilities.add(one_hundred, two_hundred)


    } yield None
  }

  //ROS_NODE **************************************************************************************************
  def make_ros_node(node: Node): Generator[ClassContext, Unit] = {

    import classCapabilities._
    for {

      stringType <- toTargetLanguageType(TypeRep.String)

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

  def register_imports(): Generator[paradigm.ProjectContext, Unit] = {
    import paradigm.projectCapabilities._

    val importsList = Seq[Seq[String]](
      Seq("org","ros","exception","RosRuntimeException"),
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

      _ <- make_ros_nodes()
      _ <- make_ros_node_listeners()
      _ <- make_ros_node_loops()
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
      override val domain: RoboticsDomain = _domain
    }
}


