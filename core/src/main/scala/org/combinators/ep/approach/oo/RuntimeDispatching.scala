package org.combinators.ep.approach.oo

/*DI:LI:AD*/

import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.domain.{GenericModel, Model}
import org.combinators.ep.generator.Command._
import org.combinators.ep.generator._
import org.combinators.ep.generator.communication._
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax._
import org.combinators.ep.generator.paradigm._
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.control.Imperative.WithBase
import org.combinators.ep.generator.paradigm.ffi.{Exceptions, Strings}

/**
 * Runtime Dispatch
 *
 * Have to decide whether to use side effects or Generics. This current implementation uses the Visitor<R> generics
 * approach, which can be adopted by different object oriented languages.
 */
abstract class RuntimeDispatching extends OOApproachImplementationProvider with SharedOO with FieldDefinition with OperationAsClass {
val ooParadigm: ObjectOriented.WithBase[paradigm.type]
val impParadigm: Imperative.WithBase[paradigm.MethodBodyContext,paradigm.type]
val exceptions: Exceptions.WithBase[paradigm.MethodBodyContext,paradigm.type]
val strings: Strings.WithBase[paradigm.MethodBodyContext,paradigm.type]

  import ooParadigm._
  import paradigm._
  import syntax._

  val expParameter: String = "exp"

  /**
   * Instantiates an instance of the domain object.
   *
   * Same implementation for OO as for visitor.
   *
   * new Add(new Lit(new Double(1.0)), new Lit(new Double(2.0)))
   */
  def instantiate(baseTpe: DataType, tpeCase: DataTypeCase, args: Expression*): Generator[MethodBodyContext, Expression] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    for {
      // access the constructor for the class associated with type case and invoke constructors with arguments.
      rt <- findClass(names.mangle(names.conceptNameOf(tpeCase)))
      _ <- resolveAndAddImport(rt)
      res <- instantiateObject(rt, args)
    } yield res
  }

  /**
   * Dispatch in visitor we need to find context on which to accept a visitor.
   *
   * That is, e.getLeft().accept(new Eval()) + e.getRight().accept(new Eval());
   *
   *          new Eval().op(e.getLeft()) + new Eval().op(e.getRight())
   *
   * @param message
   * @return
   */
  def dispatch(message: SendRequest[Expression]): Generator[MethodBodyContext, Expression] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._
    val op = message.request.op
    for {
// return exp.getLeft().eval(new ep.Eval()) + exp.getRight().eval(new ep.Eval());

      // the operation is encoded in its own class, which we must find to determine the visitor type
      //op = message.request.op
      opType <- findClass(names.mangle(names.conceptNameOf(op)))     // each visitor is named based on operation
      _ <- resolveAndAddImport(opType)            // gives resulting import statement (if needed)

      // construct the visitor object for the given type (and parameters)
      dispatcher <- instantiateObject(opType, op.parameters.map(param => message.request.arguments(param)))

      // In the 'message.to' expression, invoke the 'accept' method with a visitor argument
      method <- getMember(dispatcher, names.mangle(names.instanceNameOf(op)))   // things which are code-generated use the '<-' handles unpacking results
      rt <- toTargetLanguageType(message.request.op.returnType)
      _ <- resolveAndAddImport(rt)

      // apply to method with the visitor, resulting in the expression
      result <- apply(method, Seq(message.to))           // has to be Seq[] just for syntax of calling the method
    } yield result
  }

  /**
   *
   * {{{
   *  public op.returnType ???(rt e);
   * }}}
   * @return
   */
  def makeOperationSignature(paramType:Type, op:Operation): Generator[MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._

    for {

      // this returns mangled visitTypeParameter name and gets list of all type parameters, for which there is only one, so we get head
      rt <- toTargetLanguageType(op.returnType)
      _ <- resolveAndAddImport(rt)
      _ <- setReturnType(rt)

      expParam <- freshName(names.mangle(expParameter))
      _ <- setParameters(Seq((expParam, paramType)))      // a pair (name,type) of only one sequence
    } yield ()
  }


  /** Create an accept implementation from the accept method signature.
   * {{{
   *  public RT op(Exp exp) {
   *     if (exp instanceof Lit) {
		    	return _eval((Lit) exp);
		}

		if (exp instanceof Add) {
			return _eval((Add) exp);
		}

   * }
   * }}}
   * @return
   */
  def makeDispatchingOperation(model:Model, op:Operation): Generator[ClassContext, Unit] = {
    def ifStmt(): Generator[MethodBodyContext, Option[Expression]] = {
      import ooParadigm.methodBodyCapabilities._
      import paradigm.methodBodyCapabilities._
      import impParadigm.imperativeCapabilities._
      import exceptions.exceptionCapabilities._

      for {
        _ <- forEach(model.flatten.typeCases) { tpe =>
          for {
            tpeCase <- findClass(names.mangle(names.conceptNameOf(tpe)))
            _ <- resolveAndAddImport(tpeCase)
            args <- getArguments()
            condExpr <- instanceOfType(tpeCase, args.head._3)
            self <- selfReference()
            innerFunction <- getMember(self, names.addPrefix("_", names.mangle(names.instanceNameOf(op))))

            casted <- castObject(tpeCase, args.head._3)
            result <- apply(innerFunction, Seq(casted))
            blockReturn <- returnStmt(result)
            ifs <- ifThenElse(condExpr, addBlockDefinitions(Seq(blockReturn)), Seq.empty)
            _ <- addBlockDefinitions(Seq(ifs))
          } yield None
        }

        expr <- this.reify(InstanceRep(TypeRep.String)("Missing Subclass Case!"))
        throws <- raise(expr)
        _ <- addBlockDefinitions(Seq(throws))
      } yield None
    }

    val makeBody: Generator[MethodBodyContext, Option[Expression]] = {
      import ooParadigm.methodBodyCapabilities._
      import paradigm.methodBodyCapabilities._

      for {
        // start from the accept signature and add a method body.

        // identify Visitor<R>
        rt <- toTargetLanguageType(TypeRep.DataType(model.baseDataType))
        _ <- resolveAndAddImport(rt)

        _ <- makeOperationSignature(rt, op)
        args <- getArguments()   // get name, type, expression

        // invoke visit method on 'v' with 'this' as argument
//        innerFunction <- getMember(args.head._3, names.addPrefix("_", names.mangle(names.instanceNameOf(op))))
//        self <- selfReference()
//        result <- apply(innerFunction, Seq(self))  // make the method invocation
        _ <- ifStmt()
      } yield None
    }

    import ooParadigm.classCapabilities._
    addMethod(names.mangle(names.instanceNameOf(op)), makeBody)
  }

  /**
   * Each operation is placed in its own class, with a 'visit' method for each known data type.
   *
   * Uses the generic 'operationClass' capability to create the structure of the class.
   *
   * {{{
   * class Eval  { }
   *
   *   public Double op(Exp e) {
   *
   *     }
   *
   *     public Double _op(Lit e) {
   *         return e.getValue();
   *     }
   *
   *     public Double _op(Add e) {
   *         return op(e.left) + op(e.right)
   *     }
   *   }
   * }}}
   *
   * @param domain     Model for which all types are to be incorporated
   * @param op
   * @param domainSpecific
   * @return        The one invoking this method must be sure to add this class to project.
   */
  def makeOperationImplementation(domain:Model,
                                  op: Operation,
                                  domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ClassContext, Unit] = {
    import ooParadigm.classCapabilities._

    for {
      _ <- operationClass(names.addPrefix("_", names.mangle(names.instanceNameOf(op))), op, domain.typeCases, domain.baseDataType, domainSpecific)

      returnTpe <- toTargetLanguageType(op.returnType)
      _ <- resolveAndAddImport(returnTpe)

      _ <- makeDispatchingOperation(domain, op)
      // add multiplexing dispatcher
//      _ <- addMethod(names.mangle(names.instanceNameOf(op)), makeImplementation(tpe: DataType,
//        tpeCase: DataTypeCase, op, domainSpecific))
    } yield ()
  }

  def makeDerived(parentType: DataType, tpeCase: DataTypeCase, model: Model): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    val makeClass: Generator[ClassContext, Unit] = {
      import ooParadigm.classCapabilities._
      for {
        parent <- toTargetLanguageType(TypeRep.DataType(parentType))
        _ <- resolveAndAddImport(parent)
        _ <- addParent(parent)
        _ <- forEach (tpeCase.attributes) { att => makeField(att) }
        _ <- addConstructor(makeConstructor(tpeCase))
        _ <- forEach (tpeCase.attributes) { att => makeGetter(att) }

      } yield ()
    }
    addClassToProject(makeClass, names.mangle(names.conceptNameOf(tpeCase)))
  }

  /**
   * Define the base class for Exp which must contain the accept method as an abstract method.
   *
   * {{{
   *  public abstract class Exp {
   *    public abstract <R> R accept(Visitor<R> v);
   *  }
   * }}}
   *
   * @param tpe
   * @return
   */
  def makeBase(tpe: DataType): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._

    val makeClass: Generator[ClassContext, Unit] = {
      import ooParadigm.classCapabilities._
      for {
        _ <- setAbstract()
      } yield ()
    }

    // adds the 'Exp' class, with a single accept method
    addClassToProject( makeClass, names.mangle(names.conceptNameOf(tpe)))
  }

  /** Make a method body for each operation, which is a visit method for a defined data type
   *
   * {{{
   *     public Double _op(Sub e) {
   *         return e.getLeft().accept(new Eval()) - e.getRight().accept(new Eval());
   *         return op(e.getLeft()) - op(e.getRight);
   *     }
   * }}}
   *
   * Access the results via a visitor method which returns the information using accept method.
   *
   * @param tpe
   * @param tpeCase
   * @param op
   * @param domainSpecific
   * @return
   */
  override def makeImplementation(tpe: DataType,
                                  tpeCase: DataTypeCase,
                                  op: Operation,
                                  domainSpecific: EvolutionImplementationProvider[this.type]
                                 ): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    for {
      returnType <- toTargetLanguageType(op.returnType)
      _ <- resolveAndAddImport(returnType)

      ptype <- findClass(names.mangle(names.conceptNameOf(tpeCase)))
      _ <- resolveAndAddImport(ptype)
      _ <- makeOperationSignature(ptype, op)
      visitedRef <- getArguments().map(_.head._3)
      attAccessors: Seq[Expression] <- forEach (tpeCase.attributes) { att =>
        for {
          getter <- getMember(visitedRef, getterName(att))
          getterCall <- apply(getter, Seq.empty)
        } yield getterCall
      }

      args <- forEach (op.parameters) { param =>
        for {
          thisRef <- selfReference()
          paramField <- getMember(thisRef, names.mangle(param.name))
        } yield (param, paramField)
      }

      // body of this implementation is the result of the individual domain-specific logic.
      result <-
        domainSpecific.logic(this)(
          ReceivedRequest(
            tpe,
            tpeCase,
            visitedRef,
            tpeCase.attributes.zip(attAccessors).toMap,
            Request(op, args.toMap)
          )
        )
    } yield result
  }

  /**
   * The Visitor approach is defined as follows
   *
   * 1. Make the base class (for the domain)
   * 2. For each of the data types (in flattened set) create a derived class
   * 3. Create the Visitor interface
   *
   * @param gdomain
   * @param domainSpecific
   * @return
   */
  def implement(gdomain: GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    import paradigm.projectContextCapabilities._

    val domain = gdomain match {
      case _:Model => gdomain.asInstanceOf[Model]
      case _ => gdomain.linearize
    }

    val flatDomain = domain.flatten
    for {
      _ <- debug ("Processing RuntimeDispatching")
      _ <- strings.enable()
      _ <- exceptions.enable()
      _ <- registerTypeMapping(flatDomain)
      _ <- domainSpecific.initialize(this)
      _ <- makeBase(flatDomain.baseDataType)
      _ <- forEach (flatDomain.typeCases) { tpeCase =>
        makeDerived(flatDomain.baseDataType, tpeCase, domain)   // used to have flatDomain.ops,
      }
      _ <- forEach (flatDomain.ops) { op =>
        addClassToProject(makeOperationImplementation(flatDomain, op, domainSpecific), names.mangle(names.conceptNameOf(op)))
      }
    } yield ()
  }
}

object RuntimeDispatching {
  type WithParadigm[P <: AnyParadigm] = RuntimeDispatching { val paradigm: P }
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

  def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
  (base: P)
  (nameProvider: NameProvider[base.syntax.Name],
   impParadigmProvider: Imperative.WithBase[base.MethodBodyContext, base.type],
   stringsProvider: Strings.WithBase[base.MethodBodyContext, base.type],
   exceptionsProvider: Exceptions.WithBase[base.MethodBodyContext, base.type],
   oo: ObjectOriented.WithBase[base.type]) : RuntimeDispatching.WithParadigm[base.type] =
    new RuntimeDispatching {
      val paradigm: base.type = base
      val strings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type ] = stringsProvider
      val impParadigm: Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type ] = impParadigmProvider
      val exceptions: Exceptions.WithBase[paradigm.MethodBodyContext, paradigm.type ] = exceptionsProvider
      val names: NameProvider[paradigm.syntax.Name] = nameProvider
      val ooParadigm: oo.type = oo
    }
}
