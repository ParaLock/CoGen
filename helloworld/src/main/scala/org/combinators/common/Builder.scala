package org.combinators.common

import org.combinators.ep.domain.abstractions.{DataType, DataTypeCase, TypeRep}
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}
import org.combinators.ep.generator.Command.{Generator, _}
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax._
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.ffi.Console
import org.combinators.ep.generator.paradigm.{AddImport, AnyParadigm, ObjectOriented, ResolveImport}
import org.combinators.ep.generator.{NameProvider, Understands}


//class Builder[TContext](val provider: BaseProvider){
//
//  import provider.paradigm._
//  import provider.paradigm.syntax._
//  def class_instantiation(
//                                          typeName: String,
//                                          varName: String,
//                                          constructorParams: Seq[Expression]
//                                        ): Generator[TContext, Expression] = {
//    import provider.impParadigm.imperativeCapabilities._
//    import provider.ooParadigm.methodBodyCapabilities._
//    import provider.paradigm.methodBodyCapabilities._
//
//    for {
//      classType <- findClass(provider.names.mangle(typeName))
//      _ <- provider.resolveAndAddImport(classType)
//      classObj <- instantiateObject(classType, constructorParams)
//      classObjName <- freshName(provider.names.mangle(varName))
//      sceneObjVar <- declareVar(classObjName, classType, Some(classObj))
//    } yield sceneObjVar
//  }
//}
