package org.combinators.helloworld

/* Generates Fibonacci Program. */

import cats.effect.{ExitCode, IO, IOApp}
import org.apache.commons.io.FileUtils
import org.combinators.ep.generator.FileWithPathPersistable._
import org.combinators.ep.generator.{FileWithPath, FileWithPathPersistable}
import org.combinators.ep.language.scala.{CodeGenerator, ScalaNameProvider, Syntax}

import java.nio.file.{Path, Paths}
import scala.meta.{Pkg, Term}

/**
 * Takes paradigm-independent specification for Fibonacci and generates Java code
 */
class FibonacciIndependentMainScala {
  val generator = CodeGenerator(CodeGenerator.defaultConfig.copy(targetPackage = Pkg(Term.Name("fib"), List.empty)))
  
  val fibonacciApproach = FibonacciIndependentProvider.functional[Syntax.default.type, generator.paradigm.type](generator.paradigm)(ScalaNameProvider, generator.functional, generator.functionalInMethod, generator.intsInMethod, generator.assertionsInMethod, generator.equalityInMethod)

  //  val fibonacciApproach = FibonacciIndependentProvider.imperative[Syntax.default.type, generator.paradigm.type](generator.paradigm)(
//    ScalaNameProvider,
//    generator.ooParadigm,
//    generator.,
//    generator.intsInMethod,
//    generator.assertionsInMethod,
//    generator.equalityInMethod
//  )

  val persistable = FileWithPathPersistable[FileWithPath]

  def directToDiskTransaction(targetDirectory: Path): IO[Unit] = {

    val files =
      () => generator.paradigm.runGenerator {
        for {
          _ <- generator.intsInMethod.enable()
          _ <- generator.booleansInMethod.enable()
          _ <- generator.stringsInMethod.enable()
          _ <- generator.equalityInMethod.enable()
          _ <- generator.assertionsInMethod.enable()
          _ <- fibonacciApproach.make_project()
        } yield ()
      }

     IO {
      print("Computing Files...")
      val computed = files()
      println("[OK]")
      if (targetDirectory.toFile.exists()) {
        print(s"Cleaning Target Directory (${targetDirectory})...")
        FileUtils.deleteDirectory(targetDirectory.toFile)
        println("[OK]")
      }
      print("Persisting Files...")
      files().foreach(file => persistable.persistOverwriting(targetDirectory, file))
      println("[OK]")
    }
  }

  def runDirectToDisc(targetDirectory: Path): IO[ExitCode] = {
    for {
      _ <- directToDiskTransaction(targetDirectory)
    } yield ExitCode.Success
  }
}

object FibonacciIndependentScalaDirectToDiskMain extends IOApp {
  val targetDirectory = Paths.get("target", "ep3", "scala")

  def run(args: List[String]): IO[ExitCode] = {
    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new FibonacciIndependentMainScala() }
      _ <- IO { println("[OK]") }
      result <- main.runDirectToDisc(targetDirectory)
    } yield result
  }
}
