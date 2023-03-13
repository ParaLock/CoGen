package org.combinators.gui.providers.ros

/* Generates Fibonacci Program. */

import cats.effect.{ExitCode, IO, IOApp}
import com.github.javaparser.ast.PackageDeclaration
import org.apache.commons.io.FileUtils
import org.combinators.ep.generator.FileWithPathPersistable._
import org.combinators.ep.generator.{FileWithPath, FileWithPathPersistable}
import org.combinators.ep.language.java.paradigm.ObjectOriented
import org.combinators.ep.language.java.{CodeGenerator, JavaNameProvider, PartiallyBoxed, Syntax}
import java.nio.file.{Path, Paths}

/**
 * Takes paradigm-independent specification for Fibonacci and generates Java code
 */
class ROSApplicationMain {

  val generator = CodeGenerator(
    CodeGenerator.defaultConfig.copy(
      boxLevel = PartiallyBoxed,
      targetPackage = new PackageDeclaration(
        ObjectOriented.fromComponents("opengl_basic_application")
      )
    )
  )

  val appApproach = ROSApplicationProvider[
    Syntax.default.type,
    generator.paradigm.type
  ](
    generator.paradigm
  )(
    JavaNameProvider,
    generator.imperativeInMethod,
    generator.ooParadigm,
    generator.consoleInMethod,
    generator.arraysInMethod,
    generator.assertionsInMethod,
    generator.equalityInMethod,
    generator.intsInMethod,
    generator.assertionsInMethod,
    generator.equalityInMethod
  )


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

          _ <- generator.doublesInMethod.enable()
          _ <- generator.intsInMethod.enable()
          _ <- generator.stringsInMethod.enable()
          _ <- generator.listsInMethod.enable() // should be array, but this still needs to be added as an FFI
          _ <- generator.consoleInMethod.enable()
          _ <- generator.arraysInMethod.enable()
          _ <- generator.equalityInMethod.enable()
          _ <- generator.assertionsInMethod.enable()

          _ <- appApproach.implement()
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

object BasicApplicationDirectToDiskMain extends IOApp {
  val targetDirectory = Paths.get("target", "ros_application", "java")

  def run(args: List[String]): IO[ExitCode] = {
    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new ROSApplicationMain() }
      _ <- IO { println("[OK]") }
      result <- main.runDirectToDisc(targetDirectory)
    } yield result
  }
}
