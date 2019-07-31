package org.combinators.ep.generator  /*DI:LI:AI*/

import java.nio.file.Path

import org.combinators.templating.persistable.Persistable

/**
  * Contents of file to be stored at `persistTo`.
  *
  * Used for any general-purpose resource (like build.sbt file or compile.sh script).
  *
  * When data is BINARY, must load up using rawBytes.
  */
case class FileWithPath(code: String, rawBytes:Option[Array[Byte]] = None, persistTo: Path)

trait FileWithPathPersistableInstances {
  /** Persistable instance for [FileWithPath]. */
  implicit def fileWithPathPersistable: FileWithPathPersistable.Aux[FileWithPath] = new Persistable {
    type T = FileWithPath
    def rawText(elem: FileWithPath): Array[Byte] = {
      if (elem.rawBytes.isDefined) {
        elem.rawBytes.get
      } else {
        elem.code.getBytes
      }
    }
    def path(elem: FileWithPath): Path = elem.persistTo
  }
}

object FileWithPathPersistable extends FileWithPathPersistableInstances {
  type Aux[TT] = Persistable { type T = TT }
  def apply[T](implicit persistable: Aux[T]): Aux[T] = persistable
}
