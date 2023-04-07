package org.combinators.common

object Utils {
  def getResourcePath(path: String, cls: Class[_]) =  {
    cls.getClassLoader.getResource(path).getPath
  }
}
