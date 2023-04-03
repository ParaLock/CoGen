import java.io.File
import play.twirl.compiler.TwirlCompiler
import play.twirl.api.Html

object RuntimeTwirlCompilationExample {
  def main(args: Array[String]): Unit = {
    val templateSource =
      """
        |@(name: String, age: Int)
        |
        |<h1>Welcome @name!</h1>
        |<p>You are @age years old.</p>
      """.stripMargin

    val compiledTemplate = compileTemplate(templateSource, "MyDynamicTemplate")

    val name = "John Doe"
    val age = 30

    // Render the compiled template with the provided data
    val renderedTemplate: Html = compiledTemplate.render(name, age)

    // Convert the rendered template to a string
    val templateAsString: String = renderedTemplate.toString

    println(templateAsString)
  }
}

def compileTemplate(source: String, templateName: String): Html = {
  val sourceFile = new File(s"tmp/$templateName.scala.html")
  val compiledTemplateFile = new File(s"tmp/$templateName.template.scala")

  // Create the directories if they don't exist
  sourceFile.getParentFile.mkdirs()

  // Write the template source to a file
  java.nio.file.Files.write(sourceFile.toPath, source.getBytes(java.nio.charset.StandardCharsets.UTF_8))

  // Compile the template
  TwirlCompiler.compile(
    sourceFile,
    sourceDirectory = sourceFile.getParentFile,
    generatedDirectory = sourceFile.getParentFile,
    formatterType = "play.twirl.api.HtmlFormat",
    additionalImports = TwirlCompiler.DefaultImports
  )

  // Load the compiled template class
  val classLoader = new java.net.URLClassLoader(
    Array(compiledTemplateFile.getParentFile.toURI.toURL),
    getClass.getClassLoader
  )
  val compiledTemplateClass = classLoader.loadClass(s"${templateName}_template")

  // Instantiate the template and render it
  val templateInstance = compiledTemplateClass.getDeclaredConstructor().newInstance()
  val renderMethod = compiledTemplateClass.getMethod("render")

  renderMethod.invoke(templateInstance).asInstanceOf[Html]
}