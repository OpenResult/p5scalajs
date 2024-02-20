import mill._
import mill.scalalib._
import mill.scalajslib._
import mill.define.TaskModule

val utestVersion = "0.8.1"
val upickle = "3.1.3"
val sv = "3.3.1"

trait Common extends ScalaModule {
  def scalaVersion = sv
  def scalacOptions = Seq("-deprecation")
  def ivyDeps = super.ivyDeps() ++ Agg(ivy"com.lihaoyi::upickle::$upickle")
  def sources = T.sources(
    millSourcePath / "src",
    millSourcePath / os.up / "shared" / "src"
  )
}

object server extends Common {
  def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"com.lihaoyi::cask:0.9.1",
    ivy"com.lihaoyi::os-lib:0.9.1"
  )
  override def sources = T.sources {
    super.sources() ++ vuegui.sources()
  }
  override def compile =
    T {
      js.fastLinkJS.apply()
      val from = os.pwd / "out" / "js" / "fastLinkJS.dest"
      val to = os.pwd / "vuegui" / "dist"
      os.copy(from / "main.js", to / "main.js")
      os.copy(from / "main.js.map", to / "main.js.map")
      vuegui.npmRunBuild.apply()
      super.compile.apply()
    }
  
  object test extends ScalaTests {
    def testFramework = "utest.runner.Framework"

    def ivyDeps =
      Agg(
        ivy"com.lihaoyi::utest:$utestVersion"
      )
  }
}

object vuegui extends Module with TaskModule {
  override def defaultCommandName(): String = "npmRunBuild"
  def npmRunBuild() = T.command {
    val wd = os.pwd / Symbol("vuegui")
    val invoked = os.proc("npm", "run", "generate").call(cwd = wd)
    println(invoked.out.trim())
  }
  def sources = T.sources(
    millSourcePath / "src",
    millSourcePath / "public",
    millSourcePath / "pages",
    millSourcePath / "components"
  )
}

object js extends ScalaJSModule with Common {
  def scalaJSVersion = "1.15.0"
  def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"org.scala-js::scalajs-dom::2.8.0"
  )
}
