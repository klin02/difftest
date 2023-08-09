import mill._, scalalib._, scalafmt._
import coursier.maven.MavenRepository

trait CommonModule extends ScalaModule {
  override def scalaVersion = "2.13.10"
  override def scalacPluginIvyDeps = Agg(ivy"org.chipsalliance:::chisel-plugin:5.0.0")
  override def scalacOptions = super.scalacOptions() ++ Agg("-Ymacro-annotations", "-Ytasty-reader")
}

trait HasChisel extends ScalaModule {
  override def ivyDeps = Agg(ivy"org.chipsalliance::chisel:5.0.0")
}

object difftest extends SbtModule with ScalafmtModule with CommonModule with HasChisel {
  override def millSourcePath = os.pwd / "difftest"
}

object chiselModule extends SbtModule with ScalafmtModule with CommonModule with HasChisel {
  override def millSourcePath = millOuterCtx.millSourcePath

  override def moduleDeps = super.moduleDeps ++ Seq(
    difftest
  )

  object test extends SbtModuleTests with TestModule.ScalaTest {
    override def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"org.scalatest::scalatest:3.2.4",
      ivy"edu.berkeley.cs::chisel-iotesters:2.5+"
    )
    def testFrameworks = "org.scalatest.tools.Framework"
  }
}
