import mill._, scalalib._, scalajslib._

object highw extends ScalaJSModule {
  def scalaVersion = "3.4.2"
  def scalaJSVersion = "1.16.0"
  def ivyDeps = Agg(
    ivy"org.scala-js::scalajs-dom_sjs1:2.2.0"
  )
  def scalacOptions = Seq("-feature", "-language:implicitConversions", "-deprecation")
}