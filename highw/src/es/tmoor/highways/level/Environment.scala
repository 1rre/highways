package es.tmoor.highways.level

import org.scalajs.dom.SVGSVGElement

class Environment(val page: SVGSVGElement) {
  val roadEnvironment = RoadEnvironment(this)
  val pointEnvironment = PointEnvironment(this)
  val blockEnvironment = BlockEnvironment(this)
  
  def scaleX(xc: Double): Int = (page.clientWidth * xc).round.toInt
  def scaleY(yc: Double): Int = (page.clientHeight * yc).round.toInt
  def unScaleX(xc: Double): Double = xc / page.clientWidth
  def unScaleY(yc: Double): Double = yc / page.clientHeight
}