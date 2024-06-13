package es.tmoor.highways

import org.scalajs.dom.SVGSVGElement

trait SvgUser {
  val page: SVGSVGElement

  given implicitlyApplySVGElement[T <: SvgUser]: Conversion[SVGSVGElement => T, T] with
    def apply(x: SVGSVGElement => T): T = x(page)

  def scaleX(xc: Double): Int = (page.clientWidth * xc).round.toInt
  def scaleY(yc: Double): Int = (page.clientHeight * yc).round.toInt
  def scaleXd(xc: Double): Double = (page.clientWidth * xc)
  def scaleYd(yc: Double): Double = (page.clientHeight * yc)
  def unScaleX(xc: Double): Double = xc / page.clientWidth
  def unScaleY(yc: Double): Double = yc / page.clientHeight
}