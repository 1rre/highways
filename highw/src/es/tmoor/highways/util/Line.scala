package es.tmoor.highways.util

import es.tmoor.highways.Drawable
import math.{atan2, tanh, tan, Pi}
import org.scalajs.dom.{document, SVGPathElement, SVGCircleElement, SVGElement, SVGSVGElement}

sealed trait LineDirection(val d: Int)
case object PlusX extends LineDirection(1)
case object MinusX extends LineDirection(-1)


object Line {
  def fromPointAndAngle(x: Double, y: Double, t: Double): LineLike = {
    if t == 0 then VLine(x, MinusX)
    else if t == Pi/2 then HLine(y, PlusX)
    else if t == Pi then VLine(x, PlusX)
    else if t == 3*Pi/2 then HLine(y, MinusX)
    else if t < Pi then fromPointAndGradient(x, y, 1 / tan(t), PlusX)
    else fromPointAndGradient(x, y, 1 / tan(t), MinusX)
  }
  def fromPointAndGradient(x: Double, y: Double, m: Double, d: LineDirection): LineLike = {
    if (m == 0) HLine(y, d)
    else if (m == Double.PositiveInfinity || m == Double.NegativeInfinity) VLine(x, d)
    else Line(m, y - m*x, d)
  }
  def fromTwoPoints(x1: Double, y1: Double, x2: Double, y2: Double): LineLike = {
    fromPointAndGradient(x1, x2, (y2 - y1) / (x2 - x1), if x2 > x1 then PlusX else MinusX)
  }
}

sealed trait LineLike {
  def gradient: Double
  def gradientAt(x: Double, y: Double): Double = gradient
  protected def plot(plotable: Drawable, colour: String): SVGPathElement
  def intersectionWith(that: LineLike): Option[(Double, Double)]
  def direction: LineDirection
  def distBetween(x1: Double, y1: Double, x2: Double, y2: Double): Double
}

case class HLine(y: Double, direction: LineDirection) extends LineLike {
  def gradient: Double = 0
  protected def plot(plotable: Drawable, colour: String): SVGPathElement = plotable.plotLine(0, y, 1, y, colour)
  def intersectionWith(that: LineLike): Option[(Double, Double)] = {
    that match {
      case HLine(y, _) => None
      case VLine(x, _) => Some((x, y))
      case Line(m, c, _) => Some(((y - c) / m, y))
    }
  }
  def distBetween(x1: Double, y1: Double, x2: Double, y2: Double): Double = (x2 - x1) * direction.d
}

case class VLine(x: Double, direction: LineDirection) extends LineLike {
  def gradient: Double = Double.PositiveInfinity
  protected def plot(plotable: Drawable, colour: String): SVGPathElement = plotable.plotLine(x, 0, x, 1, colour)
  def intersectionWith(that: LineLike): Option[(Double, Double)] = {
    that match {
      case VLine(x, _) => None
      case HLine(y, _) => Some((x, y))
      case Line(m, c, _) => Some((x, m*x+c))
    }
  }
  def distBetween(x1: Double, y1: Double, x2: Double, y2: Double): Double = (y2 - y1) * direction.d
}

case class Line(m: Double, c: Double, direction: LineDirection) extends LineLike {
  def gradient: Double = m
  def fx(x: Double): Double = m * x + c
  def fy(y: Double): Double = (y - c) / m
  override def toString = s"y = $m*x + $c"
  protected def plot(plotable: Drawable, colour: String): SVGPathElement = {
    val x1 = 0
    val y1 = fx(0)
    val x2 = plotable.page.clientWidth
    val y2 = fx(x2)
    plotable.plotLine(x1, y1, x2, y2, colour)
  }
  def intersectionWith(that: LineLike): Option[(Double, Double)] = {
    that match {
      case VLine(x, _) => Some((x, fx(x)))
      case HLine(y, _) => Some((fy(y), y))
      case that: Line if this.m == that.m => None
      case that: Line =>
      val x = (that.c - this.c) / (this.m - that.m)
      val p1 = Some((x, fx(x)))
      val p2 = Some((x, that.fx(x)))
      p1
    }
  }
  
  def distBetween(x1: Double, y1: Double, x2: Double, y2: Double): Double = {
    val rawDist = math.hypot(x2 - x1, y2 - y1)
    if (y2 > y1) {
      gradient.sign * -direction.d * rawDist
    } else {
      gradient.sign * direction.d * rawDist
    }
  }
}
