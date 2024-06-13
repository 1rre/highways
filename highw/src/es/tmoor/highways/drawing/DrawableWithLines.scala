package es.tmoor.highways.drawing

import es.tmoor.highways.Drawable
import org.scalajs.dom.{document, SVGPathElement, SVGCircleElement, SVGElement, SVGSVGElement}
import math.{atan2, tanh, tan, Pi}
import es.tmoor.highways.util.{Line, VLine, HLine, PlusX, LineLike, MinusX, LineDirection}
import es.tmoor.lineslib.Bezier

trait DrawableWithLines extends Drawable {
  private val owner = this
  var path: Option[SVGPathElement] = None
  // TODO: move to individual lines
  def bezier: Option[Bezier] = {
    path.map(_.getAttribute("d")).collect {
      case s"M $x1 $y1 Q $xc $yc $x2 $y2" =>
        Bezier(
          unScaleX(x1.toDouble), unScaleY(y1.toDouble),
          unScaleX(xc.toDouble), unScaleY(yc.toDouble),
          unScaleX(x2.toDouble), unScaleY(y2.toDouble)
        )
      }
    }
  trait DrawableLine {
    this: LineLike =>
    def plot(colour: String): SVGPathElement = this.plot(owner, colour)
  }
  
  object DrawableLine {
    def fromPointAndAngle(x: Double, y: Double, t: Double): LineLike & DrawableLine = {
      if t == 0 then new VLine(x, MinusX) with DrawableLine
      else if t == Pi/2 then new HLine(y, PlusX) with DrawableLine
      else if t == Pi then new VLine(x, PlusX) with DrawableLine
      else if t == 3*Pi/2 then new HLine(y, MinusX) with DrawableLine
      else if t < Pi then fromPointAndGradient(x, y, 1 / tan(t), PlusX)
      else fromPointAndGradient(x, y, 1 / tan(t), MinusX)
    }
    def fromPointAndGradient(x: Double, y: Double, m: Double, d: LineDirection): LineLike & DrawableLine = {
      if (m == 0) new HLine(y, d) with DrawableLine
      else if (m == Double.PositiveInfinity || m == Double.NegativeInfinity) new VLine(x, d) with DrawableLine
      else new Line(m, y - m*x, d) with DrawableLine
    }
    def fromTwoPoints(x1: Double, y1: Double, x2: Double, y2: Double): LineLike & DrawableLine = {
      fromPointAndGradient(x1, x2, (y2 - y1) / (x2 - x1), if x2 > x1 then PlusX else MinusX)
    }
  }

  def drawRoad(): Unit
  final def draw(): Unit = {
    drawRoad()
  }
}