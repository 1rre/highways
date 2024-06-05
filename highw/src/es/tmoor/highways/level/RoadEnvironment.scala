package es.tmoor.highways.level

import org.scalajs.dom.{document, SVGPathElement, SVGCircleElement, SVGElement}
import collection.mutable.Buffer
import math.{atan2, tanh, tan, Pi}

class RoadEnvironment(environment: Environment) {

  def plotArc(x1s: Double, y1s: Double, x2s: Double, y2s: Double, xcs: Double, ycs: Double, colour: String, width: Int): SVGPathElement = {
    val line = document.createElementNS("http://www.w3.org/2000/svg", "path").asInstanceOf[SVGPathElement]
    val x1 = environment.scaleX(x1s)
    val y1 = environment.scaleY(y1s)
    val x2 = environment.scaleX(x2s)
    val y2 = environment.scaleY(y2s)
    val xc = environment.scaleX(xcs)
    val yc = environment.scaleY(ycs)
    line.setAttribute("style", s"fill: none; stroke: $colour; stroke-width: ${width}px; z-index: 100;")
    line.setAttribute("d", s"M $x1 $y1 Q $xc $yc $x2 $y2")
    environment.page.appendChild(line)
    line
    
  }

  def plotLine(x1In: Double, y1In: Double, x2In: Double, y2In: Double, colour: String): SVGPathElement = {
    val line = document.createElementNS("http://www.w3.org/2000/svg", "path").asInstanceOf[SVGPathElement]
    val x1 = environment.scaleX(x1In)
    val y1 = environment.scaleY(y1In)
    val x2 = environment.scaleX(x2In)
    val y2 = environment.scaleY(y2In)
    line.setAttribute("style", s"fill: none; stroke: $colour; stroke-width: 2px; z-index: 100;")
    line.setAttribute("d", s"M $x1 $y1 L $x2 $y2")
    environment.page.appendChild(line)
    line
  }

  def plotDot(x: Double, y: Double, colour: String): SVGCircleElement = {
    val node = document.createElementNS("http://www.w3.org/2000/svg", "circle").asInstanceOf[SVGCircleElement]
    node.cx.baseVal.value = environment.scaleX(x)
    node.cy.baseVal.value = environment.scaleY(y)
    node.r.baseVal.value = environment.scaleX(0.005) min environment.scaleY(0.005)
    node.setAttribute("style", s"fill: #FFFFFF00; stroke: $colour")
    environment.page.appendChild(node)
    node
  }

  object Line {
    def fromPointAndGradient(x: Double, y: Double, m: Double): Line = Line(m, y - m*x)
  }
  sealed trait LineLike {
    def plot(colour: String): SVGPathElement
    def intersectionWith(that: LineLike): Option[(Double, Double)]
  }
  case class HLine(y: Double) extends LineLike {
    def plot(colour: String): SVGPathElement = plotLine(0, y, 1, y, colour)
    def intersectionWith(that: LineLike): Option[(Double, Double)] = {
      that match {
        case HLine(y) => None
        case VLine(x) => Some((x, y))
        case Line(m, c) => Some(((y - c) / m, y))
      }
    }
  }

  case class VLine(x: Double) extends LineLike {
    def plot(colour: String): SVGPathElement = plotLine(x, 0, x, 1, colour)
    def intersectionWith(that: LineLike): Option[(Double, Double)] = {
      that match {
        case VLine(x) => None
        case HLine(y) => Some((x, y))
        case Line(m, c) => Some((x, m*x+c))
      }
    }
  }

  case class Line(m: Double, c: Double) extends LineLike {
    def fx(x: Double): Double = m * x + c
    def fy(y: Double): Double = (y - c) / m
    override def toString = s"y = $m*x + $c"
    def plot(colour: String): SVGPathElement = {
      val x1 = 0
      val y1 = fx(0)
      val x2 = environment.page.clientWidth
      val y2 = fx(x2)
      plotLine(x1, y1, x2, y2, colour)
    }
    def intersectionWith(that: LineLike): Option[(Double, Double)] = {
      that match {
        case VLine(x) => Some((x, fx(x)))
        case HLine(y) => Some((fy(y), y))
        case that: Line if this.m != that.m => None
        case that: Line =>
          val x = (this.c - that.c) / (this.m - that.m)
          Some((x, fx(x)))
      }
    }
  }

  sealed trait RoadLike extends Drawable {
    var path: Option[SVGPathElement] = None
    def points: Seq[(Double, Double)] =
      path.map { path =>
        val step = path.getTotalLength() / 100
        (0 to 100).map { i =>
          val pos = step * i
          val point = path.getPointAtLength(pos)
          (point.x, point.y)
        }
      }.getOrElse(Nil)
    def draw(): Unit
  }
  
  final class FixedRoad(val point: PointEnvironment#FixedPoint) extends RoadLike {
    def draw(): Unit = {
      val line = document.createElementNS("http://www.w3.org/2000/svg", "path").asInstanceOf[SVGPathElement]
      val x1 = environment.scaleX(point.x1)
      val y1 = environment.scaleY(point.y1)
      val x2 = environment.scaleX(point.x)
      val y2 = environment.scaleY(point.y)
      line.setAttribute("style", s"fill: none; stroke: grey; stroke-width: 4px; z-index: 100;")
      line.setAttribute("d", s"M $x1 $y1 L $x2 $y2")
      environment.page.appendChild(line)
      path = Some(line)
    }
  }

  sealed class DrawnRoad(val sourcePoint: PointEnvironment#PointLike, val destPoint: PointEnvironment#PointLike, val inputAngle: Double, var angleSkew: Double = 0d) extends RoadLike {
    val nodes = Buffer[SVGElement]()
    private var activated = false
    def strokeWidth = if activated then 4 else 2

    def activate(): Unit = {
      activated = true
      draw()
    }

    def deactivate(): Unit = {
      activated = false
      draw()
    }

    def clear(): Unit = {
      path = None
      nodes.foreach { node =>
        node.remove()
      }
    }

    def isBehind(x1: Double, y1: Double, x2: Double, y2: Double, angle: Double): Boolean = {
      val a1 = atan2(y2 - y1, x2 - x1)
      (atan2(y2 - y1, x2 - x1)  - angle).abs % (2*Pi) > Pi
    }

    def draw(useGuidelines: Boolean): Unit = {
      clear()
      val diffX = destPoint.x - sourcePoint.x
      val diffY = destPoint.y - sourcePoint.y
      val lengthOnLine = (tanh(angleSkew) + 1) / 2
      val midPointX = sourcePoint.x + lengthOnLine * diffX
      val midPointY = sourcePoint.y + lengthOnLine * diffY
      val l1 =
        if (isBehind(sourcePoint.x, sourcePoint.y, midPointX, midPointY, inputAngle))
          Line.fromPointAndGradient(midPointX, midPointY, (sourcePoint.x - destPoint.x) / (sourcePoint.y - destPoint.y))
        else
          Line.fromPointAndGradient(midPointX, midPointY, -(sourcePoint.x - destPoint.x) / (sourcePoint.y - destPoint.y))
      val l2 =
        if inputAngle == Pi || inputAngle == 0 then VLine(sourcePoint.x)
        else if inputAngle == Pi/2 || inputAngle == 3*Pi/2 then HLine(sourcePoint.y)
        else Line.fromPointAndGradient(sourcePoint.x, sourcePoint.y, 1 / tan(inputAngle))
      
      l1.intersectionWith(l2).foreach { (ix, iy) =>
        if (useGuidelines) {
          nodes += l1.plot("red")
          nodes += l2.plot("yellow")
          nodes += plotDot(midPointX, midPointY, "green")
          nodes += plotDot(ix, iy, "blue")
          }
        path = Some(plotArc(sourcePoint.x, sourcePoint.y, destPoint.x, destPoint.y, ix, iy, "grey", strokeWidth))
        nodes ++= path
      }
    }

    def draw(): Unit = draw(false)
  }
}