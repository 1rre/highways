package es.tmoor.highways.level

import org.scalajs.dom.{document, SVGPathElement, SVGCircleElement, SVGElement}
import collection.mutable.Buffer
import math.{atan2, tanh, tan, Pi}
import org.scalajs.dom.SVGSVGElement
import es.tmoor.highways.Drawable

sealed trait RoadLike extends Drawable {
  var path: Option[SVGPathElement] = None
  
  object Line {
    def fromPointAndGradient(x: Double, y: Double, m: Double): Line = Line(m, y - m*x)
    def fromTwoPoints(x1: Double, y1: Double, x2: Double, y2: Double): Line = {
      fromPointAndGradient(x1, x2, (y2 - x1) / (x2 - x1))
    }
  }
  sealed trait LineLike {
    def gradient: Double
    def plot(colour: String): SVGPathElement
    def intersectionWith(that: LineLike): Option[(Double, Double)]
  }
  case class HLine(y: Double) extends LineLike {
    def gradient: Double = 0
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
    def gradient: Double = Double.PositiveInfinity
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
    def gradient: Double = m
    def fx(x: Double): Double = m * x + c
    def fy(y: Double): Double = (y - c) / m
    override def toString = s"y = $m*x + $c"
    def plot(colour: String): SVGPathElement = {
      val x1 = 0
      val y1 = fx(0)
      val x2 = page.clientWidth
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

case class FixedRoad(val point: FixedPoint)(val page: SVGSVGElement) extends RoadLike {
  def draw(): Unit = {
    val line = document.createElementNS("http://www.w3.org/2000/svg", "path").asInstanceOf[SVGPathElement]
    val x1 = scaleX(point.x1)
    val y1 = scaleY(point.y1)
    val x2 = scaleX(point.x)
    val y2 = scaleY(point.y)
    line.setAttribute("style", s"fill: none; stroke: grey; stroke-width: 4px; z-index: 100;")
    line.setAttribute("d", s"M $x1 $y1 L $x2 $y2")
    page.appendChild(line)
    path = Some(line)
  }
}

case class DrawnRoad(val sourcePoint: PointLike, val destPoint: PointLike, val inputAngle: Double, var angleSkew: Double = 0d)(val page: SVGSVGElement) extends RoadLike {
  val addNodes = Buffer[SVGElement]()
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
  
  def clearGuidelines(): Unit = {
    addNodes.foreach(_.remove())
    addNodes.clear()
  }
  
  def clear(): Unit = {
    path.foreach(_.remove())
    path = None
    addNodes.foreach(_.remove())
    addNodes.clear()
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
        addNodes += l1.plot("red")
        addNodes += l2.plot("yellow")
        addNodes += plotDot(midPointX, midPointY, "green")
        addNodes += plotDot(ix, iy, "blue")
      }
      path = Some(plotArc(sourcePoint.x, sourcePoint.y, destPoint.x, destPoint.y, ix, iy, "grey", strokeWidth))
    }
  }
  
  def draw(): Unit = draw(false)
}
