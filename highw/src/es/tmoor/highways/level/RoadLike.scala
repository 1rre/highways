package es.tmoor.highways.level

import org.scalajs.dom.{document, SVGPathElement, SVGCircleElement, SVGElement}
import collection.mutable.Buffer
import math.{atan2, tanh, tan, Pi}
import org.scalajs.dom.SVGSVGElement
import es.tmoor.highways.Drawable

sealed trait RoadLike extends Drawable {
  var path: Option[SVGPathElement] = None
  
  object Line {
    def fromPointAndAngle(x: Double, y: Double, t: Double): LineLike = {
      if t == Pi || t == 0 then VLine(x)
      else if t == Pi/2 || t == 3*Pi/2 then HLine(y)
      else Line.fromPointAndGradient(x, y, 1 / tan(t))
    }
    def fromPointAndGradient(x: Double, y: Double, m: Double): LineLike = {
      if (m == 0) HLine(y)
      else if (m == Double.PositiveInfinity || m == Double.NegativeInfinity) VLine(x)
      else Line(m, y - m*x)
    }
    def fromTwoPoints(x1: Double, y1: Double, x2: Double, y2: Double): LineLike = {
      fromPointAndGradient(x1, x2, (y2 - x1) / (x2 - x1))
    }
  }
  sealed trait LineLike {
    def gradient: Double
    def plot(colour: String): SVGPathElement
    def intersectionWith(that: LineLike): Option[(Double, Double)]
  }

  // TODO: Add directionality for "isBehind"
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
  
  // TODO: Add directionality for "isBehind"
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
  
  // TODO: Add directionality for "isBehind"
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
        case that: Line if this.m == that.m => None
        case that: Line =>
        val x = (that.c - this.c) / (this.m - that.m)
        val p1 = Some((x, fx(x)))
        val p2 = Some((x, that.fx(x)))
        p1
      }
    }
  }
  def generatePoints() = {
    path.map { path =>
      val step = path.getTotalLength() / 100
      (0 to 100).map { i =>
        val pos = step * i
        val point = path.getPointAtLength(pos)
        (point.x, point.y)
      }
    }.getOrElse(Nil)
  }
  var pointsCache = generatePoints()
  def points: Seq[(Double, Double)] = pointsCache
  def drawRoad(): Unit
  final def draw(): Unit = {
    drawRoad()
    pointsCache = generatePoints()
  }
}

case class FixedRoad(val point: FixedPoint)(val page: SVGSVGElement) extends RoadLike {
  def drawRoad(): Unit = {
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

sealed abstract class DrawnRoad extends RoadLike {
  val sourcePoint: AngledPoint
  val destPoint: PointLike

  sourcePoint.outputs += this
  destPoint.inputs += this
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
    pointsCache = generatePoints()
  }
  
  def clear(): Unit = {
    path.foreach(_.remove())
    path = None
    addNodes.foreach(_.remove())
    addNodes.clear()
    sourcePoint.outputs -= this
    destPoint.inputs -= this
  }
  

  // TODO: Broken... Fix to maybe work on line and check f(x)?
  def isBehind(x1: Double, y1: Double, x2: Double, y2: Double, angle: Double): Boolean = {
    val a1 = atan2(y2 - y1, x2 - x1)
    //println(s"ADIFF: ${normaliseAngle((atan2(y2 - y1, x2 - x1) - angle).abs)} (> pi? ${normaliseAngle((atan2(y2 - y1, x2 - x1) - angle).abs) > Pi})")
    normaliseAngle((atan2(y2 - y1, x2 - x1) - angle).abs) > Pi
  }
  
  def draw(useGuidelines: Boolean): Unit
  
  def drawRoad(): Unit = draw(false)
  def angleSkew: Double
  def incrAngleSkew(x: Double): Unit = {}
}

sealed trait QuadraticRoad extends DrawnRoad {
  protected def diffX = destPoint.x - sourcePoint.x
  protected def diffY = destPoint.y - sourcePoint.y
  protected def lengthOnLine = (tanh(angleSkew) + 1) / 2
  protected def midPointX = sourcePoint.x + lengthOnLine * diffX
  protected def midPointY = sourcePoint.y + lengthOnLine * diffY
  protected def l1 = Line.fromPointAndGradient(midPointX, midPointY, -(sourcePoint.x - destPoint.x) / (sourcePoint.y - destPoint.y))
  protected def l2 = Line.fromPointAndAngle(sourcePoint.x, sourcePoint.y, sourcePoint.angle)
  protected def intersection = l1.intersectionWith(l2)
  def draw(useGuidelines: Boolean): Unit = {
    println(s"Drawing with Angle Skew: $angleSkew")
    clear()
    intersection.foreach { (ix, iy) =>
      if (useGuidelines) {
        addNodes += l1.plot("red")
        addNodes += l2.plot("yellow")
        addNodes += plotDot(midPointX, midPointY, "green")
        addNodes += plotDot(ix, iy, "blue")
      } else {
        pointsCache = generatePoints()
      }
      path = Some(plotArc(sourcePoint.x, sourcePoint.y, destPoint.x, destPoint.y, ix, iy, "grey", strokeWidth))
    }
  }
}

sealed trait SnappedRoad extends DrawnRoad {
  def angleSkew: Double = 0
}

case class FreeFormRoad(sourcePoint: AngledPoint, destPoint: PointLike, var angleSkew: Double = 0d)(val page: SVGSVGElement) extends QuadraticRoad {
  override def incrAngleSkew(dx: Double): Unit = {
    angleSkew += dx
  }
}

case class SnappedQuadraticRoad(sourcePoint: AngledPoint, destPoint: PointLike)(val page: SVGSVGElement) extends QuadraticRoad with SnappedRoad {
  override protected val diffX = super.diffX
  override protected val diffY = super.diffY
  override protected val lengthOnLine = super.lengthOnLine
  override protected val midPointX = super.midPointX
  override protected val midPointY = super.midPointY
  override protected val l1 = super.l1
  override protected val l2 = super.l2
  override protected val intersection = super.intersection
}


case class CubicRoad(val sourcePoint: AngledPoint, val destPoint: AngledPoint)(val page: SVGSVGElement) extends SnappedRoad {
  def draw(useGuidelines: Boolean): Unit = {
    clear()
    val startingAngle = sourcePoint.angle
    val destAngle = normaliseAngle(destPoint.angle)
    val diffX = destPoint.x - sourcePoint.x
    val diffY = destPoint.y - sourcePoint.y
    val midPointX = sourcePoint.x + diffX / 2
    val midPointY = sourcePoint.y + diffY / 2

    val l1 = Line.fromPointAndAngle(sourcePoint.x, sourcePoint.y, startingAngle)
    val l2 = Line.fromPointAndAngle(destPoint.x, destPoint.y, destAngle)

    val intersectionWith = l1.intersectionWith(l2)
    println(s"Intersect at: $intersectionWith")
    val intersectionWithB = l2.intersectionWith(l1)
    println(s"Intersect at: $intersectionWithB")
    val useCubic = intersectionWith.map{(x, y) => 
      val behindA = isBehind(startingAngle, x, y, sourcePoint.x, sourcePoint.y)
      val behindB = isBehind(destAngle, destPoint.x, destPoint.y, x, y)
      println(s"Behind? ${(behindA, behindB)}")
      behindA || behindB
    }.getOrElse(true)
    println(s"Use Cubic: $useCubic")
    if (useCubic) {
      if (useGuidelines) {
        addNodes += l1.plot("red")
        addNodes += l2.plot("yellow")
        addNodes += plotDot(midPointX, midPointY, "green")
      }
    }
    else {
      println("Using Quadratic")
      intersectionWith.foreach { (ix, iy) =>
        if (useGuidelines) {
          addNodes += l1.plot("red")
          addNodes += l2.plot("yellow")
          addNodes += plotDot(midPointX, midPointY, "green")
          addNodes += plotDot(ix, iy, "blue")
        } else {
          pointsCache = generatePoints()
        }
        path = Some(plotArc(sourcePoint.x, sourcePoint.y, destPoint.x, destPoint.y, ix, iy, "grey", strokeWidth))
      }
    }
  }
}
