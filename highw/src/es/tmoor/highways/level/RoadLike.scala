package es.tmoor.highways.level

import org.scalajs.dom.{document, SVGPathElement, SVGCircleElement, SVGElement}
import collection.mutable.Buffer
import math.{atan2, tanh, tan, Pi}
import org.scalajs.dom.SVGSVGElement
import es.tmoor.highways.Drawable

sealed trait RoadLike extends Drawable with LineDrawingTools

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
  def isBehind(x1: Double, y1: Double, x2: Double, y2: Double, l: LineLike): Boolean = {
    l.distBetween(x1, y1, x2, y2) < 0
  }
  def isBehind(x1: Double, y1: Double, x2: Double, y2: Double, a: Double): Boolean = {
    val l = Line.fromPointAndAngle(x1, y1, a)
    isBehind(x1, y1, x2, y2, l)
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
  // TODO: Find angle for l1
  protected def l1Direction = if diffX > 0 then PlusX else MinusX
  protected def l1 = Line.fromPointAndGradient(midPointX, midPointY, -(sourcePoint.x - destPoint.x) / (sourcePoint.y - destPoint.y), l1Direction)
  protected def l2 = Line.fromPointAndAngle(sourcePoint.x, sourcePoint.y, sourcePoint.angle)
  protected def intersection = l1.intersectionWith(l2)
  def draw(useGuidelines: Boolean): Unit = {
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
    val intersectionWithB = l2.intersectionWith(l1)
    val useCubic = intersectionWith.map{(x, y) => 
      val behindA = isBehind(sourcePoint.x, sourcePoint.y, x, y, l1)
      val behindB = isBehind(destPoint.x, destPoint.y, x, y, l2)
      behindA || behindB
    }.getOrElse(true)
    if (useCubic) {
      if (useGuidelines) {
        addNodes += l1.plot("red")
        addNodes += l2.plot("yellow")
        addNodes += plotDot(midPointX, midPointY, "green")
      }
    }
    else {
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
