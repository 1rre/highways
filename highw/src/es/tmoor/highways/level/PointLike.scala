package es.tmoor.highways.level

import org.scalajs.dom.{document, SVGCircleElement}
import math.{sin, cos, atan2, hypot}
import scala.collection.mutable.Buffer
import org.scalajs.dom.SVGElement
import org.scalajs.dom.SVGSVGElement
import es.tmoor.highways.Drawable

sealed trait PointLike extends Drawable {
  val x: Double
  val y: Double
  protected val colour: String
  var node: Option[SVGCircleElement] = None
  def draw(): Unit = {
    node = Some(
      document
        .createElementNS("http://www.w3.org/2000/svg", "circle")
        .asInstanceOf[SVGCircleElement]
    )
    node.foreach { node =>
      node.cx.baseVal.value = scaleX(x)
      node.cy.baseVal.value = scaleY(y)
      node.r.baseVal.value = scaleX(0.025) min scaleY(0.025)
      node.setAttribute("style", s"fill: #FFFFFF00; stroke: $colour")
      page.appendChild(node)
    }
  }
}

sealed trait AngledPoint extends PointLike {
  val angle: Double
}

sealed trait RemovablePoint extends PointLike {
  def remove(): Unit = {
    node.map(_.remove())
    node = None
  }
}

case class RoadPoint(x: Double, y: Double)(val page: SVGSVGElement) extends RemovablePoint {
  protected val colour: String = "yellow"
}
case class RoadConnectionPoint(x: Double, y: Double, owner: DrawnRoad)(val page: SVGSVGElement) extends RemovablePoint with AngledPoint {
  val angle = {
    val pts = owner.points
    val closestPoint = pts.minBy((x1, y1) => hypot(scaleX(x) - x1, scaleX(y) - y1))
    val idx = pts.indexOf(closestPoint)
    //val idx = pts.indexOf((x, y))
    val pPrev = pts((idx - 1) max 0)
    val pNext = pts((idx + 1) min (pts.length - 1))
    val a = atan2(pNext._2 - pPrev._2, pNext._1 - pPrev._1)
    println(s"Angle of $pPrev => $pNext: $a")
    a
  }
  protected val colour = "grey"
  def activate(): Unit = {
    owner.activate()
    if (node.isEmpty) draw()
  }
  def deactivate(): Unit = {
    owner.deactivate()
    node.foreach(_.remove())
    node = None
  }
}

sealed trait FixedPoint extends PointLike {
  val angle: Double
  val id: Int
  val x1: Double
  val y1: Double
  val x = x1 + sin(angle) * 0.075 * 2 / 3d
  val y = y1 - cos(angle) * 0.075

  protected var active = false

  def activate(): Unit = {
    if (!node.isDefined) draw()
    active = true
    node.foreach(node =>
      node.setAttribute(
        "style",
        s"fill: #FFFFFF00; stroke: $colour; stroke-width: 4px;"
      )
    )
  }

  def deactivate(): Unit = {
    if (!node.isDefined) draw()
    active = false
    node.foreach(node =>
      node.setAttribute("style", s"fill: #FFFFFF00; stroke: $colour;")
    )
  }

  override def draw(): Unit = {
    super.draw()
    if (active) {
      node.foreach(node =>
        node.setAttribute(
          "style",
          s"fill: #FFFFFF00; stroke: $colour; stroke-width: 4px;"
        )
      )
    }
  }
}

case class SourcePoint(x1: Double, y1: Double, angle: Double, id: Int, demand: Map[SinkPoint, Int] = Map())(val page: SVGSVGElement) extends FixedPoint with AngledPoint {
  final val IdColours = Seq(
    "red",
    "green",
    "yellow",
    "blue",
    "cyan",
    "pink",
    "orange"
  )

  protected val colour = "blue"
  val demandArrows = Buffer[SVGElement]()

  def clearDemandArrows(): Unit = {
    demandArrows.foreach(_.remove())
    demandArrows.clear()
  }

  def drawDemandArrows(): Unit = {
    demand.foreach { (point, demand) =>
      if (demand > 0) {
        val line = plotLine(
          x,
          y,
          point.x,
          point.y,
          IdColours(point.id)
        )
        line.setAttribute(
          "style",
          s"fill: none; stroke: ${IdColours(point.id)}; stroke-width: ${demand * 3}px; z-index: 100; opacity: 30%;"
        )
        demandArrows += line
      }
    }
  }

  override def activate(): Unit = {
    drawDemandArrows()
    super.activate()
  }

  override def deactivate(): Unit = {
    clearDemandArrows()
    super.deactivate()
  }

  override def draw(): Unit = {
    if (active) drawDemandArrows()
    else clearDemandArrows()
    super.draw()
  }
}

case class SinkPoint(x1: Double, y1: Double, angle: Double, id: Int)(val page: SVGSVGElement) extends FixedPoint {
  protected val colour = "red"
}
