package es.tmoor.highways.level

import org.scalajs.dom.{document, SVGCircleElement}
import math.{sin, cos, atan2, hypot, Pi, atan}
import scala.collection.mutable.Buffer
import org.scalajs.dom.SVGElement
import org.scalajs.dom.SVGSVGElement
import es.tmoor.highways.Drawable
import es.tmoor.highways.util.normaliseAngle
import es.tmoor.highways.util.MinusX

sealed trait PointLike extends Drawable {
  val x: Double
  val y: Double
  protected val colour: String
  var node: Option[SVGCircleElement] = None
  val inputs = Buffer[RoadLike]()
  val outputs = Buffer[RoadLike]()
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
  
  val dir = {
  }
  // TODO: get directionality properly!! instead of just angle
  val angle = {
    owner.bezier.map { bz =>
      val t = bz.closestParametric(x, y)
      val mod = if (bz.dydt(t) > 0) Pi else 0
      normaliseAngle(atan(bz.gradientAt(x, y)) + mod)
    }.get
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

sealed trait FixedPoint extends AngledPoint {
  val id: Int
  val x1: Double
  val y1: Double
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

  val x = x1 + sin(angle) * 0.075 * 2 / 3d
  val y = y1 - cos(angle) * 0.075

  protected val colour = "blue"
  val demandArrows = Buffer[SVGElement]()

  def clearDemandArrows(): Unit = {
    demandArrows.foreach(_.remove())
    demandArrows.clear()
  }

  def drawDemandArrows(satisfied: Seq[Int]): Unit = {
    demand.foreach { (point, demand) =>
      if (demand > 0) {
        val opacity = if (satisfied.contains(point.id)) "30%" else "80%"
        val line = plotLine(
          x,
          y,
          point.x,
          point.y,
          IdColours(point.id)
        )
        line.setAttribute(
          "style",
          s"fill: none; stroke: ${IdColours(point.id)}; stroke-width: ${demand * 3}px; z-index: 100; opacity: $opacity;"
        )
        demandArrows += line
      }
    }
  }

  override def activate(): Unit = {
    drawDemandArrows(satisfiedDemand)
    super.activate()
  }

  override def deactivate(): Unit = {
    clearDemandArrows()
    super.deactivate()
  }
  
  var satisfiedDemand: Seq[Int] = Nil

  def setSatisfiedDemand(roads: Seq[RoadLike]): Unit = {
    val reachable = Buffer[PointLike]()
    def checkPoint(p: PointLike): Unit = {
      p.outputs.foreach {
        case r: DrawnRoad => checkRoad(r)
        case _ =>
      }
    }
    def checkRoad(road: DrawnRoad): Unit = {
      if (!reachable.contains(road.destPoint)) {
        reachable += road.destPoint
        checkPoint(road.destPoint)
      }
    }
    for (case (r: DrawnRoad) <- roads if r.sourcePoint == this) {
      checkRoad(r)
    }
    satisfiedDemand = reachable.toArray.collect {
      case p: SinkPoint => p.id
    }
  }

  override def draw(): Unit = {
    if (active) drawDemandArrows(satisfiedDemand)
    else clearDemandArrows()
    super.draw()
  }
}

case class SinkPoint(x1: Double, y1: Double, angle: Double, id: Int)(val page: SVGSVGElement) extends FixedPoint {
  protected val colour = "red"
  
  val x = x1 - sin(angle) * 0.075 * 2 / 3d
  val y = y1 + cos(angle) * 0.075
}
