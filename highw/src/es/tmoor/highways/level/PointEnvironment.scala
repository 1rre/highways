package es.tmoor.highways.level

import org.scalajs.dom.{document, SVGCircleElement}
import math.{sin, cos}
import scala.collection.mutable.Buffer
import org.scalajs.dom.SVGElement

class PointEnvironment(environment: Environment) {
  sealed trait PointLike extends Drawable {
    val x: Double
    val y: Double
    protected val colour: String
    var node: Option[SVGCircleElement] = None
    def draw(): Unit = {
      node = Some(document.createElementNS("http://www.w3.org/2000/svg", "circle").asInstanceOf[SVGCircleElement])
      node.foreach { node =>
        node.cx.baseVal.value = environment.scaleX(x)
        node.cy.baseVal.value = environment.scaleY(y)
        node.r.baseVal.value = environment.scaleX(0.025) min environment.scaleY(0.025)
        node.setAttribute("style", s"fill: #FFFFFF00; stroke: $colour")
        environment.page.appendChild(node)
      }
    }
  }

  sealed trait RemovablePoint extends PointLike {
    def remove(): Unit = {
      node.map(_.remove())
      node = None
    }
  }

  case class RoadPoint(x: Double, y: Double) extends RemovablePoint {
    protected val colour: String = "yellow"
  }
  case class RoadConnectionPoint(x: Double, y: Double, owner: RoadEnvironment#DrawnRoad) extends RemovablePoint {
    protected val colour = "grey"
  }

  sealed trait FixedPoint extends PointLike {
    val angle: Double
    val id: Int
    val x1: Double
    val y1: Double
    val x = x1 + sin(angle) * 0.075 * 2/3d
    val y = y1 - cos(angle) * 0.075
    
    protected var active = false

    def activate(): Unit = {
      if (!node.isDefined) draw()
      active = true
      node.foreach( node =>
        node.setAttribute("style", s"fill: #FFFFFF00; stroke: $colour; stroke-width: 4px;")
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
          node.setAttribute("style", s"fill: #FFFFFF00; stroke: $colour; stroke-width: 4px;")
        )
      }
    }
  }
  
  case class SourcePoint(x1: Double, y1: Double, angle: Double, id: Int, demand: Map[SinkPoint, Int] = Map()) extends FixedPoint {
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
          val line = environment.roadEnvironment.plotLine(x, y, point.x, point.y, IdColours(point.id))
          line.setAttribute("style", s"fill: none; stroke: ${IdColours(point.id)}; stroke-width: ${demand * 3}px; z-index: 100; opacity: 30%;")
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

  case class SinkPoint(x1: Double, y1: Double, angle: Double, id: Int) extends FixedPoint {
    protected val colour = "red"
  }
}
