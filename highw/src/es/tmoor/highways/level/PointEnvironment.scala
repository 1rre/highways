package es.tmoor.highways.level

import org.scalajs.dom.{document, SVGCircleElement}
import math.{sin, cos}

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
    
    private var active = false

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
  
  case class SourcePoint(x1: Double, y1: Double, angle: Double, id: Int, demand: Map[Int, Int] = Map()) extends FixedPoint {
    protected val colour = "blue"
  }

  case class SinkPoint(x1: Double, y1: Double, angle: Double, id: Int) extends FixedPoint {
    protected val colour = "red"
  }
}
