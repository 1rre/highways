import org.scalajs.dom.{SVGElement, SVGCircleElement, document}
import math.{sin, cos, Pi}

trait PointLike {
  val x: Double
  val y: Double
  protected val colour: String

  var node: SVGCircleElement = null

  def draw(page: SVGElement): Unit = {
    def scaleX(xc: Double): Int = (page.clientWidth * xc).round.toInt
    def scaleY(yc: Double): Int = (page.clientHeight * yc).round.toInt

    node = document.createElementNS("http://www.w3.org/2000/svg", "circle").asInstanceOf[SVGCircleElement]
    node.cx.baseVal.value = scaleX(x)
    node.cy.baseVal.value = scaleY(y)
    node.r.baseVal.value = scaleX(0.025) min scaleY(0.025)
    node.setAttribute("style", s"fill: #FFFFFF00; stroke: $colour")
    page.appendChild(node)
  }
}

trait FixedPoint extends PointLike {
  val angle: Double
  val id: Int
  val x1: Double
  val y1: Double
  val x = x1 + sin(angle) * 0.075 * 2/3d
  val y = y1 - cos(angle) * 0.075
}

case class RoadPoint(x: Double, y: Double) extends PointLike {
  protected val colour = "yellow"
}

case class TempRoadPoint(x: Double, y: Double, owner: RoadEnv#RoadLike) extends PointLike {
  protected val colour = "grey"
  def remove(): Unit = {
    node.parentNode.removeChild(node)
  }
}

case class SourcePoint(x1: Double, y1: Double, angle: Double, id: Int, demand: Map[Int, Int] = Map()) extends FixedPoint {
  protected val colour = "blue"
  private var active = false
  def activate(): Unit = {
    active = true
    node.setAttribute("style", s"fill: #FFFFFF00; stroke: $colour; stroke-width: 4px;")
  }
  def deactivate(): Unit = {
    active = false
    node.setAttribute("style", s"fill: #FFFFFF00; stroke: $colour;")
  }
  override def draw(page: SVGElement): Unit = {
    super.draw(page)
    if (active) {
      node.setAttribute("style", s"fill: #FFFFFF00; stroke: $colour; stroke-width: 4px;")
    }
  }
}

case class SinkPoint(x1: Double, y1: Double, angle: Double, id: Int) extends FixedPoint {
  protected val colour = "red"
}
