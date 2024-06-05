package es.tmoor.highways.level

import org.scalajs.dom.{document, SVGRectElement}

class BlockEnvironment(environment: Environment) {
  case class Block(x: Double, y: Double, w: Double, h: Double) extends Drawable {
    def draw(): Unit = {
      val sx = environment.scaleX(x)
      val sy = environment.scaleY(y)
      val sw = environment.scaleX(w)
      val sh = environment.scaleY(h)
      val rect = document.createElementNS("http://www.w3.org/2000/svg", "rect").asInstanceOf[SVGRectElement]
      rect.x.baseVal.value = sx
      rect.y.baseVal.value = sy
      rect.width.baseVal.value = sw
      rect.height.baseVal.value = sh
      rect.setAttribute("style", "fill: #FFFFFF00; stroke: black")
      environment.page.appendChild(rect)
    }
  }
}