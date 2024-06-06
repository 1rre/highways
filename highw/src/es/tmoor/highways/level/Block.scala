package es.tmoor.highways.level

import org.scalajs.dom.{document, SVGRectElement}
import org.scalajs.dom.SVGSVGElement
import es.tmoor.highways.Drawable

case class Block(x: Double, y: Double, w: Double, h: Double)(val page: SVGSVGElement) extends Drawable {
  def draw(): Unit = {
    val sx = scaleX(x)
    val sy = scaleY(y)
    val sw = scaleX(w)
    val sh = scaleY(h)
    val rect = document.createElementNS("http://www.w3.org/2000/svg", "rect").asInstanceOf[SVGRectElement]
    rect.x.baseVal.value = sx
    rect.y.baseVal.value = sy
    rect.width.baseVal.value = sw
    rect.height.baseVal.value = sh
    rect.setAttribute("style", "fill: #FFFFFF00; stroke: black")
    page.appendChild(rect)
  }
}