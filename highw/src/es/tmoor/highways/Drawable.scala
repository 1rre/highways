package es.tmoor.highways

import org.scalajs.dom.{SVGSVGElement, SVGPathElement, SVGCircleElement, document}
import math.Pi

trait Drawable extends SvgUser {
  def normaliseAngle(x: Double): Double = (x + 2 * Pi) % (2 * Pi)
  def draw(): Unit
  
  def plotArc(x1s: Double, y1s: Double, x2s: Double, y2s: Double, xcs: Double, ycs: Double, colour: String, width: Int): SVGPathElement = {
    val line = document.createElementNS("http://www.w3.org/2000/svg", "path").asInstanceOf[SVGPathElement]
    val x1 = scaleX(x1s)
    val y1 = scaleY(y1s)
    val x2 = scaleX(x2s)
    val y2 = scaleY(y2s)
    val xc = scaleX(xcs)
    val yc = scaleY(ycs)
    line.setAttribute("style", s"fill: none; stroke: $colour; stroke-width: ${width}px; z-index: 100;")
    line.setAttribute("d", s"M $x1 $y1 Q $xc $yc $x2 $y2")
    line.setAttribute("marker-end", "url(#arrow)")
    page.appendChild(line)
    line
    
  }
  
  def plotLine(x1In: Double, y1In: Double, x2In: Double, y2In: Double, colour: String): SVGPathElement = {
    val line = document.createElementNS("http://www.w3.org/2000/svg", "path").asInstanceOf[SVGPathElement]
    val x1 = scaleX(x1In)
    val y1 = scaleY(y1In)
    val x2 = scaleX(x2In)
    val y2 = scaleY(y2In)
    line.setAttribute("style", s"fill: none; stroke: $colour; stroke-width: 2px; z-index: 100;")
    line.setAttribute("d", s"M $x1 $y1 L $x2 $y2")
    page.appendChild(line)
    line
  }
  
  def plotDot(x: Double, y: Double, colour: String): SVGCircleElement = {
    val node = document.createElementNS("http://www.w3.org/2000/svg", "circle").asInstanceOf[SVGCircleElement]
    node.cx.baseVal.value = scaleX(x)
    node.cy.baseVal.value = scaleY(y)
    node.r.baseVal.value = scaleX(0.005) min scaleY(0.005)
    node.setAttribute("style", s"fill: #FFFFFF00; stroke: $colour")
    page.appendChild(node)
    node
  }
}