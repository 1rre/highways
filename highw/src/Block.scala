import org.scalajs.dom.{SVGElement, SVGRectElement, document}

case class Block(x: Double, y: Double, w: Double, h: Double) {
  def toBlock(env: BlockEnv): env.Block = env.Block(x, y, w, h)
}

class BlockEnv(level: Level) {
  case class Block(x: Double, y: Double, w: Double, h: Double) {
    def draw(): Unit = {
      def scaleX(xc: Double): Int = (level.page.clientWidth * xc).round.toInt
      def scaleY(yc: Double): Int = (level.page.clientHeight * yc).round.toInt
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
      level.page.appendChild(rect)
    }
  }
}
