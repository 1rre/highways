import org.scalajs.dom.{document, window, SVGElement}
import math.{cos, sin, Pi}
import scala.concurrent.Promise

def makeHighway(x: Double, y: Double, angle: Double, id: Int, demand: Map[Int, Int], reversed: Boolean = false): (SourcePoint, SinkPoint) = {
  // TODO: Find pos based on angle...
  val diff = if reversed then -0.05 else 0.05
  val dx = math.cos(angle) * diff * 2/3d
  val dy = math.sin(angle) * diff
  val source = SourcePoint(x + dx, y + dy, angle, id, demand)
  val sink = SinkPoint(x - dx, y - dy, angle, id)
  (source, sink)
}

var await = false

@main def main = {
  val svg = document.getElementById("page-svg").asInstanceOf[SVGElement]
  val blocks = Seq(
    Block(0.062, 0, 0.125, 0.2),
    Block(0.81, 0.2, 0.13, 0.2),
  )
  val (sourceA, sinkA) = makeHighway(0, 0.5, Pi/2, 0, Map())
  val (sourceB, sinkB) = makeHighway(0.5, 0, Pi, 1, Map(), true)
  val (sourceC, sinkC) = makeHighway(0.25, 1, 0, 2, Map())
  val (sourceD, sinkD) = makeHighway(0.75, 1, 0, 3, Map())
  val sources = Seq[SourcePoint](
    sourceA,
    sourceB,
    sourceC,
    sourceD,
    SourcePoint(0.1245, 0.2, Pi, 4, Map()),
    SourcePoint(0.81, 0.3, 3*Pi/2, 5, Map())
  )
  val sinks = Seq[SinkPoint](
    sinkA,
    sinkB,
    sinkC,
    sinkD
  )
  val level = Level(svg, blocks, sources, sinks)
  level.draw()
  window.addEventListener("resize", e => {
    while (await) {}
    await = true
    svg.innerHTML = ""
    level.draw()
    await = false
  })
}