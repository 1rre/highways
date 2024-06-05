package es.tmoor.highways

import org.scalajs.dom.{document, window, SVGSVGElement}
import es.tmoor.highways.data.LevelData
import es.tmoor.highways.level.Environment
import scala.collection.mutable.Buffer
import es.tmoor.highways.drawing.DrawingHandler

class Level(val environment: Environment, data: LevelData) {
  import environment.blockEnvironment._
  import environment.roadEnvironment._
  import environment.pointEnvironment._

  val blocks =
    data.blocks.map(block => Block(block.x, block.y, block.w, block.h))
  val sources =
    data.sources.map(source => SourcePoint(source.x1, source.y1, source.angle, source.id, source.demand.zipWithIndex.toMap))
  val sinks =
    data.sinks.map(sink => SinkPoint(sink.x1, sink.y1, sink.angle, sink.id))
  val fixedRoads = (sources ++ sinks).map(FixedRoad.apply)
  val roads: Buffer[DrawnRoad] = Buffer()

  val drawingHandler = DrawingHandler(this)

  val fixedElements = blocks ++ sources ++ sinks ++ fixedRoads :+ drawingHandler
  def changableElements = Nil ++ roads

  def draw(): Unit = {
    fixedElements.foreach(_.draw())
    changableElements.foreach(_.draw())
  }

  def run(): Unit = {
    draw()
    window.addEventListener("resize", e => {
      environment.page.innerHTML = ""
      draw()
    })
  }
}