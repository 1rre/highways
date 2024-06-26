package es.tmoor.highways

import org.scalajs.dom.{document, window, SVGSVGElement}
import es.tmoor.highways.data.LevelData
import es.tmoor.highways.level._
import scala.collection.mutable.Buffer
import es.tmoor.highways.drawing.DrawingHandler
import es.tmoor.highways.util.normaliseAngle
import math.Pi

class Level(val page: SVGSVGElement, data: LevelData) extends SvgUser {
  val drawText = page.innerHTML
  val blocks: Seq[Block] =
    data.blocks.map(block => Block(block.x, block.y, block.w, block.h))
  
  val sinks: Seq[SinkPoint] =
    data.sinks.map(sink => SinkPoint(sink.x1, sink.y1, normaliseAngle(sink.angle + Pi), sink.id))
  
  val sources: Seq[SourcePoint] =
    data.sources.map(source => SourcePoint(source.x1, source.y1, source.angle, source.id, source.demand.zipWithIndex.map((d, i) => sinks(i)->d).toMap))
  val fixedRoads: Seq[FixedRoad] = (sources ++ sinks).map(FixedRoad.apply)
  val roads: Buffer[DrawnRoad] = Buffer()

  val drawingHandler = DrawingHandler(this)

  val fixedElements = blocks ++ sources ++ sinks ++ fixedRoads
  def changableElements = Nil ++ roads

  def draw(): Unit = {
    fixedElements.foreach(_.draw())
    changableElements.foreach(_.draw())
  }

  def run(): Unit = {

    draw()
    window.addEventListener("resize", e => {
      page.innerHTML = drawText
      draw()
    })
  }
}