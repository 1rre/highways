package es.tmoor.highways.drawing.tools

import es.tmoor.highways.Level
import es.tmoor.highways.level._
import org.scalajs.dom.{WheelEvent, MouseEvent}

class NewRoadTool(val level: Level) extends DrawingTool with MouseTracker {
  var source: Option[RoadConnectionPoint | SourcePoint] = None
  var sink: Option[AngledPoint] = None
  var tempRoad: Option[DrawnRoad] = None

  def activate(newSource: RoadConnectionPoint | SourcePoint, x: Double, y: Double): Unit = {
    source = Some(newSource)
    tempRoad = Some(DrawnRoad(newSource, RoadPoint(x, y), newSource.angle))
  }

  def deactivate(): Unit = {
    tempRoad.foreach(_.clearGuidelines())
    tempRoad = None
    source = None
  }
  
  def processScroll(e: WheelEvent): Unit = {
    tempRoad.foreach { tempRoad =>
      tempRoad.angleSkew += unScaleY(e.deltaY)
      tempRoad.clear()
      tempRoad.draw()
    }
  }
  
  def processRightMouseDown(e: MouseEvent): Unit = {
    tempRoad.foreach(_.clear())
    level.drawingHandler.activate(level.drawingHandler.selectionTool)
  }

  def processLeftMouseDown(e: MouseEvent): Unit = {
    tempRoad.foreach { tr =>
      level.drawingHandler.activate(level.drawingHandler.selectionTool)
      tr.clearGuidelines()
      level.roads += tr
    }
  }

  def shouldSnap(p1: PointLike, p2: PointLike): Boolean = {
    (p1, p2) match {
      case (p1: RoadConnectionPoint, p2: RoadConnectionPoint) => p1.owner != p2.owner
      case (p1: RoadConnectionPoint, p2: SinkPoint) => !p2.inputs.contains(p1.owner)
      case (p1: SourcePoint, p2: RoadConnectionPoint) => !p1.outputs.contains(p2.owner)
      case _ => true
    }
  }

  def processMouseMove(e: MouseEvent): Unit = {
    tempRoad.foreach(_.clear())
    val tempRoadAngleSkew = tempRoad.map(_.angleSkew)
    val cr = getNearestSink(e.clientX, e.clientY)
    if (cr._2 < reqDist && !e.shiftKey && shouldSnap(tempRoad.map(_.sourcePoint).get, cr._1)) {
      source.map { point =>
        tempRoad = Some(DrawnRoad(point, cr._1, point.angle, tempRoadAngleSkew.getOrElse(0d)))
        tempRoad.foreach(_.draw(true))
      }
    } else {
      val newPoint = RoadPoint(unScaleX(e.clientX), unScaleY(e.clientY))
      
      source.map { point =>
        tempRoad = Some(DrawnRoad(point, newPoint, point.angle, tempRoadAngleSkew.getOrElse(0d)))
        tempRoad.foreach(_.draw(true))
      }
    }
  }

  def draw(): Unit = {
    source.foreach(_.draw())
    tempRoad.foreach(_.draw())
  }
}