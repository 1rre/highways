package es.tmoor.highways.drawing.tools

import es.tmoor.highways.Level
import es.tmoor.highways.level._
import org.scalajs.dom.{WheelEvent, MouseEvent}

class NewRoadTool(val level: Level) extends DrawingTool {
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

  def processMouseMove(e: MouseEvent): Unit = {
    tempRoad.foreach(_.clear())
    val tempRoadAngleSkew = tempRoad.map(_.angleSkew)
    val newPoint = RoadPoint(unScaleX(e.clientX), unScaleY(e.clientY))
    
    source.map { point =>
      tempRoad = Some(DrawnRoad(point, newPoint, point.angle, tempRoadAngleSkew.getOrElse(0d)))
      tempRoad.foreach(_.draw(true))
    }
  }

  def draw(): Unit = {
    source.foreach(_.draw())
    tempRoad.foreach(_.draw())
  }
}