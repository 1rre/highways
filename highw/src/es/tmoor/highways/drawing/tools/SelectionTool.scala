package es.tmoor.highways.drawing.tools

import math.{hypot}
import org.scalajs.dom.{WheelEvent, MouseEvent}
import es.tmoor.highways.Level
import es.tmoor.highways.level._
import es.tmoor.highways.drawing.DrawingHandler

class SelectionTool(val level: Level) extends DrawingTool with MouseTracker {
  def deactivate(): Unit = {
    lastClosestRoad.foreach(_.deactivate())
    lastClosestRoad = None
    lastClosestSource.foreach(_.deactivate())
    lastClosestSource = None
  }

  var lastClosestRoad: Option[RoadConnectionPoint] = None
  var lastClosestSource: Option[SourcePoint] = None

  def nextPoint: Option[RoadConnectionPoint | SourcePoint] = {
    if (lastClosestRoad.isDefined) Some(lastClosestRoad.get)
    else lastClosestSource
  }
  
  def processScroll(e: WheelEvent): Unit = {}
  
  def processRightMouseDown(e: MouseEvent): Unit = {
    lastClosestRoad.foreach { cr =>
      cr.deactivate()
      cr.owner.clear()
      level.roads.remove(level.roads.indexOf(cr.owner))
      lastClosestRoad = None
    }
  }

  def processLeftMouseDown(e: MouseEvent): Unit = {
    nextPoint.foreach { nextPoint =>
      level.drawingHandler.newRoadTool.activate(nextPoint, unScaleX(e.clientX), unScaleY(e.clientY))
      level.drawingHandler.activate(level.drawingHandler.newRoadTool)
      if (lastClosestRoad.isDefined) {
        
      } else if (lastClosestSource.isDefined) {
        deactivate()
      }
    }
  }

  def setClosestSource(closestSourcePoint: SourcePoint): Unit = {
    closestSourcePoint.setSatisfiedDemand(level.roads.toSeq)
    closestSourcePoint.activate()
    lastClosestSource = Some(closestSourcePoint)
  }

  def setClosestRoad(road: RoadConnectionPoint): Unit = {
    road.activate()
    lastClosestRoad = Some(road)
  }

  def processMouseMove(e: MouseEvent): Unit = {
    val x = e.clientX
    val y = e.clientY
    deactivate()
    val cs = getNearestSource(x, y)
    cs match {
      case (fixed: SourcePoint, dist) if dist < reqDist =>
        setClosestSource(fixed)
      case (road: RoadConnectionPoint, dist) if dist < reqDist =>
        setClosestRoad(road)
      case _ =>
    }
  }

  def draw(): Unit = {}
}
