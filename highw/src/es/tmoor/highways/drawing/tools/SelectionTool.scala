package es.tmoor.highways.drawing.tools

import math.{hypot}
import org.scalajs.dom.{WheelEvent, MouseEvent}
import es.tmoor.highways.Level
import es.tmoor.highways.level._
import es.tmoor.highways.drawing.DrawingHandler

class SelectionTool(val level: Level) extends DrawingTool {
  def deactivate(): Unit = {
    lastClosestRoad.foreach(_.deactivate())
    lastClosestRoad = None
    lastClosestSource.foreach(_.deactivate())
    lastClosestSource = None
  }

  var lastClosestRoad: Option[RoadConnectionPoint] = None
  var lastClosestSource: Option[SourcePoint] = None

  def nextPoint: RoadConnectionPoint | SourcePoint = {
    if (lastClosestRoad.isDefined) lastClosestRoad.get
    else lastClosestSource.get
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
    level.drawingHandler.newRoadTool.activate(nextPoint, unScaleX(e.clientX), unScaleY(e.clientY))
    level.drawingHandler.activate(level.drawingHandler.newRoadTool)
    if (lastClosestRoad.isDefined) {
      
    } else if (lastClosestSource.isDefined) {
      deactivate()
    }
  }

  def reqDist = scaleX(0.025) min scaleY(0.025)

  def setClosestSource(closestSourcePoint: SourcePoint): Unit = {
    closestSourcePoint.activate()
    lastClosestSource = Some(closestSourcePoint)
    println(s"Set closest source: $lastClosestSource")
  }

  def setClosestRoad(road: DrawnRoad, x: Double, y: Double, dist: Double): Unit = {
    lastClosestRoad = Some(RoadConnectionPoint(unScaleX(x), unScaleY(y), road))
    lastClosestRoad.foreach(_.activate())
    println(s"Set closest road: $lastClosestRoad")
  }

  def processMouseMove(e: MouseEvent): Unit = {
    val x = e.clientX
    val y = e.clientY
    deactivate()
    val roadDistances = level.roads.map { road =>
      val (x1, y1, dist) = road.points
        .map((px, py) => (px, py, hypot(x - px, y - py)))
        .minByOption(_._3)
        .getOrElse((0d, 0d, Double.PositiveInfinity))
      (road, x1, y1, dist)
    }
    val sourceDistances = level.sources.map { source =>
      val px = x - scaleX(source.x)
      val py = y - scaleY(source.y)
      (source, hypot(px, py))
    }
    
    val closestRoadPoint = roadDistances.minByOption(_._4)
    val closestSourcePoint = sourceDistances.minBy(_._2)

    val closestRoadDist = closestRoadPoint.map(_._4).getOrElse(Double.PositiveInfinity)

    if (closestRoadDist < closestSourcePoint._2 && closestRoadDist < reqDist)
      closestRoadPoint.foreach(setClosestRoad)
    else if (closestSourcePoint._2 < reqDist)
      setClosestSource(closestSourcePoint._1)
  }

  def draw(): Unit = {}
}
