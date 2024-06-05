package es.tmoor.highways.drawing

import es.tmoor.highways.level.Environment
import es.tmoor.highways.level.Drawable
import es.tmoor.highways.Level
import org.scalajs.dom.{WheelEvent, MouseEvent}
import es.tmoor.highways.level.RoadEnvironment
import math.{hypot, tan, tanh, Pi}

class DrawingHandler(val level: Level) extends Drawable {
  import level.environment.blockEnvironment._
  import level.environment.roadEnvironment._
  import level.environment.pointEnvironment._
  import level.environment._

  var tempRoad: Option[DrawnRoad] = None

  var tempRoadPoint: Option[RoadConnectionPoint] = None
  var activeSource: Option[SourcePoint] = None

  var closestSource: Option[SourcePoint] = None
  var closestRoad: Option[RoadConnectionPoint] = None

  def reqDist = scaleX(0.025) min scaleY(0.025)

  def processMouseMoveActiveSource(x: Double, y: Double): Unit = {
    tempRoad.foreach(_.clear())
    val tempRoadAngleSkew = tempRoad.map(_.angleSkew)
    val newPoint = RoadPoint(unScaleX(x), unScaleY(y))
    activeSource.map { point =>
      tempRoad = Some(DrawnRoad(point, newPoint, point.angle, tempRoadAngleSkew.getOrElse(0d)))
      tempRoad.foreach(_.draw(true))
    }
  }

  def processMouseMoveTempRoad(x: Double, y: Double): Unit = {
    ???
  }

  def processMouseMoveRoadIsCloser(road: DrawnRoad, x: Double, y: Double, dist: Double): Unit = {
    closestSource.foreach(_.deactivate())
    closestSource = None
    if (dist <= reqDist) {
      closestRoad = Some(RoadConnectionPoint(x, y, road))
      closestRoad.foreach(_.owner.activate())
      closestRoad.foreach(_.draw())
    } else {
      closestRoad.foreach(_.owner.deactivate())
      closestRoad = None
    }
  }

  def processMouseMoveSourceIsCloser(closestSourcePoint: SourcePoint, dist: Double): Unit = {
      closestRoad.foreach(_.owner.deactivate())
      closestRoad = None
      if (dist < reqDist) {
        if (closestSource.map(cs => cs != closestSourcePoint).getOrElse(true)) {
          closestSource.foreach(_.deactivate())
          closestSource = Some(closestSourcePoint)
          closestSource.foreach(_.activate())
        }
      } else {
        closestSource.foreach(_.deactivate())
        closestSource = None
      }
  }

  def processMouseMoveNoSource(x: Double, y: Double): Unit = {
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

    if (closestRoadDist < closestSourcePoint._2) closestRoadPoint.foreach(processMouseMoveRoadIsCloser)
    else processMouseMoveSourceIsCloser.tupled(closestSourcePoint)
    
  }

  def processMouseMove(x: Double, y: Double): Unit = {
    if (activeSource.isDefined) processMouseMoveActiveSource(x, y)
    else if (tempRoadPoint.isDefined) processMouseMoveTempRoad(x, y)
    else processMouseMoveNoSource(x, y)
  }

  def processLeftMouseDown(e: MouseEvent): Unit = {
    if (tempRoad.isDefined) {
      tempRoad.foreach { tempRoad =>
        tempRoad.clear()
        tempRoad.draw()
        level.roads += tempRoad
      }
      tempRoad = None
      activeSource = None
    } else {
      closestSource.foreach { cs =>
        activeSource = Some(cs)
        processMouseMoveActiveSource(e.clientX, e.clientY)
      }
    }
  }

  def processRightMouseDown(e: MouseEvent): Unit = {
    // TODO: Also delete roads
    if (activeSource.isDefined) {
      activeSource.foreach { as =>
        tempRoad.foreach(_.clear())
        tempRoad = None
        activeSource = None
      }
    } else if (closestRoad.isDefined) {
      closestRoad.foreach { cr =>
        cr.owner.clear()
        level.roads.remove(level.roads.indexOf(cr.owner))
      }
      closestRoad = None
    }
  }

  def processMouseDown(e: MouseEvent): Unit = {
    processMouseMove(e.clientX, e.clientY)
    if (e.button == 0) processLeftMouseDown(e)
    else if (e.button == 2) {
      e.preventDefault()
      e.stopImmediatePropagation()
      e.stopPropagation()
      processRightMouseDown(e)
    }
  }

  def processScroll(e: WheelEvent): Unit = {
    tempRoad.foreach { tempRoad =>
      tempRoad.angleSkew += unScaleY(e.deltaY)
      tempRoad.clear()
      tempRoad.draw()
    }
  }

  def draw(): Unit = {
    (tempRoad ++ tempRoadPoint).foreach(_.draw())
  }
  
  page.addEventListener("mousemove", { case e: MouseEvent => processMouseMove(e.clientX, e.clientY) })
  page.addEventListener("mousedown", { case e: MouseEvent => processMouseDown(e)})
  page.addEventListener("wheel", { case e: WheelEvent => processScroll(e) })
}