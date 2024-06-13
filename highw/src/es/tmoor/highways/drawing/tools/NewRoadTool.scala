package es.tmoor.highways.drawing.tools

import es.tmoor.highways.Level
import es.tmoor.highways.level._
import org.scalajs.dom.{WheelEvent, MouseEvent, SVGCircleElement}
import scala.collection.mutable.Buffer

class NewRoadTool(val level: Level) extends DrawingTool with MouseTracker {
  var source: Option[RoadConnectionPoint | SourcePoint] = None
  var sink: Option[AngledPoint] = None
  var tempRoad: Option[DrawnRoad] = None

  def activate(newSource: RoadConnectionPoint | SourcePoint, x: Double, y: Double): Unit = {
    source = Some(newSource)
    tempRoad = Some(FreeFormRoad(newSource, RoadPoint(x, y)))
  }

  def deactivate(): Unit = {
    tempRoad.foreach(_.clearGuidelines())
    tempRoad = None
    source = None
  }
  
  def processScroll(e: WheelEvent): Unit = {
    tempRoad.foreach { tempRoad =>
      tempRoad.incrAngleSkew(unScaleY(e.deltaY))
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

  val intersectionDots = Buffer[SVGCircleElement]()

  def drawTempRoad(): Unit = {
    intersectionDots.foreach(_.remove())
    intersectionDots.clear()
    tempRoad.foreach(_.draw(true))
    level.roads.foreach { road =>
      println(s"Check road $road for intersections with TR")
      road.bezier.map { rbz =>
        println(s"Has BZ $rbz")
        tempRoad.foreach { tr =>
          println(s"Check TR $road for intersections with TR")
          tr.bezier.map { bz =>
            println(s"Temp has BZ $rbz")
            bz.intersectionsWith(rbz).foreach { (x,y) =>
              intersectionDots += tr.plotDot(x, y, "black")
            }
          }
        }  
      }
    }
  }

  def processMouseMove(e: MouseEvent): Unit = {
    tempRoad.foreach(_.clear())
    val tempRoadAngleSkew = tempRoad.map(_.angleSkew)
    val cr = getNearestSink(e.clientX, e.clientY)
    if (cr._2 < reqDist && shouldSnap(tempRoad.map(_.sourcePoint).get, cr._1)) {
      if (e.shiftKey) { // Shift => don't line up
        source.map { point =>
          tempRoad = Some(FreeFormRoad(point, cr._1, tempRoadAngleSkew.getOrElse(0d)))
          drawTempRoad()
        }
      } else {
        source.map { point =>
          tempRoad = Some(SnappedRoad.fromPoints(point, cr._1))
          drawTempRoad()
        }
      }
    } else {
      val newPoint = RoadPoint(unScaleX(e.clientX), unScaleY(e.clientY))
      
      source.map { point =>
        tempRoad = Some(FreeFormRoad(point, newPoint, tempRoadAngleSkew.getOrElse(0d)))
        drawTempRoad()
      }
    }
  }

  def draw(): Unit = {
    source.foreach(_.draw())
    drawTempRoad()
  }
}