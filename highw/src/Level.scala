import org.scalajs.dom.SVGElement
import org.scalajs.dom.svg
import org.scalajs.dom.{document, window}
import org.scalajs.dom.SVGPathElement
import collection.mutable.Buffer
import math.{sin, cos, Pi, hypot}
import org.scalajs.dom.SVGLineElement
import org.scalajs.dom.{MouseEvent}
import org.scalajs.dom.WheelEvent


class Level(val page: SVGElement, rects: Seq[Block], sources: Seq[SourcePoint], sinks: Seq[SinkPoint]) {
  val roadEnv = RoadEnv(this)
  val sourceRoads = sources.map(roadEnv.FixedRoad.apply)
  val sinkRoads = sinks.map(roadEnv.FixedRoad.apply)
  val roads = collection.mutable.Buffer[roadEnv.DrawnRoad]()
  var tempRoad: Option[roadEnv.DrawnRoad] = None
  
  val blockEnv = BlockEnv(this)
  val blocks = rects.map(_.toBlock(blockEnv))

  def scaleX(xc: Double): Int = (page.clientWidth * xc).round.toInt
  def scaleY(yc: Double): Int = (page.clientHeight * yc).round.toInt
  def unScaleX(xc: Double): Double = xc / page.clientWidth
  def unScaleY(yc: Double): Double = yc / page.clientHeight

  var tempRoadPoint: Option[TempRoadPoint] = None
  var activeSource: Option[SourcePoint] = None

  var closestSource: Option[SourcePoint] = None
  var closestRoad: Option[TempRoadPoint] = None

  def reqDist = scaleX(0.025) min scaleY(0.025)

  def processMouseMoveActiveSource(x: Double, y: Double): Unit = {
    tempRoad.foreach(_.clear())
    val tempRoadAngleSkew = tempRoad.map(_.angleSkew)
    val newPoint = RoadPoint(unScaleX(x), unScaleY(y))
    activeSource.map { point =>
      tempRoad = Some(roadEnv.DrawnRoad(point, newPoint, point.angle, tempRoadAngleSkew.getOrElse(0d)))
      tempRoad.foreach(_.draw())
    }
  }

  def processMouseMoveTempRoad(x: Double, y: Double): Unit = {
    ???
  }

  def processMouseMoveRoadIsCloser(closestRoad: roadEnv.DrawnRoad, dist: Double): Unit = {
      if (dist <= reqDist) {
        // TODO: set tempRoadPoint
      }
  }

  def processMouseMoveSourceIsCloser(closestSourcePoint: SourcePoint, dist: Double): Unit = {
      if (dist < reqDist) {
        if (closestSource.map(cs => cs != closestSourcePoint).getOrElse(true)) {
          closestSource.map(_.deactivate())
          closestSource = Some(closestSourcePoint)
          closestSource.map(_.activate())
        }
      } else {
        closestSource.map(_.deactivate())
        closestSource = None
      }
  }

  def processMouseMoveNoSource(x: Double, y: Double): Unit = {
    val roadDistances = roads.map { road =>
      val dist = road.points
        .map((px, py) => hypot(x - px, y - py))
        .minOption
        .getOrElse(Double.PositiveInfinity)
      (road, dist)
    }
    val sourceDistances = sources.map { source =>
      val px = x - scaleX(source.x)
      val py = y - scaleY(source.y)
      (source, hypot(px, py))
    }
    
    val closestRoadPoint = roadDistances.minByOption(_._2)
    val closestSourcePoint = sourceDistances.minBy(_._2)
    
    val closestRoadDist = closestRoadPoint.map(_._2).getOrElse(Double.PositiveInfinity)

    if (closestRoadDist < closestSourcePoint._2) closestRoadPoint.foreach(processMouseMoveRoadIsCloser)
    else processMouseMoveSourceIsCloser.tupled(closestSourcePoint)
    
  }

  def processMouseMove(x: Double, y: Double): Unit = {
    if (activeSource.isDefined) processMouseMoveActiveSource(x, y)
    else if (tempRoadPoint.isDefined) processMouseMoveTempRoad(x, y)
    else processMouseMoveNoSource(x, y)
  }

  def processLeftMouseDown(e: MouseEvent): Unit = {
    closestSource.map { cs =>
      activeSource = Some(cs)
      processMouseMoveActiveSource(e.clientX, e.clientY)
    }
  }

  def processRightMouseDown(e: MouseEvent): Unit = {
    // TODO: Also delete roads
    activeSource.map { as =>
      tempRoad.map(_.clear())
      tempRoad = None
      activeSource = None
    }
  }

  def processMouseDown(e: MouseEvent): Unit = {
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

  page.addEventListener("mousemove", { case e: MouseEvent => processMouseMove(e.clientX, e.clientY) })

  page.addEventListener("mousedown", { case e: MouseEvent => processMouseDown(e)})

  page.addEventListener("wheel", { case e: WheelEvent => processScroll(e) })

  def drawSink(sink: SinkPoint): Unit = {
    sink.draw(page)
  }

  def drawSource(source: SourcePoint) = {
    source.draw(page)
  }

  def draw(): Unit = {
    blocks.foreach(_.draw())
    sources.foreach(drawSource)
    sinks.foreach(drawSink)
    roads.foreach(_.draw())
    sourceRoads.foreach(_.draw())
    sinkRoads.foreach(_.draw())
  }
}