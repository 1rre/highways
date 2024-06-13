package es.tmoor.highways.drawing.tools

import es.tmoor.highways.level._
import math.{hypot}

trait MouseTracker extends DrawingTool {
    def reqDist = scaleX(0.025) min scaleY(0.025)
    def getNearestRoadPoint(x: Double, y: Double): Option[(DrawnRoad, Double, Double, Double)] = {
        level.roads.map(road => road.bezier.map { bz =>
            val (xs, ys) = bz.closestPoint(unScaleX(x), unScaleY(y))
            val (xt, yt) = (scaleXd(xs), scaleYd(ys))
            val d = hypot(x - xt, y - yt)
            (road, xt, yt, d)
        }).flatten.minByOption(_._4)
    }

    def getNearestFixedSource(x: Double, y: Double): (SourcePoint, Double) = {
        level.sources.map { source =>
            val px = x - scaleX(source.x)
            val py = y - scaleY(source.y)
            (source, hypot(px, py))
        }.minBy(_._2)
    }
    
    def getNearestFixedSink(x: Double, y: Double): (SinkPoint, Double) = {
        level.sinks.map { sink =>
            val px = x - scaleX(sink.x)
            val py = y - scaleY(sink.y)
            (sink, hypot(px, py))
        }.minBy(_._2)
    }
    
    def getNearestSource(x: Double, y: Double): (SourcePoint | RoadConnectionPoint, Double) = {
        val nearestFixed = getNearestFixedSource(x, y)
        val nearestRoad = getNearestRoadPoint(x, y)
        val nearestConnectionPoint: Option[(RoadConnectionPoint, Double)] =
            nearestRoad.collect{ case nr if nr._4 < nearestFixed._2 =>
                (RoadConnectionPoint(unScaleX(nr._2), unScaleY(nr._3), nr._1), nr._4)
            }
        nearestConnectionPoint.getOrElse(nearestFixed)
    }
    
    def getNearestSink(x: Double, y: Double): (SinkPoint | RoadConnectionPoint, Double) = {
        val nearestFixed = getNearestFixedSink(x, y)
        val nearestRoad = getNearestRoadPoint(x, y)
        val nearestConnectionPoint: Option[(RoadConnectionPoint, Double)] =
            nearestRoad.collect{ case nr if nr._4 < nearestFixed._2 =>
                (RoadConnectionPoint(unScaleX(nr._2), unScaleY(nr._3), nr._1), nr._4)
            }
        nearestConnectionPoint.getOrElse(nearestFixed)
    }
}