package es.tmoor.highways.util

import math.Pi
import es.tmoor.lineslib._

def normaliseAngle(x: Double): Double = (x + 2 * Pi) % (2 * Pi)
def isBehind(x1: Double, y1: Double, x2: Double, y2: Double, l: LineLike): Boolean =
  l.distBetween(x1, y1, x2, y2) < 0
def isBehind(x1: Double, y1: Double, x2: Double, y2: Double, a: Double): Boolean = 
  isBehind(x1, y1, x2, y2, Line.fromPointAndAngle(x1, y1, a))