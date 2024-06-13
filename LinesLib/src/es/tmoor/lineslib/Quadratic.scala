package es.tmoor.lineslib

object Quadratic {
  def fromBeizer(x1: Double, y1: Double, xc: Double, yc: Double, x2: Double, y2: Double): Quadratic = {
    null
  }
}

// ax² + bx translated by (xr, yr) and rotated by t around (xr, yr)?
case class Quadratic(a: Double, b: Double, xr: Double, yr: Double, t: Double) extends Formula {
  def gradientAt(x: Double, y: Double): Double = 0
}