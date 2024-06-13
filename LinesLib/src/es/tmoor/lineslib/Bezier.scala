package es.tmoor.lineslib
import math.hypot

case class Bezier(x1: Double, y1: Double, xc: Double, yc: Double, x2: Double, y2: Double) extends Formula {
  private final def MaxDiff: Double = 1e-4
  private final def NPts = 5

  def xt(t: Double): Double = (1-t)*(1-t)*x1 + 2*t*(1-t)*xc + t*t*x2
  def yt(t: Double): Double = (1-t)*(1-t)*y1 + 2*t*(1-t)*yc + t*t*y2
  def dxdt(t: Double): Double = 2*(1-t)*(xc-x1) + 2*t*(x2-xc)
  //def dxdt(t: Double): Double = 2*(t*x1 - 2*t*xc + t*x2 - x1 + xc)
  def dydt(t: Double): Double = 2*(1-t)*(yc-y1) + 2*t*(y2-yc)
  //def dydt(t: Double): Double = 2*(t*y1 - 2*t*yc + t*y2 - y1 + yc)
  def dydx(t: Double) = dxdt(t) / dydt(t)

  private def searchClosestPoint(x: Double, y: Double, lo: Double, hi: Double): Double = {
    val step = (hi - lo) / NPts
    val points = for (n <- 0 to NPts) yield {
      val t = lo + n * step
      val diff = hypot(x - xt(t), y - yt(t))
      (t, diff)
    }
    val t = points.minBy(_._2)._1
    if (step < MaxDiff) t
    else searchClosestPoint(x, y, (t-step) max 0, (t+step) min 1)
  }
  
  def closestParametric(x: Double, y: Double): Double = searchClosestPoint(x, y, 0, 1)

  def closestPoint(x: Double, y: Double): (Double, Double) = {
    val t = searchClosestPoint(x, y, 0, 1)
    (xt(t), yt(t))
  }
  
  def gradientAt(x: Double, y: Double): Double = {
    val t = closestParametric(x, y)
    dydx(t)
  }

  // TODO: Look for points which have an intersection between them
  // A non-intersection when lines are very close will be due to either:
  // * A change in direction (magnitude of dx/dt and/or dy/dt between roads 1 & 2 changes between the points)
  // * A road ending
  // Intersections can happen when:
  // * x(t) for road 1 passes road 2 when y is close? (or y can be not close? Look into y)
  def intersectionsWith(that: Bezier): Seq[(Double, Double)] = {
    // this.xt(t) = that.xt(t) && this.yt(t) = that.yt(t)
    // (1-t)²·a + 2t(1-t)·b + t²­·c = (1-u)²·d + 2u(1-u)·e + u²­·f
    val maxStep = 100
    for (x <- 0 to maxStep) {
      val t = x / maxStep.toDouble

    }
    Nil
  }
}
