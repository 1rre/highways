package es.tmoor.lineslib

trait Formula {
  def gradientAt(x: Double, y: Double): Double
}
