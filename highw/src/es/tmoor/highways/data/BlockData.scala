package es.tmoor.highways.data
import scala.scalajs.js

object BlockData {
  def convert(json: js.Dynamic): BlockData = {
    new BlockData {
      val x = json.x.asInstanceOf[Double]
      val y = json.y.asInstanceOf[Double]
      val w = json.w.asInstanceOf[Double]
      val h = json.h.asInstanceOf[Double]
    }
  }
}

sealed trait BlockData {
  val x: Double
  val y: Double
  val w: Double
  val h: Double
}
