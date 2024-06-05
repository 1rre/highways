package es.tmoor.highways.data
import scala.scalajs.js

object SinkData {
  def convert(json: js.Dynamic): SinkData = {
    new SinkData {
      val x1 = json.x1.asInstanceOf[Double]
      val y1 = json.y1.asInstanceOf[Double]
      val angle = json.angle.asInstanceOf[Double]
      val id = json.id.asInstanceOf[Int]
    }
  }
}

sealed trait SinkData {
  val x1: Double
  val y1: Double
  val angle: Double
  val id: Int
}
