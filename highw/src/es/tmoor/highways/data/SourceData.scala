package es.tmoor.highways.data
import scala.scalajs.js

object SourceData {
  def convert(json: js.Dynamic): SourceData = {
    new SourceData {
      val x1 = json.x1.asInstanceOf[Double]
      val y1 = json.y1.asInstanceOf[Double]
      val angle = json.angle.asInstanceOf[Double]
      val id = json.id.asInstanceOf[Int]
      val demand = json.demand.asInstanceOf[js.Array[Int]].toSeq
    }
  }
}

sealed trait SourceData {
  val x1: Double
  val y1: Double
  val angle: Double
  val id: Int
  val demand: Seq[Int]
}
