package es.tmoor.highways.data

import scala.scalajs.js.JSON
import scala.scalajs.js

object LevelData {
  def parse(text: String): LevelData = {
    val asJson = JSON.parse(text)
    new LevelData {
      val id = asJson.id.asInstanceOf[Int]
      val blocks = asJson.blocks.asInstanceOf[js.Array[js.Dynamic]].map(BlockData.convert).toList
      val sources = asJson.sources.asInstanceOf[js.Array[js.Dynamic]].map(SourceData.convert).toList
      val sinks = asJson.sinks.asInstanceOf[js.Array[js.Dynamic]].map(SinkData.convert).toList
    }
  }
}

sealed trait LevelData {
  val blocks: Seq[BlockData]
  val sources: Seq[SourceData]
  val sinks: Seq[SinkData]
  val id: Int
}