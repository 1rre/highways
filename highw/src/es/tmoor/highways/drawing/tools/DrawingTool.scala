package es.tmoor.highways.drawing.tools

import es.tmoor.highways.{Level, SvgUser}
import org.scalajs.dom.{WheelEvent, MouseEvent}
import es.tmoor.highways.drawing.DrawingHandler

trait DrawingTool extends SvgUser {
  val level: Level
  val page = level.page
  def deactivate(): Unit
  def processScroll(e: WheelEvent): Unit
  def processRightMouseDown(e: MouseEvent): Unit
  def processLeftMouseDown(e: MouseEvent): Unit
  def processMouseMove(e: MouseEvent): Unit
}