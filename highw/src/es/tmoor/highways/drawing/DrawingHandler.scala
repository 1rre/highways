package es.tmoor.highways.drawing

import org.scalajs.dom.{WheelEvent, MouseEvent}
import math.{hypot, tan, tanh, Pi}
import es.tmoor.highways.{Level, SvgUser}
import es.tmoor.highways.level._
import es.tmoor.highways.drawing.tools._

class DrawingHandler(val level: Level) extends SvgUser {
  val page = level.page
  val newRoadTool = NewRoadTool(level)
  val selectionTool = SelectionTool(level)

  private var activeTool: DrawingTool = selectionTool

  def activate(tool: DrawingTool): Unit = {
    activeTool.deactivate()
    activeTool = tool
  }

  page.addEventListener("mousemove", { case e: MouseEvent =>
    activeTool.processMouseMove(e)
  })
  
  page.addEventListener("mousedown", { case e: MouseEvent =>
    activeTool.processMouseMove(e)
    if (e.button == 0) activeTool.processLeftMouseDown(e)
    else if (e.button == 2) {
      e.preventDefault()
      e.stopImmediatePropagation()
      e.stopPropagation()
      activeTool.processRightMouseDown(e)
    }
  })

  page.addEventListener("wheel", { case e: WheelEvent =>
    activeTool.processMouseMove(e)
    activeTool.processScroll(e)
  })
}