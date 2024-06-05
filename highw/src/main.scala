import es.tmoor.highways.Level
import es.tmoor.highways.data.LevelData

import org.scalajs.dom.{document, SVGSVGElement}
import es.tmoor.highways.level.Environment

@main def main = {
  val svg = document.getElementById("page-svg").asInstanceOf[SVGSVGElement]
  val environment = Environment(svg)
  Level(environment, LevelData.parse(Level1.text)).run()
}