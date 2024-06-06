import es.tmoor.highways.Level
import es.tmoor.highways.data.LevelData

import org.scalajs.dom.{document, SVGSVGElement}

@main def main = {
  val svg = document.getElementById("page-svg").asInstanceOf[SVGSVGElement]
  Level(svg, LevelData.parse(Level1.text)).run()
}