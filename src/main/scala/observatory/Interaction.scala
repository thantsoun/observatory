package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import scala.math._

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    val zoomFactor = pow(2, tile.zoom)
    val lat = math.atan(math.sinh(math.Pi * (1.0 - 2.0 * tile.y) / zoomFactor))
    Location(toDegrees(lat), tile.x * maxLongitude / zoomFactor - maxLatitude)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @param tile         Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    val coordinates = for {
      i <- 0 until tileSize
      j <- 0 until tileSize
    } yield (i, j)

    val pixels = coordinates.par
      .map({case (y, x) => Tile(x + (tile.x * tileSize), y + (tile.y * tileSize), tile.zoom + zoomFactor)})
      .map(tileLocation)
      .map(Visualization.predictTemperature(temperatures, _))
      .map(Visualization.interpolateColor(colors, _))
      .map(col => Pixel(col.red, col.green, col.blue, 255))
      .toArray

    Image(tileSize, tileSize, pixels)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    *
    * @param yearlyData    Sequence of (year, data), where `data` is some data associated with
    *                      `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
                           yearlyData: Iterable[(Year, Data)],
                           generateImage: (Year, Tile, Data) => Unit
                         ): Unit = {
    for {
      zoom <- 0 to 3
      x <- 0 until pow(2, zoom).toInt
      y <- 0 until pow(2, zoom).toInt
      (year, data) <- yearlyData
    } yield generateImage(year, Tile(y, x, zoom), data)
  }

}
