import scala.math.pow

package object observatory {
  type Temperature = Double // °C, introduced in Week 1
  type Year = Int // Calendar year, introduced in Week 1
  type StationIdentifier = (String, String)
  type PixelCoordinates = (Int, Int)
  val maxLongitude = 360
  val maxLatitude = 180
  val earthRadius = 6371
  val zoomFactor = 8
  val tileSize: Int = pow(2, zoomFactor).toInt
}
