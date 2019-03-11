package object observatory {
  type Temperature = Double // °C, introduced in Week 1
  type Year = Int // Calendar year, introduced in Week 1
  type StationIdentifier = (String, String)
  type PixelCoordinates = (Int, Int)
  val maxLongitude = 360
  val maxLatitude = 180
  val earthRadius = 6371
}
