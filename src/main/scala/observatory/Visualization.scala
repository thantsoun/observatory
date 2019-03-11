package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import scala.math._

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  val width = 360
  val height = 180
  val distanceLimit = 1

  val sumKeys: (Double, (Double, Temperature)) => Temperature = (acc: Double, pair: (Double, Temperature)) => acc + pair._1
  val sumKeyMultipliedByValue: (Double, (Double, Temperature)) => Temperature = (acc: Double, pair: (Double, Temperature)) => acc + (pair._1 * pair._2)

  val inverseDistanceWeighting: Iterable[(Double, Temperature)] => Temperature = distances => {
    val inverseDistances = distances.map { case (d, t) => (1.0 / pow(d, 4), t) }
    val normalizer = inverseDistances.foldLeft(0.0)(sumKeys)
    inverseDistances.foldLeft(0.0)(sumKeyMultipliedByValue) / normalizer
  }

  val linearInterpolate: (Double, Int, Int) => Int = (coefficient, lower, upper) => math.round((upper - lower) * coefficient + lower).toInt

  val interpolateColour: (Iterable[(Temperature, Color)], Temperature) => Color = (points, value) => {
    val (smallerPairs, biggerPairs) = points.partition(_._1 < value)

    if (smallerPairs.isEmpty && biggerPairs.isEmpty) Color(0, 0, 0)
    else if (smallerPairs.isEmpty) biggerPairs.minBy(_._1)._2
    else if (biggerPairs.isEmpty) smallerPairs.maxBy(_._1)._2
    else {
      val (maxSmallerPair, minBiggerPair) = (smallerPairs.maxBy(_._1), biggerPairs.minBy(_._1))
      val minToMinMaxDeltaCoeff = (value - maxSmallerPair._1) / (minBiggerPair._1 - maxSmallerPair._1)
      val (lowerColor, upperColor) = (maxSmallerPair._2, minBiggerPair._2)

      Color(
        linearInterpolate(minToMinMaxDeltaCoeff, lowerColor.red, upperColor.red),
        linearInterpolate(minToMinMaxDeltaCoeff, lowerColor.green, upperColor.green),
        linearInterpolate(minToMinMaxDeltaCoeff, lowerColor.blue, upperColor.blue)
      )
    }
  }

  val pixelToLocation: PixelCoordinates => Location = pixel => {
    val lon = (pixel._2 - width / 2) * (maxLongitude / width)
    val lat = -(pixel._1 - height / 2) * (maxLatitude / height)
    Location(lat, lon)
  }

  val areLocationsAntipodes: (Location, Location) => Boolean = (pos1, pos2) => (pos1.lat == -pos2.lat) && (abs(pos1.lon - pos2.lon) == 180)

  val calculateDistance: (Location, Location) => Double = (pos1, pos2) => {
    if (pos1 == pos2) 0
    else if (areLocationsAntipodes(pos1, pos2)) earthRadius * Pi
    else {
      val longitudeDistance = toRadians(abs(pos1.lon - pos2.lon))
      val latPos1 = toRadians(pos1.lat)
      val latPos2 = toRadians(pos2.lat)
      val latitudeDistance = abs(latPos1 - latPos2)
      earthRadius * (2 * asin(sqrt(pow(sin(latitudeDistance / 2), 2) + cos(latPos1) * cos(latPos2) * pow(sin(longitudeDistance / 2), 2))))
    }
  }

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {

    val distances: Iterable[(Double, Temperature)] = temperatures.map { case (l, t) => (calculateDistance(location, l), t) }

    val (closestDistance, closestTemperature) = distances.min
    if (closestDistance < distanceLimit) closestTemperature
    else inverseDistanceWeighting(distances)
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    points.find(_._1 == value) match {
      case Some((_, color)) => color
      case None => interpolateColour(points, value)
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {

    val pixelCoordinates = for {
      i <- 0 until height
      j <- 0 until width
    } yield (i, j)

    val pixels = pixelCoordinates
      .map(pixelToLocation)
      .map(predictTemperature(temperatures, _))
      .map(interpolateColor(colors, _))
      .map(col => Pixel(col.red, col.green, col.blue, 255)).toArray
    
    Image(width, height, pixels)
    Image.apply(width, height, pixels)
  }

}

