package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import scala.math._

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  val width = 360
  val height = 180
  val maxLong = 360
  val maxLat = 180
  val earthRadius = 6371
  val distanceLimit = 1

  val imageCoordinatesToLocation: (Int, Int) => Location = (x, y) => {
    val lon = (y - width / 2) * (maxLong / width)
    val lat = -(x - height / 2) * (maxLat / height)
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

    val sumKeys = (acc: Double, pair: (Double, Temperature)) => acc + pair._1 
    val sumKeyMultByValue = (acc: Double, pair: (Double, Temperature)) => acc + (pair._1 * pair._2)
    
    val distances = temperatures.map { case (l, t) => (calculateDistance(location, l), t) }

    val (closestDistance, closestTemperature) = distances.min
    if (closestDistance < distanceLimit) closestTemperature
    else {
      val inverseDistances = distances.map { case (d, t) => (1.0 / pow(d, 4), t) }
      val normalizer = inverseDistances.foldLeft(0.0)(sumKeys)
      inverseDistances.foldLeft(0.0)(sumKeyMultByValue) / normalizer 
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    ???
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    ???
  }

}

