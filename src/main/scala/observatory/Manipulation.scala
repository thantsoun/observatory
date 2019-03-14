package observatory

/**
  * 4th milestone: value-added information
  */
object Manipulation {

  lazy val computeGrid: Iterable[(Location, Temperature)] => Map[GridLocation, Double] = temperatures =>
    (for {
      lat <- -89 to 90
      lon <- -180 to 179
    } yield GridLocation(lat, lon) -> Visualization.predictTemperature(temperatures, Location(lat, lon))).toMap

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {
    computeGrid(temperatures)
  }


  /**
    * @param temperatures Sequence of known temperatures over the years (each element of the collection
    *                     is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperatures: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {
    lazy val gridsPerYear: Iterable[GridLocation => Temperature] = temperatures.map(makeGrid)
    location => {
      val temperatures: Iterable[Temperature] = gridsPerYear.map(_ (location))
      temperatures.sum / temperatures.size
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param normals      A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = {
    lazy val grid = makeGrid(temperatures)
    location => grid(location) - normals(location)
  }


}

