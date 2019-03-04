package observatory

import java.nio.file.Paths
import java.time.LocalDate

import org.apache.log4j.{Level, Logger}
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.types._

import scala.collection.parallel.ParIterable


/**
  * 1st milestone: data extraction
  */
object Extraction {

  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  val spark: SparkSession = SparkSession.builder().appName("Observatory Data Extraction").config("spark.master", "local").getOrCreate()

  val pathAsString: String => String = (r: String) => Paths.get(getClass.getResource(r).toURI).toString
  val fahrenheitToCelsius: Temperature => Temperature = (f: Temperature) => (f - 32) / 1.8

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    val rawStationsData = spark.sparkContext.textFile(pathAsString(stationsFile))
    val rawTemperaturesData = spark.sparkContext.textFile(pathAsString(temperaturesFile))

    doLolocateTemperatures(year, rawStationsData, rawTemperaturesData).collect().seq
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    
    def sumRecordTemp (acc: Temperature, record: (LocalDate, Location, Temperature)) = acc + record._3
    def avgTemp(records: ParIterable[(LocalDate, Location, Temperature)]) = records.foldLeft(0.0)(sumRecordTemp) / records.size

    records.par.groupBy(_._2).mapValues(r => avgTemp(r)).seq
  }

  def doLolocateTemperatures(year: Year, stationsRaw: RDD[String], temperaturesRaw: RDD[String]): RDD[(LocalDate, Location, Temperature)] = {

    val stations: RDD[(StationIdentifier, Location)] = stationsRaw.map(_.split(',')).filter(_.length == 4).map(record => ((record(0), record(1)), Location(record(2).toDouble, record(3).toDouble)))
    val temperatures: RDD[(StationIdentifier, (LocalDate, Temperature))] = temperaturesRaw.map(_.split(',')).filter(_.length == 5).map(record => ((record(0), record(1)), (LocalDate.of(year, record(2).toInt, record(3).toInt), fahrenheitToCelsius(record(4).toDouble))))

    stations.join(temperatures).mapValues(value => (value._2._1, value._1, value._2._2)).values
  }

}
