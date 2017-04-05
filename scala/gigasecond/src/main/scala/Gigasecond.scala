import java.time.{Duration, LocalDate, LocalDateTime}


/**
  * Created by ahaljh on 2017. 1. 23..
  */
object Gigasecond {
  def addGigaseconds(input: LocalDateTime): LocalDateTime =
    input.plus(Duration.ofSeconds(1000000000))

  def addGigaseconds(input: LocalDate): LocalDateTime =
    addGigaseconds(input.atTime(0,0))


}
