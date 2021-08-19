/*
 * Copyright (c) 2007-present, Stephen Colebourne & Michael Nascimento Santos
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  * Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 *  * Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 *  * Neither the name of JSR-310 nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package org.threeten.bp.chrono

import org.threeten.bp.temporal.ChronoField.ALIGNED_DAY_OF_WEEK_IN_MONTH
import org.threeten.bp.temporal.ChronoField.ALIGNED_DAY_OF_WEEK_IN_YEAR
import org.threeten.bp.temporal.ChronoField.ALIGNED_WEEK_OF_MONTH
import org.threeten.bp.temporal.ChronoField.ALIGNED_WEEK_OF_YEAR
import org.threeten.bp.temporal.ChronoField.DAY_OF_MONTH
import org.threeten.bp.temporal.ChronoField.DAY_OF_WEEK
import org.threeten.bp.temporal.ChronoField.DAY_OF_YEAR
import org.threeten.bp.temporal.ChronoField.EPOCH_DAY
import org.threeten.bp.temporal.ChronoField.ERA
import org.threeten.bp.temporal.ChronoField.MONTH_OF_YEAR
import org.threeten.bp.temporal.ChronoField.PROLEPTIC_MONTH
import org.threeten.bp.temporal.ChronoField.YEAR
import org.threeten.bp.temporal.ChronoField.YEAR_OF_ERA
import org.threeten.bp.temporal.TemporalAdjusters.nextOrSame
import java.io.Serializable
import java.util.{ Arrays, Objects }
import org.threeten.bp.Clock
import org.threeten.bp.DateTimeException
import org.threeten.bp.DayOfWeek
import org.threeten.bp.Instant
import org.threeten.bp.LocalDate
import org.threeten.bp.LocalDateTime
import org.threeten.bp.Month
import org.threeten.bp.Year
import org.threeten.bp.ZoneId
import org.threeten.bp.ZonedDateTime
import org.threeten.bp.format.ResolverStyle
import org.threeten.bp.temporal.ChronoField
import org.threeten.bp.temporal.TemporalAccessor
import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.temporal.ValueRange

@SerialVersionUID(-1440403870442975015L)
object IsoChronology {

  /** Singleton instance of the ISO chronology. */
  lazy val INSTANCE: IsoChronology = new IsoChronology
}

/**
 * The ISO calendar system.
 *
 * This chronology defines the rules of the ISO calendar system. This calendar system is based on
 * the ISO-8601 standard, which is the <i>de facto</i> world calendar.
 *
 * The fields are defined as follows: <ul> <li>era - There are two eras, 'Current Era' (CE) and
 * 'Before Current Era' (BCE). <li>year-of-era - The year-of-era is the same as the proleptic-year
 * for the current CE era. For the BCE era before the ISO epoch the year increases from 1 upwards as
 * time goes backwards. <li>proleptic-year - The proleptic year is the same as the year-of-era for
 * the current era. For the previous era, years have zero, then negative values. <li>month-of-year -
 * There are 12 months in an ISO year, numbered from 1 to 12. <li>day-of-month - There are between
 * 28 and 31 days in each of the ISO month, numbered from 1 to 31. Months 4, 6, 9 and 11 have 30
 * days, Months 1, 3, 5, 7, 8, 10 and 12 have 31 days. Month 2 has 28 days, or 29 in a leap year.
 * <li>day-of-year - There are 365 days in a standard ISO year and 366 in a leap year. The days are
 * numbered from 1 to 365 or 1 to 366. <li>leap-year - Leap years occur every 4 years, except where
 * the year is divisble by 100 and not divisble by 400. </ul><p>
 *
 * <h3>Specification for implementors</h3> This class is immutable and thread-safe.
 *
 * @constructor
 *   Restricted constructor.
 */
@SerialVersionUID(-1440403870442975015L)
final class IsoChronology private () extends Chronology with Serializable {

  /**
   * Resolve singleton.
   *
   * @return
   *   the singleton instance, not null
   */
  private def readResolve: AnyRef = IsoChronology.INSTANCE

  /**
   * Gets the ID of the chronology - 'ISO'.
   *
   * The ID uniquely identifies the {@code Chronology}. It can be used to lookup the {@code
   * Chronology} using {@link #of(String)}.
   *
   * @return
   *   the chronology ID - 'ISO'
   * @see
   *   #getCalendarType()
   */
  def getId: String = "ISO"

  /**
   * Gets the calendar type of the underlying calendar system - 'iso8601'.
   *
   * The calendar type is an identifier defined by the <em>Unicode Locale Data Markup Language
   * (LDML)</em> specification. It can be used to lookup the {@code Chronology} using {@link
   * #of(String)}. It can also be used as part of a locale, accessible via {@link
   * Locale#getUnicodeLocaleType(String)} with the key 'ca'.
   *
   * @return
   *   the calendar system type - 'iso8601'
   * @see
   *   #getId()
   */
  def getCalendarType: String = "iso8601"

  /**
   * Obtains an ISO local date from the era, year-of-era, month-of-year and day-of-month fields.
   *
   * @param era
   *   the ISO era, not null
   * @param yearOfEra
   *   the ISO year-of-era
   * @param month
   *   the ISO month-of-year
   * @param dayOfMonth
   *   the ISO day-of-month
   * @return
   *   the ISO local date, not null
   * @throws DateTimeException
   *   if unable to create the date
   */
  override def date(era: Era, yearOfEra: Int, month: Int, dayOfMonth: Int): LocalDate =
    date(prolepticYear(era, yearOfEra), month, dayOfMonth)

  /**
   * Obtains an ISO local date from the proleptic-year, month-of-year and day-of-month fields.
   *
   * This is equivalent to {@link LocalDate#of(int, int, int)}.
   *
   * @param prolepticYear
   *   the ISO proleptic-year
   * @param month
   *   the ISO month-of-year
   * @param dayOfMonth
   *   the ISO day-of-month
   * @return
   *   the ISO local date, not null
   * @throws DateTimeException
   *   if unable to create the date
   */
  def date(prolepticYear: Int, month: Int, dayOfMonth: Int): LocalDate =
    LocalDate.of(prolepticYear, month, dayOfMonth)

  /**
   * Obtains an ISO local date from the era, year-of-era and day-of-year fields.
   *
   * @param era
   *   the ISO era, not null
   * @param yearOfEra
   *   the ISO year-of-era
   * @param dayOfYear
   *   the ISO day-of-year
   * @return
   *   the ISO local date, not null
   * @throws DateTimeException
   *   if unable to create the date
   */
  override def dateYearDay(era: Era, yearOfEra: Int, dayOfYear: Int): LocalDate =
    dateYearDay(prolepticYear(era, yearOfEra), dayOfYear)

  /**
   * Obtains an ISO local date from the proleptic-year and day-of-year fields.
   *
   * This is equivalent to {@link LocalDate#ofYearDay(int, int)}.
   *
   * @param prolepticYear
   *   the ISO proleptic-year
   * @param dayOfYear
   *   the ISO day-of-year
   * @return
   *   the ISO local date, not null
   * @throws DateTimeException
   *   if unable to create the date
   */
  def dateYearDay(prolepticYear: Int, dayOfYear: Int): LocalDate =
    LocalDate.ofYearDay(prolepticYear, dayOfYear)

  def dateEpochDay(epochDay: Long): LocalDate = LocalDate.ofEpochDay(epochDay)

  /**
   * Obtains an ISO local date from another date-time object.
   *
   * This is equivalent to {@link LocalDate#from(TemporalAccessor)}.
   *
   * @param temporal
   *   the date-time object to convert, not null
   * @return
   *   the ISO local date, not null
   * @throws DateTimeException
   *   if unable to create the date
   */
  def date(temporal: TemporalAccessor): LocalDate = LocalDate.from(temporal)

  /**
   * Obtains an ISO local date-time from another date-time object.
   *
   * This is equivalent to {@link LocalDateTime#from(TemporalAccessor)}.
   *
   * @param temporal
   *   the date-time object to convert, not null
   * @return
   *   the ISO local date-time, not null
   * @throws DateTimeException
   *   if unable to create the date-time
   */
  override def localDateTime(temporal: TemporalAccessor): LocalDateTime =
    LocalDateTime.from(temporal)

  /**
   * Obtains an ISO zoned date-time from another date-time object.
   *
   * This is equivalent to {@link ZonedDateTime#from(TemporalAccessor)}.
   *
   * @param temporal
   *   the date-time object to convert, not null
   * @return
   *   the ISO zoned date-time, not null
   * @throws DateTimeException
   *   if unable to create the date-time
   */
  override def zonedDateTime(temporal: TemporalAccessor): ZonedDateTime =
    ZonedDateTime.from(temporal)

  /**
   * Obtains an ISO zoned date-time from an instant.
   *
   * This is equivalent to {@link ZonedDateTime#ofInstant(Instant, ZoneId)}.
   *
   * @param instant
   *   the instant to convert, not null
   * @param zone
   *   the zone to use, not null
   * @return
   *   the ISO zoned date-time, not null
   * @throws DateTimeException
   *   if unable to create the date-time
   */
  override def zonedDateTime(instant: Instant, zone: ZoneId): ZonedDateTime =
    ZonedDateTime.ofInstant(instant, zone)

  /**
   * Obtains the current ISO local date from the system clock in the default time-zone.
   *
   * This will query the {@link Clock#systemDefaultZone() system clock} in the default time-zone to
   * obtain the current date.
   *
   * Using this method will prevent the ability to use an alternate clock for testing because the
   * clock is hard-coded.
   *
   * @return
   *   the current ISO local date using the system clock and default time-zone, not null
   * @throws DateTimeException
   *   if unable to create the date
   */
  override def dateNow: LocalDate = dateNow(Clock.systemDefaultZone)

  /**
   * Obtains the current ISO local date from the system clock in the specified time-zone.
   *
   * This will query the {@link Clock#system(ZoneId) system clock} to obtain the current date.
   * Specifying the time-zone avoids dependence on the default time-zone.
   *
   * Using this method will prevent the ability to use an alternate clock for testing because the
   * clock is hard-coded.
   *
   * @return
   *   the current ISO local date using the system clock, not null
   * @throws DateTimeException
   *   if unable to create the date
   */
  override def dateNow(zone: ZoneId): LocalDate = dateNow(Clock.system(zone))

  /**
   * Obtains the current ISO local date from the specified clock.
   *
   * This will query the specified clock to obtain the current date - today. Using this method
   * allows the use of an alternate clock for testing. The alternate clock may be introduced using
   * {@link Clock dependency injection}.
   *
   * @param clock
   *   the clock to use, not null
   * @return
   *   the current ISO local date, not null
   * @throws DateTimeException
   *   if unable to create the date
   */
  override def dateNow(clock: Clock): LocalDate = {
    Objects.requireNonNull(clock, "clock")
    date(LocalDate.now(clock))
  }

  /**
   * Checks if the year is a leap year, according to the ISO proleptic calendar system rules.
   *
   * This method applies the current rules for leap years across the whole time-line. In general, a
   * year is a leap year if it is divisible by four without remainder. However, years divisible by
   * 100, are not leap years, with the exception of years divisible by 400 which are.
   *
   * For example, 1904 is a leap year it is divisible by 4. 1900 was not a leap year as it is
   * divisible by 100, however 2000 was a leap year as it is divisible by 400.
   *
   * The calculation is proleptic - applying the same rules into the far future and far past. This
   * is historically inaccurate, but is correct for the ISO-8601 standard.
   *
   * @param prolepticYear
   *   the ISO proleptic year to check
   * @return
   *   true if the year is leap, false otherwise
   */
  def isLeapYear(prolepticYear: Long): Boolean =
    ((prolepticYear & 3) == 0) && ((prolepticYear % 100) != 0 || (prolepticYear % 400) == 0)

  def prolepticYear(era: Era, yearOfEra: Int): Int =
    if (!era.isInstanceOf[IsoEra]) throw new ClassCastException("Era must be IsoEra")
    else if (era eq IsoEra.CE) yearOfEra
    else 1 - yearOfEra

  def eraOf(eraValue: Int): IsoEra = IsoEra.of(eraValue)

  def eras: java.util.List[Era] = Arrays.asList[Era](IsoEra.values: _*)

  def range(field: ChronoField): ValueRange = field.range

  override def resolveDate(
    fieldValues:   java.util.Map[TemporalField, java.lang.Long],
    resolverStyle: ResolverStyle
  ): LocalDate = {
    if (fieldValues.containsKey(EPOCH_DAY))
      return LocalDate.ofEpochDay(fieldValues.remove(EPOCH_DAY))
    val prolepticMonth: java.lang.Long = fieldValues.remove(PROLEPTIC_MONTH)
    if (prolepticMonth != null) {
      if (resolverStyle ne ResolverStyle.LENIENT)
        PROLEPTIC_MONTH.checkValidValue(prolepticMonth)
      updateResolveMap(fieldValues, MONTH_OF_YEAR, Math.floorMod(prolepticMonth, 12) + 1L)
      updateResolveMap(fieldValues, YEAR, Math.floorDiv(prolepticMonth, 12))
    }
    val yoeLong: java.lang.Long        = fieldValues.remove(YEAR_OF_ERA)
    if (yoeLong != null) {
      if (resolverStyle ne ResolverStyle.LENIENT)
        YEAR_OF_ERA.checkValidValue(yoeLong)
      val era: java.lang.Long = fieldValues.remove(ERA)
      if (era == null) {
        val year: java.lang.Long = fieldValues.get(YEAR)
        if (resolverStyle eq ResolverStyle.STRICT)
          if (year != null)
            updateResolveMap(fieldValues,
                             YEAR,
                             (if (year > 0) yoeLong else Math.subtractExact(1, yoeLong))
            )
          else fieldValues.put(YEAR_OF_ERA, yoeLong)
        else
          updateResolveMap(
            fieldValues,
            YEAR,
            (if (year == null || year > 0) yoeLong else Math.subtractExact(1, yoeLong))
          )
      } else if (era.longValue == 1L)
        updateResolveMap(fieldValues, YEAR, yoeLong)
      else if (era.longValue == 0L)
        updateResolveMap(fieldValues, YEAR, Math.subtractExact(1, yoeLong))
      else
        throw new DateTimeException(s"Invalid value for era: $era")
    } else if (fieldValues.containsKey(ERA))
      ERA.checkValidValue(fieldValues.get(ERA))
    if (fieldValues.containsKey(YEAR)) {
      if (fieldValues.containsKey(MONTH_OF_YEAR)) {
        if (fieldValues.containsKey(DAY_OF_MONTH)) {
          val y: Int   = YEAR.checkValidIntValue(fieldValues.remove(YEAR))
          val moy: Int = Math.toIntExact(fieldValues.remove(MONTH_OF_YEAR))
          var dom: Int = Math.toIntExact(fieldValues.remove(DAY_OF_MONTH))
          if (resolverStyle eq ResolverStyle.LENIENT) {
            val months: Long = Math.subtractExact(moy.toLong, 1)
            val days: Long   = Math.subtractExact(dom.toLong, 1)
            return LocalDate.of(y, 1, 1).plusMonths(months).plusDays(days)
          } else if (resolverStyle eq ResolverStyle.SMART) {
            DAY_OF_MONTH.checkValidValue(dom.toLong)
            if (moy == 4 || moy == 6 || moy == 9 || moy == 11)
              dom = Math.min(dom, 30)
            else if (moy == 2)
              dom = Math.min(dom, Month.FEBRUARY.length(Year.isLeap(y.toLong)))
            return LocalDate.of(y, moy, dom)
          } else
            return LocalDate.of(y, moy, dom)
        }
        if (fieldValues.containsKey(ALIGNED_WEEK_OF_MONTH)) {
          if (fieldValues.containsKey(ALIGNED_DAY_OF_WEEK_IN_MONTH)) {
            val y: Int          = YEAR.checkValidIntValue(fieldValues.remove(YEAR))
            if (resolverStyle eq ResolverStyle.LENIENT) {
              val months: Long = Math.subtractExact(fieldValues.remove(MONTH_OF_YEAR), 1)
              val weeks: Long  = Math.subtractExact(fieldValues.remove(ALIGNED_WEEK_OF_MONTH), 1)
              val days: Long   =
                Math.subtractExact(fieldValues.remove(ALIGNED_DAY_OF_WEEK_IN_MONTH), 1)
              return LocalDate.of(y, 1, 1).plusMonths(months).plusWeeks(weeks).plusDays(days)
            }
            val moy: Int        = MONTH_OF_YEAR.checkValidIntValue(fieldValues.remove(MONTH_OF_YEAR))
            val aw: Int         =
              ALIGNED_WEEK_OF_MONTH.checkValidIntValue(fieldValues.remove(ALIGNED_WEEK_OF_MONTH))
            val ad: Int         = ALIGNED_DAY_OF_WEEK_IN_MONTH.checkValidIntValue(
              fieldValues.remove(ALIGNED_DAY_OF_WEEK_IN_MONTH)
            )
            val date: LocalDate =
              LocalDate.of(y, moy, 1).plusDays((aw.toLong - 1) * 7 + (ad.toLong - 1))
            if ((resolverStyle eq ResolverStyle.STRICT) && (date.get(MONTH_OF_YEAR) != moy))
              throw new DateTimeException("Strict mode rejected date parsed to a different month")
            return date
          }
          if (fieldValues.containsKey(DAY_OF_WEEK)) {
            val y: Int          = YEAR.checkValidIntValue(fieldValues.remove(YEAR))
            if (resolverStyle eq ResolverStyle.LENIENT) {
              val months: Long = Math.subtractExact(fieldValues.remove(MONTH_OF_YEAR), 1)
              val weeks: Long  = Math.subtractExact(fieldValues.remove(ALIGNED_WEEK_OF_MONTH), 1)
              val days: Long   = Math.subtractExact(fieldValues.remove(DAY_OF_WEEK), 1)
              return LocalDate.of(y, 1, 1).plusMonths(months).plusWeeks(weeks).plusDays(days)
            }
            val moy: Int        = MONTH_OF_YEAR.checkValidIntValue(fieldValues.remove(MONTH_OF_YEAR))
            val aw: Int         =
              ALIGNED_WEEK_OF_MONTH.checkValidIntValue(fieldValues.remove(ALIGNED_WEEK_OF_MONTH))
            val dow: Int        = DAY_OF_WEEK.checkValidIntValue(fieldValues.remove(DAY_OF_WEEK))
            val date: LocalDate =
              LocalDate.of(y, moy, 1).plusWeeks(aw.toLong - 1).`with`(nextOrSame(DayOfWeek.of(dow)))
            if ((resolverStyle eq ResolverStyle.STRICT) && (date.get(MONTH_OF_YEAR) != moy))
              throw new DateTimeException("Strict mode rejected date parsed to a different month")
            return date
          }
        }
      }
      if (fieldValues.containsKey(DAY_OF_YEAR)) {
        val y: Int   = YEAR.checkValidIntValue(fieldValues.remove(YEAR))
        if (resolverStyle eq ResolverStyle.LENIENT) {
          val days: Long = Math.subtractExact(fieldValues.remove(DAY_OF_YEAR), 1)
          return LocalDate.ofYearDay(y, 1).plusDays(days)
        }
        val doy: Int = DAY_OF_YEAR.checkValidIntValue(fieldValues.remove(DAY_OF_YEAR))
        return LocalDate.ofYearDay(y, doy)
      }
      if (fieldValues.containsKey(ALIGNED_WEEK_OF_YEAR)) {
        if (fieldValues.containsKey(ALIGNED_DAY_OF_WEEK_IN_YEAR)) {
          val y: Int          = YEAR.checkValidIntValue(fieldValues.remove(YEAR))
          if (resolverStyle eq ResolverStyle.LENIENT) {
            val weeks: Long = Math.subtractExact(fieldValues.remove(ALIGNED_WEEK_OF_YEAR), 1)
            val days: Long  = Math.subtractExact(fieldValues.remove(ALIGNED_DAY_OF_WEEK_IN_YEAR), 1)
            return LocalDate.of(y, 1, 1).plusWeeks(weeks).plusDays(days)
          }
          val aw: Int         =
            ALIGNED_WEEK_OF_YEAR.checkValidIntValue(fieldValues.remove(ALIGNED_WEEK_OF_YEAR))
          val ad: Int         = ALIGNED_DAY_OF_WEEK_IN_YEAR.checkValidIntValue(
            fieldValues.remove(ALIGNED_DAY_OF_WEEK_IN_YEAR)
          )
          val date: LocalDate = LocalDate.of(y, 1, 1).plusDays((aw.toLong - 1) * 7 + (ad - 1))
          if ((resolverStyle eq ResolverStyle.STRICT) && (date.get(YEAR) != y))
            throw new DateTimeException("Strict mode rejected date parsed to a different year")
          return date
        }
        if (fieldValues.containsKey(DAY_OF_WEEK)) {
          val y: Int          = YEAR.checkValidIntValue(fieldValues.remove(YEAR))
          if (resolverStyle eq ResolverStyle.LENIENT) {
            val weeks: Long = Math.subtractExact(fieldValues.remove(ALIGNED_WEEK_OF_YEAR), 1)
            val days: Long  = Math.subtractExact(fieldValues.remove(DAY_OF_WEEK), 1)
            return LocalDate.of(y, 1, 1).plusWeeks(weeks).plusDays(days)
          }
          val aw: Int         =
            ALIGNED_WEEK_OF_YEAR.checkValidIntValue(fieldValues.remove(ALIGNED_WEEK_OF_YEAR))
          val dow: Int        = DAY_OF_WEEK.checkValidIntValue(fieldValues.remove(DAY_OF_WEEK))
          val date: LocalDate =
            LocalDate.of(y, 1, 1).plusWeeks(aw.toLong - 1).`with`(nextOrSame(DayOfWeek.of(dow)))
          if ((resolverStyle eq ResolverStyle.STRICT) && (date.get(YEAR) != y))
            throw new DateTimeException("Strict mode rejected date parsed to a different month")
          return date
        }
      }
    }
    null
  }
}
