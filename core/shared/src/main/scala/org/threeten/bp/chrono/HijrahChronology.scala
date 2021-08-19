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
import org.threeten.bp.temporal.ChronoUnit.DAYS
import org.threeten.bp.temporal.ChronoUnit.MONTHS
import org.threeten.bp.temporal.ChronoUnit.WEEKS
import org.threeten.bp.temporal.TemporalAdjusters.nextOrSame
import java.io.Serializable
import java.util.{ Arrays, Objects }
import org.threeten.bp.Clock
import org.threeten.bp.DateTimeException
import org.threeten.bp.DayOfWeek
import org.threeten.bp.Instant
import org.threeten.bp.LocalDate
import org.threeten.bp.ZoneId
import org.threeten.bp.format.ResolverStyle
import org.threeten.bp.temporal.ChronoField
import org.threeten.bp.temporal.TemporalAccessor
import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.temporal.ValueRange

@SerialVersionUID(3127340209035924785L)
object HijrahChronology {

  /** Singleton instance of the Hijrah chronology. */
  lazy val INSTANCE: HijrahChronology = new HijrahChronology

  /** Fallback language for the era names. */
  // private val FALLBACK_LANGUAGE: String = "en"

  /** Narrow names for eras. */
  // private val ERA_NARROW_NAMES: java.util.HashMap[String, Array[String]] = {
  //   val names = new java.util.HashMap[String, Array[String]]
  //   names.put(FALLBACK_LANGUAGE, Array[String]("BH", "HE"))
  //   names
  // }
  //
  // /** Short names for eras. */
  // private val ERA_SHORT_NAMES: java.util.HashMap[String, Array[String]] = {
  //   val names = new java.util.HashMap[String, Array[String]]
  //   names.put(FALLBACK_LANGUAGE, Array[String]("B.H.", "H.E."))
  //   names
  // }
  //
  // /** Full names for eras. */
  // private val ERA_FULL_NAMES: java.util.HashMap[String, Array[String]] = {
  //   val names = new java.util.HashMap[String, Array[String]]
  //   names.put(FALLBACK_LANGUAGE, Array[String]("Before Hijrah", "Hijrah Era"))
  //   names
  // }
}

/**
 * The Hijrah calendar system.
 *
 * This chronology defines the rules of the Hijrah calendar system.
 *
 * The implementation follows the Freeman-Grenville algorithm (*1) and has following features. <ul>
 * <li>A year has 12 months.</li> <li>Over a cycle of 30 years there are 11 leap years.</li>
 * <li>There are 30 days in month number 1, 3, 5, 7, 9, and 11, and 29 days in month number 2, 4, 6,
 * 8, 10, and 12.</li> <li>In a leap year month 12 has 30 days.</li> <li>In a 30 year cycle, year 2,
 * 5, 7, 10, 13, 16, 18, 21, 24, 26, and 29 are leap years.</li> <li>Total of 10631 days in a 30
 * years cycle.</li> </ul><p>
 *
 * The table shows the features described above. <blockquote> <table border="1"> <tbody> <tr> <th>#
 * of month</th> <th>Name of month</th> <th>Number of days</th> </tr> <tr> <td>1</td>
 * <td>Muharram</td> <td>30</td> </tr> <tr> <td>2</td> <td>Safar</td> <td>29</td> </tr> <tr>
 * <td>3</td> <td>Rabi'al-Awwal</td> <td>30</td> </tr> <tr> <td>4</td> <td>Rabi'ath-Thani</td>
 * <td>29</td> </tr> <tr> <td>5</td> <td>Jumada l-Ula</td> <td>30</td> </tr> <tr> <td>6</td>
 * <td>Jumada t-Tania</td> <td>29</td> </tr> <tr> <td>7</td> <td>Rajab</td> <td>30</td> </tr> <tr>
 * <td>8</td> <td>Sha`ban</td> <td>29</td> </tr> <tr> <td>9</td> <td>Ramadan</td> <td>30</td> </tr>
 * <tr> <td>10</td> <td>Shawwal</td> <td>29</td> </tr> <tr> <td>11</td> <td>Dhu 'l-Qa`da</td>
 * <td>30</td> </tr> <tr> <td>12</td> <td>Dhu 'l-Hijja</td> <td>29, but 30 days in years 2, 5, 7,
 * 10,<br> 13, 16, 18, 21, 24, 26, and 29</td> </tr> </tbody> </table> </blockquote>
 *
 * (*1) The algorithm is taken from the book, The Muslim and Christian Calendars by G.S.P.
 * Freeman-Grenville.
 *
 * <h3>Specification for implementors</h3> This class is immutable and thread-safe.
 *
 * @constructor
 *   Restrictive constructor.
 */
@SerialVersionUID(3127340209035924785L)
final class HijrahChronology private () extends Chronology with Serializable {

  /**
   * Resolve singleton.
   *
   * @return
   *   the singleton instance, not null
   */
  private def readResolve: AnyRef = HijrahChronology.INSTANCE

  /**
   * Gets the ID of the chronology - 'Hijrah-umalqura'.
   *
   * The ID uniquely identifies the {@code Chronology}. It can be used to lookup the {@code
   * Chronology} using {@link #of(String)}.
   *
   * @return
   *   the chronology ID - 'Hijrah-umalqura'
   * @see
   *   #getCalendarType()
   */
  def getId: String = "Hijrah-umalqura"

  /**
   * Gets the calendar type of the underlying calendar system - 'islamic-umalqura'.
   *
   * The calendar type is an identifier defined by the <em>Unicode Locale Data Markup Language
   * (LDML)</em> specification. It can be used to lookup the {@code Chronology} using {@link
   * #of(String)}. It can also be used as part of a locale, accessible via {@link
   * Locale#getUnicodeLocaleType(String)} with the key 'ca'.
   *
   * @return
   *   the calendar system type - 'islamic-umalqura'
   * @see
   *   #getId()
   */
  def getCalendarType: String = "islamic-umalqura"

  override def date(era: Era, yearOfEra: Int, month: Int, dayOfMonth: Int): HijrahDate =
    super.date(era, yearOfEra, month, dayOfMonth).asInstanceOf[HijrahDate]

  def date(prolepticYear: Int, month: Int, dayOfMonth: Int): HijrahDate =
    HijrahDate.of(prolepticYear, month, dayOfMonth)

  override def dateYearDay(era: Era, yearOfEra: Int, dayOfYear: Int): HijrahDate =
    super.dateYearDay(era, yearOfEra, dayOfYear).asInstanceOf[HijrahDate]

  def dateYearDay(prolepticYear: Int, dayOfYear: Int): HijrahDate =
    HijrahDate.of(prolepticYear, 1, 1).plusDays(dayOfYear.toLong - 1)

  def dateEpochDay(epochDay: Long): HijrahDate = HijrahDate.of(LocalDate.ofEpochDay(epochDay))

  def date(temporal: TemporalAccessor): HijrahDate =
    temporal match {
      case date: HijrahDate => date
      case _                => HijrahDate.ofEpochDay(temporal.getLong(EPOCH_DAY))
    }

  override def localDateTime(temporal: TemporalAccessor): ChronoLocalDateTime[HijrahDate] =
    super.localDateTime(temporal).asInstanceOf[ChronoLocalDateTime[HijrahDate]]

  override def zonedDateTime(temporal: TemporalAccessor): ChronoZonedDateTime[HijrahDate] =
    super.zonedDateTime(temporal).asInstanceOf[ChronoZonedDateTime[HijrahDate]]

  override def zonedDateTime(instant: Instant, zone: ZoneId): ChronoZonedDateTime[HijrahDate] =
    super.zonedDateTime(instant, zone).asInstanceOf[ChronoZonedDateTime[HijrahDate]]

  override def dateNow: HijrahDate = super.dateNow.asInstanceOf[HijrahDate]

  override def dateNow(zone: ZoneId): HijrahDate = super.dateNow(zone).asInstanceOf[HijrahDate]

  override def dateNow(clock: Clock): HijrahDate = {
    Objects.requireNonNull(clock, "clock")
    super.dateNow(clock).asInstanceOf[HijrahDate]
  }

  def isLeapYear(prolepticYear: Long): Boolean = HijrahDate.isLeapYear(prolepticYear)

  def prolepticYear(era: Era, yearOfEra: Int): Int =
    if (!era.isInstanceOf[HijrahEra])
      throw new ClassCastException("Era must be HijrahEra")
    else if (era eq HijrahEra.AH) yearOfEra
    else 1 - yearOfEra

  def eraOf(eraValue: Int): HijrahEra =
    eraValue match {
      case 0 =>
        HijrahEra.BEFORE_AH
      case 1 =>
        HijrahEra.AH
      case _ =>
        throw new DateTimeException("invalid Hijrah era")
    }

  def eras: java.util.List[Era] = Arrays.asList[Era](HijrahEra.values: _*)

  def range(field: ChronoField): ValueRange = field.range

  override def resolveDate(
    fieldValues:   java.util.Map[TemporalField, java.lang.Long],
    resolverStyle: ResolverStyle
  ): HijrahDate = {
    if (fieldValues.containsKey(EPOCH_DAY))
      return dateEpochDay(fieldValues.remove(EPOCH_DAY))
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
                             if (year > 0) yoeLong else Math.subtractExact(1, yoeLong)
            )
          else
            fieldValues.put(YEAR_OF_ERA, yoeLong)
        else
          updateResolveMap(
            fieldValues,
            YEAR,
            if (year == null || year > 0) yoeLong else Math.subtractExact(1, yoeLong)
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
          val y: Int = YEAR.checkValidIntValue(fieldValues.remove(YEAR))
          if (resolverStyle eq ResolverStyle.LENIENT) {
            val months: Long = Math.subtractExact(fieldValues.remove(MONTH_OF_YEAR), 1)
            val days: Long   = Math.subtractExact(fieldValues.remove(DAY_OF_MONTH), 1)
            return date(y, 1, 1).plusMonths(months).plusDays(days)
          } else {
            val moy: Int = range(MONTH_OF_YEAR)
              .checkValidIntValue(fieldValues.remove(MONTH_OF_YEAR), MONTH_OF_YEAR)
            var dom: Int =
              range(DAY_OF_MONTH).checkValidIntValue(fieldValues.remove(DAY_OF_MONTH), DAY_OF_MONTH)
            if ((resolverStyle eq ResolverStyle.SMART) && dom > 28)
              dom = Math.min(dom, date(y, moy, 1).lengthOfMonth)
            return date(y, moy, dom)
          }
        }
        if (fieldValues.containsKey(ALIGNED_WEEK_OF_MONTH)) {
          if (fieldValues.containsKey(ALIGNED_DAY_OF_WEEK_IN_MONTH)) {
            val y: Int                 = YEAR.checkValidIntValue(fieldValues.remove(YEAR))
            if (resolverStyle eq ResolverStyle.LENIENT) {
              val months: Long = Math.subtractExact(fieldValues.remove(MONTH_OF_YEAR), 1)
              val weeks: Long  = Math.subtractExact(fieldValues.remove(ALIGNED_WEEK_OF_MONTH), 1)
              val days: Long   =
                Math.subtractExact(fieldValues.remove(ALIGNED_DAY_OF_WEEK_IN_MONTH), 1)
              return date(y, 1, 1).plus(months, MONTHS).plus(weeks, WEEKS).plus(days, DAYS)
            }
            val moy: Int               = MONTH_OF_YEAR.checkValidIntValue(fieldValues.remove(MONTH_OF_YEAR))
            val aw: Int                =
              ALIGNED_WEEK_OF_MONTH.checkValidIntValue(fieldValues.remove(ALIGNED_WEEK_OF_MONTH))
            val ad: Int                = ALIGNED_DAY_OF_WEEK_IN_MONTH.checkValidIntValue(
              fieldValues.remove(ALIGNED_DAY_OF_WEEK_IN_MONTH)
            )
            val hijrahDate: HijrahDate = date(y, moy, 1).plus((aw.toLong - 1) * 7 + (ad - 1), DAYS)
            if ((resolverStyle eq ResolverStyle.STRICT) && hijrahDate.get(MONTH_OF_YEAR) != moy)
              throw new DateTimeException("Strict mode rejected date parsed to a different month")
            return hijrahDate
          }
          if (fieldValues.containsKey(DAY_OF_WEEK)) {
            val y: Int                 = YEAR.checkValidIntValue(fieldValues.remove(YEAR))
            if (resolverStyle eq ResolverStyle.LENIENT) {
              val months: Long = Math.subtractExact(fieldValues.remove(MONTH_OF_YEAR), 1)
              val weeks: Long  = Math.subtractExact(fieldValues.remove(ALIGNED_WEEK_OF_MONTH), 1)
              val days: Long   = Math.subtractExact(fieldValues.remove(DAY_OF_WEEK), 1)
              return date(y, 1, 1).plus(months, MONTHS).plus(weeks, WEEKS).plus(days, DAYS)
            }
            val moy: Int               = MONTH_OF_YEAR.checkValidIntValue(fieldValues.remove(MONTH_OF_YEAR))
            val aw: Int                =
              ALIGNED_WEEK_OF_MONTH.checkValidIntValue(fieldValues.remove(ALIGNED_WEEK_OF_MONTH))
            val dow: Int               = DAY_OF_WEEK.checkValidIntValue(fieldValues.remove(DAY_OF_WEEK))
            val hijrahDate: HijrahDate =
              date(y, moy, 1).plus(aw.toLong - 1, WEEKS).`with`(nextOrSame(DayOfWeek.of(dow)))
            if ((resolverStyle eq ResolverStyle.STRICT) && hijrahDate.get(MONTH_OF_YEAR) != moy)
              throw new DateTimeException("Strict mode rejected date parsed to a different month")
            return hijrahDate
          }
        }
      }
      if (fieldValues.containsKey(DAY_OF_YEAR)) {
        val y: Int   = YEAR.checkValidIntValue(fieldValues.remove(YEAR))
        if (resolverStyle eq ResolverStyle.LENIENT) {
          val days: Long = Math.subtractExact(fieldValues.remove(DAY_OF_YEAR), 1)
          return dateYearDay(y, 1).plusDays(days)
        }
        val doy: Int = DAY_OF_YEAR.checkValidIntValue(fieldValues.remove(DAY_OF_YEAR))
        return dateYearDay(y, doy)
      }
      if (fieldValues.containsKey(ALIGNED_WEEK_OF_YEAR)) {
        if (fieldValues.containsKey(ALIGNED_DAY_OF_WEEK_IN_YEAR)) {
          val y: Int                 = YEAR.checkValidIntValue(fieldValues.remove(YEAR))
          if (resolverStyle eq ResolverStyle.LENIENT) {
            val weeks: Long = Math.subtractExact(fieldValues.remove(ALIGNED_WEEK_OF_YEAR), 1)
            val days: Long  = Math.subtractExact(fieldValues.remove(ALIGNED_DAY_OF_WEEK_IN_YEAR), 1)
            return date(y, 1, 1).plus(weeks, WEEKS).plus(days, DAYS)
          }
          val aw: Int                =
            ALIGNED_WEEK_OF_YEAR.checkValidIntValue(fieldValues.remove(ALIGNED_WEEK_OF_YEAR))
          val ad: Int                = ALIGNED_DAY_OF_WEEK_IN_YEAR.checkValidIntValue(
            fieldValues.remove(ALIGNED_DAY_OF_WEEK_IN_YEAR)
          )
          val hijrahDate: HijrahDate = date(y, 1, 1).plusDays((aw.toLong - 1) * 7 + (ad - 1))
          if ((resolverStyle eq ResolverStyle.STRICT) && hijrahDate.get(YEAR) != y)
            throw new DateTimeException("Strict mode rejected date parsed to a different year")
          return hijrahDate
        }
        if (fieldValues.containsKey(DAY_OF_WEEK)) {
          val y: Int                 = YEAR.checkValidIntValue(fieldValues.remove(YEAR))
          if (resolverStyle eq ResolverStyle.LENIENT) {
            val weeks: Long = Math.subtractExact(fieldValues.remove(ALIGNED_WEEK_OF_YEAR), 1)
            val days: Long  = Math.subtractExact(fieldValues.remove(DAY_OF_WEEK), 1)
            return date(y, 1, 1).plus(weeks, WEEKS).plus(days, DAYS)
          }
          val aw: Int                =
            ALIGNED_WEEK_OF_YEAR.checkValidIntValue(fieldValues.remove(ALIGNED_WEEK_OF_YEAR))
          val dow: Int               = DAY_OF_WEEK.checkValidIntValue(fieldValues.remove(DAY_OF_WEEK))
          val hijrahDate: HijrahDate =
            date(y, 1, 1).plus(aw.toLong - 1, WEEKS).`with`(nextOrSame(DayOfWeek.of(dow)))
          if ((resolverStyle eq ResolverStyle.STRICT) && hijrahDate.get(YEAR) != y)
            throw new DateTimeException("Strict mode rejected date parsed to a different month")
          return hijrahDate
        }
      }
    }
    null
  }
}
