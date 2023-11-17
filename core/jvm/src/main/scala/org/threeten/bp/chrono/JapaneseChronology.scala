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
import java.util.{ Arrays, Calendar, Locale, Objects }
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

@SerialVersionUID(459996390165777884L)
object JapaneseChronology {
  private[chrono] val LOCALE: Locale = new Locale("ja", "JP", "JP")

  /** Singleton instance for Japanese chronology. */
  lazy val INSTANCE: JapaneseChronology = new JapaneseChronology

  /** Fallback language for the era names. */
  // private val FALLBACK_LANGUAGE: String = "en"

  /** Language that has the era names. */
  // private val TARGET_LANGUAGE: String = "ja"

  /** Narrow names for eras. */
//   private val ERA_NARROW_NAMES: java.util.Map[String, Array[String]] = {
//     val names = new java.util.HashMap[String, Array[String]]
//     names.put(FALLBACK_LANGUAGE, Array[String]("Unknown", "K", "M", "T", "S", "H"))
//     names.put(TARGET_LANGUAGE, Array[String]("Unknown", "K", "M", "T", "S", "H"))
//     names
//   }
//
//   /** Short names for eras. */
//   private val ERA_SHORT_NAMES: java.util.Map[String, Array[String]] = {
//     val names = new java.util.HashMap[String, Array[String]]
//     names.put(FALLBACK_LANGUAGE, Array[String]("Unknown", "K", "M", "T", "S", "H"))
//     names.put(TARGET_LANGUAGE,
//               Array[String]("Unknown", "\u6176", "\u660e", "\u5927", "\u662d", "\u5e73"))
//     names
//   }
//
//   /** Full names for eras. */
//   private val ERA_FULL_NAMES: java.util.Map[String, Array[String]] = {
//     val names = new java.util.HashMap[String, Array[String]]
//     names.put(FALLBACK_LANGUAGE,
//               Array[String]("Unknown", "Keio", "Meiji", "Taisho", "Showa", "Heisei"))
//     names.put(TARGET_LANGUAGE,
//               Array[String]("Unknown",
//                             "\u6176\u5fdc",
//                             "\u660e\u6cbb",
//                             "\u5927\u6b63",
//                             "\u662d\u548c",
//                             "\u5e73\u6210"))
//     names
//   }
}

/**
 * The Japanese Imperial calendar system.
 *
 * This chronology defines the rules of the Japanese Imperial calendar system. This calendar system
 * is primarily used in Japan. The Japanese Imperial calendar system is the same as the ISO calendar
 * system apart from the era-based year numbering.
 *
 * Japan introduced the Gregorian calendar starting with Meiji 6. Only Meiji and later eras are
 * supported; dates before Meiji 6, January 1 are not supported.
 *
 * The supported {@code ChronoField} instances are: <ul> <li>{@code DAY_OF_WEEK} <li>{@code
 * DAY_OF_MONTH} <li>{@code DAY_OF_YEAR} <li>{@code EPOCH_DAY} <li>{@code MONTH_OF_YEAR} <li>{@code
 * PROLEPTIC_MONTH} <li>{@code YEAR_OF_ERA} <li>{@code YEAR} <li>{@code ERA} </ul>
 *
 * <h3>Specification for implementors</h3> This class is immutable and thread-safe.
 *
 * @constructor
 *   Restricted constructor.
 */
@SerialVersionUID(459996390165777884L)
final class JapaneseChronology private () extends Chronology with Serializable {

  /**
   * Resolve singleton.
   *
   * @return
   *   the singleton instance, not null
   */
  private def readResolve: AnyRef = JapaneseChronology.INSTANCE

  /**
   * Gets the ID of the chronology - 'Japanese'.
   *
   * The ID uniquely identifies the {@code Chronology} . It can be used to lookup the {@code
   * Chronology} using {@link #of(String)} .
   *
   * @return
   *   the chronology ID - 'Japanese'
   * @see
   *   #getCalendarType()
   */
  def getId: String = "Japanese"

  /**
   * Gets the calendar type of the underlying calendar system - 'japanese'.
   *
   * The calendar type is an identifier defined by the <em>Unicode Locale Data Markup Language
   * (LDML)</em> specification. It can be used to lookup the {@code Chronology} using {@link
   * #of(String)}. It can also be used as part of a locale, accessible via {@link
   * Locale#getUnicodeLocaleType(String)} with the key 'ca'.
   *
   * @return
   *   the calendar system type - 'japanese'
   * @see
   *   #getId()
   */
  def getCalendarType: String = "japanese"

  override def date(era: Era, yearOfEra: Int, month: Int, dayOfMonth: Int): JapaneseDate =
    if (!era.isInstanceOf[JapaneseEra]) throw new ClassCastException("Era must be JapaneseEra")
    else JapaneseDate.of(era.asInstanceOf[JapaneseEra], yearOfEra, month, dayOfMonth)

  def date(prolepticYear: Int, month: Int, dayOfMonth: Int): JapaneseDate =
    new JapaneseDate(LocalDate.of(prolepticYear, month, dayOfMonth))

  /**
   * Obtains a local date in Japanese calendar system from the era, year-of-era and day-of-year
   * fields.
   *
   * The day-of-year in this factory is expressed relative to the start of the year-of-era. This
   * definition changes the normal meaning of day-of-year only in those years where the year-of-era
   * is reset to one due to a change in the era. For example: <pre> 6th Jan Showa 64 = day-of-year 6
   * 7th Jan Showa 64 = day-of-year 7 8th Jan Heisei 1 = day-of-year 1 9th Jan Heisei 1 =
   * day-of-year 2 </pre>
   *
   * @param era
   *   the Japanese era, not null
   * @param yearOfEra
   *   the year-of-era
   * @param dayOfYear
   *   the day-of-year
   * @return
   *   the Japanese local date, not null
   * @throws DateTimeException
   *   if unable to create the date
   * @throws ClassCastException
   *   if the {@code era} is not a {@code JapaneseEra}
   */
  override def dateYearDay(era: Era, yearOfEra: Int, dayOfYear: Int): JapaneseDate =
    if (!era.isInstanceOf[JapaneseEra]) throw new ClassCastException("Era must be JapaneseEra")
    else JapaneseDate.ofYearDay(era.asInstanceOf[JapaneseEra], yearOfEra, dayOfYear)

  /**
   * Obtains a local date in Japanese calendar system from the proleptic-year and day-of-year
   * fields.
   *
   * The day-of-year in this factory is expressed relative to the start of the proleptic year. The
   * Japanese proleptic year and day-of-year are the same as those in the ISO calendar system. They
   * are not reset when the era changes.
   *
   * @param prolepticYear
   *   the proleptic-year
   * @param dayOfYear
   *   the day-of-year
   * @return
   *   the Japanese local date, not null
   * @throws DateTimeException
   *   if unable to create the date
   */
  def dateYearDay(prolepticYear: Int, dayOfYear: Int): JapaneseDate = {
    val localDate: LocalDate = LocalDate.ofYearDay(prolepticYear, dayOfYear)
    date(prolepticYear, localDate.getMonthValue, localDate.getDayOfMonth)
  }

  def dateEpochDay(epochDay: Long): JapaneseDate = new JapaneseDate(LocalDate.ofEpochDay(epochDay))

  def date(temporal: TemporalAccessor): JapaneseDate =
    temporal match {
      case date: JapaneseDate => date
      case _                  => new JapaneseDate(LocalDate.from(temporal))
    }

  override def localDateTime(temporal: TemporalAccessor): ChronoLocalDateTime[JapaneseDate] =
    super.localDateTime(temporal).asInstanceOf[ChronoLocalDateTime[JapaneseDate]]

  override def zonedDateTime(temporal: TemporalAccessor): ChronoZonedDateTime[JapaneseDate] =
    super.zonedDateTime(temporal).asInstanceOf[ChronoZonedDateTime[JapaneseDate]]

  override def zonedDateTime(instant: Instant, zone: ZoneId): ChronoZonedDateTime[JapaneseDate] =
    super.zonedDateTime(instant, zone).asInstanceOf[ChronoZonedDateTime[JapaneseDate]]

  override def dateNow: JapaneseDate = super.dateNow.asInstanceOf[JapaneseDate]

  override def dateNow(zone: ZoneId): JapaneseDate = super.dateNow(zone).asInstanceOf[JapaneseDate]

  override def dateNow(clock: Clock): JapaneseDate = {
    Objects.requireNonNull(clock, "clock")
    super.dateNow(clock).asInstanceOf[JapaneseDate]
  }

  /**
   * Checks if the specified year is a leap year.
   *
   * Japanese calendar leap years occur exactly in line with ISO leap years. This method does not
   * validate the year passed in, and only has a well-defined result for years in the supported
   * range.
   *
   * @param prolepticYear
   *   the proleptic-year to check, not validated for range
   * @return
   *   true if the year is a leap year
   */
  def isLeapYear(prolepticYear: Long): Boolean = IsoChronology.INSTANCE.isLeapYear(prolepticYear)

  def prolepticYear(era: Era, yearOfEra: Int): Int = {
    if (!era.isInstanceOf[JapaneseEra])
      throw new ClassCastException("Era must be JapaneseEra")
    val jera: JapaneseEra = era.asInstanceOf[JapaneseEra]
    val isoYear: Int      = jera.startDate.getYear + yearOfEra - 1
    val range: ValueRange =
      ValueRange.of(1, jera.endDate.getYear.toLong - jera.startDate.getYear.toLong + 1)
    range.checkValidValue(yearOfEra.toLong, YEAR_OF_ERA)
    isoYear
  }

  /**
   * Returns the calendar system era object from the given numeric value.
   *
   * See the description of each Era for the numeric values of: {@link JapaneseEra#HEISEI} , {@link
   * JapaneseEra#SHOWA},{@link JapaneseEra#TAISHO}, {@link JapaneseEra#MEIJI} ), only Meiji and
   * later eras are supported.
   *
   * @param eraValue
   *   the era value
   * @return
   *   the Japanese {@code Era} for the given numeric era value
   * @throws DateTimeException
   *   if {@code eraValue} is invalid
   */
  def eraOf(eraValue: Int): JapaneseEra = JapaneseEra.of(eraValue)

  def eras: java.util.List[Era] = Arrays.asList[Era](JapaneseEra.values: _*)

  def range(field: ChronoField): ValueRange = {
    import ChronoField._
    field match {
      case DAY_OF_MONTH | DAY_OF_WEEK | MICRO_OF_DAY | MICRO_OF_SECOND | HOUR_OF_DAY |
          HOUR_OF_AMPM | MINUTE_OF_DAY | MINUTE_OF_HOUR | SECOND_OF_DAY | SECOND_OF_MINUTE |
          MILLI_OF_DAY | MILLI_OF_SECOND | NANO_OF_DAY | NANO_OF_SECOND | CLOCK_HOUR_OF_DAY |
          CLOCK_HOUR_OF_AMPM | EPOCH_DAY | PROLEPTIC_MONTH =>
        field.range
      case ERA           =>
        val eras: Array[JapaneseEra] = JapaneseEra.values
        ValueRange.of(eras(0).getValue.toLong, eras(eras.length - 1).getValue.toLong)
      case YEAR          =>
        val eras: Array[JapaneseEra] = JapaneseEra.values
        ValueRange.of(JapaneseDate.MIN_DATE.getYear.toLong,
                      eras(eras.length - 1).endDate.getYear.toLong
        )
      case YEAR_OF_ERA   =>
        val eras: Array[JapaneseEra] = JapaneseEra.values
        val maxIso: Int              = eras(eras.length - 1).endDate.getYear
        val maxJapanese: Int         = maxIso - eras(eras.length - 1).startDate.getYear + 1
        var min: Int                 = Int.MaxValue

        var i: Int = 0
        while (i < eras.length) {
          min = Math.min(min, eras(i).endDate.getYear - eras(i).startDate.getYear + 1)
          i += 1
        }
        ValueRange.of(1, 6, min.toLong, maxJapanese.toLong)
      case MONTH_OF_YEAR =>
        val jcal: Calendar = Calendar.getInstance(JapaneseChronology.LOCALE)
        ValueRange.of(
          jcal.getMinimum(Calendar.MONTH) + 1L,
          jcal.getGreatestMinimum(Calendar.MONTH) + 1L,
          jcal.getLeastMaximum(Calendar.MONTH) + 1L,
          jcal.getMaximum(Calendar.MONTH) + 1L
        )
      case DAY_OF_YEAR   =>
        val eras: Array[JapaneseEra] = JapaneseEra.values
        var min: Int                 = 366

        var i: Int = 0
        while (i < eras.length) {
          min = Math.min(min, eras(i).startDate.lengthOfYear - eras(i).startDate.getDayOfYear + 1)
          i += 1
        }

        ValueRange.of(1, min.toLong, 366)
      case _             =>
        throw new UnsupportedOperationException(s"Unimplementable field: $field")
    }
  }

  override def resolveDate(
    fieldValues:   java.util.Map[TemporalField, java.lang.Long],
    resolverStyle: ResolverStyle
  ): JapaneseDate = {
    if (fieldValues.containsKey(EPOCH_DAY))
      return dateEpochDay(fieldValues.remove(EPOCH_DAY))
    val prolepticMonth: java.lang.Long = fieldValues.remove(PROLEPTIC_MONTH)
    if (prolepticMonth != null) {
      if (resolverStyle ne ResolverStyle.LENIENT)
        PROLEPTIC_MONTH.checkValidValue(prolepticMonth)
      updateResolveMap(fieldValues, MONTH_OF_YEAR, Math.floorMod(prolepticMonth, 12) + 1L)
      updateResolveMap(fieldValues, YEAR, Math.floorDiv(prolepticMonth, 12))
    }
    val eraLong: java.lang.Long        = fieldValues.get(ERA)
    var era: JapaneseEra               = null
    if (eraLong != null)
      era = eraOf(range(ERA).checkValidIntValue(eraLong, ERA))
    val yoeLong: java.lang.Long        = fieldValues.get(YEAR_OF_ERA)
    if (yoeLong != null) {
      val yoe: Int = range(YEAR_OF_ERA).checkValidIntValue(yoeLong, YEAR_OF_ERA)
      if (
        era == null && (resolverStyle ne ResolverStyle.STRICT) && !fieldValues.containsKey(YEAR)
      ) {
        val _eras: java.util.List[Era] = eras
        era = _eras.get(_eras.size - 1).asInstanceOf[JapaneseEra]
      }
      if (
        era != null && fieldValues.containsKey(MONTH_OF_YEAR) && fieldValues.containsKey(
          DAY_OF_MONTH
        )
      ) {
        fieldValues.remove(ERA)
        fieldValues.remove(YEAR_OF_ERA)
        return resolveEYMD(fieldValues, resolverStyle, era, yoe)
      }
      if (era != null && fieldValues.containsKey(DAY_OF_YEAR)) {
        fieldValues.remove(ERA)
        fieldValues.remove(YEAR_OF_ERA)
        return resolveEYD(fieldValues, resolverStyle, era, yoe)
      }
    }
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
            val y: Int                = YEAR.checkValidIntValue(fieldValues.remove(YEAR))
            if (resolverStyle eq ResolverStyle.LENIENT) {
              val months: Long = Math.subtractExact(fieldValues.remove(MONTH_OF_YEAR), 1)
              val weeks: Long  = Math.subtractExact(fieldValues.remove(ALIGNED_WEEK_OF_MONTH), 1)
              val days: Long   =
                Math.subtractExact(fieldValues.remove(ALIGNED_DAY_OF_WEEK_IN_MONTH), 1)
              return date(y, 1, 1).plus(months, MONTHS).plus(weeks, WEEKS).plus(days, DAYS)
            }
            val moy: Int              = MONTH_OF_YEAR.checkValidIntValue(fieldValues.remove(MONTH_OF_YEAR))
            val aw: Int               =
              ALIGNED_WEEK_OF_MONTH.checkValidIntValue(fieldValues.remove(ALIGNED_WEEK_OF_MONTH))
            val ad: Int               = ALIGNED_DAY_OF_WEEK_IN_MONTH.checkValidIntValue(
              fieldValues.remove(ALIGNED_DAY_OF_WEEK_IN_MONTH)
            )
            val japDate: JapaneseDate = date(y, moy, 1).plus((aw.toLong - 1) * 7 + (ad - 1), DAYS)
            if ((resolverStyle eq ResolverStyle.STRICT) && japDate.get(MONTH_OF_YEAR) != moy)
              throw new DateTimeException("Strict mode rejected date parsed to a different month")
            return japDate
          }
          if (fieldValues.containsKey(DAY_OF_WEEK)) {
            val y: Int                = YEAR.checkValidIntValue(fieldValues.remove(YEAR))
            if (resolverStyle eq ResolverStyle.LENIENT) {
              val months: Long = Math.subtractExact(fieldValues.remove(MONTH_OF_YEAR), 1)
              val weeks: Long  = Math.subtractExact(fieldValues.remove(ALIGNED_WEEK_OF_MONTH), 1)
              val days: Long   = Math.subtractExact(fieldValues.remove(DAY_OF_WEEK), 1)
              return date(y, 1, 1).plus(months, MONTHS).plus(weeks, WEEKS).plus(days, DAYS)
            }
            val moy: Int              = MONTH_OF_YEAR.checkValidIntValue(fieldValues.remove(MONTH_OF_YEAR))
            val aw: Int               =
              ALIGNED_WEEK_OF_MONTH.checkValidIntValue(fieldValues.remove(ALIGNED_WEEK_OF_MONTH))
            val dow: Int              = DAY_OF_WEEK.checkValidIntValue(fieldValues.remove(DAY_OF_WEEK))
            val japDate: JapaneseDate =
              date(y, moy, 1).plus(aw.toLong - 1, WEEKS).`with`(nextOrSame(DayOfWeek.of(dow)))
            if ((resolverStyle eq ResolverStyle.STRICT) && japDate.get(MONTH_OF_YEAR) != moy)
              throw new DateTimeException("Strict mode rejected date parsed to a different month")
            return japDate
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
          val y: Int                = YEAR.checkValidIntValue(fieldValues.remove(YEAR))
          if (resolverStyle eq ResolverStyle.LENIENT) {
            val weeks: Long = Math.subtractExact(fieldValues.remove(ALIGNED_WEEK_OF_YEAR), 1)
            val days: Long  = Math.subtractExact(fieldValues.remove(ALIGNED_DAY_OF_WEEK_IN_YEAR), 1)
            return date(y, 1, 1).plus(weeks, WEEKS).plus(days, DAYS)
          }
          val aw: Int               =
            ALIGNED_WEEK_OF_YEAR.checkValidIntValue(fieldValues.remove(ALIGNED_WEEK_OF_YEAR))
          val ad: Int               = ALIGNED_DAY_OF_WEEK_IN_YEAR.checkValidIntValue(
            fieldValues.remove(ALIGNED_DAY_OF_WEEK_IN_YEAR)
          )
          val japDate: JapaneseDate = date(y, 1, 1).plusDays((aw.toLong - 1) * 7 + (ad - 1))
          if ((resolverStyle eq ResolverStyle.STRICT) && japDate.get(YEAR) != y)
            throw new DateTimeException("Strict mode rejected date parsed to a different year")
          return japDate
        }
        if (fieldValues.containsKey(DAY_OF_WEEK)) {
          val y: Int                = YEAR.checkValidIntValue(fieldValues.remove(YEAR))
          if (resolverStyle eq ResolverStyle.LENIENT) {
            val weeks: Long = Math.subtractExact(fieldValues.remove(ALIGNED_WEEK_OF_YEAR), 1)
            val days: Long  = Math.subtractExact(fieldValues.remove(DAY_OF_WEEK), 1)
            return date(y, 1, 1).plus(weeks, WEEKS).plus(days, DAYS)
          }
          val aw: Int               =
            ALIGNED_WEEK_OF_YEAR.checkValidIntValue(fieldValues.remove(ALIGNED_WEEK_OF_YEAR))
          val dow: Int              = DAY_OF_WEEK.checkValidIntValue(fieldValues.remove(DAY_OF_WEEK))
          val japDate: JapaneseDate =
            date(y, 1, 1).plus(aw.toLong - 1, WEEKS).`with`(nextOrSame(DayOfWeek.of(dow)))
          if ((resolverStyle eq ResolverStyle.STRICT) && japDate.get(YEAR) != y)
            throw new DateTimeException("Strict mode rejected date parsed to a different month")
          return japDate
        }
      }
    }
    null
  }

  private def resolveEYMD(
    fieldValues:   java.util.Map[TemporalField, java.lang.Long],
    resolverStyle: ResolverStyle,
    era:           JapaneseEra,
    yoe:           Int
  ): JapaneseDate = {
    if (resolverStyle eq ResolverStyle.LENIENT) {
      val y: Int       = era.startDate.getYear + yoe - 1
      val months: Long = Math.subtractExact(fieldValues.remove(MONTH_OF_YEAR), 1)
      val days: Long   = Math.subtractExact(fieldValues.remove(DAY_OF_MONTH), 1)
      return date(y, 1, 1).plus(months, MONTHS).plus(days, DAYS)
    }
    val moy: Int =
      range(MONTH_OF_YEAR).checkValidIntValue(fieldValues.remove(MONTH_OF_YEAR), MONTH_OF_YEAR)
    var dom: Int =
      range(DAY_OF_MONTH).checkValidIntValue(fieldValues.remove(DAY_OF_MONTH), DAY_OF_MONTH)
    if (resolverStyle eq ResolverStyle.SMART) {
      if (yoe < 1)
        throw new DateTimeException(s"Invalid YearOfEra: $yoe")
      val y: Int           = era.startDate.getYear + yoe - 1
      if (dom > 28)
        dom = Math.min(dom, date(y, moy, 1).lengthOfMonth)
      val jd: JapaneseDate = date(y, moy, dom)
      if (jd.getEra ne era) {
        if (Math.abs(jd.getEra.getValue - era.getValue) > 1)
          throw new DateTimeException(s"Invalid Era/YearOfEra: $era $yoe")
        if (jd.get(YEAR_OF_ERA) != 1 && yoe != 1)
          throw new DateTimeException(s"Invalid Era/YearOfEra: $era $yoe")
      }
      return jd
    }
    date(era, yoe, moy, dom)
  }

  private def resolveEYD(
    fieldValues:   java.util.Map[TemporalField, java.lang.Long],
    resolverStyle: ResolverStyle,
    era:           JapaneseEra,
    yoe:           Int
  ): JapaneseDate = {
    if (resolverStyle eq ResolverStyle.LENIENT) {
      val y: Int     = era.startDate.getYear + yoe - 1
      val days: Long = Math.subtractExact(fieldValues.remove(DAY_OF_YEAR), 1)
      return dateYearDay(y, 1).plus(days, DAYS)
    }
    val doy: Int =
      range(DAY_OF_YEAR).checkValidIntValue(fieldValues.remove(DAY_OF_YEAR), DAY_OF_YEAR)
    dateYearDay(era, yoe, doy)
  }
}
