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
package org.threeten.bp.temporal

import org.threeten.bp.temporal.ChronoUnit.DAYS
import org.threeten.bp.temporal.ChronoUnit.ERAS
import org.threeten.bp.temporal.ChronoUnit.FOREVER
import org.threeten.bp.temporal.ChronoUnit.HALF_DAYS
import org.threeten.bp.temporal.ChronoUnit.HOURS
import org.threeten.bp.temporal.ChronoUnit.MICROS
import org.threeten.bp.temporal.ChronoUnit.MILLIS
import org.threeten.bp.temporal.ChronoUnit.MINUTES
import org.threeten.bp.temporal.ChronoUnit.MONTHS
import org.threeten.bp.temporal.ChronoUnit.NANOS
import org.threeten.bp.temporal.ChronoUnit.SECONDS
import org.threeten.bp.temporal.ChronoUnit.WEEKS
import org.threeten.bp.temporal.ChronoUnit.YEARS
import java.util.{ Locale, Objects }
import org.threeten.bp.Year
import org.threeten.bp.format.ResolverStyle

/**
 * A standard set of fields.
 *
 * This set of fields provide field-based access to manipulate a date, time or date-time. The
 * standard set of fields can be extended by implementing {@link TemporalField}.
 *
 * These fields are intended to be applicable in multiple calendar systems. For example, most
 * non-ISO calendar systems define dates as a year, month and day, just with slightly different
 * rules. The documentation of each field explains how it operates.
 *
 * <h3>Specification for implementors</h3> This is a final, immutable and thread-safe enum.
 */
object ChronoField {

  /**
   * The nano-of-second.
   *
   * This counts the nanosecond within the second, from 0 to 999,999,999. This field has the same
   * meaning for all calendar systems.
   *
   * This field is used to represent the nano-of-second handling any fraction of the second.
   * Implementations of {@code TemporalAccessor} should provide a value for this field if they can
   * return a value for {@link #SECOND_OF_MINUTE}, {@link #SECOND_OF_DAY} or {@link
   * #INSTANT_SECONDS} filling unknown precision with zero.
   *
   * When this field is used for setting a value, it should set as much precision as the object
   * stores, using integer division to remove excess precision. For example, if the {@code
   * TemporalAccessor} stores time to millisecond precision, then the nano-of-second must be divided
   * by 1,000,000 before replacing the milli-of-second.
   */
  lazy val NANO_OF_SECOND =
    new ChronoField("NanoOfSecond", 0, NANOS, SECONDS, ValueRange.of(0, 999999999))

  /**
   * The nano-of-day.
   *
   * This counts the nanosecond within the day, from 0 to (24 * 60 * 60 * 1,000,000,000) - 1. This
   * field has the same meaning for all calendar systems.
   *
   * This field is used to represent the nano-of-day handling any fraction of the second.
   * Implementations of {@code TemporalAccessor} should provide a value for this field if they can
   * return a value for {@link #SECOND_OF_DAY} filling unknown precision with zero.
   */
  lazy val NANO_OF_DAY =
    new ChronoField("NanoOfDay", 1, NANOS, DAYS, ValueRange.of(0, 86400L * 1000000000L - 1))

  /**
   * The micro-of-second.
   *
   * This counts the microsecond within the second, from 0 to 999,999. This field has the same
   * meaning for all calendar systems.
   *
   * This field is used to represent the micro-of-second handling any fraction of the second.
   * Implementations of {@code TemporalAccessor} should provide a value for this field if they can
   * return a value for {@link #SECOND_OF_MINUTE}, {@link #SECOND_OF_DAY} or {@link
   * #INSTANT_SECONDS} filling unknown precision with zero.
   *
   * When this field is used for setting a value, it should behave in the same way as setting {@link
   * #NANO_OF_SECOND} with the value multiplied by 1,000.
   */
  lazy val MICRO_OF_SECOND =
    new ChronoField("MicroOfSecond", 2, MICROS, SECONDS, ValueRange.of(0, 999999))

  /**
   * The micro-of-day.
   *
   * This counts the microsecond within the day, from 0 to (24 * 60 * 60 * 1,000,000) - 1. This
   * field has the same meaning for all calendar systems.
   *
   * This field is used to represent the micro-of-day handling any fraction of the second.
   * Implementations of {@code TemporalAccessor} should provide a value for this field if they can
   * return a value for {@link #SECOND_OF_DAY} filling unknown precision with zero.
   *
   * When this field is used for setting a value, it should behave in the same way as setting {@link
   * #NANO_OF_DAY} with the value multiplied by 1,000.
   */
  lazy val MICRO_OF_DAY =
    new ChronoField("MicroOfDay", 3, MICROS, DAYS, ValueRange.of(0, 86400L * 1000000L - 1))

  /**
   * The milli-of-second.
   *
   * This counts the millisecond within the second, from 0 to 999. This field has the same meaning
   * for all calendar systems.
   *
   * This field is used to represent the milli-of-second handling any fraction of the second.
   * Implementations of {@code TemporalAccessor} should provide a value for this field if they can
   * return a value for {@link #SECOND_OF_MINUTE}, {@link #SECOND_OF_DAY} or {@link
   * #INSTANT_SECONDS} filling unknown precision with zero.
   *
   * When this field is used for setting a value, it should behave in the same way as setting {@link
   * #NANO_OF_SECOND} with the value multiplied by 1,000,000.
   */
  lazy val MILLI_OF_SECOND =
    new ChronoField("MilliOfSecond", 4, MILLIS, SECONDS, ValueRange.of(0, 999))

  /**
   * The milli-of-day.
   *
   * This counts the millisecond within the day, from 0 to (24 * 60 * 60 * 1,000) - 1. This field
   * has the same meaning for all calendar systems.
   *
   * This field is used to represent the milli-of-day handling any fraction of the second.
   * Implementations of {@code TemporalAccessor} should provide a value for this field if they can
   * return a value for {@link #SECOND_OF_DAY} filling unknown precision with zero.
   *
   * When this field is used for setting a value, it should behave in the same way as setting {@link
   * #NANO_OF_DAY} with the value multiplied by 1,000,000.
   */
  lazy val MILLI_OF_DAY =
    new ChronoField("MilliOfDay", 5, MILLIS, DAYS, ValueRange.of(0, 86400L * 1000L - 1))

  /**
   * The second-of-minute.
   *
   * This counts the second within the minute, from 0 to 59. This field has the same meaning for all
   * calendar systems.
   */
  lazy val SECOND_OF_MINUTE =
    new ChronoField("SecondOfMinute", 6, SECONDS, MINUTES, ValueRange.of(0, 59))

  /**
   * The second-of-day.
   *
   * This counts the second within the day, from 0 to (24 * 60 * 60) - 1. This field has the same
   * meaning for all calendar systems.
   */
  lazy val SECOND_OF_DAY =
    new ChronoField("SecondOfDay", 7, SECONDS, DAYS, ValueRange.of(0, 86400L - 1))

  /**
   * The minute-of-hour.
   *
   * This counts the minute within the hour, from 0 to 59. This field has the same meaning for all
   * calendar systems.
   */
  lazy val MINUTE_OF_HOUR = new ChronoField("MinuteOfHour", 8, MINUTES, HOURS, ValueRange.of(0, 59))

  /**
   * The minute-of-day.
   *
   * This counts the minute within the day, from 0 to (24 * 60) - 1. This field has the same meaning
   * for all calendar systems.
   */
  lazy val MINUTE_OF_DAY =
    new ChronoField("MinuteOfDay", 9, MINUTES, DAYS, ValueRange.of(0, (24 * 60) - 1))

  /**
   * The hour-of-am-pm.
   *
   * This counts the hour within the AM/PM, from 0 to 11. This is the hour that would be observed on
   * a standard 12-hour digital clock. This field has the same meaning for all calendar systems.
   */
  lazy val HOUR_OF_AMPM = new ChronoField("HourOfAmPm", 10, HOURS, HALF_DAYS, ValueRange.of(0, 11))

  /**
   * The clock-hour-of-am-pm.
   *
   * This counts the hour within the AM/PM, from 1 to 12. This is the hour that would be observed on
   * a standard 12-hour analog wall clock. This field has the same meaning for all calendar systems.
   */
  lazy val CLOCK_HOUR_OF_AMPM =
    new ChronoField("ClockHourOfAmPm", 11, HOURS, HALF_DAYS, ValueRange.of(1, 12))

  /**
   * The hour-of-day.
   *
   * This counts the hour within the day, from 0 to 23. This is the hour that would be observed on a
   * standard 24-hour digital clock. This field has the same meaning for all calendar systems.
   */
  lazy val HOUR_OF_DAY = new ChronoField("HourOfDay", 12, HOURS, DAYS, ValueRange.of(0, 23))

  /**
   * The clock-hour-of-day.
   *
   * This counts the hour within the AM/PM, from 1 to 24. This is the hour that would be observed on
   * a 24-hour analog wall clock. This field has the same meaning for all calendar systems.
   */
  lazy val CLOCK_HOUR_OF_DAY =
    new ChronoField("ClockHourOfDay", 13, HOURS, DAYS, ValueRange.of(1, 24))

  /**
   * The am-pm-of-day.
   *
   * This counts the AM/PM within the day, from 0 (AM) to 1 (PM). This field has the same meaning
   * for all calendar systems.
   */
  lazy val AMPM_OF_DAY = new ChronoField("AmPmOfDay", 14, HALF_DAYS, DAYS, ValueRange.of(0, 1))

  /**
   * The day-of-week, such as Tuesday.
   *
   * This represents the standard concept of the day of the week. In the default ISO calendar
   * system, this has values from Monday (1) to Sunday (7). The {@link DayOfWeek} class can be used
   * to interpret the result.
   *
   * Most non-ISO calendar systems also define a seven day week that aligns with ISO. Those calendar
   * systems must also use the same numbering system, from Monday (1) to Sunday (7), which allows
   * {@code DayOfWeek} to be used.
   *
   * Calendar systems that do not have a standard seven day week should implement this field if they
   * have a similar concept of named or numbered days within a period similar to a week. It is
   * recommended that the numbering starts from 1.
   */
  lazy val DAY_OF_WEEK = new ChronoField("DayOfWeek", 15, DAYS, WEEKS, ValueRange.of(1, 7))

  /**
   * The aligned day-of-week within a month.
   *
   * This represents concept of the count of days within the period of a week where the weeks are
   * aligned to the start of the month. This field is typically used with {@link
   * #ALIGNED_WEEK_OF_MONTH}.
   *
   * For example, in a calendar systems with a seven day week, the first aligned-week-of-month
   * starts on day-of-month 1, the second aligned-week starts on day-of-month 8, and so on. Within
   * each of these aligned-weeks, the days are numbered from 1 to 7 and returned as the value of
   * this field. As such, day-of-month 1 to 7 will have aligned-day-of-week values from 1 to 7. And
   * day-of-month 8 to 14 will repeat this with aligned-day-of-week values from 1 to 7.
   *
   * Calendar systems that do not have a seven day week should typically implement this field in the
   * same way, but using the alternate week length.
   */
  lazy val ALIGNED_DAY_OF_WEEK_IN_MONTH =
    new ChronoField("AlignedDayOfWeekInMonth", 16, DAYS, WEEKS, ValueRange.of(1, 7))

  /**
   * The aligned day-of-week within a year.
   *
   * This represents concept of the count of days within the period of a week where the weeks are
   * aligned to the start of the year. This field is typically used with {@link
   * #ALIGNED_WEEK_OF_YEAR}.
   *
   * For example, in a calendar systems with a seven day week, the first aligned-week-of-year starts
   * on day-of-year 1, the second aligned-week starts on day-of-year 8, and so on. Within each of
   * these aligned-weeks, the days are numbered from 1 to 7 and returned as the value of this field.
   * As such, day-of-year 1 to 7 will have aligned-day-of-week values from 1 to 7. And day-of-year 8
   * to 14 will repeat this with aligned-day-of-week values from 1 to 7.
   *
   * Calendar systems that do not have a seven day week should typically implement this field in the
   * same way, but using the alternate week length.
   */
  lazy val ALIGNED_DAY_OF_WEEK_IN_YEAR =
    new ChronoField("AlignedDayOfWeekInYear", 17, DAYS, WEEKS, ValueRange.of(1, 7))

  /**
   * The day-of-month.
   *
   * This represents the concept of the day within the month. In the default ISO calendar system,
   * this has values from 1 to 31 in most months. April, June, September, November have days from 1
   * to 30, while February has days from 1 to 28, or 29 in a leap year.
   *
   * Non-ISO calendar systems should implement this field using the most recognized day-of-month
   * values for users of the calendar system. Normally, this is a count of days from 1 to the length
   * of the month.
   */
  lazy val DAY_OF_MONTH = new ChronoField("DayOfMonth", 18, DAYS, MONTHS, ValueRange.of(1, 28, 31))

  /**
   * The day-of-year.
   *
   * This represents the concept of the day within the year. In the default ISO calendar system,
   * this has values from 1 to 365 in standard years and 1 to 366 in leap years.
   *
   * Non-ISO calendar systems should implement this field using the most recognized day-of-year
   * values for users of the calendar system. Normally, this is a count of days from 1 to the length
   * of the year.
   */
  lazy val DAY_OF_YEAR = new ChronoField("DayOfYear", 19, DAYS, YEARS, ValueRange.of(1, 365, 366))

  /**
   * The epoch-day, based on the Java epoch of 1970-01-01 (ISO).
   *
   * This field is the sequential count of days where 1970-01-01 (ISO) is zero. Note that this uses
   * the <i>local</i> time-line, ignoring offset and time-zone.
   *
   * This field is strictly defined to have the same meaning in all calendar systems. This is
   * necessary to ensure interoperability between calendars.
   */
  lazy val EPOCH_DAY =
    new ChronoField("EpochDay", 20, DAYS, FOREVER, ValueRange.of(-365243219162L, 365241780471L))

  /**
   * The aligned week within a month.
   *
   * This represents concept of the count of weeks within the period of a month where the weeks are
   * aligned to the start of the month. This field is typically used with {@link
   * #ALIGNED_DAY_OF_WEEK_IN_MONTH}.
   *
   * For example, in a calendar systems with a seven day week, the first aligned-week-of-month
   * starts on day-of-month 1, the second aligned-week starts on day-of-month 8, and so on. Thus,
   * day-of-month values 1 to 7 are in aligned-week 1, while day-of-month values 8 to 14 are in
   * aligned-week 2, and so on.
   *
   * Calendar systems that do not have a seven day week should typically implement this field in the
   * same way, but using the alternate week length.
   */
  lazy val ALIGNED_WEEK_OF_MONTH =
    new ChronoField("AlignedWeekOfMonth", 21, WEEKS, MONTHS, ValueRange.of(1, 4, 5))

  /**
   * The aligned week within a year.
   *
   * This represents concept of the count of weeks within the period of a year where the weeks are
   * aligned to the start of the year. This field is typically used with {@link
   * #ALIGNED_DAY_OF_WEEK_IN_YEAR}.
   *
   * For example, in a calendar systems with a seven day week, the first aligned-week-of-year starts
   * on day-of-year 1, the second aligned-week starts on day-of-year 8, and so on. Thus, day-of-year
   * values 1 to 7 are in aligned-week 1, while day-of-year values 8 to 14 are in aligned-week 2,
   * and so on.
   *
   * Calendar systems that do not have a seven day week should typically implement this field in the
   * same way, but using the alternate week length.
   */
  lazy val ALIGNED_WEEK_OF_YEAR =
    new ChronoField("AlignedWeekOfYear", 22, WEEKS, YEARS, ValueRange.of(1, 53))

  /**
   * The month-of-year, such as March.
   *
   * This represents the concept of the month within the year. In the default ISO calendar system,
   * this has values from January (1) to December (12).
   *
   * Non-ISO calendar systems should implement this field using the most recognized month-of-year
   * values for users of the calendar system. Normally, this is a count of months starting from 1.
   */
  lazy val MONTH_OF_YEAR = new ChronoField("MonthOfYear", 23, MONTHS, YEARS, ValueRange.of(1, 12))

  /**
   * The proleptic-month, which counts months sequentially from year 0.
   *
   * The first month in year zero has the value zero. The value increase for later months and
   * decrease for earlier ones. Note that this uses the <i>local</i> time-line, ignoring offset and
   * time-zone.
   *
   * This field is defined to have the same meaning in all calendar systems. It is simply a count of
   * months from whatever the calendar defines as year 0.
   */
  lazy val PROLEPTIC_MONTH = new ChronoField(
    "ProlepticMonth",
    24,
    MONTHS,
    FOREVER,
    ValueRange.of(Year.MIN_VALUE * 12L, Year.MAX_VALUE * 12L + 11)
  )

  /**
   * The year within the era.
   *
   * This represents the concept of the year within the era. This field is typically used with
   * {@link #ERA}.
   *
   * The standard mental model for a date is based on three concepts - year, month and day. These
   * map onto the {@code YEAR}, {@code MONTH_OF_YEAR} and {@code DAY_OF_MONTH} fields. Note that
   * there is no reference to eras. The full model for a date requires four concepts - era, year,
   * month and day. These map onto the {@code ERA}, {@code YEAR_OF_ERA}, {@code MONTH_OF_YEAR} and
   * {@code DAY_OF_MONTH} fields. Whether this field or {@code YEAR} is used depends on which mental
   * model is being used. See {@link ChronoLocalDate} for more discussion on this topic.
   *
   * In the default ISO calendar system, there are two eras defined, 'BCE' and 'CE'. The era 'CE' is
   * the one currently in use and year-of-era runs from 1 to the maximum value. The era 'BCE' is the
   * previous era, and the year-of-era runs backwards.
   *
   * For example, subtracting a year each time yield the following:<br>
   *   - year-proleptic 2 = 'CE' year-of-era 2<br>
   *   - year-proleptic 1 = 'CE' year-of-era 1<br>
   *   - year-proleptic 0 = 'BCE' year-of-era 1<br>
   *   - year-proleptic -1 = 'BCE' year-of-era 2<br>
   *
   * Note that the ISO-8601 standard does not actually define eras. Note also that the ISO eras do
   * not align with the well-known AD/BC eras due to the change between the Julian and Gregorian
   * calendar systems.
   *
   * Non-ISO calendar systems should implement this field using the most recognized year-of-era
   * value for users of the calendar system. Since most calendar systems have only two eras, the
   * year-of-era numbering approach will typically be the same as that used by the ISO calendar
   * system. The year-of-era value should typically always be positive, however this is not
   * required.
   */
  lazy val YEAR_OF_ERA = new ChronoField(
    "YearOfEra",
    25,
    YEARS,
    FOREVER,
    ValueRange.of(1, Year.MAX_VALUE.toLong, Year.MAX_VALUE.toLong + 1)
  )

  /**
   * The proleptic year, such as 2012.
   *
   * This represents the concept of the year, counting sequentially and using negative numbers. The
   * proleptic year is not interpreted in terms of the era. See {@link #YEAR_OF_ERA} for an example
   * showing the mapping from proleptic year to year-of-era.
   *
   * The standard mental model for a date is based on three concepts - year, month and day. These
   * map onto the {@code YEAR}, {@code MONTH_OF_YEAR} and {@code DAY_OF_MONTH} fields. Note that
   * there is no reference to eras. The full model for a date requires four concepts - era, year,
   * month and day. These map onto the {@code ERA}, {@code YEAR_OF_ERA}, {@code MONTH_OF_YEAR} and
   * {@code DAY_OF_MONTH} fields. Whether this field or {@code YEAR_OF_ERA} is used depends on which
   * mental model is being used. See {@link ChronoLocalDate} for more discussion on this topic.
   *
   * Non-ISO calendar systems should implement this field as follows. If the calendar system has
   * only two eras, before and after a fixed date, then the proleptic-year value must be the same as
   * the year-of-era value for the later era, and increasingly negative for the earlier era. If the
   * calendar system has more than two eras, then the proleptic-year value may be defined with any
   * appropriate value, although defining it to be the same as ISO may be the best option.
   */
  lazy val YEAR =
    new ChronoField("Year",
                    26,
                    YEARS,
                    FOREVER,
                    ValueRange.of(Year.MIN_VALUE.toLong, Year.MAX_VALUE.toLong)
    )

  /**
   * The era.
   *
   * This represents the concept of the era, which is the largest division of the time-line. This
   * field is typically used with {@link #YEAR_OF_ERA}.
   *
   * In the default ISO calendar system, there are two eras defined, 'BCE' and 'CE'. The era 'CE' is
   * the one currently in use and year-of-era runs from 1 to the maximum value. The era 'BCE' is the
   * previous era, and the year-of-era runs backwards. See {@link #YEAR_OF_ERA} for a full example.
   *
   * Non-ISO calendar systems should implement this field to define eras. The value of the era that
   * was active on 1970-01-01 (ISO) must be assigned the value 1. Earlier eras must have
   * sequentially smaller values. Later eras must have sequentially larger values,
   */
  lazy val ERA = new ChronoField("Era", 27, ERAS, FOREVER, ValueRange.of(0, 1))

  /**
   * The instant epoch-seconds.
   *
   * This represents the concept of the sequential count of seconds where 1970-01-01T00:00Z (ISO) is
   * zero. This field may be used with {@link #NANO_OF_DAY} to represent the fraction of the day.
   *
   * An {@link Instant} represents an instantaneous point on the time-line. On their own they have
   * no elements which allow a local date-time to be obtained. Only when paired with an offset or
   * time-zone can the local date or time be found. This field allows the seconds part of the
   * instant to be queried.
   *
   * This field is strictly defined to have the same meaning in all calendar systems. This is
   * necessary to ensure interoperation between calendars.
   */
  lazy val INSTANT_SECONDS = new ChronoField("InstantSeconds",
                                             28,
                                             SECONDS,
                                             FOREVER,
                                             ValueRange.of(Long.MinValue, Long.MaxValue)
  )

  /**
   * The offset from UTC/Greenwich.
   *
   * This represents the concept of the offset in seconds of local time from UTC/Greenwich.
   *
   * A {@link ZoneOffset} represents the period of time that local time differs from UTC/Greenwich.
   * This is usually a fixed number of hours and minutes. It is equivalent to the {@link
   * ZoneOffset#getTotalSeconds() total amount} of the offset in seconds. For example, during the
   * winter Paris has an offset of {@code +01:00}, which is 3600 seconds.
   *
   * This field is strictly defined to have the same meaning in all calendar systems. This is
   * necessary to ensure interoperation between calendars.
   */
  lazy val OFFSET_SECONDS =
    new ChronoField("OffsetSeconds", 29, SECONDS, FOREVER, ValueRange.of(-18 * 3600, 18 * 3600))

  lazy val values: Array[ChronoField] = Array(
    NANO_OF_SECOND,
    NANO_OF_DAY,
    MICRO_OF_SECOND,
    MICRO_OF_DAY,
    MILLI_OF_SECOND,
    MILLI_OF_DAY,
    SECOND_OF_MINUTE,
    SECOND_OF_DAY,
    MINUTE_OF_HOUR,
    MINUTE_OF_DAY,
    HOUR_OF_AMPM,
    CLOCK_HOUR_OF_AMPM,
    HOUR_OF_DAY,
    CLOCK_HOUR_OF_DAY,
    AMPM_OF_DAY,
    DAY_OF_WEEK,
    ALIGNED_DAY_OF_WEEK_IN_MONTH,
    ALIGNED_DAY_OF_WEEK_IN_YEAR,
    DAY_OF_MONTH,
    DAY_OF_YEAR,
    EPOCH_DAY,
    ALIGNED_WEEK_OF_MONTH,
    ALIGNED_WEEK_OF_YEAR,
    MONTH_OF_YEAR,
    PROLEPTIC_MONTH,
    YEAR_OF_ERA,
    YEAR,
    ERA,
    INSTANT_SECONDS,
    OFFSET_SECONDS
  )
}

/// !!! FIXME: Passing of name to the Enum constructor is not quite right.
//             We should have a look at the compiled code to figure out what's happening exactly in the Java version.
final class ChronoField private (
  name:                  String,
  ordinal:               Int,
  private val baseUnit:  TemporalUnit,
  private val rangeUnit: TemporalUnit,
  private val _range:    ValueRange
) extends Enum[ChronoField](name, ordinal)
    with TemporalField {

  /**
   * Gets the range of valid values for the field.
   *
   * All fields can be expressed as a {@code long} integer. This method returns an object that
   * describes the valid range for that value.
   *
   * This method returns the range of the field in the ISO-8601 calendar system. This range may be
   * incorrect for other calendar systems. Use {@link Chronology#range(ChronoField)} to access the
   * correct range for a different calendar system.
   *
   * Note that the result only describes the minimum and maximum valid values and it is important
   * not to read too much into them. For example, there could be values within the range that are
   * invalid for the field.
   *
   * @return
   *   the range of valid values for the field, not null
   */
  def range = _range

  def getBaseUnit: TemporalUnit = baseUnit

  def getRangeUnit: TemporalUnit = rangeUnit

  /**
   * Checks if this field represents a component of a date.
   *
   * @return
   *   true if it is a component of a date
   */
  def isDateBased: Boolean =
    ordinal >= ChronoField.DAY_OF_WEEK.ordinal && ordinal <= ChronoField.ERA.ordinal

  /**
   * Checks if this field represents a component of a time.
   *
   * @return
   *   true if it is a component of a time
   */
  def isTimeBased: Boolean = ordinal < ChronoField.DAY_OF_WEEK.ordinal

  /**
   * Checks that the specified value is valid for this field.
   *
   * This validates that the value is within the outer range of valid values returned by {@link
   * #range()}.
   *
   * This method checks against the range of the field in the ISO-8601 calendar system. This range
   * may be incorrect for other calendar systems. Use {@link Chronology#range(ChronoField)} to
   * access the correct range for a different calendar system.
   *
   * @param value
   *   the value to check
   * @return
   *   the value that was passed in
   */
  def checkValidValue(value: Long): Long = _range.checkValidValue(value, this)

  /**
   * Checks that the specified value is valid and fits in an {@code int}.
   *
   * This validates that the value is within the outer range of valid values returned by {@link
   * #range()}. It also checks that all valid values are within the bounds of an {@code int}.
   *
   * This method checks against the range of the field in the ISO-8601 calendar system. This range
   * may be incorrect for other calendar systems. Use {@link Chronology#range(ChronoField)} to
   * access the correct range for a different calendar system.
   *
   * @param value
   *   the value to check
   * @return
   *   the value that was passed in
   */
  def checkValidIntValue(value: Long): Int = _range.checkValidIntValue(value, this)

  def isSupportedBy(temporal: TemporalAccessor): Boolean = temporal.isSupported(this)

  def rangeRefinedBy(temporal: TemporalAccessor): ValueRange = temporal.range(this)

  def getFrom(temporal: TemporalAccessor): Long = temporal.getLong(this)

  def adjustInto[R <: Temporal](temporal: R, newValue: Long): R =
    temporal.`with`(this, newValue).asInstanceOf[R]

  def getDisplayName(locale: Locale): String = {
    Objects.requireNonNull(locale, "locale")
    toString
  }

  def resolve(
    fieldValues:     java.util.Map[TemporalField, java.lang.Long],
    partialTemporal: TemporalAccessor,
    resolverStyle:   ResolverStyle
  ): TemporalAccessor =
    null

  override def toString: String = name
}
