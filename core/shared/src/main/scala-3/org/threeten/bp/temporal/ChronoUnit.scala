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

import org.threeten.bp.Duration
import org.threeten.bp.chrono.ChronoLocalDate
import org.threeten.bp.chrono.ChronoLocalDateTime
import org.threeten.bp.chrono.ChronoZonedDateTime

/**
 * A standard set of date periods units.
 *
 * This set of units provide unit-based access to manipulate a date, time or date-time. The standard
 * set of units can be extended by implementing {@link TemporalUnit} .
 *
 * These units are intended to be applicable in multiple calendar systems. For example, most non-ISO
 * calendar systems define units of years, months and days, just with slightly different rules. The
 * documentation of each unit explains how it operates.
 *
 * <h3>Specification for implementors</h3> This is a final, immutable and thread-safe enum.
 */

/// !!! FIXME: Passing of name to the Enum constructor is not quite right.
//             We should have a look at the compiled code to figure out what's happening exactly in the Java version.
enum ChronoUnit private (name: String, ordinal: Int, private val duration: Duration)
    extends java.lang.Enum[ChronoUnit]
    with TemporalUnit {

  /**
   * Unit that represents the concept of a nanosecond, the smallest supported unit of time. For the
   * ISO calendar system, it is equal to the 1,000,000,000th part of the second unit.
   */
  case NANOS extends ChronoUnit("Nanos", 0, Duration.ofNanos(1))

  /**
   * Unit that represents the concept of a microsecond. For the ISO calendar system, it is equal to
   * the 1,000,000th part of the second unit.
   */
  case MICROS extends ChronoUnit("Micros", 1, Duration.ofNanos(1000))

  /**
   * Unit that represents the concept of a millisecond. For the ISO calendar system, it is equal to
   * the 1000th part of the second unit.
   */
  case MILLIS extends ChronoUnit("Millis", 2, Duration.ofNanos(1000000))

  /**
   * Unit that represents the concept of a second. For the ISO calendar system, it is equal to the
   * second in the SI system of units, except around a leap-second.
   */
  case SECONDS extends ChronoUnit("Seconds", 3, Duration.ofSeconds(1))

  /**
   * Unit that represents the concept of a minute. For the ISO calendar system, it is equal to 60
   * seconds.
   */
  case MINUTES extends ChronoUnit("Minutes", 4, Duration.ofSeconds(60))

  /**
   * Unit that represents the concept of an hour. For the ISO calendar system, it is equal to 60
   * minutes.
   */
  case HOURS extends ChronoUnit("Hours", 5, Duration.ofSeconds(3600))

  /**
   * Unit that represents the concept of half a day, as used in AM/PM. For the ISO calendar system,
   * it is equal to 12 hours.
   */
  case HALF_DAYS extends ChronoUnit("HalfDays", 6, Duration.ofSeconds(43200))

  /**
   * Unit that represents the concept of a day. For the ISO calendar system, it is the standard day
   * from midnight to midnight. The estimated duration of a day is {@code 24 Hours} .
   *
   * When used with other calendar systems it must correspond to the day defined by the rising and
   * setting of the Sun on Earth. It is not required that days begin at midnight - when converting
   * between calendar systems, the date should be equivalent at midday.
   */
  case DAYS extends ChronoUnit("Days", 7, Duration.ofSeconds(86400))

  /**
   * Unit that represents the concept of a week. For the ISO calendar system, it is equal to 7 days.
   *
   * When used with other calendar systems it must correspond to an integral number of days.
   */
  case WEEKS extends ChronoUnit("Weeks", 8, Duration.ofSeconds(7 * 86400L))

  /**
   * Unit that represents the concept of a month. For the ISO calendar system, the length of the
   * month varies by month-of-year. The estimated duration of a month is one twelfth of {@code
   * 365.2425 Days}.
   *
   * When used with other calendar systems it must correspond to an integral number of days.
   */
  case MONTHS extends ChronoUnit("Months", 9, Duration.ofSeconds(31556952L / 12))

  /**
   * Unit that represents the concept of a year. For the ISO calendar system, it is equal to 12
   * months. The estimated duration of a year is {@code 365.2425 Days} .
   *
   * When used with other calendar systems it must correspond to an integral number of days or
   * months roughly equal to a year defined by the passage of the Earth around the Sun.
   */
  case YEARS extends ChronoUnit("Years", 10, Duration.ofSeconds(31556952L))

  /**
   * Unit that represents the concept of a decade. For the ISO calendar system, it is equal to 10
   * years.
   *
   * When used with other calendar systems it must correspond to an integral number of days and is
   * normally an integral number of years.
   */
  case DECADES extends ChronoUnit("Decades", 11, Duration.ofSeconds(31556952L * 10L))

  /**
   * Unit that represents the concept of a century. For the ISO calendar system, it is equal to 100
   * years.
   *
   * When used with other calendar systems it must correspond to an integral number of days and is
   * normally an integral number of years.
   */
  case CENTURIES extends ChronoUnit("Centuries", 12, Duration.ofSeconds(31556952L * 100L))

  /**
   * Unit that represents the concept of a millennium. For the ISO calendar system, it is equal to
   * 1000 years.
   *
   * When used with other calendar systems it must correspond to an integral number of days and is
   * normally an integral number of years.
   */
  case MILLENNIA extends ChronoUnit("Millenia", 13, Duration.ofSeconds(31556952L * 1000L))

  /**
   * Unit that represents the concept of an era. The ISO calendar system doesn't have eras thus it
   * is impossible to add an era to a date or date-time. The estimated duration of the era is
   * artificially defined as {@code 1,000,000,000 Years} .
   *
   * When used with other calendar systems there are no restrictions on the unit.
   */
  case ERAS extends ChronoUnit("Eras", 14, Duration.ofSeconds(31556952L * 1000000000L))

  /**
   * Artificial unit that represents the concept of forever. This is primarily used with {@link
   * TemporalField} to represent unbounded fields such as the year or era. The estimated duration of
   * the era is artificially defined as the largest duration supported by {@code Duration} .
   */
  case FOREVER extends ChronoUnit("Forever", 15, Duration.ofSeconds(Long.MaxValue, 999999999))

  lazy val values: Array[ChronoUnit] = Array(NANOS,
                                             MICROS,
                                             MILLIS,
                                             SECONDS,
                                             MINUTES,
                                             HOURS,
                                             HALF_DAYS,
                                             DAYS,
                                             WEEKS,
                                             MONTHS,
                                             YEARS,
                                             DECADES,
                                             CENTURIES,
                                             MILLENNIA,
                                             ERAS,
                                             FOREVER
  )

  /**
   * Gets the estimated duration of this unit in the ISO calendar system.
   *
   * All of the units in this class have an estimated duration. Days vary due to daylight saving
   * time, while months have different lengths.
   *
   * @return
   *   the estimated duration of this unit, not null
   */
  def getDuration: Duration = duration

  /**
   * Checks if the duration of the unit is an estimate.
   *
   * All time units in this class are considered to be accurate, while all date units in this class
   * are considered to be estimated.
   *
   * This definition ignores leap seconds, but considers that Days vary due to daylight saving time
   * and months have different lengths.
   *
   * @return
   *   true if the duration is estimated, false if accurate
   */
  def isDurationEstimated: Boolean = isDateBased || (this eq ChronoUnit.FOREVER)

  /**
   * Checks if this unit is a date unit.
   *
   * @return
   *   true if a date unit, false if a time unit
   */
  def isDateBased: Boolean = this.compareTo(ChronoUnit.DAYS) >= 0 && (this ne ChronoUnit.FOREVER)

  /**
   * Checks if this unit is a time unit.
   *
   * @return
   *   true if a time unit, false if a date unit
   */
  def isTimeBased: Boolean = this.compareTo(ChronoUnit.DAYS) < 0

  def isSupportedBy(temporal: Temporal): Boolean = {
    if (this eq ChronoUnit.FOREVER)
      return false
    if (temporal.isInstanceOf[ChronoLocalDate])
      return isDateBased
    if (
      temporal.isInstanceOf[ChronoLocalDateTime[_]] || temporal
        .isInstanceOf[ChronoZonedDateTime[_]]
    )
      return true
    try {
      temporal.plus(1, this)
      true
    } catch {
      case _: RuntimeException =>
        try {
          temporal.plus(-1, this)
          true
        } catch {
          case _: RuntimeException => false
        }
    }
  }

  def addTo[R <: Temporal](dateTime: R, periodToAdd: Long): R =
    dateTime.plus(periodToAdd, this).asInstanceOf[R]

  def between(temporal1: Temporal, temporal2: Temporal): Long = temporal1.until(temporal2, this)

  override def toString: String = name
}
