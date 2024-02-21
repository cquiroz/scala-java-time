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

import org.threeten.bp.temporal.ChronoField.EPOCH_DAY
import org.threeten.bp.temporal.ChronoField.NANO_OF_DAY
import org.threeten.bp.temporal.ChronoUnit.NANOS
import java.util.{ Comparator, Objects }
import org.threeten.bp.DateTimeException
import org.threeten.bp.Instant
import org.threeten.bp.LocalDate
import org.threeten.bp.LocalTime
import org.threeten.bp.ZoneId
import org.threeten.bp.ZoneOffset
import org.threeten.bp.format.DateTimeFormatter
import org.threeten.bp.temporal.Temporal
import org.threeten.bp.temporal.TemporalAccessor
import org.threeten.bp.temporal.TemporalAdjuster
import org.threeten.bp.temporal.TemporalAmount
import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.temporal.TemporalQueries
import org.threeten.bp.temporal.TemporalQuery
import org.threeten.bp.temporal.TemporalUnit

object ChronoLocalDateTime {

  /**
   * Gets a comparator that compares {@code ChronoLocalDateTime} in time-line order ignoring the
   * chronology.
   *
   * This comparator differs from the comparison in {@link #compareTo} in that it only compares the
   * underlying date-time and not the chronology. This allows dates in different calendar systems to
   * be compared based on the position of the date-time on the local time-line. The underlying
   * comparison is equivalent to comparing the epoch-day and nano-of-day.
   *
   * @return
   *   a comparator that compares in time-line order ignoring the chronology
   * @see
   *   #isAfter
   * @see
   *   #isBefore
   * @see
   *   #isEqual
   */
  def timeLineOrder: Comparator[ChronoLocalDateTime[_ <: ChronoLocalDate]] = DATE_TIME_COMPARATOR

  private lazy val DATE_TIME_COMPARATOR: Comparator[ChronoLocalDateTime[_ <: ChronoLocalDate]] =
    new Comparator[ChronoLocalDateTime[_ <: ChronoLocalDate]] {
      override def compare(
        datetime1: ChronoLocalDateTime[_ <: ChronoLocalDate],
        datetime2: ChronoLocalDateTime[_ <: ChronoLocalDate]
      ): Int = {
        var cmp: Int =
          java.lang.Long.compare(datetime1.toLocalDate.toEpochDay, datetime2.toLocalDate.toEpochDay)
        if (cmp == 0)
          cmp = java.lang.Long
            .compare(datetime1.toLocalTime.toNanoOfDay, datetime2.toLocalTime.toNanoOfDay)
        cmp
      }
    }

  /**
   * Obtains an instance of {@code ChronoLocalDateTime} from a temporal object.
   *
   * This obtains a local date-time based on the specified temporal. A {@code TemporalAccessor}
   * represents an arbitrary set of date and time information, which this factory converts to an
   * instance of {@code ChronoLocalDateTime} .
   *
   * The conversion extracts and combines the chronology and the date-time from the temporal object.
   * The behavior is equivalent to using {@link Chronology#localDateTime(TemporalAccessor)} with the
   * extracted chronology. Implementations are permitted to perform optimizations such as accessing
   * those fields that are equivalent to the relevant objects.
   *
   * This method matches the signature of the functional interface {@link TemporalQuery} allowing it
   * to be used as a query via method reference, {@code ChronoLocalDateTime::from} .
   *
   * @param temporal
   *   the temporal object to convert, not null
   * @return
   *   the date-time, not null
   * @throws DateTimeException
   *   if unable to convert to a {@code ChronoLocalDateTime}
   * @see
   *   Chronology#localDateTime(TemporalAccessor)
   */
  def from(temporal: TemporalAccessor): ChronoLocalDateTime[_] = {
    Objects.requireNonNull(temporal, "temporal")
    if (temporal.isInstanceOf[ChronoLocalDateTime[_ <: ChronoLocalDate]])
      return temporal.asInstanceOf[ChronoLocalDateTime[_]]
    val chrono: Chronology = temporal.query(TemporalQueries.chronology)
    if (chrono == null)
      throw new DateTimeException(
        s"No Chronology found to create ChronoLocalDateTime: ${temporal.getClass}"
      )
    chrono.localDateTime(temporal)
  }
}

abstract class ChronoLocalDateTime[D <: ChronoLocalDate]
    extends Temporal
    with TemporalAdjuster
    with Ordered[ChronoLocalDateTime[_]] {

  /**
   * Gets the chronology of this date-time.
   *
   * The {@code Chronology} represents the calendar system in use. The era and other fields in
   * {@link ChronoField} are defined by the chronology.
   *
   * @return
   *   the chronology, not null
   */
  def getChronology: Chronology = toLocalDate.getChronology

  /**
   * Gets the local date part of this date-time.
   *
   * This returns a local date with the same year, month and day as this date-time.
   *
   * @return
   *   the date part of this date-time, not null
   */
  def toLocalDate: D

  /**
   * Gets the local time part of this date-time.
   *
   * This returns a local time with the same hour, minute, second and nanosecond as this date-time.
   *
   * @return
   *   the time part of this date-time, not null
   */
  def toLocalTime: LocalTime

  override def `with`(adjuster: TemporalAdjuster): ChronoLocalDateTime[D] =
    toLocalDate.getChronology.ensureChronoLocalDateTime(super.`with`(adjuster))

  def `with`(field: TemporalField, newValue: Long): ChronoLocalDateTime[D]

  override def plus(amount: TemporalAmount): ChronoLocalDateTime[D] =
    toLocalDate.getChronology.ensureChronoLocalDateTime(super.plus(amount))

  def plus(amountToAdd: Long, unit: TemporalUnit): ChronoLocalDateTime[D]

  override def minus(amount: TemporalAmount): ChronoLocalDateTime[D] =
    toLocalDate.getChronology.ensureChronoLocalDateTime(super.minus(amount))

  override def minus(amountToSubtract: Long, unit: TemporalUnit): ChronoLocalDateTime[D] =
    toLocalDate.getChronology.ensureChronoLocalDateTime(super.minus(amountToSubtract, unit))

  override def query[R](query: TemporalQuery[R]): R =
    query match {
      case TemporalQueries.chronology                                             => getChronology.asInstanceOf[R]
      case TemporalQueries.precision                                              => NANOS.asInstanceOf[R]
      case TemporalQueries.localDate                                              => LocalDate.ofEpochDay(toLocalDate.toEpochDay).asInstanceOf[R]
      case TemporalQueries.localTime                                              => toLocalTime.asInstanceOf[R]
      case TemporalQueries.zone | TemporalQueries.zoneId | TemporalQueries.offset =>
        null.asInstanceOf[R]
      case _                                                                      => super.query(query)
    }

  def adjustInto(temporal: Temporal): Temporal =
    temporal.`with`(EPOCH_DAY, toLocalDate.toEpochDay).`with`(NANO_OF_DAY, toLocalTime.toNanoOfDay)

  /**
   * Formats this date-time using the specified formatter.
   *
   * This date-time will be passed to the formatter to produce a string.
   *
   * The default implementation must behave as follows: <pre> return formatter.format(this); </pre>
   *
   * @param formatter
   *   the formatter to use, not null
   * @return
   *   the formatted date-time string, not null
   * @throws DateTimeException
   *   if an error occurs during printing
   */
  def format(formatter: DateTimeFormatter): String = {
    Objects.requireNonNull(formatter, "formatter")
    formatter.format(this)
  }

  /**
   * Combines this time with a time-zone to create a {@code ChronoZonedDateTime} .
   *
   * This returns a {@code ChronoZonedDateTime} formed from this date-time at the specified
   * time-zone. The result will match this date-time as closely as possible. Time-zone rules, such
   * as daylight savings, mean that not every local date-time is valid for the specified zone, thus
   * the local date-time may be adjusted.
   *
   * The local date-time is resolved to a single instant on the time-line. This is achieved by
   * finding a valid offset from UTC/Greenwich for the local date-time as defined by the {@link
   * ZoneRules rules} of the zone ID. <p> In most cases, there is only one valid offset for a local
   * date-time. In the case of an overlap, where clocks are set back, there are two valid offsets.
   * This method uses the earlier offset typically corresponding to "summer".
   *
   * In the case of a gap, where clocks jump forward, there is no valid offset. Instead, the local
   * date-time is adjusted to be later by the length of the gap. For a typical one hour daylight
   * savings change, the local date-time will be moved one hour later into the offset typically
   * corresponding to "summer".
   *
   * To obtain the later offset during an overlap, call {@link
   * ChronoZonedDateTime#withLaterOffsetAtOverlap()} on the result of this method.
   *
   * @param zone
   *   the time-zone to use, not null
   * @return
   *   the zoned date-time formed from this date-time, not null
   */
  def atZone(zone: ZoneId): ChronoZonedDateTime[D]

  /**
   * Converts this date-time to an {@code Instant} .
   *
   * This combines this local date-time and the specified offset to form an {@code Instant} .
   *
   * @param offset
   *   the offset to use for the conversion, not null
   * @return
   *   an {@code Instant} representing the same instant, not null
   */
  def toInstant(offset: ZoneOffset): Instant =
    Instant.ofEpochSecond(toEpochSecond(offset), toLocalTime.getNano.toLong)

  /**
   * Converts this date-time to the number of seconds from the epoch of 1970-01-01T00:00:00Z.
   *
   * This combines this local date-time and the specified offset to calculate the epoch-second
   * value, which is the number of elapsed seconds from 1970-01-01T00:00:00Z. Instants on the
   * time-line after the epoch are positive, earlier are negative.
   *
   * @param offset
   *   the offset to use for the conversion, not null
   * @return
   *   the number of seconds from the epoch of 1970-01-01T00:00:00Z
   */
  def toEpochSecond(offset: ZoneOffset): Long = {
    Objects.requireNonNull(offset, "offset")
    val epochDay: Long = toLocalDate.toEpochDay
    var secs: Long     = epochDay * 86400 + toLocalTime.toSecondOfDay
    secs -= offset.getTotalSeconds
    secs
  }

  /**
   * Compares this date-time to another date-time, including the chronology.
   *
   * The comparison is based first on the underlying time-line date-time, then on the chronology. It
   * is "consistent with equals", as defined by {@link Comparable} .
   *
   * For example, the following is the comparator order: <ol> <li>{@code 2012-12-03T12:00
   * (ISO)}</li> <li>{@code 2012-12-04T12:00 (ISO)}</li> <li>{@code 2555-12-04T12:00
   * (ThaiBuddhist)}</li> <li>{@code 2012-12-05T12:00 (ISO)}</li> </ol> Values #2 and #3 represent
   * the same date-time on the time-line. When two values represent the same date-time, the
   * chronology ID is compared to distinguish them. This step is needed to make the ordering
   * "consistent with equals".
   *
   * If all the date-time objects being compared are in the same chronology, then the additional
   * chronology stage is not required and only the local date-time is used.
   *
   * @param other
   *   the other date-time to compare to, not null
   * @return
   *   the comparator value, negative if less, positive if greater
   */
  def compare(other: ChronoLocalDateTime[_]): Int = {
    var cmp: Int = toLocalDate.compareTo(other.toLocalDate.asInstanceOf[ChronoLocalDate])
    if (cmp == 0) {
      cmp = toLocalTime.compareTo(other.toLocalTime)
      if (cmp == 0)
        cmp = getChronology.compareTo(other.getChronology)
    }
    cmp
  }

  /**
   * Checks if this date-time is after the specified date-time ignoring the chronology.
   *
   * This method differs from the comparison in {@link #compareTo} in that it only compares the
   * underlying date-time and not the chronology. This allows dates in different calendar systems to
   * be compared based on the time-line position.
   *
   * @param other
   *   the other date-time to compare to, not null
   * @return
   *   true if this is after the specified date-time
   */
  def isAfter(other: ChronoLocalDateTime[_ <: ChronoLocalDate]): Boolean = {
    val thisEpDay: Long  = this.toLocalDate.toEpochDay
    val otherEpDay: Long = other.toLocalDate.toEpochDay
    thisEpDay > otherEpDay || (thisEpDay == otherEpDay && this.toLocalTime.toNanoOfDay > other.toLocalTime.toNanoOfDay)
  }

  /**
   * Checks if this date-time is before the specified date-time ignoring the chronology.
   *
   * This method differs from the comparison in {@link #compareTo} in that it only compares the
   * underlying date-time and not the chronology. This allows dates in different calendar systems to
   * be compared based on the time-line position.
   *
   * @param other
   *   the other date-time to compare to, not null
   * @return
   *   true if this is before the specified date-time
   */
  def isBefore(other: ChronoLocalDateTime[_ <: ChronoLocalDate]): Boolean = {
    val thisEpDay: Long  = this.toLocalDate.toEpochDay
    val otherEpDay: Long = other.toLocalDate.toEpochDay
    thisEpDay < otherEpDay || (thisEpDay == otherEpDay && this.toLocalTime.toNanoOfDay < other.toLocalTime.toNanoOfDay)
  }

  /**
   * Checks if this date-time is equal to the specified date-time ignoring the chronology.
   *
   * This method differs from the comparison in {@link #compareTo} in that it only compares the
   * underlying date and time and not the chronology. This allows date-times in different calendar
   * systems to be compared based on the time-line position.
   *
   * @param other
   *   the other date-time to compare to, not null
   * @return
   *   true if the underlying date-time is equal to the specified date-time on the timeline
   */
  def isEqual(other: ChronoLocalDateTime[_ <: ChronoLocalDate]): Boolean =
    this.toLocalTime.toNanoOfDay == other.toLocalTime.toNanoOfDay && this.toLocalDate.toEpochDay == other.toLocalDate.toEpochDay

  /**
   * Checks if this date-time is equal to another date-time, including the chronology.
   *
   * Compares this date-time with another ensuring that the date-time and chronology are the same.
   *
   * @param obj
   *   the object to check, null returns false
   * @return
   *   true if this is equal to the other date
   */
  override def equals(obj: Any): Boolean =
    obj match {
      case other: ChronoLocalDateTime[_] => (this eq other) || (compareTo(other) == 0)
      case _                             => false
    }

  /**
   * A hash code for this date-time.
   *
   * @return
   *   a suitable hash code
   */
  override def hashCode: Int = toLocalDate.hashCode ^ toLocalTime.hashCode

  /**
   * Outputs this date-time as a {@code String} .
   *
   * The output will include the full local date-time and the chronology ID.
   *
   * @return
   *   a string representation of this date-time, not null
   */
  override def toString: String = toLocalDate.toString + 'T' + toLocalTime.toString
}
