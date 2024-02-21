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

import org.threeten.bp.temporal.ChronoField.INSTANT_SECONDS
import org.threeten.bp.temporal.ChronoField.OFFSET_SECONDS
import org.threeten.bp.temporal.ChronoUnit.NANOS
import java.util.{ Comparator, Objects }
import org.threeten.bp.DateTimeException
import org.threeten.bp.Instant
import org.threeten.bp.LocalDate
import org.threeten.bp.LocalTime
import org.threeten.bp.ZoneId
import org.threeten.bp.ZoneOffset
import org.threeten.bp.format.DateTimeFormatter
import org.threeten.bp.temporal.ChronoField
import org.threeten.bp.temporal.Temporal
import org.threeten.bp.temporal.TemporalAccessor
import org.threeten.bp.temporal.TemporalAdjuster
import org.threeten.bp.temporal.TemporalAmount
import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.temporal.TemporalQueries
import org.threeten.bp.temporal.TemporalQuery
import org.threeten.bp.temporal.TemporalUnit
import org.threeten.bp.temporal.UnsupportedTemporalTypeException
import org.threeten.bp.temporal.ValueRange

object ChronoZonedDateTime {

  /**
   * Gets a comparator that compares {@code ChronoZonedDateTime} in time-line order ignoring the
   * chronology.
   *
   * This comparator differs from the comparison in {@link #compareTo} in that it only compares the
   * underlying instant and not the chronology. This allows dates in different calendar systems to
   * be compared based on the position of the date-time on the instant time-line. The underlying
   * comparison is equivalent to comparing the epoch-second and nano-of-second.
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
  def timeLineOrder: Comparator[ChronoZonedDateTime[_]] = INSTANT_COMPARATOR

  private lazy val INSTANT_COMPARATOR: Comparator[ChronoZonedDateTime[_]] =
    new Comparator[ChronoZonedDateTime[_]] {
      override def compare(
        datetime1: ChronoZonedDateTime[_],
        datetime2: ChronoZonedDateTime[_]
      ): Int = {
        var cmp: Int = java.lang.Long.compare(datetime1.toEpochSecond, datetime2.toEpochSecond)
        if (cmp == 0)
          cmp = java.lang.Long
            .compare(datetime1.toLocalTime.toNanoOfDay, datetime2.toLocalTime.toNanoOfDay)
        cmp
      }
    }

  /**
   * Obtains an instance of {@code ChronoZonedDateTime} from a temporal object.
   *
   * This creates a zoned date-time based on the specified temporal. A {@code TemporalAccessor}
   * represents an arbitrary set of date and time information, which this factory converts to an
   * instance of {@code ChronoZonedDateTime} .
   *
   * The conversion extracts and combines the chronology, date, time and zone from the temporal
   * object. The behavior is equivalent to using {@link Chronology#zonedDateTime(TemporalAccessor)}
   * with the extracted chronology. Implementations are permitted to perform optimizations such as
   * accessing those fields that are equivalent to the relevant objects.
   *
   * This method matches the signature of the functional interface {@link TemporalQuery} allowing it
   * to be used as a query via method reference, {@code ChronoZonedDateTime::from} .
   *
   * @param temporal
   *   the temporal object to convert, not null
   * @return
   *   the date-time, not null
   * @throws DateTimeException
   *   if unable to convert to a {@code ChronoZonedDateTime}
   * @see
   *   Chronology#zonedDateTime(TemporalAccessor)
   */
  def from(temporal: TemporalAccessor): ChronoZonedDateTime[_] = {
    Objects.requireNonNull(temporal, "temporal")
    if (temporal.isInstanceOf[ChronoZonedDateTime[_ <: ChronoLocalDate]])
      temporal.asInstanceOf[ChronoZonedDateTime[_]]
    else {
      val chrono: Chronology = temporal.query(TemporalQueries.chronology)
      if (chrono == null)
        throw new DateTimeException(
          s"No Chronology found to create ChronoZonedDateTime: ${temporal.getClass}"
        )
      else chrono.zonedDateTime(temporal)
    }
  }
}

/**
 * A date-time with a time-zone in an arbitrary chronology, intended for advanced globalization use
 * cases.
 *
 * <b>Most applications should declare method signatures, fields and variables as {@link
 * ZonedDateTime}, not this interface.</b>
 *
 * A {@code ChronoZonedDateTime} is the abstract representation of an offset date-time where the
 * {@code Chronology chronology} , or calendar system, is pluggable. The date-time is defined in
 * terms of fields expressed by {@link TemporalField} , where most common implementations are
 * defined in {@link ChronoField} . The chronology defines how the calendar system operates and the
 * meaning of the standard fields.
 *
 * <h4>When to use this interface</h4> The design of the API encourages the use of {@code
 * ZonedDateTime} rather than this interface, even in the case where the application needs to deal
 * with multiple calendar systems. The rationale for this is explored in detail in {@link
 * ChronoLocalDate}.
 *
 * Ensure that the discussion in {@code ChronoLocalDate} has been read and understood before using
 * this interface.
 *
 * <h3>Specification for implementors</h3> This interface must be implemented with care to ensure
 * other classes operate correctly. All implementations that can be instantiated must be final,
 * immutable and thread-safe. Subclasses should be Serializable wherever possible.
 *
 * @tparam D
 *   the date type
 */
trait ChronoZonedDateTime[D <: ChronoLocalDate]
    extends Temporal
    with Ordered[ChronoZonedDateTime[_]] {
  override def range(field: TemporalField): ValueRange =
    field match {
      case INSTANT_SECONDS | OFFSET_SECONDS => field.range
      case _: ChronoField                   => toLocalDateTime.range(field)
      case _                                => field.rangeRefinedBy(this)
    }

  override def get(field: TemporalField): Int =
    field match {
      case INSTANT_SECONDS =>
        throw new UnsupportedTemporalTypeException(s"Field too large for an int: $field")
      case OFFSET_SECONDS  => getOffset.getTotalSeconds
      case _: ChronoField  => toLocalDateTime.get(field)
      case _               => super.get(field)
    }

  def getLong(field: TemporalField): Long =
    field match {
      case INSTANT_SECONDS => toEpochSecond
      case OFFSET_SECONDS  => getOffset.getTotalSeconds.toLong
      case _: ChronoField  => toLocalDateTime.getLong(field)
      case _               => field.getFrom(this)
    }

  /**
   * Gets the local date part of this date-time.
   *
   * This returns a local date with the same year, month and day as this date-time.
   *
   * @return
   *   the date part of this date-time, not null
   */
  def toLocalDate: D = toLocalDateTime.toLocalDate

  /**
   * Gets the local time part of this date-time.
   *
   * This returns a local time with the same hour, minute, second and nanosecond as this date-time.
   *
   * @return
   *   the time part of this date-time, not null
   */
  def toLocalTime: LocalTime = toLocalDateTime.toLocalTime

  /**
   * Gets the local date-time part of this date-time.
   *
   * This returns a local date with the same year, month and day as this date-time.
   *
   * @return
   *   the local date-time part of this date-time, not null
   */
  def toLocalDateTime: ChronoLocalDateTime[D]

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
   * Gets the zone offset, such as '+01:00'.
   *
   * This is the offset of the local date-time from UTC/Greenwich.
   *
   * @return
   *   the zone offset, not null
   */
  def getOffset: ZoneOffset

  /**
   * Gets the zone ID, such as 'Europe/Paris'.
   *
   * This returns the stored time-zone id used to determine the time-zone rules.
   *
   * @return
   *   the zone ID, not null
   */
  def getZone: ZoneId

  /**
   * Returns a copy of this date-time changing the zone offset to the earlier of the two valid
   * offsets at a local time-line overlap.
   *
   * This method only has any effect when the local time-line overlaps, such as at an autumn
   * daylight savings cutover. In this scenario, there are two valid offsets for the local
   * date-time. Calling this method will return a zoned date-time with the earlier of the two
   * selected.
   *
   * If this method is called when it is not an overlap, {@code this} is returned.
   *
   * This instance is immutable and unaffected by this method call.
   *
   * @return
   *   a {@code ZoneChronoDateTime} based on this date-time with the earlier offset, not null
   * @throws DateTimeException
   *   if no rules can be found for the zone
   * @throws DateTimeException
   *   if no rules are valid for this date-time
   */
  def withEarlierOffsetAtOverlap: ChronoZonedDateTime[D]

  /**
   * Returns a copy of this date-time changing the zone offset to the later of the two valid offsets
   * at a local time-line overlap.
   *
   * This method only has any effect when the local time-line overlaps, such as at an autumn
   * daylight savings cutover. In this scenario, there are two valid offsets for the local
   * date-time. Calling this method will return a zoned date-time with the later of the two
   * selected.
   *
   * If this method is called when it is not an overlap, {@code this} is returned.
   *
   * This instance is immutable and unaffected by this method call.
   *
   * @return
   *   a {@code ChronoZonedDateTime} based on this date-time with the later offset, not null
   * @throws DateTimeException
   *   if no rules can be found for the zone
   * @throws DateTimeException
   *   if no rules are valid for this date-time
   */
  def withLaterOffsetAtOverlap: ChronoZonedDateTime[D]

  /**
   * Returns a copy of this ZonedDateTime with a different time-zone, retaining the local date-time
   * if possible.
   *
   * This method changes the time-zone and retains the local date-time. The local date-time is only
   * changed if it is invalid for the new zone.
   *
   * To change the zone and adjust the local date-time, use {@link #withZoneSameInstant(ZoneId)} .
   *
   * This instance is immutable and unaffected by this method call.
   *
   * @param zoneId
   *   the time-zone to change to, not null
   * @return
   *   a {@code ChronoZonedDateTime} based on this date-time with the requested zone, not null
   */
  def withZoneSameLocal(zoneId: ZoneId): ChronoZonedDateTime[D]

  /**
   * Returns a copy of this date-time with a different time-zone, retaining the instant.
   *
   * This method changes the time-zone and retains the instant. This normally results in a change to
   * the local date-time.
   *
   * This method is based on retaining the same instant, thus gaps and overlaps in the local
   * time-line have no effect on the result.
   *
   * To change the offset while keeping the local time, use {@link #withZoneSameLocal(ZoneId)} .
   *
   * @param zoneId
   *   the time-zone to change to, not null
   * @return
   *   a {@code ChronoZonedDateTime} based on this date-time with the requested zone, not null
   * @throws DateTimeException
   *   if the result exceeds the supported date range
   */
  def withZoneSameInstant(zoneId: ZoneId): ChronoZonedDateTime[D]

  override def `with`(adjuster: TemporalAdjuster): ChronoZonedDateTime[D] =
    toLocalDate.getChronology.ensureChronoZonedDateTime(super.`with`(adjuster))

  def `with`(field: TemporalField, newValue: Long): ChronoZonedDateTime[D]

  override def plus(amount: TemporalAmount): ChronoZonedDateTime[D] =
    toLocalDate.getChronology.ensureChronoZonedDateTime(super.plus(amount))

  def plus(amountToAdd: Long, unit: TemporalUnit): ChronoZonedDateTime[D]

  override def minus(amount: TemporalAmount): ChronoZonedDateTime[D] =
    toLocalDate.getChronology.ensureChronoZonedDateTime(super.minus(amount))

  override def minus(amountToSubtract: Long, unit: TemporalUnit): ChronoZonedDateTime[D] =
    toLocalDate.getChronology.ensureChronoZonedDateTime(super.minus(amountToSubtract, unit))

  override def query[R](query: TemporalQuery[R]): R =
    query match {
      case TemporalQueries.zoneId | TemporalQueries.zone => getZone.asInstanceOf[R]
      case TemporalQueries.chronology                    => toLocalDate.getChronology.asInstanceOf[R]
      case TemporalQueries.precision                     => NANOS.asInstanceOf[R]
      case TemporalQueries.offset                        => getOffset.asInstanceOf[R]
      case TemporalQueries.localDate                     => LocalDate.ofEpochDay(toLocalDate.toEpochDay).asInstanceOf[R]
      case TemporalQueries.localTime                     => toLocalTime.asInstanceOf[R]
      case _                                             => super.query(query)
    }

  /**
   * Outputs this date-time as a {@code String} using the formatter.
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
   * Converts this date-time to an {@code Instant} .
   *
   * This returns an {@code Instant} representing the same point on the time-line as this date-time.
   * The calculation combines the {@linkplain #toLocalDateTime() local date-time} and {@linkplain
   * #getOffset() offset}.
   *
   * @return
   *   an {@code Instant} representing the same instant, not null
   */
  def toInstant: Instant = Instant.ofEpochSecond(toEpochSecond, toLocalTime.getNano.toLong)

  /**
   * Converts this date-time to the number of seconds from the epoch of 1970-01-01T00:00:00Z.
   *
   * This uses the {@linkplain #toLocalDateTime() local date-time} and {@linkplain #getOffset()
   * offset} to calculate the epoch-second value, which is the number of elapsed seconds from
   * 1970-01-01T00:00:00Z. Instants on the time-line after the epoch are positive, earlier are
   * negative.
   *
   * @return
   *   the number of seconds from the epoch of 1970-01-01T00:00:00Z
   */
  def toEpochSecond: Long = {
    val epochDay: Long = toLocalDate.toEpochDay
    var secs: Long     = epochDay * 86400 + toLocalTime.toSecondOfDay
    secs -= getOffset.getTotalSeconds
    secs
  }

  /**
   * Compares this date-time to another date-time, including the chronology.
   *
   * The comparison is based first on the instant, then on the local date-time, then on the zone ID,
   * then on the chronology. It is "consistent with equals", as defined by {@link Comparable} .
   *
   * If all the date-time objects being compared are in the same chronology, then the additional
   * chronology stage is not required.
   *
   * @param other
   *   the other date-time to compare to, not null
   * @return
   *   the comparator value, negative if less, positive if greater
   */
  def compare(other: ChronoZonedDateTime[_]): Int = {
    var cmp: Int = java.lang.Long.compare(toEpochSecond, other.toEpochSecond)
    if (cmp == 0) {
      cmp = toLocalTime.getNano - other.toLocalTime.getNano
      if (cmp == 0) {
        cmp = toLocalDateTime.compareTo(other.toLocalDateTime)
        if (cmp == 0) {
          cmp = getZone.getId.compareTo(other.getZone.getId)
          if (cmp == 0)
            cmp = toLocalDate.getChronology.compareTo(
              other.toLocalDate.asInstanceOf[ChronoLocalDate].getChronology
            )
        }
      }
    }
    cmp
  }

  /**
   * Checks if the instant of this date-time is after that of the specified date-time.
   *
   * This method differs from the comparison in {@link #compareTo} in that it only compares the
   * instant of the date-time. This is equivalent to using {@code
   * dateTime1.toInstant().isAfter(dateTime2.toInstant());}.
   *
   * @param other
   *   the other date-time to compare to, not null
   * @return
   *   true if this is after the specified date-time
   */
  def isAfter(other: ChronoZonedDateTime[_]): Boolean = {
    val thisEpochSec: Long  = toEpochSecond
    val otherEpochSec: Long = other.toEpochSecond
    thisEpochSec > otherEpochSec || (thisEpochSec == otherEpochSec && toLocalTime.getNano > other.toLocalTime.getNano)
  }

  /**
   * Checks if the instant of this date-time is before that of the specified date-time.
   *
   * This method differs from the comparison in {@link #compareTo} in that it only compares the
   * instant of the date-time. This is equivalent to using {@code
   * dateTime1.toInstant().isBefore(dateTime2.toInstant());}.
   *
   * @param other
   *   the other date-time to compare to, not null
   * @return
   *   true if this point is before the specified date-time
   */
  def isBefore(other: ChronoZonedDateTime[_]): Boolean = {
    val thisEpochSec: Long  = toEpochSecond
    val otherEpochSec: Long = other.toEpochSecond
    thisEpochSec < otherEpochSec || (thisEpochSec == otherEpochSec && toLocalTime.getNano < other.toLocalTime.getNano)
  }

  /**
   * Checks if the instant of this date-time is equal to that of the specified date-time.
   *
   * This method differs from the comparison in {@link #compareTo} and {@link #equals} in that it
   * only compares the instant of the date-time. This is equivalent to using {@code
   * dateTime1.toInstant().equals(dateTime2.toInstant());}.
   *
   * @param other
   *   the other date-time to compare to, not null
   * @return
   *   true if the instant equals the instant of the specified date-time
   */
  def isEqual(other: ChronoZonedDateTime[_]): Boolean =
    toEpochSecond == other.toEpochSecond && toLocalTime.getNano == other.toLocalTime.getNano

  /**
   * Checks if this date-time is equal to another date-time.
   *
   * The comparison is based on the offset date-time and the zone. To compare for the same instant
   * on the time-line, use {@link #compareTo} . Only objects of type {@code ChronoZoneDateTime} are
   * compared, other types return false.
   *
   * @param obj
   *   the object to check, null returns false
   * @return
   *   true if this is equal to the other date-time
   */
  override def equals(obj: Any): Boolean =
    obj match {
      case other: ChronoZonedDateTime[_] => (this eq other) || (compareTo(other) == 0)
      case _                             => false
    }

  /**
   * A hash code for this date-time.
   *
   * @return
   *   a suitable hash code
   */
  override def hashCode: Int =
    toLocalDateTime.hashCode ^ getOffset.hashCode ^ Integer.rotateLeft(getZone.hashCode, 3)

  /**
   * Outputs this date-time as a {@code String} .
   *
   * The output will include the full zoned date-time and the chronology ID.
   *
   * @return
   *   a string representation of this date-time, not null
   */
  override def toString: String = {
    var str: String = toLocalDateTime.toString + getOffset.toString
    if (getOffset ne getZone)
      str += s"[$getZone]"
    str
  }
}
