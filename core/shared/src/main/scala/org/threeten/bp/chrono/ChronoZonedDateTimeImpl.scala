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

import java.util.Objects

import org.threeten.bp.temporal.ChronoUnit.SECONDS
import org.threeten.bp.Instant
import org.threeten.bp.LocalDateTime
import org.threeten.bp.ZoneId
import org.threeten.bp.ZoneOffset
import org.threeten.bp.temporal.ChronoField
import org.threeten.bp.temporal.ChronoUnit
import org.threeten.bp.temporal.Temporal
import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.temporal.TemporalUnit
import org.threeten.bp.zone.ZoneOffsetTransition
import org.threeten.bp.zone.ZoneRules

private[chrono] object ChronoZonedDateTimeImpl {

  /**
   * Obtains an instance from a local date-time using the preferred offset if possible.
   *
   * @param localDateTime
   *   the local date-time, not null
   * @param zone
   *   the zone identifier, not null
   * @param preferredOffset
   *   the zone offset, null if no preference
   * @return
   *   the zoned date-time, not null
   */
  private[chrono] def ofBest[R <: ChronoLocalDate](
    localDateTime:   ChronoLocalDateTimeImpl[R],
    zone:            ZoneId,
    preferredOffset: ZoneOffset
  ): ChronoZonedDateTime[R] = {
    var _localDateTime = localDateTime
    Objects.requireNonNull(_localDateTime, "localDateTime")
    Objects.requireNonNull(zone, "zone")
    zone match {
      case offset: ZoneOffset => new ChronoZonedDateTimeImpl[R](_localDateTime, offset, zone)
      case _                  =>
        val rules: ZoneRules                         = zone.getRules
        val isoLDT: LocalDateTime                    = LocalDateTime.from(_localDateTime)
        val validOffsets: java.util.List[ZoneOffset] = rules.getValidOffsets(isoLDT)
        var offset: ZoneOffset                       = null
        if (validOffsets.size == 1)
          offset = validOffsets.get(0)
        else if (validOffsets.size == 0) {
          val trans: ZoneOffsetTransition = rules.getTransition(isoLDT)
          _localDateTime = _localDateTime.plusSeconds(trans.getDuration.getSeconds)
          offset = trans.getOffsetAfter
        } else if (preferredOffset != null && validOffsets.contains(preferredOffset))
          offset = preferredOffset
        else
          offset = validOffsets.get(0)
        Objects.requireNonNull(offset, "offset")
        new ChronoZonedDateTimeImpl[R](_localDateTime, offset, zone)
    }
  }

  /**
   * Obtains an instance from an instant using the specified time-zone.
   *
   * @param chrono
   *   the chronology, not null
   * @param instant
   *   the instant, not null
   * @param zone
   *   the zone identifier, not null
   * @return
   *   the zoned date-time, not null
   */
  private[chrono] def ofInstant[R <: ChronoLocalDate](
    chrono:  Chronology,
    instant: Instant,
    zone:    ZoneId
  ): ChronoZonedDateTimeImpl[R] = {
    val rules: ZoneRules                                                       = zone.getRules
    val offset: ZoneOffset                                                     = rules.getOffset(instant)
    Objects.requireNonNull(offset, "offset")
    val ldt: LocalDateTime                                                     =
      LocalDateTime.ofEpochSecond(instant.getEpochSecond, instant.getNano, offset)
    @SuppressWarnings(Array("unchecked")) val cldt: ChronoLocalDateTimeImpl[R] =
      chrono.localDateTime(ldt).asInstanceOf[ChronoLocalDateTimeImpl[R]]
    new ChronoZonedDateTimeImpl[R](cldt, offset, zone)
  }

}

/**
 * A date-time with a time-zone in the calendar neutral API.
 *
 * {@code ZoneChronoDateTime} is an immutable representation of a date-time with a time-zone. This
 * class stores all date and time fields, to a precision of nanoseconds, as well as a time-zone and
 * zone offset.
 *
 * The purpose of storing the time-zone is to distinguish the ambiguous case where the local
 * time-line overlaps, typically as a result of the end of daylight time. Information about the
 * local-time can be obtained using methods on the time-zone.
 *
 * <h3>Specification for implementors</h3> This class is immutable and thread-safe.
 *
 * @tparam D
 *   the date type
 * @constructor
 * @param dateTime
 *   the date-time, not null
 * @param offset
 *   the zone offset, not null
 * @param zone
 *   the zone ID, not null
 */
@SerialVersionUID(-5261813987200935591L)
final class ChronoZonedDateTimeImpl[D <: ChronoLocalDate] private (
  private val dateTime: ChronoLocalDateTimeImpl[D],
  private val offset:   ZoneOffset,
  private val zone:     ZoneId
) extends ChronoZonedDateTime[D]
    with Serializable {
  Objects.requireNonNull(dateTime, "dateTime")
  Objects.requireNonNull(offset, "offset")
  Objects.requireNonNull(zone, "zone")

  /**
   * Obtains an instance from an {@code Instant} .
   *
   * @param instant
   *   the instant to create the date-time from, not null
   * @param zone
   *   the time-zone to use, validated not null
   * @return
   *   the zoned date-time, validated not null
   */
  private def create(instant: Instant, zone: ZoneId): ChronoZonedDateTimeImpl[D] =
    ChronoZonedDateTimeImpl.ofInstant(toLocalDate.getChronology, instant, zone)

  def isSupported(unit: TemporalUnit): Boolean =
    if (unit.isInstanceOf[ChronoUnit]) unit.isDateBased || unit.isTimeBased
    else unit != null && unit.isSupportedBy(this)

  def getOffset: ZoneOffset = offset

  def withEarlierOffsetAtOverlap: ChronoZonedDateTime[D] = {
    val trans: ZoneOffsetTransition = getZone.getRules.getTransition(LocalDateTime.from(this))
    if (trans != null && trans.isOverlap) {
      val earlierOffset: ZoneOffset = trans.getOffsetBefore
      if (!(earlierOffset == offset))
        return new ChronoZonedDateTimeImpl[D](dateTime, earlierOffset, zone)
    }
    this
  }

  def withLaterOffsetAtOverlap: ChronoZonedDateTime[D] = {
    val trans: ZoneOffsetTransition = getZone.getRules.getTransition(LocalDateTime.from(this))
    if (trans != null) {
      val offset: ZoneOffset = trans.getOffsetAfter
      if (!(offset == getOffset))
        return new ChronoZonedDateTimeImpl[D](dateTime, offset, zone)
    }
    this
  }

  def toLocalDateTime: ChronoLocalDateTime[D] = dateTime

  def getZone: ZoneId = zone

  def withZoneSameLocal(zone: ZoneId): ChronoZonedDateTime[D] =
    ChronoZonedDateTimeImpl.ofBest(dateTime, zone, offset)

  def withZoneSameInstant(zone: ZoneId): ChronoZonedDateTime[D] = {
    Objects.requireNonNull(zone, "zone")
    if (this.zone == zone) this
    else create(dateTime.toInstant(offset), zone)
  }

  def isSupported(field: TemporalField): Boolean =
    field.isInstanceOf[ChronoField] || (field != null && field.isSupportedBy(this))

  def `with`(field: TemporalField, newValue: Long): ChronoZonedDateTime[D] =
    field match {
      case f: ChronoField =>
        import ChronoField._
        f match {
          case INSTANT_SECONDS => plus(newValue - toEpochSecond, SECONDS)
          case OFFSET_SECONDS  =>
            val offset: ZoneOffset = ZoneOffset.ofTotalSeconds(f.checkValidIntValue(newValue))
            create(dateTime.toInstant(offset), zone)
          case _               => ChronoZonedDateTimeImpl.ofBest(dateTime.`with`(field, newValue), zone, offset)
        }
      case _              =>
        toLocalDate.getChronology.ensureChronoZonedDateTime(field.adjustInto(this, newValue))
    }

  def plus(amountToAdd: Long, unit: TemporalUnit): ChronoZonedDateTime[D] =
    if (unit.isInstanceOf[ChronoUnit]) `with`(dateTime.plus(amountToAdd, unit))
    else toLocalDate.getChronology.ensureChronoZonedDateTime(unit.addTo(this, amountToAdd))

  def until(endExclusive: Temporal, unit: TemporalUnit): Long = {
    var end: ChronoZonedDateTime[D] =
      toLocalDate.getChronology.zonedDateTime(endExclusive).asInstanceOf[ChronoZonedDateTime[D]]
    if (unit.isInstanceOf[ChronoUnit]) {
      end = end.withZoneSameInstant(offset)
      dateTime.until(end.toLocalDateTime, unit)
    } else
      unit.between(this, end)
  }

  override def equals(obj: Any): Boolean =
    obj match {
      case other: ChronoZonedDateTime[_] => (this eq other) || (compareTo(other) == 0)
      case _                             => false
    }

  override def hashCode: Int =
    toLocalDateTime.hashCode ^ getOffset.hashCode ^ Integer.rotateLeft(getZone.hashCode, 3)

  override def toString: String = {
    var str: String = toLocalDateTime.toString + getOffset.toString
    if (getOffset ne getZone)
      str += s"[$getZone]"
    str
  }
}
