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
package org.threeten.bp.zone

import java.io.Serializable
import java.util.{ Arrays, Collections, Objects }
import org.threeten.bp.Duration
import org.threeten.bp.Instant
import org.threeten.bp.LocalDateTime
import org.threeten.bp.ZoneOffset

/**
 * A transition between two offsets caused by a discontinuity in the local time-line.
 *
 * A transition between two offsets is normally the result of a daylight savings cutover. The
 * discontinuity is normally a gap in spring and an overlap in autumn. {@code ZoneOffsetTransition}
 * models the transition between the two offsets.
 *
 * Gaps occur where there are local date-times that simply do not not exist. An example would be
 * when the offset changes from {@code +03:00} to {@code +04:00} . This might be described as 'the
 * clocks will move forward one hour tonight at 1am'.
 *
 * Overlaps occur where there are local date-times that exist twice. An example would be when the
 * offset changes from {@code +04:00} to {@code +03:00} . This might be described as 'the clocks
 * will move back one hour tonight at 2am'.
 *
 * <h3>Specification for implementors</h3> This class is immutable and thread-safe.
 */
object ZoneOffsetTransition {

  /**
   * Obtains an instance defining a transition between two offsets.
   *
   * Applications should normally obtain an instance from {@link ZoneRules} . This factory is only
   * intended for use when creating {@link ZoneRules} .
   *
   * @param transition
   *   the transition date-time at the transition, which never actually occurs, expressed local to
   *   the before offset, not null
   * @param offsetBefore
   *   the offset before the transition, not null
   * @param offsetAfter
   *   the offset at and after the transition, not null
   * @return
   *   the transition, not null
   * @throws IllegalArgumentException
   *   if {@code offsetBefore} and {@code offsetAfter} are equal, or {@code transition.getNano()}
   *   returns non-zero value
   */
  def of(
    transition:   LocalDateTime,
    offsetBefore: ZoneOffset,
    offsetAfter:  ZoneOffset
  ): ZoneOffsetTransition = {
    Objects.requireNonNull(transition, "transition")
    Objects.requireNonNull(offsetBefore, "offsetBefore")
    Objects.requireNonNull(offsetAfter, "offsetAfter")
    if (offsetBefore == offsetAfter)
      throw new IllegalArgumentException("Offsets must not be equal")
    if (transition.getNano != 0)
      throw new IllegalArgumentException("Nano-of-second must be zero")
    new ZoneOffsetTransition(transition, offsetBefore, offsetAfter)
  }

}

/**
 * Creates an instance defining a transition between two offsets.
 *
 * @param transition
 *   the transition date-time with the offset before the transition, not null
 * @param offsetBefore
 *   the offset before the transition, not null
 * @param offsetAfter
 *   the offset at and after the transition, not null
 */
final class ZoneOffsetTransition private[zone] (
  private val transition:   LocalDateTime,
  private val offsetBefore: ZoneOffset,
  private val offsetAfter:  ZoneOffset
) extends Ordered[ZoneOffsetTransition]
    with Serializable {

  /**
   * Creates an instance from epoch-second and offsets.
   *
   * @param epochSecond
   *   the transition epoch-second
   * @param offsetBefore
   *   the offset before the transition, not null
   * @param offsetAfter
   *   the offset at and after the transition, not null
   */
  private[zone] def this(epochSecond: Long, offsetBefore: ZoneOffset, offsetAfter: ZoneOffset) =
    this(LocalDateTime.ofEpochSecond(epochSecond, 0, offsetBefore), offsetBefore, offsetAfter)

  /**
   * Gets the transition instant.
   *
   * This is the instant of the discontinuity, which is defined as the first instant that the
   * 'after' offset applies.
   *
   * The methods {@link #getInstant()} , {@link #getDateTimeBefore()} and
   * {@link #getDateTimeAfter()} all represent the same instant.
   *
   * @return
   *   the transition instant, not null
   */
  def getInstant: Instant = transition.toInstant(offsetBefore)

  /**
   * Gets the transition instant as an epoch second.
   *
   * @return
   *   the transition epoch second
   */
  def toEpochSecond: Long = transition.toEpochSecond(offsetBefore)

  /**
   * Gets the local transition date-time, as would be expressed with the 'before' offset.
   *
   * This is the date-time where the discontinuity begins expressed with the 'before' offset. At
   * this instant, the 'after' offset is actually used, therefore the combination of this date-time
   * and the 'before' offset will never occur.
   *
   * The combination of the 'before' date-time and offset represents the same instant as the 'after'
   * date-time and offset.
   *
   * @return
   *   the transition date-time expressed with the before offset, not null
   */
  def getDateTimeBefore: LocalDateTime = transition

  /**
   * Gets the local transition date-time, as would be expressed with the 'after' offset.
   *
   * This is the first date-time after the discontinuity, when the new offset applies.
   *
   * The combination of the 'before' date-time and offset represents the same instant as the 'after'
   * date-time and offset.
   *
   * @return
   *   the transition date-time expressed with the after offset, not null
   */
  def getDateTimeAfter: LocalDateTime = transition.plusSeconds(getDurationSeconds.toLong)

  /**
   * Gets the offset before the transition.
   *
   * This is the offset in use before the instant of the transition.
   *
   * @return
   *   the offset before the transition, not null
   */
  def getOffsetBefore: ZoneOffset = offsetBefore

  /**
   * Gets the offset after the transition.
   *
   * This is the offset in use on and after the instant of the transition.
   *
   * @return
   *   the offset after the transition, not null
   */
  def getOffsetAfter: ZoneOffset = offsetAfter

  /**
   * Gets the duration of the transition.
   *
   * In most cases, the transition duration is one hour, however this is not always the case. The
   * duration will be positive for a gap and negative for an overlap. Time-zones are second-based,
   * so the nanosecond part of the duration will be zero.
   *
   * @return
   *   the duration of the transition, positive for gaps, negative for overlaps
   */
  def getDuration: Duration = Duration.ofSeconds(getDurationSeconds.toLong)

  /**
   * Gets the duration of the transition in seconds.
   *
   * @return
   *   the duration in seconds
   */
  private def getDurationSeconds: Int =
    getOffsetAfter.getTotalSeconds - getOffsetBefore.getTotalSeconds

  /**
   * Does this transition represent a gap in the local time-line.
   *
   * Gaps occur where there are local date-times that simply do not not exist. An example would be
   * when the offset changes from {@code +01:00} to {@code +02:00} . This might be described as 'the
   * clocks will move forward one hour tonight at 1am'.
   *
   * @return
   *   true if this transition is a gap, false if it is an overlap
   */
  def isGap: Boolean = getOffsetAfter.getTotalSeconds > getOffsetBefore.getTotalSeconds

  /**
   * Does this transition represent a gap in the local time-line.
   *
   * Overlaps occur where there are local date-times that exist twice. An example would be when the
   * offset changes from {@code +02:00} to {@code +01:00} . This might be described as 'the clocks
   * will move back one hour tonight at 2am'.
   *
   * @return
   *   true if this transition is an overlap, false if it is a gap
   */
  def isOverlap: Boolean = getOffsetAfter.getTotalSeconds < getOffsetBefore.getTotalSeconds

  /**
   * Checks if the specified offset is valid during this transition.
   *
   * This checks to see if the given offset will be valid at some point in the transition. A gap
   * will always return false. An overlap will return true if the offset is either the before or
   * after offset.
   *
   * @param offset
   *   the offset to check, null returns false
   * @return
   *   true if the offset is valid during the transition
   */
  def isValidOffset(offset: ZoneOffset): Boolean =
    if (isGap) false else (getOffsetBefore == offset) || (getOffsetAfter == offset)

  /**
   * Gets the valid offsets during this transition.
   *
   * A gap will return an empty list, while an overlap will return both offsets.
   *
   * @return
   *   the list of valid offsets
   */
  private[zone] def getValidOffsets: java.util.List[ZoneOffset] =
    if (isGap)
      Collections.emptyList[ZoneOffset]
    else
      Arrays.asList(getOffsetBefore, getOffsetAfter)

  /**
   * Compares this transition to another based on the transition instant.
   *
   * This compares the instants of each transition. The offsets are ignored, making this order
   * inconsistent with equals.
   *
   * @param transition
   *   the transition to compare to, not null
   * @return
   *   the comparator value, negative if less, positive if greater
   */
  def compare(transition: ZoneOffsetTransition): Int =
    this.getInstant.compareTo(transition.getInstant)

  override def compareTo(other: ZoneOffsetTransition): Int = compare(other)

  /**
   * Checks if this object equals another.
   *
   * The entire state of the object is compared.
   *
   * @param other
   *   the other object to compare to, null returns false
   * @return
   *   true if equal
   */
  override def equals(other: Any): Boolean =
    other match {
      case zot: ZoneOffsetTransition =>
        (this eq zot) || ((transition == zot.transition) && (offsetBefore == zot.offsetBefore) && (offsetAfter == zot.offsetAfter))
      case _                         => false
    }

  /**
   * Returns a suitable hash code.
   *
   * @return
   *   the hash code
   */
  override def hashCode: Int =
    transition.hashCode ^ offsetBefore.hashCode ^ Integer.rotateLeft(offsetAfter.hashCode, 16)

  /**
   * Returns a string describing this object.
   *
   * @return
   *   a string for debugging, not null
   */
  override def toString: String = {
    val buf: StringBuilder = new StringBuilder
    buf
      .append("Transition[")
      .append(if (isGap) "Gap" else "Overlap")
      .append(" at ")
      .append(transition)
      .append(offsetBefore)
      .append(" to ")
      .append(offsetAfter)
      .append(']')
    buf.toString
  }
}
