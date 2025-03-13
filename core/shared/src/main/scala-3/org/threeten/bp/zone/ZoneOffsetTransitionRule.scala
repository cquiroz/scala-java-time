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

import java.util.Objects
import org.threeten.bp.temporal.TemporalAdjusters.nextOrSame
import org.threeten.bp.temporal.TemporalAdjusters.previousOrSame
import java.io.DataInput
import java.io.IOException
import java.io.Serializable
import org.threeten.bp.DayOfWeek
import org.threeten.bp.LocalDate
import org.threeten.bp.LocalDateTime
import org.threeten.bp.LocalTime
import org.threeten.bp.Month
import org.threeten.bp.ZoneOffset
import org.threeten.bp.chrono.IsoChronology

/**
 * A rule expressing how to create a transition.
 *
 * This class allows rules for identifying future transitions to be expressed. A rule might be
 * written in many forms: <ul> <li>the 16th March <li>the Sunday on or after the 16th March <li>the
 * Sunday on or before the 16th March <li>the last Sunday in February </ul><p> These different rule
 * types can be expressed and queried.
 *
 * <h3>Specification for implementors</h3> This class is immutable and thread-safe.
 */
object ZoneOffsetTransitionRule {

  /**
   * Obtains an instance defining the yearly rule to create transitions between two offsets.
   *
   * Applications should normally obtain an instance from {@link ZoneRules}. This factory is only
   * intended for use when creating {@link ZoneRules}.
   *
   * @param month
   *   the month of the month-day of the first day of the cutover week, not null
   * @param dayOfMonthIndicator
   *   the day of the month-day of the cutover week, positive if the week is that day or later,
   *   negative if the week is that day or earlier, counting from the last day of the month, from
   *   -28 to 31 excluding 0
   * @param dayOfWeek
   *   the required day-of-week, null if the month-day should not be changed
   * @param time
   *   the cutover time in the 'before' offset, not null
   * @param timeEndOfDay
   *   whether the time is midnight at the end of day
   * @param timeDefnition
   *   how to interpret the cutover
   * @param standardOffset
   *   the standard offset in force at the cutover, not null
   * @param offsetBefore
   *   the offset before the cutover, not null
   * @param offsetAfter
   *   the offset after the cutover, not null
   * @return
   *   the rule, not null
   * @throws IllegalArgumentException
   *   if the day of month indicator is invalid
   * @throws IllegalArgumentException
   *   if the end of day flag is true when the time is not midnight
   */
  def of(
    month:               Month,
    dayOfMonthIndicator: Int,
    dayOfWeek:           DayOfWeek,
    time:                LocalTime,
    timeEndOfDay:        Boolean,
    timeDefnition:       ZoneOffsetTransitionRule.TimeDefinition,
    standardOffset:      ZoneOffset,
    offsetBefore:        ZoneOffset,
    offsetAfter:         ZoneOffset
  ): ZoneOffsetTransitionRule = {
    Objects.requireNonNull(month, "month")
    Objects.requireNonNull(time, "time")
    Objects.requireNonNull(timeDefnition, "timeDefnition")
    Objects.requireNonNull(standardOffset, "standardOffset")
    Objects.requireNonNull(offsetBefore, "offsetBefore")
    Objects.requireNonNull(offsetAfter, "offsetAfter")
    if (dayOfMonthIndicator < -28 || dayOfMonthIndicator > 31 || dayOfMonthIndicator == 0)
      throw new IllegalArgumentException(
        "Day of month indicator must be between -28 and 31 inclusive excluding zero"
      )
    if (timeEndOfDay && !(time == LocalTime.MIDNIGHT))
      throw new IllegalArgumentException("Time must be midnight when end of day flag is true")
    new ZoneOffsetTransitionRule(month,
                                 dayOfMonthIndicator,
                                 dayOfWeek,
                                 time,
                                 timeEndOfDay,
                                 timeDefnition,
                                 standardOffset,
                                 offsetBefore,
                                 offsetAfter
    )
  }

  /**
   * Reads the state from the stream.
   *
   * @param in
   *   the input stream, not null
   * @return
   *   the created object, not null
   * @throws IOException
   *   if an error occurs
   */
  @throws[IOException]
  private[zone] def readExternal(in: DataInput): ZoneOffsetTransitionRule = {
    val data: Int                                     = in.readInt
    val month: Month                                  = Month.of(data >>> 28)
    val dom: Int                                      = ((data & (63 << 22)) >>> 22) - 32
    val dowByte: Int                                  = (data & (7 << 19)) >>> 19
    val dow: DayOfWeek                                = if (dowByte == 0) null else DayOfWeek.of(dowByte)
    val timeByte: Int                                 = (data & (31 << 14)) >>> 14
    val defn: ZoneOffsetTransitionRule.TimeDefinition =
      ZoneOffsetTransitionRule.TimeDefinition.values((data & (3 << 12)) >>> 12)
    val stdByte: Int                                  = (data & (255 << 4)) >>> 4
    val beforeByte: Int                               = (data & (3 << 2)) >>> 2
    val afterByte: Int                                = data & 3
    val time: LocalTime                               =
      if (timeByte == 31) LocalTime.ofSecondOfDay(in.readInt.toLong)
      else LocalTime.of(timeByte % 24, 0)
    val std: ZoneOffset                               =
      if (stdByte == 255) ZoneOffset.ofTotalSeconds(in.readInt)
      else ZoneOffset.ofTotalSeconds((stdByte - 128) * 900)
    val before: ZoneOffset                            =
      if (beforeByte == 3) ZoneOffset.ofTotalSeconds(in.readInt)
      else ZoneOffset.ofTotalSeconds(std.getTotalSeconds + beforeByte * 1800)
    val after: ZoneOffset                             =
      if (afterByte == 3) ZoneOffset.ofTotalSeconds(in.readInt)
      else ZoneOffset.ofTotalSeconds(std.getTotalSeconds + afterByte * 1800)
    ZoneOffsetTransitionRule.of(month, dom, dow, time, timeByte == 24, defn, std, before, after)
  }

  /**
   * A definition of the way a local time can be converted to the actual transition date-time.
   *
   * Time zone rules are expressed in one of three ways: <ul> <li>Relative to UTC</li> <li>Relative
   * to the standard offset in force</li> <li>Relative to the wall offset (what you would see on a
   * clock on the wall)</li> </ul><p>
   */
  enum TimeDefinition(name: String, ordinal: Int) extends java.lang.Enum[TimeDefinition] {

    /** The local date-time is expressed in terms of the UTC offset. */
    case UTC extends TimeDefinition("UTC", 0)

    /** The local date-time is expressed in terms of the wall offset. */
    case WALL extends TimeDefinition("WALL", 1)

    /** The local date-time is expressed in terms of the standard offset. */
    case STANDARD extends TimeDefinition("STANDARD", 2)

    /**
     * Converts the specified local date-time to the local date-time actually seen on a wall clock.
     *
     * This method converts using the type of this enum. The output is defined relative to the
     * 'before' offset of the transition.
     *
     * The UTC type uses the UTC offset. The STANDARD type uses the standard offset. The WALL type
     * returns the input date-time. The result is intended for use with the wall-offset.
     *
     * @param dateTime
     *   the local date-time, not null
     * @param standardOffset
     *   the standard offset, not null
     * @param wallOffset
     *   the wall offset, not null
     * @return
     *   the date-time relative to the wall/before offset, not null
     */
    def createDateTime(
      dateTime:       LocalDateTime,
      standardOffset: ZoneOffset,
      wallOffset:     ZoneOffset
    ): LocalDateTime =
      this match {
        case TimeDefinition.UTC      =>
          val difference: Int = wallOffset.getTotalSeconds - ZoneOffset.UTC.getTotalSeconds
          dateTime.plusSeconds(difference.toLong)
        case TimeDefinition.STANDARD =>
          val difference: Int = wallOffset.getTotalSeconds - standardOffset.getTotalSeconds
          dateTime.plusSeconds(difference.toLong)
        case _                       =>
          dateTime
      }
  }
}

/**
 * Creates an instance defining the yearly rule to create transitions between two offsets.
 *
 * @param month
 *   The month of the month-day of the first day of the cutover week, not null. The actual date will
 *   be adjusted by the dowChange field
 * @param dayOfMonthIndicator
 *   the day of the month-day of the cutover week, positive if the week is that day or later,
 *   negative if the week is that day or earlier, counting from the last day of the month, from -28
 *   to 31 excluding 0
 * @param dayOfWeek
 *   the required day-of-week, null if the month-day should not be changed
 * @param time
 *   the cutover time in the 'before' offset, not null
 * @param timeEndOfDay
 *   whether the time is midnight at the end of day
 * @param timeDefinition
 *   how to interpret the cutover
 * @param standardOffset
 *   the standard offset in force at the cutover, not null
 * @param offsetBefore
 *   the offset before the cutover, not null
 * @param offsetAfter
 *   the offset after the cutover, not null
 * @throws IllegalArgumentException
 *   if the day of month indicator is invalid
 * @throws IllegalArgumentException
 *   if the end of day flag is true when the time is not midnight
 */
@SerialVersionUID(6889046316657758795L)
final class ZoneOffsetTransitionRule private[zone] (
  private val month:          Month,
  dayOfMonthIndicator:        Int,
  private val dayOfWeek:      DayOfWeek,
  private val time:           LocalTime,
  private val timeEndOfDay:   Boolean,
  private val timeDefinition: ZoneOffsetTransitionRule.TimeDefinition,
  private val standardOffset: ZoneOffset,
  private val offsetBefore:   ZoneOffset,
  private val offsetAfter:    ZoneOffset
) extends Serializable {

  /**
   * The day-of-month of the month-day of the cutover week. If positive, it is the start of the week
   * where the cutover can occur. If negative, it represents the end of the week where cutover can
   * occur. The value is the number of days from the end of the month, such that {@code -1} is the
   * last day of the month, {@code -2} is the second to last day, and so on.
   */
  private val dom: Byte = dayOfMonthIndicator.toByte

  /**
   * Gets the month of the transition.
   *
   * If the rule defines an exact date then the month is the month of that date.
   *
   * If the rule defines a week where the transition might occur, then the month if the month of
   * either the earliest or latest possible date of the cutover.
   *
   * @return
   *   the month of the transition, not null
   */
  def getMonth: Month = month

  /**
   * Gets the indicator of the day-of-month of the transition.
   *
   * If the rule defines an exact date then the day is the month of that date.
   *
   * If the rule defines a week where the transition might occur, then the day defines either the
   * start of the end of the transition week.
   *
   * If the value is positive, then it represents a normal day-of-month, and is the earliest
   * possible date that the transition can be. The date may refer to 29th February which should be
   * treated as 1st March in non-leap years.
   *
   * If the value is negative, then it represents the number of days back from the end of the month
   * where {@code -1} is the last day of the month. In this case, the day identified is the latest
   * possible date that the transition can be.
   *
   * @return
   *   the day-of-month indicator, from -28 to 31 excluding 0
   */
  def getDayOfMonthIndicator: Int = dom.toInt

  /**
   * Gets the day-of-week of the transition.
   *
   * If the rule defines an exact date then this returns null.
   *
   * If the rule defines a week where the cutover might occur, then this method returns the
   * day-of-week that the month-day will be adjusted to. If the day is positive then the adjustment
   * is later. If the day is negative then the adjustment is earlier.
   *
   * @return
   *   the day-of-week that the transition occurs, null if the rule defines an exact date
   */
  def getDayOfWeek: DayOfWeek = dayOfWeek

  /**
   * Gets the local time of day of the transition which must be checked with {@link
   * #isMidnightEndOfDay()}.
   *
   * The time is converted into an instant using the time definition.
   *
   * @return
   *   the local time of day of the transition, not null
   */
  def getLocalTime: LocalTime = time

  /**
   * Is the transition local time midnight at the end of day.
   *
   * The transition may be represented as occurring at 24:00.
   *
   * @return
   *   whether a local time of midnight is at the start or end of the day
   */
  def isMidnightEndOfDay: Boolean = timeEndOfDay

  /**
   * Gets the time definition, specifying how to convert the time to an instant.
   *
   * The local time can be converted to an instant using the standard offset, the wall offset or
   * UTC.
   *
   * @return
   *   the time definition, not null
   */
  def getTimeDefinition: ZoneOffsetTransitionRule.TimeDefinition = timeDefinition

  /**
   * Gets the standard offset in force at the transition.
   *
   * @return
   *   the standard offset, not null
   */
  def getStandardOffset: ZoneOffset = standardOffset

  /**
   * Gets the offset before the transition.
   *
   * @return
   *   the offset before, not null
   */
  def getOffsetBefore: ZoneOffset = offsetBefore

  /**
   * Gets the offset after the transition.
   *
   * @return
   *   the offset after, not null
   */
  def getOffsetAfter: ZoneOffset = offsetAfter

  /**
   * Creates a transition instance for the specified year.
   *
   * Calculations are performed using the ISO-8601 chronology.
   *
   * @param year
   *   the year to create a transition for, not null
   * @return
   *   the transition instance, not null
   */
  def createTransition(year: Int): ZoneOffsetTransition = {
    var date: LocalDate           = null
    if (dom < 0) {
      date = LocalDate.of(year,
                          month,
                          month.length(IsoChronology.INSTANCE.isLeapYear(year.toLong)) + 1 + dom
      )
      if (dayOfWeek != null)
        date = date.`with`(previousOrSame(dayOfWeek))
    } else {
      date = LocalDate.of(year, month, dom.toInt)
      if (dayOfWeek != null)
        date = date.`with`(nextOrSame(dayOfWeek))
    }
    if (timeEndOfDay)
      date = date.plusDays(1)
    val localDT: LocalDateTime    = LocalDateTime.of(date, time)
    val transition: LocalDateTime =
      timeDefinition.createDateTime(localDT, standardOffset, offsetBefore)
    new ZoneOffsetTransition(transition, offsetBefore, offsetAfter)
  }

  /**
   * Checks if this object equals another.
   *
   * The entire state of the object is compared.
   *
   * @param otherRule
   *   the other object to compare to, null returns false
   * @return
   *   true if equal
   */
  override def equals(otherRule: Any): Boolean =
    otherRule match {
      case other: ZoneOffsetTransitionRule =>
        (this eq other) || ((month eq other.month) && (dom == other.dom) && (dayOfWeek eq other.dayOfWeek) && (timeDefinition eq other.timeDefinition) && (time == other.time) && (timeEndOfDay == other.timeEndOfDay) && (standardOffset == other.standardOffset) && (offsetBefore == other.offsetBefore) && (offsetAfter == other.offsetAfter))
      case _                               => false
    }

  /**
   * Returns a suitable hash code.
   *
   * @return
   *   the hash code
   */
  override def hashCode: Int = {
    val hash: Int =
      ((time.toSecondOfDay + (if (timeEndOfDay) 1
                              else
                                0)) << 15) + (month.ordinal << 11) + ((dom + 32) << 5) + ((if (
                                                                                             dayOfWeek == null
                                                                                           )
                                                                                             7
                                                                                           else
                                                                                             dayOfWeek.ordinal) << 2) + timeDefinition.ordinal
    hash ^ standardOffset.hashCode ^ offsetBefore.hashCode ^ offsetAfter.hashCode
  }

  /**
   * Returns a string describing this object.
   *
   * @return
   *   a string for debugging, not null
   */
  override def toString: String = {
    val buf: StringBuilder = new StringBuilder
    buf
      .append("TransitionRule[")
      .append(if (offsetBefore.compareTo(offsetAfter) > 0) "Gap " else "Overlap ")
      .append(offsetBefore)
      .append(" to ")
      .append(offsetAfter)
      .append(", ")
    if (dayOfWeek != null)
      if (dom == -1)
        buf.append(dayOfWeek.name).append(" on or before last day of ").append(month.name)
      else if (dom < 0)
        buf
          .append(dayOfWeek.name)
          .append(" on or before last day minus ")
          .append(-dom - 1)
          .append(" of ")
          .append(month.name)
      else
        buf
          .append(dayOfWeek.name)
          .append(" on or after ")
          .append(month.name)
          .append(' ')
          .append(dom)
    else
      buf.append(month.name).append(' ').append(dom)
    buf
      .append(" at ")
      .append(if (timeEndOfDay) "24:00" else time.toString)
      .append(" ")
      .append(timeDefinition)
      .append(", standard offset ")
      .append(standardOffset)
      .append(']')
    buf.toString
  }
}
