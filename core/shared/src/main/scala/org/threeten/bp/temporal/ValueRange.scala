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

import java.io.Serializable
import org.threeten.bp.DateTimeException

object ValueRange {

  /**
   * Obtains a fixed value range.
   *
   * This factory obtains a range where the minimum and maximum values are fixed. For example, the
   * ISO month-of-year always runs from 1 to 12.
   *
   * @param min
   *   the minimum value
   * @param max
   *   the maximum value
   * @return
   *   the ValueRange for min, max, not null
   * @throws IllegalArgumentException
   *   if the minimum is greater than the maximum
   */
  def of(min: Long, max: Long): ValueRange =
    if (min > max)
      throw new IllegalArgumentException("Minimum value must be less than maximum value")
    else new ValueRange(min, min, max, max)

  /**
   * Obtains a variable value range.
   *
   * This factory obtains a range where the minimum value is fixed and the maximum value may vary.
   * For example, the ISO day-of-month always starts at 1, but ends between 28 and 31.
   *
   * @param min
   *   the minimum value
   * @param maxSmallest
   *   the smallest maximum value
   * @param maxLargest
   *   the largest maximum value
   * @return
   *   the ValueRange for min, smallest max, largest max, not null
   * @throws IllegalArgumentException
   *   if the minimum is greater than the smallest maximum, or the smallest maximum is greater than
   *   the largest maximum
   */
  def of(min: Long, maxSmallest: Long, maxLargest: Long): ValueRange =
    of(min, min, maxSmallest, maxLargest)

  /**
   * Obtains a fully variable value range.
   *
   * This factory obtains a range where both the minimum and maximum value may vary.
   *
   * @param minSmallest
   *   the smallest minimum value
   * @param minLargest
   *   the largest minimum value
   * @param maxSmallest
   *   the smallest maximum value
   * @param maxLargest
   *   the largest maximum value
   * @return
   *   the ValueRange for smallest min, largest min, smallest max, largest max, not null
   * @throws IllegalArgumentException
   *   if the smallest minimum is greater than the smallest maximum, or the smallest maximum is
   *   greater than the largest maximum or the largest minimum is greater than the largest maximum
   */
  def of(minSmallest: Long, minLargest: Long, maxSmallest: Long, maxLargest: Long): ValueRange = {
    if (minSmallest > minLargest)
      throw new IllegalArgumentException(
        "Smallest minimum value must be less than largest minimum value"
      )
    if (maxSmallest > maxLargest)
      throw new IllegalArgumentException(
        "Smallest maximum value must be less than largest maximum value"
      )
    if (minLargest > maxLargest)
      throw new IllegalArgumentException("Minimum value must be less than maximum value")
    new ValueRange(minSmallest, minLargest, maxSmallest, maxLargest)
  }
}

/**
 * The range of valid values for a date-time field.
 *
 * All {@link TemporalField} instances have a valid range of values. For example, the ISO
 * day-of-month runs from 1 to somewhere between 28 and 31. This class captures that valid range.
 *
 * It is important to be aware of the limitations of this class. Only the minimum and maximum values
 * are provided. It is possible for there to be invalid values within the outer range. For example,
 * a weird field may have valid values of 1, 2, 4, 6, 7, thus have a range of '1 - 7', despite that
 * fact that values 3 and 5 are invalid.
 *
 * Instances of this class are not tied to a specific field.
 *
 * <h3>Specification for implementors</h3> This class is immutable and thread-safe.
 *
 * @constructor
 *   Restrictive constructor.
 *
 * @param minSmallest
 *   the smallest minimum value
 * @param minLargest
 *   the largest minimum value
 * @param maxSmallest
 *   the smallest minimum value
 * @param maxLargest
 *   the largest minimum value
 */
final class ValueRange private (
  private val minSmallest: Long,
  private val minLargest:  Long,
  private val maxSmallest: Long,
  private val maxLargest:  Long
) extends Serializable {

  /**
   * Is the value range fixed and fully known.
   *
   * For example, the ISO day-of-month runs from 1 to between 28 and 31. Since there is uncertainty
   * about the maximum value, the range is not fixed. However, for the month of January, the range
   * is always 1 to 31, thus it is fixed.
   *
   * @return
   *   true if the set of values is fixed
   */
  def isFixed: Boolean = minSmallest == minLargest && maxSmallest == maxLargest

  /**
   * Gets the minimum value that the field can take.
   *
   * For example, the ISO day-of-month always starts at 1. The minimum is therefore 1.
   *
   * @return
   *   the minimum value for this field
   */
  def getMinimum: Long = minSmallest

  /**
   * Gets the largest possible minimum value that the field can take.
   *
   * For example, the ISO day-of-month always starts at 1. The largest minimum is therefore 1.
   *
   * @return
   *   the largest possible minimum value for this field
   */
  def getLargestMinimum: Long = minLargest

  /**
   * Gets the smallest possible maximum value that the field can take.
   *
   * For example, the ISO day-of-month runs to between 28 and 31 days. The smallest maximum is
   * therefore 28.
   *
   * @return
   *   the smallest possible maximum value for this field
   */
  def getSmallestMaximum: Long = maxSmallest

  /**
   * Gets the maximum value that the field can take.
   *
   * For example, the ISO day-of-month runs to between 28 and 31 days. The maximum is therefore 31.
   *
   * @return
   *   the maximum value for this field
   */
  def getMaximum: Long = maxLargest

  /**
   * Checks if all values in the range fit in an {@code int}.
   *
   * This checks that all valid values are within the bounds of an {@code int}.
   *
   * For example, the ISO month-of-year has values from 1 to 12, which fits in an {@code int}. By
   * comparison, ISO nano-of-day runs from 1 to 86,400,000,000,000 which does not fit in an {@code
   * int}.
   *
   * This implementation uses {@link #getMinimum()} and {@link #getMaximum()}.
   *
   * @return
   *   true if a valid value always fits in an {@code int}
   */
  def isIntValue: Boolean = getMinimum >= Int.MinValue && getMaximum <= Int.MaxValue

  /**
   * Checks if the value is within the valid range.
   *
   * This checks that the value is within the stored range of values.
   *
   * @param value
   *   the value to check
   * @return
   *   true if the value is valid
   */
  def isValidValue(value: Long): Boolean = value >= getMinimum && value <= getMaximum

  /**
   * Checks if the value is within the valid range and that all values in the range fit in an {@code
   * int}.
   *
   * This method combines {@link #isIntValue()} and {@link #isValidValue(long)}.
   *
   * @param value
   *   the value to check
   * @return
   *   true if the value is valid and fits in an {@code int}
   */
  def isValidIntValue(value: Long): Boolean = isIntValue && isValidValue(value)

  /**
   * Checks that the specified value is valid.
   *
   * This validates that the value is within the valid range of values. The field is only used to
   * improve the error message.
   *
   * @param value
   *   the value to check
   * @param field
   *   the field being checked, may be null
   * @return
   *   the value that was passed in
   * @see
   *   #isValidValue(long)
   */
  def checkValidValue(value: Long, field: TemporalField): Long =
    if (!isValidValue(value))
      if (field != null)
        throw new DateTimeException(s"Invalid value for $field (valid values $this): $value")
      else throw new DateTimeException(s"Invalid value (valid values $this): $value")
    else
      value

  /**
   * Checks that the specified value is valid and fits in an {@code int}.
   *
   * This validates that the value is within the valid range of values and that all valid values are
   * within the bounds of an {@code int}. The field is only used to improve the error message.
   *
   * @param value
   *   the value to check
   * @param field
   *   the field being checked, may be null
   * @return
   *   the value that was passed in
   * @see
   *   #isValidIntValue(long)
   */
  def checkValidIntValue(value: Long, field: TemporalField): Int =
    if (!isValidIntValue(value))
      throw new DateTimeException(s"Invalid int value for $field: $value")
    else value.toInt

  /**
   * Checks if this range is equal to another range.
   *
   * The comparison is based on the four values, minimum, largest minimum, smallest maximum and
   * maximum. Only objects of type {@code ValueRange} are compared, other types return false.
   *
   * @param obj
   *   the object to check, null returns false
   * @return
   *   true if this is equal to the other range
   */
  override def equals(obj: Any): Boolean =
    obj match {
      case other: ValueRange =>
        (this eq other) || (minSmallest == other.minSmallest && minLargest == other.minLargest && maxSmallest == other.maxSmallest && maxLargest == other.maxLargest)
      case _                 =>
        false
    }

  /**
   * A hash code for this range.
   *
   * @return
   *   a suitable hash code
   */
  override def hashCode: Int = {
    val hash: Long =
      minSmallest + minLargest << 16 + minLargest >> 48 + maxSmallest << 32 + maxSmallest >> 32 + maxLargest << 48 + maxLargest >> 16
    (hash ^ (hash >>> 32)).toInt
  }

  /**
   * Outputs this range as a {@code String}.
   *
   * The format will be '{min}/{largestMin} - {smallestMax}/{max}', where the largestMin or
   * smallestMax sections may be omitted, together with associated slash, if they are the same as
   * the min or max.
   *
   * @return
   *   a string representation of this range, not null
   */
  override def toString: String = {
    val ss = if (minSmallest != minLargest) s"/$minLargest" else ""
    val sl = if (maxSmallest != maxLargest) s"/$maxLargest" else ""

    s"$minSmallest$ss - $maxSmallest$sl"
  }
}
