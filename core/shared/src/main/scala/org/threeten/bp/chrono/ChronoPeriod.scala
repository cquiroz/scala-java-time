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

import org.threeten.bp.temporal.Temporal
import org.threeten.bp.temporal.TemporalAmount
import org.threeten.bp.temporal.TemporalUnit

object ChronoPeriod {

  /**
   * Obtains a {@code ChronoPeriod} consisting of amount of time between two dates.
   *
   * The start date is included, but the end date is not. The period is calculated using {@link
   * ChronoLocalDate#until(ChronoLocalDate)}. As such, the calculation is chronology specific.
   *
   * The chronology of the first date is used. The chronology of the second date is ignored, with
   * the date being converted to the target chronology system before the calculation starts.
   *
   * The result of this method can be a negative period if the end is before the start. In most
   * cases, the positive/negative sign will be the same in each of the supported fields.
   *
   * @param startDateInclusive
   *   the start date, inclusive, specifying the chronology of the calculation, not null
   * @param endDateExclusive
   *   the end date, exclusive, in any chronology, not null
   * @return
   *   the period between this date and the end date, not null
   * @see
   *   ChronoLocalDate#until(ChronoLocalDate)
   */
  def between(
    startDateInclusive: ChronoLocalDate,
    endDateExclusive:   ChronoLocalDate
  ): ChronoPeriod = {
    Objects.requireNonNull(startDateInclusive, "startDateInclusive")
    Objects.requireNonNull(endDateExclusive, "endDateExclusive")
    startDateInclusive.until(endDateExclusive)
  }
}

/**
 * A date-based amount of time, such as '3 years, 4 months and 5 days' in an arbitrary chronology,
 * intended for advanced globalization use cases.
 *
 * This interface models a date-based amount of time in a calendar system. While most calendar
 * systems use years, months and days, some do not. Therefore, this interface operates solely in
 * terms of a set of supported units that are defined by the {@code Chronology}. The set of
 * supported units is fixed for a given chronology. The amount of a supported unit may be set to
 * zero.
 *
 * The period is modeled as a directed amount of time, meaning that individual parts of the period
 * may be negative.
 *
 * <h3>Specification for implementors</h3> This abstract class must be implemented with care to
 * ensure other classes operate correctly. All implementations that can be instantiated must be
 * final, immutable and thread-safe. Subclasses should be Serializable wherever possible.
 */
trait ChronoPeriod extends TemporalAmount {

  /**
   * Gets the value of the requested unit.
   *
   * The supported units are chronology specific. They will typically be {@link ChronoUnit#YEARS
   * YEARS}, {@link ChronoUnit#MONTHS MONTHS} and {@link ChronoUnit#DAYS DAYS}. Requesting an
   * unsupported unit will throw an exception.
   *
   * @param unit
   *   the { @code TemporalUnit} for which to return the value
   * @return
   *   the long value of the unit
   * @throws DateTimeException
   *   if the unit is not supported
   * @throws UnsupportedTemporalTypeException
   *   if the unit is not supported
   */
  def get(unit: TemporalUnit): Long

  /**
   * Gets the set of units supported by this period.
   *
   * The supported units are chronology specific. They will typically be {@link ChronoUnit#YEARS
   * YEARS}, {@link ChronoUnit#MONTHS MONTHS} and {@link ChronoUnit#DAYS DAYS}. They are returned in
   * order from largest to smallest.
   *
   * This set can be used in conjunction with {@link #get(TemporalUnit)} to access the entire state
   * of the period.
   *
   * @return
   *   a list containing the supported units, not null
   */
  def getUnits: java.util.List[TemporalUnit]

  /**
   * Gets the chronology that defines the meaning of the supported units.
   *
   * The period is defined by the chronology. It controls the supported units and restricts
   * addition/subtraction to {@code ChronoLocalDate} instances of the same chronology.
   *
   * @return
   *   the chronology defining the period, not null
   */
  def getChronology: Chronology

  /**
   * Checks if all the supported units of this period are zero.
   *
   * @return
   *   true if this period is zero-length
   */
  def isZero: Boolean = {
    val units = getUnits.iterator
    while (units.hasNext) {
      val unit = units.next()
      if (get(unit) != 0)
        return false
    }
    true
  }

  /**
   * Checks if any of the supported units of this period are negative.
   *
   * @return
   *   true if any unit of this period is negative
   */
  def isNegative: Boolean = {
    val units = getUnits.iterator
    while (units.hasNext) {
      val unit = units.next()
      if (get(unit) < 0)
        return true
    }
    false
  }

  /**
   * Returns a copy of this period with the specified period added.
   *
   * If the specified amount is a {@code ChronoPeriod} then it must have the same chronology as this
   * period. Implementations may choose to accept or reject other {@code TemporalAmount}
   * implementations.
   *
   * This instance is immutable and unaffected by this method call.
   *
   * @param amountToAdd
   *   the period to add, not null
   * @return
   *   a { @code ChronoPeriod} based on this period with the requested period added, not null
   * @throws ArithmeticException
   *   if numeric overflow occurs
   */
  def plus(amountToAdd: TemporalAmount): ChronoPeriod

  /**
   * Returns a copy of this period with the specified period subtracted.
   *
   * If the specified amount is a {@code ChronoPeriod} then it must have the same chronology as this
   * period. Implementations may choose to accept or reject other {@code TemporalAmount}
   * implementations.
   *
   * This instance is immutable and unaffected by this method call.
   *
   * @param amountToSubtract
   *   the period to subtract, not null
   * @return
   *   a { @code ChronoPeriod} based on this period with the requested period subtracted, not null
   * @throws ArithmeticException
   *   if numeric overflow occurs
   */
  def minus(amountToSubtract: TemporalAmount): ChronoPeriod

  /**
   * Returns a new instance with each amount in this period in this period multiplied by the
   * specified scalar.
   *
   * This returns a period with each supported unit individually multiplied. For example, a period
   * of "2 years, -3 months and 4 days" multiplied by 3 will return "6 years, -9 months and 12
   * days". No normalization is performed.
   *
   * @param scalar
   *   the scalar to multiply by, not null
   * @return
   *   a { @code ChronoPeriod} based on this period with the amounts multiplied by the scalar, not
   *   null
   * @throws ArithmeticException
   *   if numeric overflow occurs
   */
  def multipliedBy(scalar: Int): ChronoPeriod

  /**
   * Returns a new instance with each amount in this period negated.
   *
   * This returns a period with each supported unit individually negated. For example, a period of
   * "2 years, -3 months and 4 days" will be negated to "-2 years, 3 months and -4 days". No
   * normalization is performed.
   *
   * @return
   *   a { @code ChronoPeriod} based on this period with the amounts negated, not null
   * @throws ArithmeticException
   *   if numeric overflow occurs, which only happens if one of the units has the value { @code
   *   Long.MIN_VALUE}
   */
  def negated: ChronoPeriod = multipliedBy(-1)

  /**
   * Returns a copy of this period with the amounts of each unit normalized.
   *
   * The process of normalization is specific to each calendar system. For example, in the ISO
   * calendar system, the years and months are normalized but the days are not, such that "15
   * months" would be normalized to "1 year and 3 months".
   *
   * This instance is immutable and unaffected by this method call.
   *
   * @return
   *   a { @code ChronoPeriod} based on this period with the amounts of each unit normalized, not
   *   null
   * @throws ArithmeticException
   *   if numeric overflow occurs
   */
  def normalized: ChronoPeriod

  /**
   * Adds this period to the specified temporal object.
   *
   * This returns a temporal object of the same observable type as the input with this period added.
   *
   * In most cases, it is clearer to reverse the calling pattern by using {@link
   * Temporal#plus(TemporalAmount)}. <pre> // these two lines are equivalent, but the second
   * approach is recommended dateTime = thisPeriod.addTo(dateTime); dateTime =
   * dateTime.plus(thisPeriod); </pre>
   *
   * The specified temporal must have the same chronology as this period. This returns a temporal
   * with the non-zero supported units added.
   *
   * This instance is immutable and unaffected by this method call.
   *
   * @param temporal
   *   the temporal object to adjust, not null
   * @return
   *   an object of the same type with the adjustment made, not null
   * @throws DateTimeException
   *   if unable to add
   * @throws ArithmeticException
   *   if numeric overflow occurs
   */
  def addTo(temporal: Temporal): Temporal

  /**
   * Subtracts this period from the specified temporal object.
   *
   * This returns a temporal object of the same observable type as the input with this period
   * subtracted.
   *
   * In most cases, it is clearer to reverse the calling pattern by using {@link
   * Temporal#minus(TemporalAmount)}. <pre> // these two lines are equivalent, but the second
   * approach is recommended dateTime = thisPeriod.subtractFrom(dateTime); dateTime =
   * dateTime.minus(thisPeriod); </pre>
   *
   * The specified temporal must have the same chronology as this period. This returns a temporal
   * with the non-zero supported units subtracted.
   *
   * This instance is immutable and unaffected by this method call.
   *
   * @param temporal
   *   the temporal object to adjust, not null
   * @return
   *   an object of the same type with the adjustment made, not null
   * @throws DateTimeException
   *   if unable to subtract
   * @throws ArithmeticException
   *   if numeric overflow occurs
   */
  def subtractFrom(temporal: Temporal): Temporal

  /**
   * Checks if this period is equal to another period, including the chronology.
   *
   * Compares this period with another ensuring that the type, each amount and the chronology are
   * the same. Note that this means that a period of "15 Months" is not equal to a period of "1 Year
   * and 3 Months".
   *
   * @param obj
   *   the object to check, null returns false
   * @return
   *   true if this is equal to the other period
   */
  override def equals(obj: Any): Boolean

  /**
   * A hash code for this period.
   *
   * @return
   *   a suitable hash code
   */
  override def hashCode: Int

  /**
   * Outputs this period as a {@code String}.
   *
   * The output will include the period amounts and chronology.
   *
   * @return
   *   a string representation of this period, not null
   */
  override def toString: String
}
