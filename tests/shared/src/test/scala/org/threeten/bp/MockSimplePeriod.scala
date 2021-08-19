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
package org.threeten.bp

import java.util.{ Collections, Objects }

import org.threeten.bp.temporal.ChronoUnit.{ DAYS, FOREVER, SECONDS }
import org.threeten.bp.temporal.{ Temporal, TemporalAmount, TemporalUnit }

/** Mock period of time measured using a single unit, such as {@code 3 Days}. */
object MockSimplePeriod {

  /** A constant for a period of zero, measured in days. */
  val ZERO_DAYS: MockSimplePeriod = new MockSimplePeriod(0, DAYS)

  /** A constant for a period of zero, measured in seconds. */
  val ZERO_SECONDS: MockSimplePeriod = new MockSimplePeriod(0, SECONDS)

  /**
   * Obtains a {@code MockSimplePeriod} from an amount and unit. <p> The parameters represent the
   * two parts of a phrase like '6 Days'.
   *
   * @param amount
   *   the amount of the period, measured in terms of the unit, positive or negative
   * @param unit
   *   the unit that the period is measured in, must not be the 'Forever' unit, not null
   * @return
   *   the { @code MockSimplePeriod} instance, not null
   * @throws DateTimeException
   *   if the period unit is { @link org.threeten.bp.temporal.ChronoUnit#FOREVER}.
   */
  def of(amount: Long, unit: TemporalUnit): MockSimplePeriod = new MockSimplePeriod(amount, unit)
}

/**
 * @param amount
 *   the amount of the period
 * @param unit
 *   the unit the period is measured in
 */
final class MockSimplePeriod private (private val amount: Long, private val unit: TemporalUnit)
    extends TemporalAmount
    with Comparable[MockSimplePeriod] {
  Objects.requireNonNull(unit, "unit")
  if (unit eq FOREVER)
    throw new DateTimeException("Cannot create a period of the Forever unit")

  def getUnits: java.util.List[TemporalUnit] = Collections.singletonList(unit)

  def get(unit: TemporalUnit): Long =
    if (this.unit == unit)
      amount
    else
      throw new DateTimeException("Unsupported unit: " + unit)

  def getAmount: Long = amount

  def getUnit: TemporalUnit = unit

  def addTo(dateTime: Temporal): Temporal = dateTime.plus(amount, unit)

  def subtractFrom(dateTime: Temporal): Temporal = dateTime.minus(amount, unit)

  def compareTo(otherPeriod: MockSimplePeriod): Int =
    if (unit != otherPeriod.getUnit)
      throw new IllegalArgumentException(
        "Units cannot be compared: " + unit + " and " + otherPeriod.getUnit
      )
    else
      java.lang.Long.compare(amount, otherPeriod.amount)

  override def equals(obj: Any): Boolean =
    obj match {
      case other: MockSimplePeriod =>
        (this eq other) || (this.amount == other.amount && (this.unit == other.unit))
      case _                       => false
    }

  override def hashCode: Int = unit.hashCode ^ (amount ^ (amount >>> 32)).toInt

  override def toString: String = amount + " " + unit
}
