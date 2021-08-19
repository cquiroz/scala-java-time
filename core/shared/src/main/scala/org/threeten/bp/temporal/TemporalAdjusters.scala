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

import java.util.Objects
import org.threeten.bp.temporal.ChronoField.DAY_OF_MONTH
import org.threeten.bp.temporal.ChronoField.DAY_OF_WEEK
import org.threeten.bp.temporal.ChronoField.DAY_OF_YEAR
import org.threeten.bp.temporal.ChronoUnit.DAYS
import org.threeten.bp.temporal.ChronoUnit.MONTHS
import org.threeten.bp.temporal.ChronoUnit.YEARS
import org.threeten.bp.LocalDate
import org.threeten.bp.DayOfWeek

/**
 * Common implementations of {@code TemporalAdjuster}.
 *
 * This class provides common implementations of {@link TemporalAdjuster}. They are especially
 * useful to document the intent of business logic and often link well to requirements. For example,
 * these two pieces of code do the same thing, but the second one is clearer (assuming that there is
 * a static import of this class): <pre> // direct manipulation
 * date.withDayOfMonth(1).plusMonths(1).minusDays(1); // use of an adjuster from this class
 * date.with(lastDayOfMonth()); </pre> There are two equivalent ways of using a {@code
 * TemporalAdjuster}. The first is to invoke the method on the interface directly. The second is to
 * use {@link Temporal#with(TemporalAdjuster)}: <pre> // these two lines are equivalent, but the
 * second approach is recommended dateTime = adjuster.adjustInto(dateTime); dateTime =
 * dateTime.with(adjuster); </pre> It is recommended to use the second approach, {@code
 * with(TemporalAdjuster)}, as it is a lot clearer to read in code.
 *
 * <h3>Specification for implementors</h3> This is a thread-safe utility class. All returned
 * adjusters are immutable and thread-safe.
 */
object TemporalAdjusters {

  // "The JDK 8 ofDateAdjuster(UnaryOperator) method is not backported."
  // !!! FIXME: I made something up based on the name ...
  // someone needs to describe the behaviour of the OpenJDK method, and someone else needs to implement it (just to be save)
  def ofDateAdjuster(localDateAdjuster: LocalDate => LocalDate): TemporalAdjuster = {
    Objects.requireNonNull(localDateAdjuster, "localDateAdjuster")
    new TemporalAdjuster {
      def adjustInto(temporal: Temporal): Temporal =
        temporal.`with`(localDateAdjuster(LocalDate.from(temporal)))
    }
  }

  /**
   * Returns the "first day of month" adjuster, which returns a new date set to the first day of the
   * current month.
   *
   * The ISO calendar system behaves as follows:<br> The input 2011-01-15 will return
   * 2011-01-01.<br> The input 2011-02-15 will return 2011-02-01.
   *
   * The behavior is suitable for use with most calendar systems. It is equivalent to: <pre>
   * temporal.with(DAY_OF_MONTH, 1); </pre>
   *
   * @return
   *   the first day-of-month adjuster, not null
   */
  def firstDayOfMonth: TemporalAdjuster = Impl.FIRST_DAY_OF_MONTH

  /**
   * Returns the "last day of month" adjuster, which returns a new date set to the last day of the
   * current month.
   *
   * The ISO calendar system behaves as follows:<br> The input 2011-01-15 will return
   * 2011-01-31.<br> The input 2011-02-15 will return 2011-02-28.<br> The input 2012-02-15 will
   * return 2012-02-29 (leap year).<br> The input 2011-04-15 will return 2011-04-30.
   *
   * The behavior is suitable for use with most calendar systems. It is equivalent to: <pre> val
   * lastDay: Long = temporal.range(DAY_OF_MONTH).getMaximum(); temporal.with(DAY_OF_MONTH,
   * lastDay); </pre>
   *
   * @return
   *   the last day-of-month adjuster, not null
   */
  def lastDayOfMonth: TemporalAdjuster = Impl.LAST_DAY_OF_MONTH

  /**
   * Returns the "first day of next month" adjuster, which returns a new date set to the first day
   * of the next month.
   *
   * The ISO calendar system behaves as follows:<br> The input 2011-01-15 will return
   * 2011-02-01.<br> The input 2011-02-15 will return 2011-03-01.
   *
   * The behavior is suitable for use with most calendar systems. It is equivalent to: <pre>
   * temporal.with(DAY_OF_MONTH, 1).plus(1, MONTHS); </pre>
   *
   * @return
   *   the first day of next month adjuster, not null
   */
  def firstDayOfNextMonth: TemporalAdjuster = Impl.FIRST_DAY_OF_NEXT_MONTH

  /**
   * Returns the "first day of year" adjuster, which returns a new date set to the first day of the
   * current year.
   *
   * The ISO calendar system behaves as follows:<br> The input 2011-01-15 will return
   * 2011-01-01.<br> The input 2011-02-15 will return 2011-01-01.<br>
   *
   * The behavior is suitable for use with most calendar systems. It is equivalent to: <pre>
   * temporal.with(DAY_OF_YEAR, 1); </pre>
   *
   * @return
   *   the first day-of-year adjuster, not null
   */
  def firstDayOfYear: TemporalAdjuster = Impl.FIRST_DAY_OF_YEAR

  /**
   * Returns the "last day of year" adjuster, which returns a new date set to the last day of the
   * current year.
   *
   * The ISO calendar system behaves as follows:<br> The input 2011-01-15 will return
   * 2011-12-31.<br> The input 2011-02-15 will return 2011-12-31.<br>
   *
   * The behavior is suitable for use with most calendar systems. It is equivalent to: <pre> long
   * lastDay = temporal.range(DAY_OF_YEAR).getMaximum(); temporal.with(DAY_OF_YEAR, lastDay); </pre>
   *
   * @return
   *   the last day-of-year adjuster, not null
   */
  def lastDayOfYear: TemporalAdjuster = Impl.LAST_DAY_OF_YEAR

  /**
   * Returns the "first day of next year" adjuster, which returns a new date set to the first day of
   * the next year.
   *
   * The ISO calendar system behaves as follows:<br> The input 2011-01-15 will return 2012-01-01.
   *
   * The behavior is suitable for use with most calendar systems. It is equivalent to: <pre>
   * temporal.with(DAY_OF_YEAR, 1).plus(1, YEARS); </pre>
   *
   * @return
   *   the first day of next month adjuster, not null
   */
  def firstDayOfNextYear: TemporalAdjuster = Impl.FIRST_DAY_OF_NEXT_YEAR

  /** Enum implementing the adjusters. */
  private[TemporalAdjusters] object Impl {

    /** First day of month adjuster. */
    def FIRST_DAY_OF_MONTH: TemporalAdjusters.Impl = new TemporalAdjusters.Impl(0)

    /** Last day of month adjuster. */
    def LAST_DAY_OF_MONTH: TemporalAdjusters.Impl = new TemporalAdjusters.Impl(1)

    /** First day of next month adjuster. */
    def FIRST_DAY_OF_NEXT_MONTH: TemporalAdjusters.Impl = new TemporalAdjusters.Impl(2)

    /** First day of year adjuster. */
    def FIRST_DAY_OF_YEAR: TemporalAdjusters.Impl = new TemporalAdjusters.Impl(3)

    /** Last day of year adjuster. */
    def LAST_DAY_OF_YEAR: TemporalAdjusters.Impl = new TemporalAdjusters.Impl(4)

    /** First day of next month adjuster. */
    def FIRST_DAY_OF_NEXT_YEAR: TemporalAdjusters.Impl = new TemporalAdjusters.Impl(5)
  }

  private class Impl(private val ordinal: Int) extends TemporalAdjuster {

    def adjustInto(temporal: Temporal): Temporal =
      ordinal match {
        case 0 => temporal.`with`(DAY_OF_MONTH, 1)
        case 1 => temporal.`with`(DAY_OF_MONTH, temporal.range(DAY_OF_MONTH).getMaximum)
        case 2 => temporal.`with`(DAY_OF_MONTH, 1).plus(1, MONTHS)
        case 3 => temporal.`with`(DAY_OF_YEAR, 1)
        case 4 => temporal.`with`(DAY_OF_YEAR, temporal.range(DAY_OF_YEAR).getMaximum)
        case 5 => temporal.`with`(DAY_OF_YEAR, 1).plus(1, YEARS)
        case _ => throw new IllegalStateException("Unreachable")
      }
  }

  /**
   * Returns the first in month adjuster, which returns a new date in the same month with the first
   * matching day-of-week. This is used for expressions like 'first Tuesday in March'.
   *
   * The ISO calendar system behaves as follows:<br> The input 2011-12-15 for (MONDAY) will return
   * 2011-12-05.<br> The input 2011-12-15 for (FRIDAY) will return 2011-12-02.<br>
   *
   * The behavior is suitable for use with most calendar systems. It uses the {@code DAY_OF_WEEK}
   * and {@code DAY_OF_MONTH} fields and the {@code DAYS} unit, and assumes a seven day week.
   *
   * @param dayOfWeek
   *   the day-of-week, not null
   * @return
   *   the first in month adjuster, not null
   */
  def firstInMonth(dayOfWeek: DayOfWeek): TemporalAdjuster = {
    Objects.requireNonNull(dayOfWeek, "dayOfWeek")
    new TemporalAdjusters.DayOfWeekInMonth(1, dayOfWeek)
  }

  /**
   * Returns the last in month adjuster, which returns a new date in the same month with the last
   * matching day-of-week. This is used for expressions like 'last Tuesday in March'.
   *
   * The ISO calendar system behaves as follows:<br> The input 2011-12-15 for (MONDAY) will return
   * 2011-12-26.<br> The input 2011-12-15 for (FRIDAY) will return 2011-12-30.<br>
   *
   * The behavior is suitable for use with most calendar systems. It uses the {@code DAY_OF_WEEK}
   * and {@code DAY_OF_MONTH} fields and the {@code DAYS} unit, and assumes a seven day week.
   *
   * @param dayOfWeek
   *   the day-of-week, not null
   * @return
   *   the first in month adjuster, not null
   */
  def lastInMonth(dayOfWeek: DayOfWeek): TemporalAdjuster = {
    Objects.requireNonNull(dayOfWeek, "dayOfWeek")
    new TemporalAdjusters.DayOfWeekInMonth(-1, dayOfWeek)
  }

  /**
   * Returns the day-of-week in month adjuster, which returns a new date in the same month with the
   * ordinal day-of-week. This is used for expressions like the 'second Tuesday in March'.
   *
   * The ISO calendar system behaves as follows:<br> The input 2011-12-15 for (1,TUESDAY) will
   * return 2011-12-06.<br> The input 2011-12-15 for (2,TUESDAY) will return 2011-12-13.<br> The
   * input 2011-12-15 for (3,TUESDAY) will return 2011-12-20.<br> The input 2011-12-15 for
   * (4,TUESDAY) will return 2011-12-27.<br> The input 2011-12-15 for (5,TUESDAY) will return
   * 2012-01-03.<br> The input 2011-12-15 for (-1,TUESDAY) will return 2011-12-27 (last in
   * month).<br> The input 2011-12-15 for (-4,TUESDAY) will return 2011-12-06 (3 weeks before last
   * in month).<br> The input 2011-12-15 for (-5,TUESDAY) will return 2011-11-29 (4 weeks before
   * last in month).<br> The input 2011-12-15 for (0,TUESDAY) will return 2011-11-29 (last in
   * previous month).<br>
   *
   * For a positive or zero ordinal, the algorithm is equivalent to finding the first day-of-week
   * that matches within the month and then adding a number of weeks to it. For a negative ordinal,
   * the algorithm is equivalent to finding the last day-of-week that matches within the month and
   * then subtracting a number of weeks to it. The ordinal number of weeks is not validated and is
   * interpreted leniently according to this algorithm. This definition means that an ordinal of
   * zero finds the last matching day-of-week in the previous month.
   *
   * The behavior is suitable for use with most calendar systems. It uses the {@code DAY_OF_WEEK}
   * and {@code DAY_OF_MONTH} fields and the {@code DAYS} unit, and assumes a seven day week.
   *
   * @param ordinal
   *   the week within the month, unbounded but typically from -5 to 5
   * @param dayOfWeek
   *   the day-of-week, not null
   * @return
   *   the day-of-week in month adjuster, not null
   */
  def dayOfWeekInMonth(ordinal: Int, dayOfWeek: DayOfWeek): TemporalAdjuster = {
    Objects.requireNonNull(dayOfWeek, "dayOfWeek")
    new TemporalAdjusters.DayOfWeekInMonth(ordinal, dayOfWeek)
  }

  /** Class implementing day-of-week in month adjuster. */
  private[temporal] final class DayOfWeekInMonth private[temporal] (
    private val ordinal: Int,
    dow:                 DayOfWeek
  ) extends TemporalAdjuster {

    /** The day-of-week value, from 1 to 7. */
    private val dowValue: Int = dow.getValue

    def adjustInto(temporal: Temporal): Temporal =
      if (ordinal >= 0) {
        val temp: Temporal = temporal.`with`(DAY_OF_MONTH, 1)
        val curDow: Int    = temp.get(DAY_OF_WEEK)
        var dowDiff: Int   = (dowValue - curDow + 7) % 7
        dowDiff += ((ordinal - 1L) * 7L).toInt
        temp.plus(dowDiff.toLong, DAYS)
      } else {
        val temp: Temporal = temporal.`with`(DAY_OF_MONTH, temporal.range(DAY_OF_MONTH).getMaximum)
        val curDow: Int    = temp.get(DAY_OF_WEEK)
        var daysDiff: Int  = dowValue - curDow
        daysDiff = if (daysDiff == 0) 0 else (if (daysDiff > 0) daysDiff - 7 else daysDiff)
        daysDiff -= ((-ordinal - 1L) * 7L).toInt
        temp.plus(daysDiff.toLong, DAYS)
      }
  }

  /**
   * Returns the next day-of-week adjuster, which adjusts the date to the first occurrence of the
   * specified day-of-week after the date being adjusted.
   *
   * The ISO calendar system behaves as follows:<br> The input 2011-01-15 (a Saturday) for parameter
   * (MONDAY) will return 2011-01-17 (two days later).<br> The input 2011-01-15 (a Saturday) for
   * parameter (WEDNESDAY) will return 2011-01-19 (four days later).<br> The input 2011-01-15 (a
   * Saturday) for parameter (SATURDAY) will return 2011-01-22 (seven days later).
   *
   * The behavior is suitable for use with most calendar systems. It uses the {@code DAY_OF_WEEK}
   * field and the {@code DAYS} unit, and assumes a seven day week.
   *
   * @param dayOfWeek
   *   the day-of-week to move the date to, not null
   * @return
   *   the next day-of-week adjuster, not null
   */
  def next(dayOfWeek: DayOfWeek): TemporalAdjuster =
    new TemporalAdjusters.RelativeDayOfWeek(2, dayOfWeek)

  /**
   * Returns the next-or-same day-of-week adjuster, which adjusts the date to the first occurrence
   * of the specified day-of-week after the date being adjusted unless it is already on that day in
   * which case the same object is returned.
   *
   * The ISO calendar system behaves as follows:<br> The input 2011-01-15 (a Saturday) for parameter
   * (MONDAY) will return 2011-01-17 (two days later).<br> The input 2011-01-15 (a Saturday) for
   * parameter (WEDNESDAY) will return 2011-01-19 (four days later).<br> The input 2011-01-15 (a
   * Saturday) for parameter (SATURDAY) will return 2011-01-15 (same as input).
   *
   * The behavior is suitable for use with most calendar systems. It uses the {@code DAY_OF_WEEK}
   * field and the {@code DAYS} unit, and assumes a seven day week.
   *
   * @param dayOfWeek
   *   the day-of-week to check for or move the date to, not null
   * @return
   *   the next-or-same day-of-week adjuster, not null
   */
  def nextOrSame(dayOfWeek: DayOfWeek): TemporalAdjuster =
    new TemporalAdjusters.RelativeDayOfWeek(0, dayOfWeek)

  /**
   * Returns the previous day-of-week adjuster, which adjusts the date to the first occurrence of
   * the specified day-of-week before the date being adjusted.
   *
   * The ISO calendar system behaves as follows:<br> The input 2011-01-15 (a Saturday) for parameter
   * (MONDAY) will return 2011-01-10 (five days earlier).<br> The input 2011-01-15 (a Saturday) for
   * parameter (WEDNESDAY) will return 2011-01-12 (three days earlier).<br> The input 2011-01-15 (a
   * Saturday) for parameter (SATURDAY) will return 2011-01-08 (seven days earlier).
   *
   * The behavior is suitable for use with most calendar systems. It uses the {@code DAY_OF_WEEK}
   * field and the {@code DAYS} unit, and assumes a seven day week.
   *
   * @param dayOfWeek
   *   the day-of-week to move the date to, not null
   * @return
   *   the previous day-of-week adjuster, not null
   */
  def previous(dayOfWeek: DayOfWeek): TemporalAdjuster =
    new TemporalAdjusters.RelativeDayOfWeek(3, dayOfWeek)

  /**
   * Returns the previous-or-same day-of-week adjuster, which adjusts the date to the first
   * occurrence of the specified day-of-week before the date being adjusted unless it is already on
   * that day in which case the same object is returned.
   *
   * The ISO calendar system behaves as follows:<br> The input 2011-01-15 (a Saturday) for parameter
   * (MONDAY) will return 2011-01-10 (five days earlier).<br> The input 2011-01-15 (a Saturday) for
   * parameter (WEDNESDAY) will return 2011-01-12 (three days earlier).<br> The input 2011-01-15 (a
   * Saturday) for parameter (SATURDAY) will return 2011-01-15 (same as input).
   *
   * The behavior is suitable for use with most calendar systems. It uses the {@code DAY_OF_WEEK}
   * field and the {@code DAYS} unit, and assumes a seven day week.
   *
   * @param dayOfWeek
   *   the day-of-week to check for or move the date to, not null
   * @return
   *   the previous-or-same day-of-week adjuster, not null
   */
  def previousOrSame(dayOfWeek: DayOfWeek): TemporalAdjuster =
    new TemporalAdjusters.RelativeDayOfWeek(1, dayOfWeek)

  /** Implementation of next, previous or current day-of-week. */
  private[temporal] final class RelativeDayOfWeek private[temporal] (
    private val relative: Int,
    dayOfWeek:            DayOfWeek
  ) extends TemporalAdjuster {
    Objects.requireNonNull(dayOfWeek, "dayOfWeek")

    /** The day-of-week value, from 1 to 7. */
    private val dowValue: Int = dayOfWeek.getValue

    def adjustInto(temporal: Temporal): Temporal = {
      val calDow: Int = temporal.get(DAY_OF_WEEK)
      if (relative < 2 && calDow == dowValue)
        temporal
      else if ((relative & 1) == 0) {
        val daysDiff: Int = calDow - dowValue
        temporal.plus(if (daysDiff >= 0) 7L - daysDiff.toLong else -daysDiff.toLong, DAYS)
      } else {
        val daysDiff: Int = dowValue - calDow
        temporal.minus(if (daysDiff >= 0) 7L - daysDiff.toLong else -daysDiff.toLong, DAYS)
      }
    }
  }

}
