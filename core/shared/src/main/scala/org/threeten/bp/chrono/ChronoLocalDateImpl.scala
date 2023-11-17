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

import java.io.Serializable
import org.threeten.bp.DateTimeException
import org.threeten.bp.LocalDate
import org.threeten.bp.LocalTime
import org.threeten.bp.temporal.ChronoUnit
import org.threeten.bp.temporal.Temporal
import org.threeten.bp.temporal.TemporalAdjuster
import org.threeten.bp.temporal.TemporalUnit

/**
 * A date expressed in terms of a standard year-month-day calendar system.
 *
 * This class is used by applications seeking to handle dates in non-ISO calendar systems. For
 * example, the Japanese, Minguo, Thai Buddhist and others.
 *
 * {@code ChronoLocalDate} is built on the generic concepts of year, month and day. The calendar
 * system, represented by a {@link Chronology} , expresses the relationship between the fields and
 * this class allows the resulting date to be manipulated.
 *
 * Note that not all calendar systems are suitable for use with this class. For example, the Mayan
 * calendar uses a system that bears no relation to years, months and days.
 *
 * The API design encourages the use of {@code LocalDate} for the majority of the application. This
 * includes code to read and write from a persistent data store, such as a database, and to send
 * dates and times across a network. The {@code ChronoLocalDate} instance is then used at the user
 * interface level to deal with localized input/output.
 *
 * Example: </p> <pre> System.out.printf("Example()%n"); // Enumerate the list of available
 * calendars and print today for each Set&lt;Chrono&gt; chronos = Chrono.getAvailableChronologies();
 * for (Chrono chrono : chronos) { ChronoLocalDate date = chrono.dateNow(); System.out.printf("
 * %20s: %s%n", chrono.getID(), date.toString()); }
 *
 * // Print the Hijrah date and calendar ChronoLocalDate date = Chrono.of("Hijrah").dateNow(); int
 * day = date.get(ChronoField.DAY_OF_MONTH); int dow = date.get(ChronoField.DAY_OF_WEEK); int month
 * \= date.get(ChronoField.MONTH_OF_YEAR); int year = date.get(ChronoField.YEAR);
 * System.out.printf(" Today is %s %s %d-%s-%d%n", date.getChrono().getID(), dow, day, month, year);
 *
 * // Print today's date and the last day of the year ChronoLocalDate now1 =
 * Chrono.of("Hijrah").dateNow(); ChronoLocalDate first = now1.with(ChronoField.DAY_OF_MONTH, 1)
 * .with(ChronoField.MONTH_OF_YEAR, 1); ChronoLocalDate last = first.plus(1, ChronoUnit.YEARS)
 * .minus(1, ChronoUnit.DAYS); System.out.printf(" Today is %s: start: %s; end: %s%n",
 * last.getChrono().getID(), first, last); </pre>
 *
 * <h4>Adding Calendars</h4> The set of calendars is extensible by defining a subclass of {@link
 * ChronoLocalDate} to represent a date instance and an implementation of {@code Chronology} to be
 * the factory for the ChronoLocalDate subclass. </p> To permit the discovery of the additional
 * calendar types the implementation of {@code Chronology} must be registered as a Service
 * implementing the {@code Chronology} interface in the {@code META-INF/Services} file as per the
 * specification of {@link java.util.ServiceLoader} . The subclass must function according to the
 * {@code Chronology} class description and must provide its {@link Chronology#getID calendar name}
 * and {@link Chronology#getCalendarType() calendar type} . </p>
 *
 * <h3>Specification for implementors</h3> This abstract class must be implemented with care to
 * ensure other classes operate correctly. All implementations that can be instantiated must be
 * final, immutable and thread-safe. Subclasses should be Serializable wherever possible.
 *
 * @tparam D
 *   the date type
 */
@SerialVersionUID(6282433883239719096L)
abstract class ChronoLocalDateImpl[D <: ChronoLocalDate] private[chrono] ()
    extends ChronoLocalDate
    with Temporal
    with TemporalAdjuster
    with Serializable {

  def plus(amountToAdd: Long, unit: TemporalUnit): ChronoLocalDateImpl[D] = {
    import ChronoUnit._
    unit match {
      case f: ChronoUnit =>
        f match {
          case DAYS      => plusDays(amountToAdd)
          case WEEKS     => plusDays(Math.multiplyExact(amountToAdd, 7L))
          case MONTHS    => plusMonths(amountToAdd)
          case YEARS     => plusYears(amountToAdd)
          case DECADES   => plusYears(Math.multiplyExact(amountToAdd, 10L))
          case CENTURIES => plusYears(Math.multiplyExact(amountToAdd, 100L))
          case MILLENNIA => plusYears(Math.multiplyExact(amountToAdd, 1000L))
          case _         =>
            throw new DateTimeException(s"$unit not valid for chronology ${getChronology.getId}")
        }
      case _             =>
        getChronology
          .ensureChronoLocalDate(unit.addTo(this, amountToAdd))
      // .asInstanceOf[ChronoLocalDateImpl[D]]
    }
  }

  /**
   * Returns a copy of this date with the specified period in years added.
   *
   * This adds the specified period in years to the date. In some cases, adding years can cause the
   * resulting date to become invalid. If this occurs, then other fields, typically the
   * day-of-month, will be adjusted to ensure that the result is valid. Typically this will select
   * the last valid day of the month.
   *
   * This instance is immutable and unaffected by this method call.
   *
   * @param yearsToAdd
   *   the years to add, may be negative
   * @return
   *   a date based on this one with the years added, not null
   * @throws DateTimeException
   *   if the result exceeds the supported date range
   */
  private[chrono] def plusYears(yearsToAdd: Long): ChronoLocalDateImpl[D]

  /**
   * Returns a copy of this date with the specified period in months added.
   *
   * This adds the specified period in months to the date. In some cases, adding months can cause
   * the resulting date to become invalid. If this occurs, then other fields, typically the
   * day-of-month, will be adjusted to ensure that the result is valid. Typically this will select
   * the last valid day of the month.
   *
   * This instance is immutable and unaffected by this method call.
   *
   * @param monthsToAdd
   *   the months to add, may be negative
   * @return
   *   a date based on this one with the months added, not null
   * @throws DateTimeException
   *   if the result exceeds the supported date range
   */
  private[chrono] def plusMonths(monthsToAdd: Long): ChronoLocalDateImpl[D]

  /**
   * Returns a copy of this date with the specified period in weeks added.
   *
   * This adds the specified period in weeks to the date. In some cases, adding weeks can cause the
   * resulting date to become invalid. If this occurs, then other fields will be adjusted to ensure
   * that the result is valid.
   *
   * The default implementation uses {@link #plusDays(long)} using a 7 day week.
   *
   * This instance is immutable and unaffected by this method call.
   *
   * @param weeksToAdd
   *   the weeks to add, may be negative
   * @return
   *   a date based on this one with the weeks added, not null
   * @throws DateTimeException
   *   if the result exceeds the supported date range
   */
  private[chrono] def plusWeeks(weeksToAdd: Long): ChronoLocalDateImpl[D] =
    plusDays(Math.multiplyExact(weeksToAdd, 7))

  /**
   * Returns a copy of this date with the specified number of days added.
   *
   * This adds the specified period in days to the date.
   *
   * This instance is immutable and unaffected by this method call.
   *
   * @param daysToAdd
   *   the days to add, may be negative
   * @return
   *   a date based on this one with the days added, not null
   * @throws DateTimeException
   *   if the result exceeds the supported date range
   */
  private[chrono] def plusDays(daysToAdd: Long): ChronoLocalDateImpl[D]

  /**
   * Returns a copy of this date with the specified period in years subtracted.
   *
   * This subtracts the specified period in years to the date. In some cases, subtracting years can
   * cause the resulting date to become invalid. If this occurs, then other fields, typically the
   * day-of-month, will be adjusted to ensure that the result is valid. Typically this will select
   * the last valid day of the month.
   *
   * The default implementation uses {@link #plusYears(long)} .
   *
   * This instance is immutable and unaffected by this method call.
   *
   * @param yearsToSubtract
   *   the years to subtract, may be negative
   * @return
   *   a date based on this one with the years subtracted, not null
   * @throws DateTimeException
   *   if the result exceeds the supported date range
   */
  private[chrono] def minusYears(yearsToSubtract: Long): ChronoLocalDateImpl[D] =
    if (yearsToSubtract == Long.MinValue) plusYears(Long.MaxValue).plusYears(1)
    else plusYears(-yearsToSubtract)

  /**
   * Returns a copy of this date with the specified period in months subtracted.
   *
   * This subtracts the specified period in months to the date. In some cases, subtracting months
   * can cause the resulting date to become invalid. If this occurs, then other fields, typically
   * the day-of-month, will be adjusted to ensure that the result is valid. Typically this will
   * select the last valid day of the month.
   *
   * The default implementation uses {@link #plusMonths(long)} .
   *
   * This instance is immutable and unaffected by this method call.
   *
   * @param monthsToSubtract
   *   the months to subtract, may be negative
   * @return
   *   a date based on this one with the months subtracted, not null
   * @throws DateTimeException
   *   if the result exceeds the supported date range
   */
  private[chrono] def minusMonths(monthsToSubtract: Long): ChronoLocalDateImpl[D] =
    if (monthsToSubtract == Long.MinValue) plusMonths(Long.MaxValue).plusMonths(1)
    else plusMonths(-monthsToSubtract)

  /**
   * Returns a copy of this date with the specified period in weeks subtracted.
   *
   * This subtracts the specified period in weeks to the date. In some cases, subtracting weeks can
   * cause the resulting date to become invalid. If this occurs, then other fields will be adjusted
   * to ensure that the result is valid.
   *
   * The default implementation uses {@link #plusWeeks(long)} .
   *
   * This instance is immutable and unaffected by this method call.
   *
   * @param weeksToSubtract
   *   the weeks to subtract, may be negative
   * @return
   *   a date based on this one with the weeks subtracted, not null
   * @throws DateTimeException
   *   if the result exceeds the supported date range
   */
  private[chrono] def minusWeeks(weeksToSubtract: Long): ChronoLocalDateImpl[D] =
    if (weeksToSubtract == Long.MinValue) plusWeeks(Long.MaxValue).plusWeeks(1)
    else plusWeeks(-weeksToSubtract)

  /**
   * Returns a copy of this date with the specified number of days subtracted.
   *
   * This subtracts the specified period in days to the date.
   *
   * The default implementation uses {@link #plusDays(long)} .
   *
   * This instance is immutable and unaffected by this method call.
   *
   * @param daysToSubtract
   *   the days to subtract, may be negative
   * @return
   *   a date based on this one with the days subtracted, not null
   * @throws DateTimeException
   *   if the result exceeds the supported date range
   */
  private[chrono] def minusDays(daysToSubtract: Long): ChronoLocalDateImpl[D] =
    if (daysToSubtract == Long.MinValue) plusDays(Long.MaxValue).plusDays(1)
    else plusDays(-daysToSubtract)

  override def atTime(localTime: LocalTime): ChronoLocalDateTime[_ <: ChronoLocalDate] =
    ChronoLocalDateTimeImpl.of(this, localTime)

  def until(endExclusive: Temporal, unit: TemporalUnit): Long = {
    val end: ChronoLocalDate = getChronology.date(endExclusive)
    if (unit.isInstanceOf[ChronoUnit])
      LocalDate.from(this).until(end, unit)
    else
      unit.between(this, end)
  }

  def until(endDate: ChronoLocalDate): ChronoPeriod =
    throw new UnsupportedOperationException("Not supported in ThreeTen backport")
}
