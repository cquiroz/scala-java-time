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

import org.threeten.bp.chrono.MinguoChronology.YEARS_DIFFERENCE
import org.threeten.bp.temporal.ChronoField._
import java.io.Serializable
import org.threeten.bp.Clock
import org.threeten.bp.LocalDate
import org.threeten.bp.LocalTime
import org.threeten.bp.Period
import org.threeten.bp.ZoneId
import org.threeten.bp.temporal.ChronoField
import org.threeten.bp.temporal.TemporalAccessor
import org.threeten.bp.temporal.TemporalAdjuster
import org.threeten.bp.temporal.TemporalAmount
import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.temporal.TemporalUnit
import org.threeten.bp.temporal.UnsupportedTemporalTypeException
import org.threeten.bp.temporal.ValueRange

object MinguoDate {

  /**
   * Obtains the current {@code MinguoDate} from the system clock in the default time-zone.
   *
   * This will query the {@link Clock#systemDefaultZone() system clock} in the default time-zone to
   * obtain the current date.
   *
   * Using this method will prevent the ability to use an alternate clock for testing because the
   * clock is hard-coded.
   *
   * @return
   *   the current date using the system clock and default time-zone, not null
   */
  def now: MinguoDate = now(Clock.systemDefaultZone)

  /**
   * Obtains the current {@code MinguoDate} from the system clock in the specified time-zone.
   *
   * This will query the {@link Clock#system(ZoneId) system clock} to obtain the current date.
   * Specifying the time-zone avoids dependence on the default time-zone.
   *
   * Using this method will prevent the ability to use an alternate clock for testing because the
   * clock is hard-coded.
   *
   * @param zone
   *   the zone ID to use, not null
   * @return
   *   the current date using the system clock, not null
   */
  def now(zone: ZoneId): MinguoDate = now(Clock.system(zone))

  /**
   * Obtains the current {@code MinguoDate} from the specified clock.
   *
   * This will query the specified clock to obtain the current date - today. Using this method
   * allows the use of an alternate clock for testing. The alternate clock may be introduced using
   * {@linkplain Clock dependency injection}.
   *
   * @param clock
   *   the clock to use, not null
   * @return
   *   the current date, not null
   * @throws DateTimeException
   *   if the current date cannot be obtained
   */
  def now(clock: Clock): MinguoDate = new MinguoDate(LocalDate.now(clock))

  /**
   * Obtains a {@code MinguoDate} representing a date in the Minguo calendar system from the
   * proleptic-year, month-of-year and day-of-month fields.
   *
   * This returns a {@code MinguoDate} with the specified fields. The day must be valid for the year
   * and month, otherwise an exception will be thrown.
   *
   * @param prolepticYear
   *   the Minguo proleptic-year
   * @param month
   *   the Minguo month-of-year, from 1 to 12
   * @param dayOfMonth
   *   the Minguo day-of-month, from 1 to 31
   * @return
   *   the date in Minguo calendar system, not null
   * @throws DateTimeException
   *   if the value of any field is out of range, or if the day-of-month is invalid for the
   *   month-year
   */
  def of(prolepticYear: Int, month: Int, dayOfMonth: Int): MinguoDate =
    MinguoChronology.INSTANCE.date(prolepticYear, month, dayOfMonth)

  /**
   * Obtains a {@code MinguoDate} from a temporal object.
   *
   * This obtains a date in the Minguo calendar system based on the specified temporal. A {@code
   * TemporalAccessor} represents an arbitrary set of date and time information, which this factory
   * converts to an instance of {@code MinguoDate}.
   *
   * The conversion typically uses the {@link ChronoField#EPOCH_DAY EPOCH_DAY} field, which is
   * standardized across calendar systems.
   *
   * This method matches the signature of the functional interface {@link TemporalQuery} allowing it
   * to be used as a query via method reference, {@code MinguoDate::from}.
   *
   * @param temporal
   *   the temporal object to convert, not null
   * @return
   *   the date in Minguo calendar system, not null
   * @throws DateTimeException
   *   if unable to convert to a { @code MinguoDate}
   */
  def from(temporal: TemporalAccessor): MinguoDate = MinguoChronology.INSTANCE.date(temporal)

}

/**
 * A date in the Minguo calendar system.
 *
 * This date operates using the {@linkplain MinguoChronology Minguo calendar}. This calendar system
 * is primarily used in the Republic of China, often known as Taiwan. Dates are aligned such that
 * {@code 0001-01-01 (Minguo)} is {@code 1912-01-01 (ISO)}.
 *
 * <h3>Specification for implementors</h3> This class is immutable and thread-safe.
 *
 * @constructor
 *   Creates an instance from an ISO date.
 *
 * @param isoDate
 *   the standard local date, validated not null
 */
final class MinguoDate private[chrono] (private val isoDate: LocalDate)
    extends ChronoDateImpl[MinguoDate]
    with Serializable {
  Objects.requireNonNull(isoDate, "date")

  def getChronology: MinguoChronology = MinguoChronology.INSTANCE

  override def getEra: MinguoEra = super.getEra.asInstanceOf[MinguoEra]

  def lengthOfMonth: Int = isoDate.lengthOfMonth

  override def range(field: TemporalField): ValueRange =
    if (field.isInstanceOf[ChronoField])
      if (isSupported(field)) {
        val f: ChronoField = field.asInstanceOf[ChronoField]
        f match {
          case DAY_OF_MONTH | DAY_OF_YEAR | ALIGNED_WEEK_OF_MONTH => isoDate.range(field)
          case YEAR_OF_ERA                                        =>
            val range: ValueRange = YEAR.range
            val max: Long         =
              if (getProlepticYear <= 0) -range.getMinimum + 1 + YEARS_DIFFERENCE
              else range.getMaximum - YEARS_DIFFERENCE
            ValueRange.of(1, max)
          case _                                                  => getChronology.range(f)
        }
      } else
        throw new UnsupportedTemporalTypeException(s"Unsupported field: $field")
    else
      field.rangeRefinedBy(this)

  def getLong(field: TemporalField): Long =
    if (field.isInstanceOf[ChronoField])
      field.asInstanceOf[ChronoField] match {
        case PROLEPTIC_MONTH => getProlepticMonth
        case YEAR_OF_ERA     =>
          val prolepticYear: Int = getProlepticYear
          if (prolepticYear >= 1) prolepticYear.toLong else 1 - prolepticYear.toLong
        case YEAR            => getProlepticYear.toLong
        case ERA             => if (getProlepticYear >= 1) 1 else 0
        case _               => isoDate.getLong(field)
      }
    else
      field.getFrom(this)

  private def getProlepticMonth: Long = getProlepticYear * 12L + isoDate.getMonthValue - 1

  private def getProlepticYear: Int = isoDate.getYear - YEARS_DIFFERENCE

  override def `with`(adjuster: TemporalAdjuster): MinguoDate =
    super.`with`(adjuster).asInstanceOf[MinguoDate]

  def `with`(field: TemporalField, newValue: Long): MinguoDate =
    if (field.isInstanceOf[ChronoField]) {
      val f: ChronoField = field.asInstanceOf[ChronoField]
      if (getLong(f) == newValue)
        this
      else
        f match {
          case PROLEPTIC_MONTH          =>
            getChronology.range(f).checkValidValue(newValue, f)
            plusMonths(newValue - getProlepticMonth)
          case YEAR_OF_ERA | YEAR | ERA =>
            val nvalue: Int = getChronology.range(f).checkValidIntValue(newValue, f)
            f match {
              case YEAR_OF_ERA =>
                `with`(
                  isoDate.withYear(
                    if (getProlepticYear >= 1) nvalue + YEARS_DIFFERENCE
                    else (1 - nvalue) + YEARS_DIFFERENCE
                  )
                )
              case YEAR        => `with`(isoDate.withYear(nvalue + YEARS_DIFFERENCE))
              case ERA         => `with`(isoDate.withYear((1 - getProlepticYear) + YEARS_DIFFERENCE))
              case _           => throw new UnsupportedOperationException()
            }
          case _                        => `with`(isoDate.`with`(field, newValue))
        }
    } else
      field.adjustInto(this, newValue)

  override def plus(amount: TemporalAmount): MinguoDate =
    super.plus(amount).asInstanceOf[MinguoDate]

  override def plus(amountToAdd: Long, unit: TemporalUnit): MinguoDate =
    super.plus(amountToAdd, unit).asInstanceOf[MinguoDate]

  override def minus(amount: TemporalAmount): MinguoDate =
    super.minus(amount).asInstanceOf[MinguoDate]

  override def minus(amountToAdd: Long, unit: TemporalUnit): MinguoDate =
    super.minus(amountToAdd, unit).asInstanceOf[MinguoDate]

  private[chrono] def plusYears(years: Long): MinguoDate = `with`(isoDate.plusYears(years))

  private[chrono] def plusMonths(months: Long): MinguoDate = `with`(isoDate.plusMonths(months))

  private[chrono] def plusDays(days: Long): MinguoDate = `with`(isoDate.plusDays(days))

  private def `with`(newDate: LocalDate): MinguoDate =
    if (newDate == isoDate) this else new MinguoDate(newDate)

  override def atTime(localTime: LocalTime): ChronoLocalDateTime[MinguoDate] =
    super.atTime(localTime).asInstanceOf[ChronoLocalDateTime[MinguoDate]]

  override def until(endDate: ChronoLocalDate): ChronoPeriod = {
    val period: Period = isoDate.until(endDate)
    getChronology.period(period.getYears, period.getMonths, period.getDays)
  }

  override def toEpochDay: Long = isoDate.toEpochDay

  override def equals(obj: Any): Boolean =
    obj match {
      case otherDate: MinguoDate => (this eq otherDate) || this.isoDate == otherDate.isoDate
      case _                     => false
    }

  override def hashCode: Int = getChronology.getId.hashCode ^ isoDate.hashCode

}
