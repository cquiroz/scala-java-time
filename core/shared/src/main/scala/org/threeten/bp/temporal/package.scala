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

/**
 * Access to date and time using fields and units.
 *
 * This package expands on the base package to provide additional functionality for more powerful
 * use cases. Support is included for:
 *
 *   - Units of date-time, such as years, months, days and hours
 *   - Fields of date-time, such as month-of-year, day-of-week or hour-of-day
 *   - Date-time adjustment functions
 *   - Different definitions of weeks
 *
 * ==Fields and Units==
 *
 * Dates and times are expressed in terms of fields and units. A unit is used to measure an amount
 * of time, such as years, days or minutes. All units implement {@link
 * org.threeten.bp.temporal.TemporalUnit}. The set of well known units is defined in {@link
 * org.threeten.bp.temporal.ChronoUnit}, for example, {@link
 * org.threeten.bp.temporal.ChronoUnit#DAYS}. The unit interface is designed to allow applications
 * to add their own units.
 *
 * A field is used to express part of a larger date-time, such as year, month-of-year or
 * second-of-minute. All fields implement {@link org.threeten.bp.temporal.TemporalField}. The set of
 * well known fields are defined in {@link org.threeten.bp.temporal.ChronoField}, for example,
 * {@link org.threeten.bp.temporal.ChronoField#HOUR_OF_DAY}. An additional fields are defined by
 * {@link org.threeten.bp.temporal.JulianFields}. The field interface is designed to allow
 * applications to add their own fields.
 *
 * This package provides tools that allow the units and fields of date and time to be accessed in a
 * general way most suited for frameworks. {@link org.threeten.bp.temporal.Temporal} provides the
 * abstraction for date time types that support fields. Its methods support getting the value of a
 * field, creating a new date time with the value of a field modified, and extracting another date
 * time type, typically used to extract the offset or time-zone.
 *
 * One use of fields in application code is to retrieve fields for which there is no convenience
 * method. For example, getting the day-of-month is common enough that there is a method on {@code
 * LocalDate} called {@code getDayOfMonth()}. However for more unusual fields it is necessary to use
 * the field. For example, {@code date.get(ChronoField.ALIGNED_WEEK_OF_MONTH)}. The fields also
 * provide access to the range of valid values.
 *
 * ==Adjustment==
 *
 * A key part of the date-time problem space is adjusting a date to a new, related value, such as
 * the "last day of the month", or "next Wednesday". These are modeled as functions that adjust a
 * base date-time. The functions implement {@link org.threeten.bp.temporal.TemporalAdjuster} and
 * operate on {@link org.threeten.bp.temporal.Temporal}. A set of common functions are provided in
 * {@link org.threeten.bp.temporal.TemporalAdjusters}. For example, to find the first occurrence of
 * a day-of-week after a given date, use {@link
 * org.threeten.bp.temporal.TemporalAdjusters#next(DayOfWeek)}, such as {@code
 * date.with(next(MONDAY))}.
 *
 * ==Weeks==
 *
 * Different locales have different definitions of the week. For example, in Europe the week
 * typically starts on a Monday, while in the US it starts on a Sunday. The {@link
 * org.threeten.bp.temporal.WeekFields} class models this distinction.
 *
 * The ISO calendar system defines an additional week-based division of years. This defines a year
 * based on whole Monday to Monday weeks. This is modeled in {@link
 * org.threeten.bp.temporal.IsoFields}.
 */
package object temporal {

  private[temporal] def WWBY = "WeekOfYWeekBasedYear"
  private[temporal] def WBY  = "WeekBasedYear"

}
