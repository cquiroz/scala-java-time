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

import org.threeten.bp.temporal.ChronoField.DAY_OF_MONTH
import org.threeten.bp.temporal.TemporalAdjusters.previousOrSame
import org.threeten.bp.format.DateTimeFormatter
import org.threeten.bp.format.DateTimeFormatterBuilder
import org.threeten.bp.temporal.ChronoField
import org.threeten.bp.temporal.ChronoUnit
import org.threeten.bp.temporal.TemporalAccessor
import org.threeten.bp.temporal.TemporalField

/** Usability class for package. */
class UsabilityBasic {
  def main(): Unit = {
    simpleCalendar()
    System.out.println("------")
    lookup()
    System.out.println("------")
    period()
    System.out.println("------")
    print1()
    System.out.println("------")
    print2()
  }

  private def simpleCalendar(): Unit = {
    var date: LocalDate = LocalDate.now
    System.out.println(date)
    date = date.withDayOfMonth(1)
    System.out.println(date)
    val month: Int      = date.getMonth.getValue
    date = date.`with`(previousOrSame(DayOfWeek.MONDAY))
    System.out.println(date)
    while (date.getMonth.getValue <= month) {
      var row: String = ""

      {
        var i: Int = 0
        while (i < 7) {
          {
            row += date.getDayOfMonth + " "
            date = date.plusDays(1)
          }
          {
            i += 1
            i - 1
          }
        }
      }
      System.out.println(row)
    }
  }

  private def lookup(): Unit = {
    val date: LocalDate         = LocalDate.now
    val time: LocalTime         = LocalTime.now
    val dateTime: LocalDateTime = LocalDateTime.now
    output(date, ChronoField.DAY_OF_MONTH)
    output(date, ChronoField.MONTH_OF_YEAR)
    output(date, ChronoField.YEAR)
    output(dateTime, ChronoField.DAY_OF_MONTH)
    output(time, ChronoField.HOUR_OF_DAY)
    output(time, ChronoField.MINUTE_OF_HOUR)
    val cal: TemporalAccessor   = date
    System.out.println("DoM: " + cal.get(DAY_OF_MONTH))
  }

  protected def output(date: LocalDate, field: TemporalField): Unit =
    System.out.println(field + " " + date.getLong(field))

  protected def output(dateTime: LocalDateTime, field: TemporalField): Unit =
    System.out.println(field + " " + dateTime.getLong(field))

  protected def output(time: LocalTime, field: TemporalField): Unit =
    System.out.println(field + " " + time.getLong(field))

  private def period(): Unit = {
    var date1: LocalDate = LocalDate.now
    var date2: LocalDate = LocalDate.now.plusDays(25367)
    System.out.println(ChronoUnit.DAYS.between(date1, date2))
    System.out.println(ChronoUnit.YEARS.between(date1, date2))
    date1 = LocalDate.of(2012, 2, 20)
    date2 = LocalDate.of(2014, 2, 19)
    System.out.println(ChronoUnit.YEARS.between(date1, date2))
    date2 = LocalDate.of(2014, 2, 20)
    System.out.println(ChronoUnit.YEARS.between(date1, date2))
    date2 = LocalDate.of(2014, 2, 21)
    System.out.println(ChronoUnit.YEARS.between(date1, date2))
    date2 = LocalDate.of(2010, 2, 19)
    System.out.println(ChronoUnit.YEARS.between(date1, date2))
    date2 = LocalDate.of(2010, 2, 20)
    System.out.println(ChronoUnit.YEARS.between(date1, date2))
    date2 = LocalDate.of(2010, 2, 21)
    System.out.println(ChronoUnit.YEARS.between(date1, date2))
    val date3: LocalDate = LocalDate.now.plus(3, ChronoUnit.DAYS)
    System.out.println("3 days later " + date3)
  }

  private def print1(): Unit = {
    val f: DateTimeFormatter = new DateTimeFormatterBuilder
      .appendText(ChronoField.AMPM_OF_DAY)
      .appendLiteral(' ')
      .appendValue(ChronoField.AMPM_OF_DAY)
      .toFormatter
    System.out.println(f.format(LocalTime.of(12, 30)))
    System.out.println(f.format(ZonedDateTime.now))
  }

  private def print2(): Unit = {
    val f: DateTimeFormatter = new DateTimeFormatterBuilder
      .appendText(ChronoField.MONTH_OF_YEAR)
      .appendLiteral(' ')
      .appendValue(ChronoField.YEAR)
      .toFormatter
    System.out.println(f.format(LocalDate.now))
    System.out.println(f.format(YearMonth.now))
    System.out.println(f.format(ZonedDateTime.now))
  }
}
