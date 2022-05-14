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

import org.scalatest.funsuite.AnyFunSuite
import org.threeten.bp.DayOfWeek._
import org.threeten.bp.{ AssertionsHelper, DayOfWeek, LocalDate }
import org.threeten.bp.format.{ DateTimeFormatter, DateTimeFormatterBuilder }
import org.threeten.bp.temporal.ChronoField.DAY_OF_WEEK

/** Test. */
class TestIsoFields extends AnyFunSuite with AssertionsHelper {
  test("enum") {
    assertTrue(IsoFields.WEEK_OF_WEEK_BASED_YEAR.isInstanceOf[Enum[_]])
    assertTrue(IsoFields.WEEK_BASED_YEAR.isInstanceOf[Enum[_]])
    assertTrue(IsoFields.WEEK_BASED_YEARS.isInstanceOf[Enum[_]])
  }

  def data_week: List[List[Any]] =
    List(
      List(LocalDate.of(1969, 12, 29), MONDAY, 1, 1970),
      List(LocalDate.of(2012, 12, 23), SUNDAY, 51, 2012),
      List(LocalDate.of(2012, 12, 24), MONDAY, 52, 2012),
      List(LocalDate.of(2012, 12, 27), THURSDAY, 52, 2012),
      List(LocalDate.of(2012, 12, 28), FRIDAY, 52, 2012),
      List(LocalDate.of(2012, 12, 29), SATURDAY, 52, 2012),
      List(LocalDate.of(2012, 12, 30), SUNDAY, 52, 2012),
      List(LocalDate.of(2012, 12, 31), MONDAY, 1, 2013),
      List(LocalDate.of(2013, 1, 1), TUESDAY, 1, 2013),
      List(LocalDate.of(2013, 1, 2), WEDNESDAY, 1, 2013),
      List(LocalDate.of(2013, 1, 6), SUNDAY, 1, 2013),
      List(LocalDate.of(2013, 1, 7), MONDAY, 2, 2013)
    )

  test("WOWBY") {
    data_week.foreach {
      case (date: LocalDate) :: (dow: DayOfWeek) :: (week: Int) :: (_: Int) :: Nil =>
        assertEquals(date.getDayOfWeek, dow)
        assertEquals(IsoFields.WEEK_OF_WEEK_BASED_YEAR.getFrom(date), week)
        assertEquals(date.get(IsoFields.WEEK_OF_WEEK_BASED_YEAR), week)
      case _                                                                       =>
        fail()
    }
  }

  test("WBY") {
    data_week.foreach {
      case (date: LocalDate) :: (dow: DayOfWeek) :: (_: Int) :: (wby: Int) :: Nil =>
        assertEquals(date.getDayOfWeek, dow)
        assertEquals(IsoFields.WEEK_BASED_YEAR.getFrom(date), wby)
        assertEquals(date.get(IsoFields.WEEK_BASED_YEAR), wby)
      case _                                                                      =>
        fail()
    }
  }

  test("parse_weeks") {
    data_week.foreach {
      case (date: LocalDate) :: (dow: DayOfWeek) :: (week: Int) :: (wby: Int) :: Nil =>
        val f: DateTimeFormatter = new DateTimeFormatterBuilder
          .appendValue(IsoFields.WEEK_BASED_YEAR)
          .appendLiteral('-')
          .appendValue(IsoFields.WEEK_OF_WEEK_BASED_YEAR)
          .appendLiteral('-')
          .appendValue(DAY_OF_WEEK)
          .toFormatter
        val parsed: LocalDate    = LocalDate.parse(wby + "-" + week + "-" + dow.getValue, f)
        assertEquals(parsed, date)
      case _                                                                         =>
        fail()
    }
  }

  test("loop") {
    var date: LocalDate = LocalDate.of(1960, 1, 5)
    var year: Int       = 1960
    var wby: Int        = 1960
    var weekLen: Int    = 52
    var week: Int       = 1
    while (date.getYear < 2400) {
      val loopDow: DayOfWeek = date.getDayOfWeek
      if (date.getYear != year)
        year = date.getYear
      if (loopDow eq MONDAY) {
        week += 1
        if (week == 53 && weekLen == 52 || week == 54) {
          week = 1
          val firstDayOfWeekBasedYear: LocalDate = date.plusDays(14).withDayOfYear(1)
          val firstDay: DayOfWeek                = firstDayOfWeekBasedYear.getDayOfWeek
          weekLen = if (
            (firstDay eq THURSDAY) || (firstDay eq WEDNESDAY) && firstDayOfWeekBasedYear.isLeapYear
          )
            53
          else 52
          wby += 1
        }
      }
      assertEquals(IsoFields.WEEK_OF_WEEK_BASED_YEAR.rangeRefinedBy(date),
                   ValueRange.of(1, weekLen),
                   "Failed on " + date + " " + date.getDayOfWeek
      )
      assertEquals(IsoFields.WEEK_OF_WEEK_BASED_YEAR.getFrom(date),
                   week,
                   "Failed on " + date + " " + date.getDayOfWeek
      )
      assertEquals(date.get(IsoFields.WEEK_OF_WEEK_BASED_YEAR),
                   week,
                   "Failed on " + date + " " + date.getDayOfWeek
      )
      assertEquals(IsoFields.WEEK_BASED_YEAR.getFrom(date),
                   wby,
                   "Failed on " + date + " " + date.getDayOfWeek
      )
      assertEquals(date.get(IsoFields.WEEK_BASED_YEAR),
                   wby,
                   "Failed on " + date + " " + date.getDayOfWeek
      )
      date = date.plusDays(1)
    }
  }

  def data_quartersBetween: List[List[Any]] =
    List(
      List(LocalDate.of(2000, 1, 1), LocalDate.of(2000, 1, 1), 0),
      List(LocalDate.of(2000, 1, 1), LocalDate.of(2000, 1, 2), 0),
      List(LocalDate.of(2000, 1, 1), LocalDate.of(2000, 2, 1), 0),
      List(LocalDate.of(2000, 1, 1), LocalDate.of(2000, 3, 1), 0),
      List(LocalDate.of(2000, 1, 1), LocalDate.of(2000, 3, 31), 0),
      List(LocalDate.of(2000, 1, 1), LocalDate.of(2000, 4, 1), 1),
      List(LocalDate.of(2000, 1, 1), LocalDate.of(2000, 4, 2), 1),
      List(LocalDate.of(2000, 1, 1), LocalDate.of(2000, 6, 30), 1),
      List(LocalDate.of(2000, 1, 1), LocalDate.of(2000, 7, 1), 2),
      List(LocalDate.of(2000, 1, 1), LocalDate.of(2000, 10, 1), 3),
      List(LocalDate.of(2000, 1, 1), LocalDate.of(2000, 12, 31), 3),
      List(LocalDate.of(2000, 1, 1), LocalDate.of(2001, 1, 1), 4),
      List(LocalDate.of(2000, 1, 1), LocalDate.of(2002, 1, 1), 8),
      List(LocalDate.of(2000, 1, 1), LocalDate.of(1999, 12, 31), 0),
      List(LocalDate.of(2000, 1, 1), LocalDate.of(1999, 10, 2), 0),
      List(LocalDate.of(2000, 1, 1), LocalDate.of(1999, 10, 1), -1),
      List(LocalDate.of(2000, 1, 1), LocalDate.of(1999, 7, 2), -1),
      List(LocalDate.of(2000, 1, 1), LocalDate.of(1999, 7, 1), -2),
      List(LocalDate.of(2000, 1, 1), LocalDate.of(1999, 4, 2), -2),
      List(LocalDate.of(2000, 1, 1), LocalDate.of(1999, 4, 1), -3),
      List(LocalDate.of(2000, 1, 1), LocalDate.of(1999, 1, 2), -3),
      List(LocalDate.of(2000, 1, 1), LocalDate.of(1999, 1, 1), -4),
      List(LocalDate.of(2000, 1, 1), LocalDate.of(1998, 12, 31), -4),
      List(LocalDate.of(2000, 1, 1), LocalDate.of(1998, 10, 2), -4),
      List(LocalDate.of(2000, 1, 1), LocalDate.of(1998, 10, 1), -5)
    )

  test("quarters_between") {
    data_quartersBetween.foreach {
      case (start: LocalDate) :: (end: LocalDate) :: (expected: Int) :: Nil =>
        assertEquals(IsoFields.QUARTER_YEARS.between(start, end), expected)
      case _                                                                =>
        fail()
    }
  }
}
