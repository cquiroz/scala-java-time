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

import org.scalatest.funsuite.AnyFunSuite

import org.threeten.bp.AssertionsHelper
import org.threeten.bp.temporal.ChronoField.ERA
import org.threeten.bp.temporal.ChronoField.YEAR
import org.threeten.bp.temporal.ChronoField.YEAR_OF_ERA
import org.threeten.bp.DateTimeException
import org.threeten.bp.LocalDate
import org.threeten.bp.LocalDateTime
import org.threeten.bp.Month
import org.threeten.bp.temporal.ChronoField
import org.threeten.bp.temporal.TemporalAdjusters

/** Test. */
class TestIsoChronology extends AnyFunSuite with AssertionsHelper {
  test("test_chrono_byName") {
    val c: Chronology    = IsoChronology.INSTANCE
    val test: Chronology = Chronology.of("ISO")
    assertNotNull(test, "The ISO calendar could not be found byName")
    assertEquals(test.getId, "ISO", "ID mismatch")
    assertEquals(test.getCalendarType, "iso8601", "Type mismatch")
    assertEquals(test, c)
  }

  test("tanceNotNull") {
    assertNotNull(IsoChronology.INSTANCE)
  }

  test("test_eraOf") {
    assertEquals(IsoChronology.INSTANCE.eraOf(0), IsoEra.BCE)
    assertEquals(IsoChronology.INSTANCE.eraOf(1), IsoEra.CE)
  }

  val data_samples: List[(ChronoLocalDate, LocalDate)] =
    List(
      (IsoChronology.INSTANCE.date(1, 7, 8), LocalDate.of(1, 7, 8)),
      (IsoChronology.INSTANCE.date(1, 7, 20), LocalDate.of(1, 7, 20)),
      (IsoChronology.INSTANCE.date(1, 7, 21), LocalDate.of(1, 7, 21)),
      (IsoChronology.INSTANCE.date(2, 7, 8), LocalDate.of(2, 7, 8)),
      (IsoChronology.INSTANCE.date(3, 6, 27), LocalDate.of(3, 6, 27)),
      (IsoChronology.INSTANCE.date(3, 5, 23), LocalDate.of(3, 5, 23)),
      (IsoChronology.INSTANCE.date(4, 6, 16), LocalDate.of(4, 6, 16)),
      (IsoChronology.INSTANCE.date(4, 7, 3), LocalDate.of(4, 7, 3)),
      (IsoChronology.INSTANCE.date(4, 7, 4), LocalDate.of(4, 7, 4)),
      (IsoChronology.INSTANCE.date(5, 1, 1), LocalDate.of(5, 1, 1)),
      (IsoChronology.INSTANCE.date(1727, 3, 3), LocalDate.of(1727, 3, 3)),
      (IsoChronology.INSTANCE.date(1728, 10, 28), LocalDate.of(1728, 10, 28)),
      (IsoChronology.INSTANCE.date(2012, 10, 29), LocalDate.of(2012, 10, 29))
    )

  test("test_toLocalDate") {
    data_samples.foreach { case (isoDate, iso) =>
      assertEquals(LocalDate.from(isoDate), iso)
    }
  }

  test("test_fromCalendrical") {
    data_samples.foreach { case (isoDate, iso) =>
      assertEquals(IsoChronology.INSTANCE.date(iso), isoDate)
    }
  }

  val data_badDates: List[(Int, Int, Int)] =
    List((2012, 0, 0),
         (2012, -1, 1),
         (2012, 0, 1),
         (2012, 14, 1),
         (2012, 15, 1),
         (2012, 1, -1),
         (2012, 1, 0),
         (2012, 1, 32),
         (2012, 12, -1),
         (2012, 12, 0),
         (2012, 12, 32)
    )

  test("test_badDates") {
    data_badDates.foreach { case (year, month, dom) =>
      assertThrows[DateTimeException] {
        IsoChronology.INSTANCE.date(year, month, dom)
      }
    }
  }

  test("test_date_withEra") {
    val year: Int             = 5
    val month: Int            = 5
    val dayOfMonth: Int       = 5
    val test: ChronoLocalDate = IsoChronology.INSTANCE.date(IsoEra.BCE, year, month, dayOfMonth)
    assertEquals(test.getEra, IsoEra.BCE)
    assertEquals(test.get(ChronoField.YEAR_OF_ERA), year)
    assertEquals(test.get(ChronoField.MONTH_OF_YEAR), month)
    assertEquals(test.get(ChronoField.DAY_OF_MONTH), dayOfMonth)
    assertEquals(test.get(YEAR), 1 + -1 * year)
    assertEquals(test.get(ERA), 0)
    assertEquals(test.get(YEAR_OF_ERA), year)
  }

  test("test_date_withEra_withWrongEra") {
    assertThrows[ClassCastException] {
      IsoChronology.INSTANCE.date(HijrahEra.AH.asInstanceOf[Era], 1, 1, 1)
    }
  }

  test("test_adjust1") {
    val base: ChronoLocalDate = IsoChronology.INSTANCE.date(1728, 10, 28)
    val test: ChronoLocalDate = base.`with`(TemporalAdjusters.lastDayOfMonth)
    assertEquals(test, IsoChronology.INSTANCE.date(1728, 10, 31))
  }

  test("test_adjust2") {
    val base: ChronoLocalDate = IsoChronology.INSTANCE.date(1728, 12, 2)
    val test: ChronoLocalDate = base.`with`(TemporalAdjusters.lastDayOfMonth)
    assertEquals(test, IsoChronology.INSTANCE.date(1728, 12, 31))
  }

  test("test_adjust_toLocalDate") {
    val isoDate: ChronoLocalDate = IsoChronology.INSTANCE.date(1726, 1, 4)
    val test: ChronoLocalDate    = isoDate.`with`(LocalDate.of(2012, 7, 6))
    assertEquals(test, IsoChronology.INSTANCE.date(2012, 7, 6))
  }

  test("test_adjust_toMonth") {
    val isoDate: ChronoLocalDate = IsoChronology.INSTANCE.date(1726, 1, 4)
    assertEquals(IsoChronology.INSTANCE.date(1726, 4, 4), isoDate.`with`(Month.APRIL))
  }

  test("test_LocalDate_adjustToISODate") {
    val isoDate: ChronoLocalDate = IsoChronology.INSTANCE.date(1728, 10, 29)
    val test: LocalDate          = LocalDate.MIN.`with`(isoDate)
    assertEquals(test, LocalDate.of(1728, 10, 29))
  }

  test("test_LocalDateTime_adjustToISODate") {
    val isoDate: ChronoLocalDate = IsoChronology.INSTANCE.date(1728, 10, 29)
    val test: LocalDateTime      = LocalDateTime.MIN.`with`(isoDate)
    assertEquals(test, LocalDateTime.of(1728, 10, 29, 0, 0))
  }

  val leapYearInformation: List[(Int, Boolean)] =
    List(
      (2000, true),
      (1996, true),
      (1600, true),
      (1900, false),
      (2100, false),
      (-500, false),
      (-400, true),
      (-300, false),
      (-100, false),
      (-5, false),
      (-4, true),
      (-3, false),
      (-2, false),
      (-1, false),
      (0, true),
      (1, false),
      (3, false),
      (4, true),
      (5, false),
      (100, false),
      (300, false),
      (400, true),
      (500, false)
    )

  test("test_isLeapYear") {
    leapYearInformation.foreach { case (year, isLeapYear) =>
      assertEquals(IsoChronology.INSTANCE.isLeapYear(year), isLeapYear)
    }
  }

  test("test_now") {
    assertEquals(LocalDate.from(IsoChronology.INSTANCE.dateNow), LocalDate.now)
  }

  val data_toString: List[(ChronoLocalDate, String)] =
    List(
      (IsoChronology.INSTANCE.date(1, 1, 1), "0001-01-01"),
      (IsoChronology.INSTANCE.date(1728, 10, 28), "1728-10-28"),
      (IsoChronology.INSTANCE.date(1728, 10, 29), "1728-10-29"),
      (IsoChronology.INSTANCE.date(1727, 12, 5), "1727-12-05"),
      (IsoChronology.INSTANCE.date(1727, 12, 6), "1727-12-06")
    )

  test("test_toString") {
    data_toString.foreach { case (isoDate, expected) =>
      assertEquals(isoDate.toString, expected)
    }
  }

  test("test_equals_true") {
    assertTrue(IsoChronology.INSTANCE == IsoChronology.INSTANCE)
  }

  test("test_equals_false") {
    assertFalse(IsoChronology.INSTANCE == HijrahChronology.INSTANCE)
  }
}
