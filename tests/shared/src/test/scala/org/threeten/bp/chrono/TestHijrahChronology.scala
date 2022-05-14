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
import org.scalatest.BeforeAndAfterEach

import org.threeten.bp.AssertionsHelper
import org.threeten.bp.DateTimeException
import org.threeten.bp.LocalDate
import org.threeten.bp.LocalDateTime
import org.threeten.bp.Month
import org.threeten.bp.temporal.TemporalAdjusters
import org.threeten.bp.temporal.ChronoField.ALIGNED_DAY_OF_WEEK_IN_MONTH
import org.threeten.bp.temporal.ChronoField.ALIGNED_WEEK_OF_MONTH

/** Test. */
class TestHijrahChronology extends AnyFunSuite with BeforeAndAfterEach with AssertionsHelper {
  test("test_chrono_byName") {
    val c: Chronology    = HijrahChronology.INSTANCE
    val test: Chronology = Chronology.of("Hijrah")
    assertNotNull(test, "The Hijrah calendar could not be found byName")
    assertEquals(test.getId, "Hijrah-umalqura", "ID mismatch")
    assertEquals(test.getCalendarType, "islamic-umalqura", "Type mismatch")
    assertEquals(test, c)
  }

  val data_samples: List[(ChronoLocalDate, LocalDate)] =
    List(
      (HijrahChronology.INSTANCE.date(1, 1, 1), LocalDate.of(622, 7, 19)),
      (HijrahChronology.INSTANCE.date(1, 1, 2), LocalDate.of(622, 7, 20)),
      (HijrahChronology.INSTANCE.date(1, 1, 3), LocalDate.of(622, 7, 21)),
      (HijrahChronology.INSTANCE.date(2, 1, 1), LocalDate.of(623, 7, 8)),
      (HijrahChronology.INSTANCE.date(3, 1, 1), LocalDate.of(624, 6, 27)),
      (HijrahChronology.INSTANCE.date(3, 12, 6), LocalDate.of(625, 5, 23)),
      (HijrahChronology.INSTANCE.date(4, 1, 1), LocalDate.of(625, 6, 16)),
      (HijrahChronology.INSTANCE.date(4, 7, 3), LocalDate.of(625, 12, 12)),
      (HijrahChronology.INSTANCE.date(4, 7, 4), LocalDate.of(625, 12, 13)),
      (HijrahChronology.INSTANCE.date(5, 1, 1), LocalDate.of(626, 6, 5)),
      (HijrahChronology.INSTANCE.date(1662, 3, 3), LocalDate.of(2234, 4, 3)),
      (HijrahChronology.INSTANCE.date(1728, 10, 28), LocalDate.of(2298, 12, 3)),
      (HijrahChronology.INSTANCE.date(1728, 10, 29), LocalDate.of(2298, 12, 4))
    )

  test("test_toLocalDate") {
    data_samples.foreach { case (hijrahDate, iso) =>
      assertEquals(LocalDate.from(hijrahDate), iso)
    }
  }

  test("test_fromCalendrical") {
    data_samples.foreach { case (hijrahDate, iso) =>
      assertEquals(HijrahChronology.INSTANCE.date(iso), hijrahDate)
    }
  }

  val data_badDates: List[(Int, Int, Int)] =
    List((1728, 0, 0),
         (1728, -1, 1),
         (1728, 0, 1),
         (1728, 14, 1),
         (1728, 15, 1),
         (1728, 1, -1),
         (1728, 1, 0),
         (1728, 1, 32),
         (1728, 12, -1),
         (1728, 12, 0),
         (1728, 12, 32)
    )

  test("test_badDates") {
    data_badDates.foreach { case (year, month, dom) =>
      assertThrows[DateTimeException] {
        HijrahChronology.INSTANCE.date(year, month, dom)
      }
    }
  }

  test("test_alignedDayOfWeekInMonth") {
    var dom = 1
    while (dom <= 29) {
      var date: HijrahDate = HijrahChronology.INSTANCE.date(1728, 10, dom)
      assertEquals(date.getLong(ALIGNED_WEEK_OF_MONTH), (dom - 1) / 7 + 1)
      assertEquals(date.getLong(ALIGNED_DAY_OF_WEEK_IN_MONTH), (dom - 1) % 7 + 1)
      date = date.plusDays(1)
      dom += 1
    }
  }

  test("test_adjust1") {
    val base: ChronoLocalDate = HijrahChronology.INSTANCE.date(1728, 10, 28)
    val test: ChronoLocalDate = base.`with`(TemporalAdjusters.lastDayOfMonth)
    assertEquals(test, HijrahChronology.INSTANCE.date(1728, 10, 29))
  }

  test("test_adjust2") {
    val base: ChronoLocalDate = HijrahChronology.INSTANCE.date(1728, 12, 2)
    val test: ChronoLocalDate = base.`with`(TemporalAdjusters.lastDayOfMonth)
    assertEquals(test, HijrahChronology.INSTANCE.date(1728, 12, 30))
  }

  test("test_adjust_toLocalDate") {
    val hijrahDate: ChronoLocalDate = HijrahChronology.INSTANCE.date(1726, 1, 4)
    val test: ChronoLocalDate       = hijrahDate.`with`(LocalDate.of(2012, 7, 6))
    assertEquals(test, HijrahChronology.INSTANCE.date(1433, 8, 16))
  }

  test("test_adjust_toMonth") {
    assertThrows[DateTimeException] {
      val hijrahDate: ChronoLocalDate = HijrahChronology.INSTANCE.date(1726, 1, 4)
      hijrahDate.`with`(Month.APRIL)
    }
  }

  test("test_LocalDate_adjustToHijrahDate") {
    val hijrahDate: ChronoLocalDate = HijrahChronology.INSTANCE.date(1728, 10, 29)
    val test: LocalDate             = LocalDate.MIN.`with`(hijrahDate)
    assertEquals(test, LocalDate.of(2298, 12, 4))
  }

  test("test_LocalDateTime_adjustToHijrahDate") {
    val hijrahDate: ChronoLocalDate = HijrahChronology.INSTANCE.date(1728, 10, 29)
    val test: LocalDateTime         = LocalDateTime.MIN.`with`(hijrahDate)
    assertEquals(test, LocalDateTime.of(2298, 12, 4, 0, 0))
  }

  val data_toString: List[(ChronoLocalDate, String)] =
    List(
      (HijrahChronology.INSTANCE.date(1, 1, 1), "Hijrah-umalqura AH 1-01-01"),
      (HijrahChronology.INSTANCE.date(1728, 10, 28), "Hijrah-umalqura AH 1728-10-28"),
      (HijrahChronology.INSTANCE.date(1728, 10, 29), "Hijrah-umalqura AH 1728-10-29"),
      (HijrahChronology.INSTANCE.date(1727, 12, 5), "Hijrah-umalqura AH 1727-12-05"),
      (HijrahChronology.INSTANCE.date(1727, 12, 6), "Hijrah-umalqura AH 1727-12-06")
    )

  test("test_toString") {
    data_toString.foreach { case (hijrahDate, expected) =>
      assertEquals(hijrahDate.toString, expected)
    }
  }

  test("test_equals_true") {
    assertTrue(HijrahChronology.INSTANCE == HijrahChronology.INSTANCE)
  }

  test("test_equals_false") {
    assertFalse(HijrahChronology.INSTANCE == IsoChronology.INSTANCE)
  }
}
