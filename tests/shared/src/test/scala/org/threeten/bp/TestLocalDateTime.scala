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

import org.threeten.bp.temporal.ChronoField.ALIGNED_DAY_OF_WEEK_IN_MONTH
import org.threeten.bp.temporal.ChronoField.ALIGNED_DAY_OF_WEEK_IN_YEAR
import org.threeten.bp.temporal.ChronoField.ALIGNED_WEEK_OF_MONTH
import org.threeten.bp.temporal.ChronoField.ALIGNED_WEEK_OF_YEAR
import org.threeten.bp.temporal.ChronoField.AMPM_OF_DAY
import org.threeten.bp.temporal.ChronoField.CLOCK_HOUR_OF_AMPM
import org.threeten.bp.temporal.ChronoField.CLOCK_HOUR_OF_DAY
import org.threeten.bp.temporal.ChronoField.DAY_OF_MONTH
import org.threeten.bp.temporal.ChronoField.DAY_OF_WEEK
import org.threeten.bp.temporal.ChronoField.DAY_OF_YEAR
import org.threeten.bp.temporal.ChronoField.EPOCH_DAY
import org.threeten.bp.temporal.ChronoField.ERA
import org.threeten.bp.temporal.ChronoField.HOUR_OF_AMPM
import org.threeten.bp.temporal.ChronoField.HOUR_OF_DAY
import org.threeten.bp.temporal.ChronoField.MICRO_OF_DAY
import org.threeten.bp.temporal.ChronoField.MICRO_OF_SECOND
import org.threeten.bp.temporal.ChronoField.MILLI_OF_DAY
import org.threeten.bp.temporal.ChronoField.MILLI_OF_SECOND
import org.threeten.bp.temporal.ChronoField.MINUTE_OF_DAY
import org.threeten.bp.temporal.ChronoField.MINUTE_OF_HOUR
import org.threeten.bp.temporal.ChronoField.MONTH_OF_YEAR
import org.threeten.bp.temporal.ChronoField.NANO_OF_DAY
import org.threeten.bp.temporal.ChronoField.NANO_OF_SECOND
import org.threeten.bp.temporal.ChronoField.PROLEPTIC_MONTH
import org.threeten.bp.temporal.ChronoField.SECOND_OF_DAY
import org.threeten.bp.temporal.ChronoField.SECOND_OF_MINUTE
import org.threeten.bp.temporal.ChronoField.YEAR
import org.threeten.bp.temporal.ChronoField.YEAR_OF_ERA
import org.threeten.bp.temporal.ChronoUnit.HALF_DAYS
import org.threeten.bp.temporal.ChronoUnit.HOURS
import org.threeten.bp.temporal.ChronoUnit.MICROS
import org.threeten.bp.temporal.ChronoUnit.MILLIS
import org.threeten.bp.temporal.ChronoUnit.MINUTES
import org.threeten.bp.temporal.ChronoUnit.NANOS
import org.threeten.bp.temporal.ChronoUnit.SECONDS
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.BeforeAndAfter
import org.threeten.bp.chrono.IsoChronology
import org.threeten.bp.format.DateTimeFormatter
import org.threeten.bp.format.DateTimeParseException
import org.threeten.bp.temporal.ChronoField
import org.threeten.bp.temporal.ChronoUnit
import org.threeten.bp.temporal.JulianFields
import org.threeten.bp.temporal.MockFieldNoValue
import org.threeten.bp.temporal.Temporal
import org.threeten.bp.temporal.TemporalAccessor
import org.threeten.bp.temporal.TemporalAdjuster
import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.temporal.TemporalQueries
import org.threeten.bp.temporal.TemporalUnit

/** Test LocalDateTime. */
object TestLocalDateTime {
  private val OFFSET_PONE: ZoneOffset = ZoneOffset.ofHours(1)
  private val OFFSET_PTWO: ZoneOffset = ZoneOffset.ofHours(2)
  private val OFFSET_MTWO: ZoneOffset = ZoneOffset.ofHours(-2)
  private val ZONE_PARIS: ZoneId      = ZoneId.of("Europe/Paris")
  private val ZONE_GAZA: ZoneId       = ZoneId.of("Asia/Gaza")
}

class TestLocalDateTime
    extends AnyFunSuite
    with GenDateTimeTest
    with AssertionsHelper
    with BeforeAndAfter {
  private val TEST_2007_07_15_12_30_40_987654321: LocalDateTime =
    LocalDateTime.of(2007, 7, 15, 12, 30, 40, 987654321)
  private var MAX_DATE_TIME: LocalDateTime                      = null
  private var MIN_DATE_TIME: LocalDateTime                      = null
  private var MAX_INSTANT: Instant                              = null
  private var MIN_INSTANT: Instant                              = null

  before {
    MAX_DATE_TIME = LocalDateTime.MAX
    MIN_DATE_TIME = LocalDateTime.MIN
    MAX_INSTANT = MAX_DATE_TIME.atZone(ZoneOffset.UTC).toInstant
    MIN_INSTANT = MIN_DATE_TIME.atZone(ZoneOffset.UTC).toInstant
  }

  override protected def samples: List[TemporalAccessor] =
    List(TEST_2007_07_15_12_30_40_987654321, LocalDateTime.MAX, LocalDateTime.MIN)

  override protected def validFields: List[TemporalField] =
    List(
      NANO_OF_SECOND,
      NANO_OF_DAY,
      MICRO_OF_SECOND,
      MICRO_OF_DAY,
      MILLI_OF_SECOND,
      MILLI_OF_DAY,
      SECOND_OF_MINUTE,
      SECOND_OF_DAY,
      MINUTE_OF_HOUR,
      MINUTE_OF_DAY,
      CLOCK_HOUR_OF_AMPM,
      HOUR_OF_AMPM,
      CLOCK_HOUR_OF_DAY,
      HOUR_OF_DAY,
      AMPM_OF_DAY,
      DAY_OF_WEEK,
      ALIGNED_DAY_OF_WEEK_IN_MONTH,
      ALIGNED_DAY_OF_WEEK_IN_YEAR,
      DAY_OF_MONTH,
      DAY_OF_YEAR,
      EPOCH_DAY,
      ALIGNED_WEEK_OF_MONTH,
      ALIGNED_WEEK_OF_YEAR,
      MONTH_OF_YEAR,
      PROLEPTIC_MONTH,
      YEAR_OF_ERA,
      YEAR,
      ERA,
      JulianFields.JULIAN_DAY,
      JulianFields.MODIFIED_JULIAN_DAY,
      JulianFields.RATA_DIE
    )

  override protected def invalidFields: List[TemporalField] = {
    val list: List[TemporalField] = List(ChronoField.values: _*)
    list.filterNot(validFields.contains)
  }

  private def check(
    dateTime: LocalDateTime,
    y:        Int,
    m:        Int,
    d:        Int,
    h:        Int,
    mi:       Int,
    s:        Int,
    n:        Int
  ): Unit = {
    assertEquals(dateTime.getYear, y)
    assertEquals(dateTime.getMonth.getValue, m)
    assertEquals(dateTime.getDayOfMonth, d)
    assertEquals(dateTime.getHour, h)
    assertEquals(dateTime.getMinute, mi)
    assertEquals(dateTime.getSecond, s)
    assertEquals(dateTime.getNano, n)
  }

  private def createDateMidnight(year: Int, month: Int, day: Int): LocalDateTime =
    LocalDateTime.of(year, month, day, 0, 0)

  test("now") {
    var expected: LocalDateTime = LocalDateTime.now(Clock.systemDefaultZone)
    var test: LocalDateTime     = LocalDateTime.now
    var diff: Long              = Math.abs(test.toLocalTime.toNanoOfDay - expected.toLocalTime.toNanoOfDay)
    if (diff >= 100000000) {
      expected = LocalDateTime.now(Clock.systemDefaultZone)
      test = LocalDateTime.now
      diff = Math.abs(test.toLocalTime.toNanoOfDay - expected.toLocalTime.toNanoOfDay)
    }
    assertTrue(diff < 100000000)
  }

  test("now_ZoneId_nullZoneId") {
    assertThrows[NullPointerException] {
      LocalDateTime.now(null.asInstanceOf[ZoneId])
    }
  }

  test("now_ZoneId") {
    val zone: ZoneId            = ZoneId.of("UTC+01:02:03")
    var expected: LocalDateTime = LocalDateTime.now(Clock.system(zone))
    var test: LocalDateTime     = LocalDateTime.now(zone)
    var i: Int                  = 0
    while (i < 100 && expected != test) {
      expected = LocalDateTime.now(Clock.system(zone))
      test = LocalDateTime.now(zone)
      i += 1
    }
    assertEquals(test, expected)
  }

  test("now_Clock_nullClock") {
    assertThrows[NullPointerException] {
      LocalDateTime.now(null.asInstanceOf[Clock])
    }
  }

  test("now_Clock_allSecsInDay_utc") {
    var i: Int = 0
    while (i < (2 * 24 * 60 * 60)) {
      val instant: Instant    = Instant.ofEpochSecond(i).plusNanos(123456789L)
      val clock: Clock        = Clock.fixed(instant, ZoneOffset.UTC)
      val test: LocalDateTime = LocalDateTime.now(clock)
      assertEquals(test.getYear, 1970)
      assertEquals(test.getMonth, Month.JANUARY)
      assertEquals(test.getDayOfMonth, if (i < 24 * 60 * 60) 1 else 2)
      assertEquals(test.getHour, (i / (60 * 60)) % 24)
      assertEquals(test.getMinute, (i / 60)      % 60)
      assertEquals(test.getSecond, i             % 60)
      assertEquals(test.getNano, 123456789)
      i += 1
    }
  }

  test("now_Clock_allSecsInDay_offset") {
    var i: Int = 0
    while (i < (2 * 24 * 60 * 60)) {
      val instant: Instant    = Instant.ofEpochSecond(i).plusNanos(123456789L)
      val clock: Clock        =
        Clock.fixed(instant.minusSeconds(TestLocalDateTime.OFFSET_PONE.getTotalSeconds),
                    TestLocalDateTime.OFFSET_PONE
        )
      val test: LocalDateTime = LocalDateTime.now(clock)
      assertEquals(test.getYear, 1970)
      assertEquals(test.getMonth, Month.JANUARY)
      assertEquals(test.getDayOfMonth, if (i < 24 * 60 * 60) 1 else 2)
      assertEquals(test.getHour, (i / (60 * 60)) % 24)
      assertEquals(test.getMinute, (i / 60)      % 60)
      assertEquals(test.getSecond, i             % 60)
      assertEquals(test.getNano, 123456789)
      i += 1
    }
  }

  test("now_Clock_allSecsInDay_beforeEpoch") {
    var expected: LocalTime = LocalTime.MIDNIGHT.plusNanos(123456789L)
    var i: Int              = -1
    while (i >= -(24 * 60 * 60)) {
      val instant: Instant    = Instant.ofEpochSecond(i).plusNanos(123456789L)
      val clock: Clock        = Clock.fixed(instant, ZoneOffset.UTC)
      val test: LocalDateTime = LocalDateTime.now(clock)
      assertEquals(test.getYear, 1969)
      assertEquals(test.getMonth, Month.DECEMBER)
      assertEquals(test.getDayOfMonth, 31)
      expected = expected.minusSeconds(1)
      assertEquals(test.toLocalTime, expected)
      i -= 1
    }
  }

  test("now_Clock_maxYear") {
    val clock: Clock        = Clock.fixed(MAX_INSTANT, ZoneOffset.UTC)
    val test: LocalDateTime = LocalDateTime.now(clock)
    assertEquals(test, MAX_DATE_TIME)
  }

  test("now_Clock_tooBig") {
    assertThrows[DateTimeException] {
      val clock: Clock = Clock.fixed(MAX_INSTANT.plusSeconds(24 * 60 * 60), ZoneOffset.UTC)
      LocalDateTime.now(clock)
    }
  }

  test("now_Clock_minYear") {
    val clock: Clock        = Clock.fixed(MIN_INSTANT, ZoneOffset.UTC)
    val test: LocalDateTime = LocalDateTime.now(clock)
    assertEquals(test, MIN_DATE_TIME)
  }

  test("now_Clock_tooLow") {
    assertThrows[DateTimeException] {
      val clock: Clock = Clock.fixed(MIN_INSTANT.minusNanos(1), ZoneOffset.UTC)
      LocalDateTime.now(clock)
    }
  }

  test("factory_of_4intsMonth") {
    val dateTime: LocalDateTime = LocalDateTime.of(2007, Month.JULY, 15, 12, 30)
    check(dateTime, 2007, 7, 15, 12, 30, 0, 0)
  }

  test("factory_of_4intsMonth_yearTooLow") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(Integer.MIN_VALUE, Month.JULY, 15, 12, 30)
    }
  }

  test("factory_of_4intsMonth_nullMonth") {
    assertThrows[NullPointerException] {
      LocalDateTime.of(2007, null, 15, 12, 30)
    }
  }

  test("factory_of_4intsMonth_dayTooLow") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, Month.JULY, -1, 12, 30)
    }
  }

  test("factory_of_4intsMonth_dayTooHigh") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, Month.JULY, 32, 12, 30)
    }
  }

  test("factory_of_4intsMonth_hourTooLow") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, Month.JULY, 15, -1, 30)
    }
  }

  test("factory_of_4intsMonth_hourTooHigh") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, Month.JULY, 15, 24, 30)
    }
  }

  test("factory_of_4intsMonth_minuteTooLow") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, Month.JULY, 15, 12, -1)
    }
  }

  test("factory_of_4intsMonth_minuteTooHigh") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, Month.JULY, 15, 12, 60)
    }
  }

  test("factory_of_5intsMonth") {
    val dateTime: LocalDateTime = LocalDateTime.of(2007, Month.JULY, 15, 12, 30, 40)
    check(dateTime, 2007, 7, 15, 12, 30, 40, 0)
  }

  test("factory_of_5intsMonth_yearTooLow") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(Integer.MIN_VALUE, Month.JULY, 15, 12, 30, 40)
    }
  }

  test("factory_of_5intsMonth_nullMonth") {
    assertThrows[NullPointerException] {
      LocalDateTime.of(2007, null, 15, 12, 30, 40)
    }
  }

  test("factory_of_5intsMonth_dayTooLow") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, Month.JULY, -1, 12, 30, 40)
    }
  }

  test("factory_of_5intsMonth_dayTooHigh") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, Month.JULY, 32, 12, 30, 40)
    }
  }

  test("factory_of_5intsMonth_hourTooLow") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, Month.JULY, 15, -1, 30, 40)
    }
  }

  test("factory_of_5intsMonth_hourTooHigh") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, Month.JULY, 15, 24, 30, 40)
    }
  }

  test("factory_of_5intsMonth_minuteTooLow") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, Month.JULY, 15, 12, -1, 40)
    }
  }

  test("factory_of_5intsMonth_minuteTooHigh") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, Month.JULY, 15, 12, 60, 40)
    }
  }

  test("factory_of_5intsMonth_secondTooLow") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, Month.JULY, 15, 12, 30, -1)
    }
  }

  test("factory_of_5intsMonth_secondTooHigh") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, Month.JULY, 15, 12, 30, 60)
    }
  }

  test("factory_of_6intsMonth") {
    val dateTime: LocalDateTime = LocalDateTime.of(2007, Month.JULY, 15, 12, 30, 40, 987654321)
    check(dateTime, 2007, 7, 15, 12, 30, 40, 987654321)
  }

  test("factory_of_6intsMonth_yearTooLow") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(Integer.MIN_VALUE, Month.JULY, 15, 12, 30, 40, 987654321)
    }
  }

  test("factory_of_6intsMonth_nullMonth") {
    assertThrows[NullPointerException] {
      LocalDateTime.of(2007, null, 15, 12, 30, 40, 987654321)
    }
  }

  test("factory_of_6intsMonth_dayTooLow") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, Month.JULY, -1, 12, 30, 40, 987654321)
    }
  }

  test("factory_of_6intsMonth_dayTooHigh") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, Month.JULY, 32, 12, 30, 40, 987654321)
    }
  }

  test("factory_of_6intsMonth_hourTooLow") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, Month.JULY, 15, -1, 30, 40, 987654321)
    }
  }

  test("factory_of_6intsMonth_hourTooHigh") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, Month.JULY, 15, 24, 30, 40, 987654321)
    }
  }

  test("factory_of_6intsMonth_minuteTooLow") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, Month.JULY, 15, 12, -1, 40, 987654321)
    }
  }

  test("factory_of_6intsMonth_minuteTooHigh") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, Month.JULY, 15, 12, 60, 40, 987654321)
    }
  }

  test("factory_of_6intsMonth_secondTooLow") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, Month.JULY, 15, 12, 30, -1, 987654321)
    }
  }

  test("factory_of_6intsMonth_secondTooHigh") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, Month.JULY, 15, 12, 30, 60, 987654321)
    }
  }

  test("factory_of_6intsMonth_nanoTooLow") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, Month.JULY, 15, 12, 30, 40, -1)
    }
  }

  test("factory_of_6intsMonth_nanoTooHigh") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, Month.JULY, 15, 12, 30, 40, 1000000000)
    }
  }

  test("factory_of_5ints") {
    val dateTime: LocalDateTime = LocalDateTime.of(2007, 7, 15, 12, 30)
    check(dateTime, 2007, 7, 15, 12, 30, 0, 0)
  }

  test("factory_of_5ints_yearTooLow") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(Integer.MIN_VALUE, 7, 15, 12, 30)
    }
  }

  test("factory_of_5ints_monthTooLow") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, 0, 15, 12, 30)
    }
  }

  test("factory_of_5ints_monthTooHigh") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, 13, 15, 12, 30)
    }
  }

  test("factory_of_5ints_dayTooLow") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, 7, -1, 12, 30)
    }
  }

  test("factory_of_5ints_dayTooHigh") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, 7, 32, 12, 30)
    }
  }

  test("factory_of_5ints_hourTooLow") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, 7, 15, -1, 30)
    }
  }

  test("factory_of_5ints_hourTooHigh") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, 7, 15, 24, 30)
    }
  }

  test("factory_of_5ints_minuteTooLow") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, 7, 15, 12, -1)
    }
  }

  test("factory_of_5ints_minuteTooHigh") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, 7, 15, 12, 60)
    }
  }

  test("factory_of_6ints") {
    val dateTime: LocalDateTime = LocalDateTime.of(2007, 7, 15, 12, 30, 40)
    check(dateTime, 2007, 7, 15, 12, 30, 40, 0)
  }

  test("factory_of_6ints_yearTooLow") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(Integer.MIN_VALUE, 7, 15, 12, 30, 40)
    }
  }

  test("factory_of_6ints_monthTooLow") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, 0, 15, 12, 30, 40)
    }
  }

  test("factory_of_6ints_monthTooHigh") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, 13, 15, 12, 30, 40)
    }
  }

  test("factory_of_6ints_dayTooLow") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, 7, -1, 12, 30, 40)
    }
  }

  test("factory_of_6ints_dayTooHigh") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, 7, 32, 12, 30, 40)
    }
  }

  test("factory_of_6ints_hourTooLow") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, 7, 15, -1, 30, 40)
    }
  }

  test("factory_of_6ints_hourTooHigh") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, 7, 15, 24, 30, 40)
    }
  }

  test("factory_of_6ints_minuteTooLow") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, 7, 15, 12, -1, 40)
    }
  }

  test("factory_of_6ints_minuteTooHigh") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, 7, 15, 12, 60, 40)
    }
  }

  test("factory_of_6ints_secondTooLow") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, 7, 15, 12, 30, -1)
    }
  }

  test("factory_of_6ints_secondTooHigh") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, 7, 15, 12, 30, 60)
    }
  }

  test("factory_of_7ints") {
    val dateTime: LocalDateTime = LocalDateTime.of(2007, 7, 15, 12, 30, 40, 987654321)
    check(dateTime, 2007, 7, 15, 12, 30, 40, 987654321)
  }

  test("factory_of_7ints_yearTooLow") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(Integer.MIN_VALUE, 7, 15, 12, 30, 40, 987654321)
    }
  }

  test("factory_of_7ints_monthTooLow") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, 0, 15, 12, 30, 40, 987654321)
    }
  }

  test("factory_of_7ints_monthTooHigh") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, 13, 15, 12, 30, 40, 987654321)
    }
  }

  test("factory_of_7ints_dayTooLow") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, 7, -1, 12, 30, 40, 987654321)
    }
  }

  test("factory_of_7ints_dayTooHigh") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, 7, 32, 12, 30, 40, 987654321)
    }
  }

  test("factory_of_7ints_hourTooLow") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, 7, 15, -1, 30, 40, 987654321)
    }
  }

  test("factory_of_7ints_hourTooHigh") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, 7, 15, 24, 30, 40, 987654321)
    }
  }

  test("factory_of_7ints_minuteTooLow") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, 7, 15, 12, -1, 40, 987654321)
    }
  }

  test("factory_of_7ints_minuteTooHigh") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, 7, 15, 12, 60, 40, 987654321)
    }
  }

  test("factory_of_7ints_secondTooLow") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, 7, 15, 12, 30, -1, 987654321)
    }
  }

  test("factory_of_7ints_secondTooHigh") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, 7, 15, 12, 30, 60, 987654321)
    }
  }

  test("factory_of_7ints_nanoTooLow") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, 7, 15, 12, 30, 40, -1)
    }
  }

  test("factory_of_7ints_nanoTooHigh") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, 7, 15, 12, 30, 40, 1000000000)
    }
  }

  test("factory_of_LocalDate_LocalTime") {
    val dateTime: LocalDateTime =
      LocalDateTime.of(LocalDate.of(2007, 7, 15), LocalTime.of(12, 30, 40, 987654321))
    check(dateTime, 2007, 7, 15, 12, 30, 40, 987654321)
  }

  test("factory_of_LocalDate_LocalTime_nullLocalDate") {
    assertThrows[NullPointerException] {
      LocalDateTime.of(null, LocalTime.of(12, 30, 40, 987654321))
    }
  }

  test("factory_of_LocalDate_LocalTime_nullLocalTime") {
    assertThrows[NullPointerException] {
      LocalDateTime.of(LocalDate.of(2007, 7, 15), null)
    }
  }

  test("factory_ofInstant_zone") {
    val test: LocalDateTime =
      LocalDateTime.ofInstant(Instant.ofEpochSecond(86400 + 3600 + 120 + 4, 500),
                              TestLocalDateTime.ZONE_PARIS
      )
    assertEquals(test, LocalDateTime.of(1970, 1, 2, 2, 2, 4, 500))
  }

  test("factory_ofInstant_offset") {
    val test: LocalDateTime =
      LocalDateTime.ofInstant(Instant.ofEpochSecond(86400 + 3600 + 120 + 4, 500),
                              TestLocalDateTime.OFFSET_MTWO
      )
    assertEquals(test, LocalDateTime.of(1970, 1, 1, 23, 2, 4, 500))
  }

  test("factory_ofInstant_offsetBeforeEpoch") {
    val test: LocalDateTime =
      LocalDateTime.ofInstant(Instant.ofEpochSecond(-86400 + 4, 500), TestLocalDateTime.OFFSET_PTWO)
    assertEquals(test, LocalDateTime.of(1969, 12, 31, 2, 0, 4, 500))
  }

  test("factory_ofInstant_instantTooBig") {
    assertThrows[DateTimeException] {
      LocalDateTime.ofInstant(Instant.ofEpochSecond(Long.MaxValue), TestLocalDateTime.OFFSET_PONE)
    }
  }

  test("factory_ofInstant_instantTooSmall") {
    assertThrows[DateTimeException] {
      LocalDateTime.ofInstant(Instant.ofEpochSecond(Long.MinValue), TestLocalDateTime.OFFSET_PONE)
    }
  }

  test("factory_ofInstant_nullInstant") {
    assertThrows[NullPointerException] {
      LocalDateTime.ofInstant(null.asInstanceOf[Instant], TestLocalDateTime.ZONE_GAZA)
    }
  }

  test("factory_ofInstant_nullZone") {
    assertThrows[NullPointerException] {
      LocalDateTime.ofInstant(Instant.EPOCH, null.asInstanceOf[ZoneId])
    }
  }

  test("factory_ofEpochSecond_longOffset_afterEpoch") {
    val base: LocalDateTime = LocalDateTime.of(1970, 1, 1, 2, 0, 0, 500)

    {
      var i: Int = 0
      while (i < 100000) {
        {
          val test: LocalDateTime =
            LocalDateTime.ofEpochSecond(i, 500, TestLocalDateTime.OFFSET_PTWO)
          assertEquals(test, base.plusSeconds(i))
        }
        {
          i += 1
          i - 1
        }
      }
    }
  }

  test("factory_ofEpochSecond_longOffset_beforeEpoch") {
    val base: LocalDateTime = LocalDateTime.of(1970, 1, 1, 2, 0, 0, 500)

    {
      var i: Int = 0
      while (i < 100000) {
        {
          val test: LocalDateTime =
            LocalDateTime.ofEpochSecond(-i, 500, TestLocalDateTime.OFFSET_PTWO)
          assertEquals(test, base.minusSeconds(i))
        }
        {
          i += 1
          i - 1
        }
      }
    }
  }

  test("factory_ofEpochSecond_longOffset_tooBig") {
    assertThrows[DateTimeException] {
      LocalDateTime.ofEpochSecond(Long.MaxValue, 500, TestLocalDateTime.OFFSET_PONE)
    }
  }

  test("factory_ofEpochSecond_longOffset_tooSmall") {
    assertThrows[DateTimeException] {
      LocalDateTime.ofEpochSecond(Long.MinValue, 500, TestLocalDateTime.OFFSET_PONE)
    }
  }

  test("factory_ofEpochSecond_badNanos_toBig") {
    assertThrows[DateTimeException] {
      LocalDateTime.ofEpochSecond(0, 1000000000, TestLocalDateTime.OFFSET_PONE)
    }
  }

  test("factory_ofEpochSecond_badNanos_toSmall") {
    assertThrows[DateTimeException] {
      LocalDateTime.ofEpochSecond(0, -1, TestLocalDateTime.OFFSET_PONE)
    }
  }

  test("factory_ofEpochSecond_longOffset_nullOffset") {
    assertThrows[NullPointerException] {
      LocalDateTime.ofEpochSecond(0L, 500, null)
    }
  }

  test("test_from_Accessor") {
    val base: LocalDateTime = LocalDateTime.of(2007, 7, 15, 17, 30)
    assertEquals(LocalDateTime.from(base), base)
    assertEquals(LocalDateTime.from(ZonedDateTime.of(base, ZoneOffset.ofHours(2))), base)
  }

  test("from_Accessor_invalid_noDerive") {
    assertThrows[DateTimeException] {
      LocalDateTime.from(LocalTime.of(12, 30))
    }
  }

  test("test_from_Accessor_null") {
    assertThrows[Platform.NPE] {
      LocalDateTime.from(null.asInstanceOf[TemporalAccessor])
    }
  }

  test("parse") {
    provider_sampleToString.foreach {
      case (y: Int) :: (m: Int) :: (d: Int) :: (h: Int) :: (mi: Int) :: (s: Int) :: (n: Int) :: (expected: String) :: Nil =>
        val t: LocalDateTime = LocalDateTime.parse(expected)
        assertEquals(t.getYear, y)
        assertEquals(t.getMonth.getValue, m)
        assertEquals(t.getDayOfMonth, d)
        assertEquals(t.getHour, h)
        assertEquals(t.getMinute, mi)
        assertEquals(t.getSecond, s)
        assertEquals(t.getNano, n)
      case _                                                                                                              =>
        fail()
    }
  }

  test("factory_parse_illegalValue") {
    assertThrows[DateTimeParseException] {
      LocalDateTime.parse("2008-06-32T11:15")
    }
  }

  test("factory_parse_invalidValue") {
    assertThrows[DateTimeParseException] {
      LocalDateTime.parse("2008-06-31T11:15")
    }
  }

  test("factory_parse_nullText") {
    assertThrows[NullPointerException] {
      LocalDateTime.parse(null.asInstanceOf[String])
    }
  }

  test("factory_parse_formatter") {
    val f: DateTimeFormatter = DateTimeFormatter.ofPattern("u M d H m s")
    val test: LocalDateTime  = LocalDateTime.parse("2010 12 3 11 30 45", f)
    assertEquals(test, LocalDateTime.of(2010, 12, 3, 11, 30, 45))
  }

  test("factory_parse_formatter_nullText") {
    assertThrows[NullPointerException] {
      val f: DateTimeFormatter = DateTimeFormatter.ofPattern("u M d H m s")
      LocalDateTime.parse(null.asInstanceOf[String], f)
    }
  }

  test("factory_parse_formatter_nullFormatter") {
    assertThrows[NullPointerException] {
      LocalDateTime.parse("ANY", null)
    }
  }

  test("test_get_DateTimeField") {
    val test: LocalDateTime = LocalDateTime.of(2008, 6, 30, 12, 30, 40, 987654321)
    assertEquals(test.getLong(ChronoField.YEAR), 2008)
    assertEquals(test.getLong(ChronoField.MONTH_OF_YEAR), 6)
    assertEquals(test.getLong(ChronoField.DAY_OF_MONTH), 30)
    assertEquals(test.getLong(ChronoField.DAY_OF_WEEK), 1)
    assertEquals(test.getLong(ChronoField.DAY_OF_YEAR), 182)
    assertEquals(test.getLong(ChronoField.HOUR_OF_DAY), 12)
    assertEquals(test.getLong(ChronoField.MINUTE_OF_HOUR), 30)
    assertEquals(test.getLong(ChronoField.SECOND_OF_MINUTE), 40)
    assertEquals(test.getLong(ChronoField.NANO_OF_SECOND), 987654321)
    assertEquals(test.getLong(ChronoField.HOUR_OF_AMPM), 0)
    assertEquals(test.getLong(ChronoField.AMPM_OF_DAY), 1)
  }

  test("test_get_DateTimeField_null") {
    assertThrows[Platform.NPE] {
      val test: LocalDateTime = LocalDateTime.of(2008, 6, 30, 12, 30, 40, 987654321)
      test.getLong(null.asInstanceOf[TemporalField])
    }
  }

  test("get_DateTimeField_invalidField") {
    assertThrows[DateTimeException] {
      TEST_2007_07_15_12_30_40_987654321.getLong(MockFieldNoValue.INSTANCE)
    }
  }

  test("query") {
    assertEquals(TEST_2007_07_15_12_30_40_987654321.query(TemporalQueries.chronology),
                 IsoChronology.INSTANCE
    )
    assertEquals(TEST_2007_07_15_12_30_40_987654321.query(TemporalQueries.localDate),
                 TEST_2007_07_15_12_30_40_987654321.toLocalDate
    )
    assertEquals(TEST_2007_07_15_12_30_40_987654321.query(TemporalQueries.localTime),
                 TEST_2007_07_15_12_30_40_987654321.toLocalTime
    )
    assertEquals(TEST_2007_07_15_12_30_40_987654321.query(TemporalQueries.offset), null)
    assertEquals(TEST_2007_07_15_12_30_40_987654321.query(TemporalQueries.precision),
                 ChronoUnit.NANOS
    )
    assertEquals(TEST_2007_07_15_12_30_40_987654321.query(TemporalQueries.zone), null)
    assertEquals(TEST_2007_07_15_12_30_40_987654321.query(TemporalQueries.zoneId), null)
  }

  test("test_query_null") {
    assertThrows[Platform.NPE] {
      TEST_2007_07_15_12_30_40_987654321.query(null)
    }
  }

  val provider_sampleDates: List[List[Int]] =
    List(List(2008, 7, 5),
         List(2007, 7, 5),
         List(2006, 7, 5),
         List(2005, 7, 5),
         List(2004, 1, 1),
         List(-1, 1, 2)
    )

  val provider_sampleTimes: List[List[Int]] =
    List(
      List(0, 0, 0, 0),
      List(0, 0, 0, 1),
      List(0, 0, 1, 0),
      List(0, 0, 1, 1),
      List(0, 1, 0, 0),
      List(0, 1, 0, 1),
      List(0, 1, 1, 0),
      List(0, 1, 1, 1),
      List(1, 0, 0, 0),
      List(1, 0, 0, 1),
      List(1, 0, 1, 0),
      List(1, 0, 1, 1),
      List(1, 1, 0, 0),
      List(1, 1, 0, 1),
      List(1, 1, 1, 0),
      List(1, 1, 1, 1)
    )

  test("get_dates") {
    provider_sampleDates.foreach {
      case y :: m :: d :: Nil =>
        val a: LocalDateTime = LocalDateTime.of(y, m, d, 12, 30)
        assertEquals(a.getYear, y)
        assertEquals(a.getMonth, Month.of(m))
        assertEquals(a.getDayOfMonth, d)
      case _                  =>
        fail()
    }
  }

  test("getDOY") {
    provider_sampleDates.foreach {
      case y :: m :: d :: Nil =>
        val a: LocalDateTime = LocalDateTime.of(y, m, d, 12, 30)
        var total: Int       = 0

        {
          var i: Int = 1
          while (i < m) {
            {
              total += Month.of(i).length(isIsoLeap(y))
            }
            {
              i += 1
              i - 1
            }
          }
        }
        val doy: Int = total + d
        assertEquals(a.getDayOfYear, doy)
      case _                  =>
        fail()
    }
  }

  test("get_times") {
    provider_sampleTimes.foreach {
      case h :: m :: s :: ns :: Nil =>
        val a: LocalDateTime = LocalDateTime.of(TEST_2007_07_15_12_30_40_987654321.toLocalDate,
                                                LocalTime.of(h, m, s, ns)
        )
        assertEquals(a.getHour, h)
        assertEquals(a.getMinute, m)
        assertEquals(a.getSecond, s)
        assertEquals(a.getNano, ns)
      case _                        =>
        fail()
    }
  }

  test("getDayOfWeek") {
    var dow: DayOfWeek = DayOfWeek.MONDAY
    for (month <- Month.values) {
      val length: Int = month.length(false)

      {
        var i: Int = 1
        while (i <= length) {
          {
            val d: LocalDateTime = LocalDateTime.of(LocalDate.of(2007, month, i),
                                                    TEST_2007_07_15_12_30_40_987654321.toLocalTime
            )
            assertSame(d.getDayOfWeek, dow)
            dow = dow.plus(1)
          }
          {
            i += 1
            i - 1
          }
        }
      }
    }
  }

  test("with_adjustment") {
    val sample: LocalDateTime      = LocalDateTime.of(2012, 3, 4, 23, 5)
    val adjuster: TemporalAdjuster = new TemporalAdjuster {
      override def adjustInto(temporal: Temporal): Temporal = sample
    }
    assertEquals(TEST_2007_07_15_12_30_40_987654321.`with`(adjuster), sample)
  }

  test("test_with_adjustment_null") {
    assertThrows[Platform.NPE] {
      TEST_2007_07_15_12_30_40_987654321.`with`(null.asInstanceOf[TemporalAdjuster])
    }
  }

  test("withYear_int_normal") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.withYear(2008)
    check(t, 2008, 7, 15, 12, 30, 40, 987654321)
  }

  test("withYear_int_invalid") {
    assertThrows[DateTimeException] {
      TEST_2007_07_15_12_30_40_987654321.withYear(Year.MIN_VALUE - 1)
    }
  }

  test("withYear_int_adjustDay") {
    val t: LocalDateTime        = LocalDateTime.of(2008, 2, 29, 12, 30).withYear(2007)
    val expected: LocalDateTime = LocalDateTime.of(2007, 2, 28, 12, 30)
    assertEquals(t, expected)
  }

  test("withMonth_int_normal") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.withMonth(1)
    check(t, 2007, 1, 15, 12, 30, 40, 987654321)
  }

  test("withMonth_int_invalid") {
    assertThrows[DateTimeException] {
      TEST_2007_07_15_12_30_40_987654321.withMonth(13)
    }
  }

  test("withMonth_int_adjustDay") {
    val t: LocalDateTime        = LocalDateTime.of(2007, 12, 31, 12, 30).withMonth(11)
    val expected: LocalDateTime = LocalDateTime.of(2007, 11, 30, 12, 30)
    assertEquals(t, expected)
  }

  test("withDayOfMonth_normal") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.withDayOfMonth(1)
    check(t, 2007, 7, 1, 12, 30, 40, 987654321)
  }

  test("withDayOfMonth_invalid") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, 11, 30, 12, 30).withDayOfMonth(32)
    }
  }

  test("withDayOfMonth_invalidCombination") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, 11, 30, 12, 30).withDayOfMonth(31)
    }
  }

  test("withDayOfYear_normal") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.withDayOfYear(33)
    assertEquals(t, LocalDateTime.of(2007, 2, 2, 12, 30, 40, 987654321))
  }

  test("withDayOfYear_illegal") {
    assertThrows[DateTimeException] {
      TEST_2007_07_15_12_30_40_987654321.withDayOfYear(367)
    }
  }

  test("withDayOfYear_invalid") {
    assertThrows[DateTimeException] {
      TEST_2007_07_15_12_30_40_987654321.withDayOfYear(366)
    }
  }

  test("withHour_normal") {
    var t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321

    {
      var i: Int = 0
      while (i < 24) {
        {
          t = t.withHour(i)
          assertEquals(t.getHour, i)
        }
        {
          i += 1
          i - 1
        }
      }
    }
  }

  test("withHour_hourTooLow") {
    assertThrows[DateTimeException] {
      TEST_2007_07_15_12_30_40_987654321.withHour(-1)
    }
  }

  test("withHour_hourTooHigh") {
    assertThrows[DateTimeException] {
      TEST_2007_07_15_12_30_40_987654321.withHour(24)
    }
  }

  test("withMinute_normal") {
    var t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321

    {
      var i: Int = 0
      while (i < 60) {
        {
          t = t.withMinute(i)
          assertEquals(t.getMinute, i)
        }
        {
          i += 1
          i - 1
        }
      }
    }
  }

  test("withMinute_minuteTooLow") {
    assertThrows[DateTimeException] {
      TEST_2007_07_15_12_30_40_987654321.withMinute(-1)
    }
  }

  test("withMinute_minuteTooHigh") {
    assertThrows[DateTimeException] {
      TEST_2007_07_15_12_30_40_987654321.withMinute(60)
    }
  }

  test("withSecond_normal") {
    var t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321

    {
      var i: Int = 0
      while (i < 60) {
        {
          t = t.withSecond(i)
          assertEquals(t.getSecond, i)
        }
        {
          i += 1
          i - 1
        }
      }
    }
  }

  test("withSecond_secondTooLow") {
    assertThrows[DateTimeException] {
      TEST_2007_07_15_12_30_40_987654321.withSecond(-1)
    }
  }

  test("withSecond_secondTooHigh") {
    assertThrows[DateTimeException] {
      TEST_2007_07_15_12_30_40_987654321.withSecond(60)
    }
  }

  test("withNanoOfSecond_normal") {
    var t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321
    t = t.withNano(1)
    assertEquals(t.getNano, 1)
    t = t.withNano(10)
    assertEquals(t.getNano, 10)
    t = t.withNano(100)
    assertEquals(t.getNano, 100)
    t = t.withNano(999999999)
    assertEquals(t.getNano, 999999999)
  }

  test("withNanoOfSecond_nanoTooLow") {
    assertThrows[DateTimeException] {
      TEST_2007_07_15_12_30_40_987654321.withNano(-1)
    }
  }

  test("withNanoOfSecond_nanoTooHigh") {
    assertThrows[DateTimeException] {
      TEST_2007_07_15_12_30_40_987654321.withNano(1000000000)
    }
  }

  test("plus_adjuster") {
    val p: Duration      = Duration.ofSeconds(62, 3)
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.plus(p)
    assertEquals(t, LocalDateTime.of(2007, 7, 15, 12, 31, 42, 987654324))
  }

  test("test_plus_adjuster_null") {
    assertThrows[Platform.NPE] {
      TEST_2007_07_15_12_30_40_987654321.plus(null)
    }
  }

  test("plus_Period_positiveMonths") {
    val period: MockSimplePeriod = MockSimplePeriod.of(7, ChronoUnit.MONTHS)
    val t: LocalDateTime         = TEST_2007_07_15_12_30_40_987654321.plus(period)
    assertEquals(t, LocalDateTime.of(2008, 2, 15, 12, 30, 40, 987654321))
  }

  test("plus_Period_negativeDays") {
    val period: MockSimplePeriod = MockSimplePeriod.of(-25, ChronoUnit.DAYS)
    val t: LocalDateTime         = TEST_2007_07_15_12_30_40_987654321.plus(period)
    assertEquals(t, LocalDateTime.of(2007, 6, 20, 12, 30, 40, 987654321))
  }

  test("test_plus_Period_null") {
    assertThrows[Platform.NPE] {
      TEST_2007_07_15_12_30_40_987654321.plus(null.asInstanceOf[MockSimplePeriod])
    }
  }

  test("plus_Period_invalidTooLarge") {
    assertThrows[DateTimeException] {
      val period: MockSimplePeriod = MockSimplePeriod.of(1, ChronoUnit.YEARS)
      LocalDateTime.of(Year.MAX_VALUE, 1, 1, 0, 0).plus(period)
    }
  }

  test("plus_Period_invalidTooSmall") {
    assertThrows[DateTimeException] {
      val period: MockSimplePeriod = MockSimplePeriod.of(-1, ChronoUnit.YEARS)
      LocalDateTime.of(Year.MIN_VALUE, 1, 1, 0, 0).plus(period)
    }
  }

  test("plus_longPeriodUnit_positiveMonths") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.plus(7, ChronoUnit.MONTHS)
    assertEquals(t, LocalDateTime.of(2008, 2, 15, 12, 30, 40, 987654321))
  }

  test("plus_longPeriodUnit_negativeDays") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.plus(-25, ChronoUnit.DAYS)
    assertEquals(t, LocalDateTime.of(2007, 6, 20, 12, 30, 40, 987654321))
  }

  test("test_plus_longPeriodUnit_null") {
    assertThrows[Platform.NPE] {
      TEST_2007_07_15_12_30_40_987654321.plus(1, null.asInstanceOf[TemporalUnit])
    }
  }

  test("plus_longPeriodUnit_invalidTooLarge") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(Year.MAX_VALUE, 1, 1, 0, 0).plus(1, ChronoUnit.YEARS)
    }
  }

  test("plus_longPeriodUnit_invalidTooSmall") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(Year.MIN_VALUE, 1, 1, 0, 0).plus(-1, ChronoUnit.YEARS)
    }
  }

  test("plusYears_int_normal") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.plusYears(1)
    check(t, 2008, 7, 15, 12, 30, 40, 987654321)
  }

  test("plusYears_int_negative") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.plusYears(-1)
    check(t, 2006, 7, 15, 12, 30, 40, 987654321)
  }

  test("plusYears_int_adjustDay") {
    val t: LocalDateTime = createDateMidnight(2008, 2, 29).plusYears(1)
    check(t, 2009, 2, 28, 0, 0, 0, 0)
  }

  test("plusYears_int_invalidTooLarge") {
    assertThrows[DateTimeException] {
      createDateMidnight(Year.MAX_VALUE, 1, 1).plusYears(1)
    }
  }

  test("plusYears_int_invalidTooSmall") {
    assertThrows[DateTimeException] {
      LocalDate.of(Year.MIN_VALUE, 1, 1).plusYears(-1)
    }
  }

  test("plusMonths_int_normal") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.plusMonths(1)
    check(t, 2007, 8, 15, 12, 30, 40, 987654321)
  }

  test("plusMonths_int_overYears") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.plusMonths(25)
    check(t, 2009, 8, 15, 12, 30, 40, 987654321)
  }

  test("plusMonths_int_negative") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.plusMonths(-1)
    check(t, 2007, 6, 15, 12, 30, 40, 987654321)
  }

  test("plusMonths_int_negativeAcrossYear") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.plusMonths(-7)
    check(t, 2006, 12, 15, 12, 30, 40, 987654321)
  }

  test("plusMonths_int_negativeOverYears") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.plusMonths(-31)
    check(t, 2004, 12, 15, 12, 30, 40, 987654321)
  }

  test("plusMonths_int_adjustDayFromLeapYear") {
    val t: LocalDateTime = createDateMidnight(2008, 2, 29).plusMonths(12)
    check(t, 2009, 2, 28, 0, 0, 0, 0)
  }

  test("plusMonths_int_adjustDayFromMonthLength") {
    val t: LocalDateTime = createDateMidnight(2007, 3, 31).plusMonths(1)
    check(t, 2007, 4, 30, 0, 0, 0, 0)
  }

  test("plusMonths_int_invalidTooLarge") {
    assertThrows[DateTimeException] {
      createDateMidnight(Year.MAX_VALUE, 12, 1).plusMonths(1)
    }
  }

  test("plusMonths_int_invalidTooSmall") {
    assertThrows[DateTimeException] {
      createDateMidnight(Year.MIN_VALUE, 1, 1).plusMonths(-1)
    }
  }

  val provider_samplePlusWeeksSymmetry: List[LocalDateTime] =
    List(
      createDateMidnight(-1, 1, 1),
      createDateMidnight(-1, 2, 28),
      createDateMidnight(-1, 3, 1),
      createDateMidnight(-1, 12, 31),
      createDateMidnight(0, 1, 1),
      createDateMidnight(0, 2, 28),
      createDateMidnight(0, 2, 29),
      createDateMidnight(0, 3, 1),
      createDateMidnight(0, 12, 31),
      createDateMidnight(2007, 1, 1),
      createDateMidnight(2007, 2, 28),
      createDateMidnight(2007, 3, 1),
      createDateMidnight(2007, 12, 31),
      createDateMidnight(2008, 1, 1),
      createDateMidnight(2008, 2, 28),
      createDateMidnight(2008, 2, 29),
      createDateMidnight(2008, 3, 1),
      createDateMidnight(2008, 12, 31),
      createDateMidnight(2099, 1, 1),
      createDateMidnight(2099, 2, 28),
      createDateMidnight(2099, 3, 1),
      createDateMidnight(2099, 12, 31),
      createDateMidnight(2100, 1, 1),
      createDateMidnight(2100, 2, 28),
      createDateMidnight(2100, 3, 1),
      createDateMidnight(2100, 12, 31)
    )

  test("plusWeeks_symmetry") {
    provider_samplePlusWeeksSymmetry.foreach { reference =>
      var weeks: Int = 0
      while (weeks < 365 * 8) {
        {
          var t: LocalDateTime = reference.plusWeeks(weeks).plusWeeks(-weeks)
          assertEquals(t, reference)
          t = reference.plusWeeks(-weeks).plusWeeks(weeks)
          assertEquals(t, reference)
        }
        {
          weeks += 1
          weeks - 1
        }
      }
    }
  }

  test("plusWeeks_normal") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.plusWeeks(1)
    check(t, 2007, 7, 22, 12, 30, 40, 987654321)
  }

  test("plusWeeks_overMonths") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.plusWeeks(9)
    check(t, 2007, 9, 16, 12, 30, 40, 987654321)
  }

  test("plusWeeks_overYears") {
    val t: LocalDateTime = LocalDateTime.of(2006, 7, 16, 12, 30, 40, 987654321).plusWeeks(52)
    assertEquals(t, TEST_2007_07_15_12_30_40_987654321)
  }

  test("plusWeeks_overLeapYears") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.plusYears(-1).plusWeeks(104)
    check(t, 2008, 7, 12, 12, 30, 40, 987654321)
  }

  test("plusWeeks_negative") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.plusWeeks(-1)
    check(t, 2007, 7, 8, 12, 30, 40, 987654321)
  }

  test("plusWeeks_negativeAcrossYear") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.plusWeeks(-28)
    check(t, 2006, 12, 31, 12, 30, 40, 987654321)
  }

  test("plusWeeks_negativeOverYears") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.plusWeeks(-104)
    check(t, 2005, 7, 17, 12, 30, 40, 987654321)
  }

  test("plusWeeks_maximum") {
    val t: LocalDateTime = createDateMidnight(Year.MAX_VALUE, 12, 24).plusWeeks(1)
    check(t, Year.MAX_VALUE, 12, 31, 0, 0, 0, 0)
  }

  test("plusWeeks_minimum") {
    val t: LocalDateTime = createDateMidnight(Year.MIN_VALUE, 1, 8).plusWeeks(-1)
    check(t, Year.MIN_VALUE, 1, 1, 0, 0, 0, 0)
  }

  test("plusWeeks_invalidTooLarge") {
    assertThrows[DateTimeException] {
      createDateMidnight(Year.MAX_VALUE, 12, 25).plusWeeks(1)
    }
  }

  test("plusWeeks_invalidTooSmall") {
    assertThrows[DateTimeException] {
      createDateMidnight(Year.MIN_VALUE, 1, 7).plusWeeks(-1)
    }
  }

  val provider_samplePlusDaysSymmetry: List[LocalDateTime] =
    List(
      createDateMidnight(-1, 1, 1),
      createDateMidnight(-1, 2, 28),
      createDateMidnight(-1, 3, 1),
      createDateMidnight(-1, 12, 31),
      createDateMidnight(0, 1, 1),
      createDateMidnight(0, 2, 28),
      createDateMidnight(0, 2, 29),
      createDateMidnight(0, 3, 1),
      createDateMidnight(0, 12, 31),
      createDateMidnight(2007, 1, 1),
      createDateMidnight(2007, 2, 28),
      createDateMidnight(2007, 3, 1),
      createDateMidnight(2007, 12, 31),
      createDateMidnight(2008, 1, 1),
      createDateMidnight(2008, 2, 28),
      createDateMidnight(2008, 2, 29),
      createDateMidnight(2008, 3, 1),
      createDateMidnight(2008, 12, 31),
      createDateMidnight(2099, 1, 1),
      createDateMidnight(2099, 2, 28),
      createDateMidnight(2099, 3, 1),
      createDateMidnight(2099, 12, 31),
      createDateMidnight(2100, 1, 1),
      createDateMidnight(2100, 2, 28),
      createDateMidnight(2100, 3, 1),
      createDateMidnight(2100, 12, 31)
    )

  test("plusDays_symmetry") {
    provider_samplePlusDaysSymmetry.foreach { reference =>
      var days: Int = 0
      while (days < 365 * 8) {
        {
          var t: LocalDateTime = reference.plusDays(days).plusDays(-days)
          assertEquals(t, reference)
          t = reference.plusDays(-days).plusDays(days)
          assertEquals(t, reference)
        }
        {
          days += 1
          days - 1
        }
      }
    }
  }

  test("plusDays_normal") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.plusDays(1)
    check(t, 2007, 7, 16, 12, 30, 40, 987654321)
  }

  test("plusDays_overMonths") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.plusDays(62)
    check(t, 2007, 9, 15, 12, 30, 40, 987654321)
  }

  test("plusDays_overYears") {
    val t: LocalDateTime = LocalDateTime.of(2006, 7, 14, 12, 30, 40, 987654321).plusDays(366)
    assertEquals(t, TEST_2007_07_15_12_30_40_987654321)
  }

  test("plusDays_overLeapYears") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.plusYears(-1).plusDays(365 + 366)
    check(t, 2008, 7, 15, 12, 30, 40, 987654321)
  }

  test("plusDays_negative") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.plusDays(-1)
    check(t, 2007, 7, 14, 12, 30, 40, 987654321)
  }

  test("plusDays_negativeAcrossYear") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.plusDays(-196)
    check(t, 2006, 12, 31, 12, 30, 40, 987654321)
  }

  test("plusDays_negativeOverYears") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.plusDays(-730)
    check(t, 2005, 7, 15, 12, 30, 40, 987654321)
  }

  test("plusDays_maximum") {
    val t: LocalDateTime = createDateMidnight(Year.MAX_VALUE, 12, 30).plusDays(1)
    check(t, Year.MAX_VALUE, 12, 31, 0, 0, 0, 0)
  }

  test("plusDays_minimum") {
    val t: LocalDateTime = createDateMidnight(Year.MIN_VALUE, 1, 2).plusDays(-1)
    check(t, Year.MIN_VALUE, 1, 1, 0, 0, 0, 0)
  }

  test("plusDays_invalidTooLarge") {
    assertThrows[DateTimeException] {
      createDateMidnight(Year.MAX_VALUE, 12, 31).plusDays(1)
    }
  }

  test("plusDays_invalidTooSmall") {
    assertThrows[DateTimeException] {
      createDateMidnight(Year.MIN_VALUE, 1, 1).plusDays(-1)
    }
  }

  test("plusDays_overflowTooLarge") {
    assertThrows[ArithmeticException] {
      createDateMidnight(Year.MAX_VALUE, 12, 31).plusDays(Long.MaxValue)
    }
  }

  test("plusDays_overflowTooSmall") {
    assertThrows[ArithmeticException] {
      createDateMidnight(Year.MIN_VALUE, 1, 1).plusDays(Long.MinValue)
    }
  }

  test("plusHours_one") {
    var t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.`with`(LocalTime.MIDNIGHT)
    var d: LocalDate     = t.toLocalDate

    {
      var i: Int = 0
      while (i < 50) {
        {
          t = t.plusHours(1)
          if ((i + 1) % 24 == 0)
            d = d.plusDays(1)
          assertEquals(t.toLocalDate, d)
          assertEquals(t.getHour, (i + 1) % 24)
        }
        {
          i += 1
          i - 1
        }
      }
    }
  }

  test("plusHours_fromZero") {
    val base: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.`with`(LocalTime.MIDNIGHT)
    var d: LocalDate        = base.toLocalDate.minusDays(3)
    var t: LocalTime        = LocalTime.of(21, 0)

    {
      var i: Int = -50
      while (i < 50) {
        {
          val dt: LocalDateTime = base.plusHours(i)
          t = t.plusHours(1)
          if (t.getHour == 0)
            d = d.plusDays(1)
          assertEquals(dt.toLocalDate, d)
          assertEquals(dt.toLocalTime, t)
        }
        {
          i += 1
          i - 1
        }
      }
    }
  }

  test("plusHours_fromOne") {
    val base: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.`with`(LocalTime.of(1, 0))
    var d: LocalDate        = base.toLocalDate.minusDays(3)
    var t: LocalTime        = LocalTime.of(22, 0)

    {
      var i: Int = -50
      while (i < 50) {
        {
          val dt: LocalDateTime = base.plusHours(i)
          t = t.plusHours(1)
          if (t.getHour == 0)
            d = d.plusDays(1)
          assertEquals(dt.toLocalDate, d)
          assertEquals(dt.toLocalTime, t)
        }
        {
          i += 1
          i - 1
        }
      }
    }
  }

  test("plusMinutes_one") {
    var t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.`with`(LocalTime.MIDNIGHT)
    val d: LocalDate     = t.toLocalDate
    var hour: Int        = 0
    var min: Int         = 0

    {
      var i: Int = 0
      while (i < 70) {
        {
          t = t.plusMinutes(1)
          min += 1
          if (min == 60) {
            hour += 1
            min = 0
          }
          assertEquals(t.toLocalDate, d)
          assertEquals(t.getHour, hour)
          assertEquals(t.getMinute, min)
        }
        {
          i += 1
          i - 1
        }
      }
    }
  }

  test("plusMinutes_fromZero") {
    val base: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.`with`(LocalTime.MIDNIGHT)
    var d: LocalDate        = base.toLocalDate.minusDays(1)
    var t: LocalTime        = LocalTime.of(22, 49)

    {
      var i: Int = -70
      while (i < 70) {
        {
          val dt: LocalDateTime = base.plusMinutes(i)
          t = t.plusMinutes(1)
          if (t eq LocalTime.MIDNIGHT)
            d = d.plusDays(1)
          assertEquals(dt.toLocalDate, d, String.valueOf(i))
          assertEquals(dt.toLocalTime, t, String.valueOf(i))
        }
        {
          i += 1
          i - 1
        }
      }
    }
  }

  test("plusMinutes_noChange_oneDay") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.plusMinutes(24 * 60)
    assertEquals(t.toLocalDate, TEST_2007_07_15_12_30_40_987654321.toLocalDate.plusDays(1))
  }

  test("plusSeconds_one") {
    var t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.`with`(LocalTime.MIDNIGHT)
    val d: LocalDate     = t.toLocalDate
    var hour: Int        = 0
    var min: Int         = 0
    var sec: Int         = 0

    {
      var i: Int = 0
      while (i < 3700) {
        {
          t = t.plusSeconds(1)
          sec += 1
          if (sec == 60) {
            min += 1
            sec = 0
          }
          if (min == 60) {
            hour += 1
            min = 0
          }
          assertEquals(t.toLocalDate, d)
          assertEquals(t.getHour, hour)
          assertEquals(t.getMinute, min)
          assertEquals(t.getSecond, sec)
        }
        {
          i += 1
          i - 1
        }
      }
    }
  }

  val plusSeconds_fromZero: java.util.Iterator[List[Any]] =
    new java.util.Iterator[List[Any]]() {
      private[bp] var delta: Int      = 30
      private[bp] var i: Int          = -3660
      private[bp] var date: LocalDate = TEST_2007_07_15_12_30_40_987654321.toLocalDate.minusDays(1)
      private[bp] var hour: Int       = 22
      private[bp] var min: Int        = 59
      private[bp] var sec: Int        = 0

      def hasNext: Boolean = i <= 3660

      def next: List[Any] = {
        i += delta
        sec += delta
        if (sec >= 60) {
          min += 1
          sec -= 60
          if (min == 60) {
            hour += 1
            min = 0
            if (hour == 24)
              hour = 0
          }
        }
        if (i == 0)
          date = date.plusDays(1)
        List[Any](i, date, hour, min, sec)
      }

      override def remove(): Unit = throw new UnsupportedOperationException
    }

  test("plusSeconds_fromZero") {
    import scala.collection.JavaConverters._
    plusSeconds_fromZero.asScala.toList.foreach {
      case (seconds: Int) :: (date: LocalDate) :: (hour: Int) :: (min: Int) :: (sec: Int) :: Nil =>
        val base: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.`with`(LocalTime.MIDNIGHT)
        val t: LocalDateTime    = base.plusSeconds(seconds)
        assertEquals(date, t.toLocalDate)
        assertEquals(hour, t.getHour)
        assertEquals(min, t.getMinute)
        assertEquals(sec, t.getSecond)
      case _                                                                                     =>
        fail()
    }
  }

  test("plusSeconds_noChange_oneDay") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.plusSeconds(24 * 60 * 60)
    assertEquals(t.toLocalDate, TEST_2007_07_15_12_30_40_987654321.toLocalDate.plusDays(1))
  }

  test("plusNanos_halfABillion") {
    var t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.`with`(LocalTime.MIDNIGHT)
    val d: LocalDate     = t.toLocalDate
    var hour: Int        = 0
    var min: Int         = 0
    var sec: Int         = 0
    var nanos: Int       = 0

    {
      var i: Long = 0
      while (i < 3700 * 1000000000L) {
        {
          t = t.plusNanos(500000000)
          nanos += 500000000
          if (nanos == 1000000000) {
            sec += 1
            nanos = 0
          }
          if (sec == 60) {
            min += 1
            sec = 0
          }
          if (min == 60) {
            hour += 1
            min = 0
          }
          assertEquals(t.toLocalDate, d, String.valueOf(i))
          assertEquals(t.getHour, hour)
          assertEquals(t.getMinute, min)
          assertEquals(t.getSecond, sec)
          assertEquals(t.getNano, nanos)
        }
        i += 500000000
      }
    }
  }

  val plusNanos_fromZero: java.util.Iterator[List[Any]] =
    new java.util.Iterator[List[Any]]() {
      private[bp] var delta: Long     = 7500000000L
      private[bp] var i: Long         = -3660 * 1000000000L
      private[bp] var date: LocalDate = TEST_2007_07_15_12_30_40_987654321.toLocalDate.minusDays(1)
      private[bp] var hour: Int       = 22
      private[bp] var min: Int        = 59
      private[bp] var sec: Int        = 0
      private[bp] var nanos: Long     = 0

      def hasNext: Boolean = i <= 3660 * 1000000000L

      def next: List[Any] = {
        i += delta
        nanos += delta
        if (nanos >= 1000000000L) {
          sec += (nanos / 1000000000L).toInt // !!!
          nanos %= 1000000000L
          if (sec >= 60) {
            min += 1
            sec %= 60
            if (min == 60) {
              hour += 1
              min = 0
              if (hour == 24) {
                hour = 0
                date = date.plusDays(1)
              }
            }
          }
        }
        List[Any](i, date, hour, min, sec, nanos.toInt)
      }

      override def remove(): Unit = throw new UnsupportedOperationException
    }

  test("plusNanos_fromZero") {
    import scala.collection.JavaConverters._
    plusNanos_fromZero.asScala.toList.foreach {
      case (nanoseconds: Long) :: (date: LocalDate) :: (hour: Int) :: (min: Int) :: (sec: Int) :: (nanos: Int) :: Nil =>
        val base: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.`with`(LocalTime.MIDNIGHT)
        val t: LocalDateTime    = base.plusNanos(nanoseconds)
        assertEquals(date, t.toLocalDate)
        assertEquals(hour, t.getHour)
        assertEquals(min, t.getMinute)
        assertEquals(sec, t.getSecond)
        assertEquals(nanos, t.getNano)
      case _                                                                                                          =>
        fail()
    }
  }

  test("plusNanos_noChange_oneDay") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.plusNanos(24 * 60 * 60 * 1000000000L)
    assertEquals(t.toLocalDate, TEST_2007_07_15_12_30_40_987654321.toLocalDate.plusDays(1))
  }

  test("minus_adjuster") {
    val p: Duration      = Duration.ofSeconds(62, 3)
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.minus(p)
    assertEquals(t, LocalDateTime.of(2007, 7, 15, 12, 29, 38, 987654318))
  }

  test("test_minus_adjuster_null") {
    assertThrows[Platform.NPE] {
      TEST_2007_07_15_12_30_40_987654321.minus(null)
    }
  }

  test("minus_Period_positiveMonths") {
    val period: MockSimplePeriod = MockSimplePeriod.of(7, ChronoUnit.MONTHS)
    val t: LocalDateTime         = TEST_2007_07_15_12_30_40_987654321.minus(period)
    assertEquals(t, LocalDateTime.of(2006, 12, 15, 12, 30, 40, 987654321))
  }

  test("minus_Period_negativeDays") {
    val period: MockSimplePeriod = MockSimplePeriod.of(-25, ChronoUnit.DAYS)
    val t: LocalDateTime         = TEST_2007_07_15_12_30_40_987654321.minus(period)
    assertEquals(t, LocalDateTime.of(2007, 8, 9, 12, 30, 40, 987654321))
  }

  test("test_minus_Period_null") {
    assertThrows[Platform.NPE] {
      TEST_2007_07_15_12_30_40_987654321.minus(null.asInstanceOf[MockSimplePeriod])
    }
  }

  test("minus_Period_invalidTooLarge") {
    assertThrows[DateTimeException] {
      val period: MockSimplePeriod = MockSimplePeriod.of(-1, ChronoUnit.YEARS)
      LocalDateTime.of(Year.MAX_VALUE, 1, 1, 0, 0).minus(period)
    }
  }

  test("minus_Period_invalidTooSmall") {
    assertThrows[DateTimeException] {
      val period: MockSimplePeriod = MockSimplePeriod.of(1, ChronoUnit.YEARS)
      LocalDateTime.of(Year.MIN_VALUE, 1, 1, 0, 0).minus(period)
    }
  }

  test("minus_longPeriodUnit_positiveMonths") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.minus(7, ChronoUnit.MONTHS)
    assertEquals(t, LocalDateTime.of(2006, 12, 15, 12, 30, 40, 987654321))
  }

  test("minus_longPeriodUnit_negativeDays") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.minus(-25, ChronoUnit.DAYS)
    assertEquals(t, LocalDateTime.of(2007, 8, 9, 12, 30, 40, 987654321))
  }

  test("test_minus_longPeriodUnit_null") {
    assertThrows[Platform.NPE] {
      TEST_2007_07_15_12_30_40_987654321.minus(1, null.asInstanceOf[TemporalUnit])
    }
  }

  test("minus_longPeriodUnit_invalidTooLarge") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(Year.MAX_VALUE, 1, 1, 0, 0).minus(-1, ChronoUnit.YEARS)
    }
  }

  test("minus_longPeriodUnit_invalidTooSmall") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(Year.MIN_VALUE, 1, 1, 0, 0).minus(1, ChronoUnit.YEARS)
    }
  }

  test("minusYears_int_normal") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.minusYears(1)
    check(t, 2006, 7, 15, 12, 30, 40, 987654321)
  }

  test("minusYears_int_negative") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.minusYears(-1)
    check(t, 2008, 7, 15, 12, 30, 40, 987654321)
  }

  test("minusYears_int_adjustDay") {
    val t: LocalDateTime = createDateMidnight(2008, 2, 29).minusYears(1)
    check(t, 2007, 2, 28, 0, 0, 0, 0)
  }

  test("minusYears_int_invalidTooLarge") {
    assertThrows[DateTimeException] {
      createDateMidnight(Year.MAX_VALUE, 1, 1).minusYears(-1)
    }
  }

  test("minusYears_int_invalidTooSmall") {
    assertThrows[DateTimeException] {
      createDateMidnight(Year.MIN_VALUE, 1, 1).minusYears(1)
    }
  }

  test("minusMonths_int_normal") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.minusMonths(1)
    check(t, 2007, 6, 15, 12, 30, 40, 987654321)
  }

  test("minusMonths_int_overYears") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.minusMonths(25)
    check(t, 2005, 6, 15, 12, 30, 40, 987654321)
  }

  test("minusMonths_int_negative") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.minusMonths(-1)
    check(t, 2007, 8, 15, 12, 30, 40, 987654321)
  }

  test("minusMonths_int_negativeAcrossYear") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.minusMonths(-7)
    check(t, 2008, 2, 15, 12, 30, 40, 987654321)
  }

  test("minusMonths_int_negativeOverYears") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.minusMonths(-31)
    check(t, 2010, 2, 15, 12, 30, 40, 987654321)
  }

  test("minusMonths_int_adjustDayFromLeapYear") {
    val t: LocalDateTime = createDateMidnight(2008, 2, 29).minusMonths(12)
    check(t, 2007, 2, 28, 0, 0, 0, 0)
  }

  test("minusMonths_int_adjustDayFromMonthLength") {
    val t: LocalDateTime = createDateMidnight(2007, 3, 31).minusMonths(1)
    check(t, 2007, 2, 28, 0, 0, 0, 0)
  }

  test("minusMonths_int_invalidTooLarge") {
    assertThrows[DateTimeException] {
      createDateMidnight(Year.MAX_VALUE, 12, 1).minusMonths(-1)
    }
  }

  test("minusMonths_int_invalidTooSmall") {
    assertThrows[DateTimeException] {
      createDateMidnight(Year.MIN_VALUE, 1, 1).minusMonths(1)
    }
  }

  val provider_sampleMinusWeeksSymmetry: List[LocalDateTime] =
    List(
      createDateMidnight(-1, 1, 1),
      createDateMidnight(-1, 2, 28),
      createDateMidnight(-1, 3, 1),
      createDateMidnight(-1, 12, 31),
      createDateMidnight(0, 1, 1),
      createDateMidnight(0, 2, 28),
      createDateMidnight(0, 2, 29),
      createDateMidnight(0, 3, 1),
      createDateMidnight(0, 12, 31),
      createDateMidnight(2007, 1, 1),
      createDateMidnight(2007, 2, 28),
      createDateMidnight(2007, 3, 1),
      createDateMidnight(2007, 12, 31),
      createDateMidnight(2008, 1, 1),
      createDateMidnight(2008, 2, 28),
      createDateMidnight(2008, 2, 29),
      createDateMidnight(2008, 3, 1),
      createDateMidnight(2008, 12, 31),
      createDateMidnight(2099, 1, 1),
      createDateMidnight(2099, 2, 28),
      createDateMidnight(2099, 3, 1),
      createDateMidnight(2099, 12, 31),
      createDateMidnight(2100, 1, 1),
      createDateMidnight(2100, 2, 28),
      createDateMidnight(2100, 3, 1),
      createDateMidnight(2100, 12, 31)
    )

  test("minusWeeks_symmetry") {
    provider_sampleMinusWeeksSymmetry.foreach { reference =>
      var weeks: Int = 0
      while (weeks < 365 * 8) {
        {
          var t: LocalDateTime = reference.minusWeeks(weeks).minusWeeks(-weeks)
          assertEquals(t, reference)
          t = reference.minusWeeks(-weeks).minusWeeks(weeks)
          assertEquals(t, reference)
        }
        {
          weeks += 1
          weeks - 1
        }
      }
    }
  }

  test("minusWeeks_normal") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.minusWeeks(1)
    check(t, 2007, 7, 8, 12, 30, 40, 987654321)
  }

  test("minusWeeks_overMonths") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.minusWeeks(9)
    check(t, 2007, 5, 13, 12, 30, 40, 987654321)
  }

  test("minusWeeks_overYears") {
    val t: LocalDateTime = LocalDateTime.of(2008, 7, 13, 12, 30, 40, 987654321).minusWeeks(52)
    assertEquals(t, TEST_2007_07_15_12_30_40_987654321)
  }

  test("minusWeeks_overLeapYears") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.minusYears(-1).minusWeeks(104)
    check(t, 2006, 7, 18, 12, 30, 40, 987654321)
  }

  test("minusWeeks_negative") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.minusWeeks(-1)
    check(t, 2007, 7, 22, 12, 30, 40, 987654321)
  }

  test("minusWeeks_negativeAcrossYear") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.minusWeeks(-28)
    check(t, 2008, 1, 27, 12, 30, 40, 987654321)
  }

  test("minusWeeks_negativeOverYears") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.minusWeeks(-104)
    check(t, 2009, 7, 12, 12, 30, 40, 987654321)
  }

  test("minusWeeks_maximum") {
    val t: LocalDateTime = createDateMidnight(Year.MAX_VALUE, 12, 24).minusWeeks(-1)
    check(t, Year.MAX_VALUE, 12, 31, 0, 0, 0, 0)
  }

  test("minusWeeks_minimum") {
    val t: LocalDateTime = createDateMidnight(Year.MIN_VALUE, 1, 8).minusWeeks(1)
    check(t, Year.MIN_VALUE, 1, 1, 0, 0, 0, 0)
  }

  test("minusWeeks_invalidTooLarge") {
    assertThrows[DateTimeException] {
      createDateMidnight(Year.MAX_VALUE, 12, 25).minusWeeks(-1)
    }
  }

  test("minusWeeks_invalidTooSmall") {
    assertThrows[DateTimeException] {
      createDateMidnight(Year.MIN_VALUE, 1, 7).minusWeeks(1)
    }
  }

  val provider_sampleMinusDaysSymmetry: List[LocalDateTime] =
    List(
      createDateMidnight(-1, 1, 1),
      createDateMidnight(-1, 2, 28),
      createDateMidnight(-1, 3, 1),
      createDateMidnight(-1, 12, 31),
      createDateMidnight(0, 1, 1),
      createDateMidnight(0, 2, 28),
      createDateMidnight(0, 2, 29),
      createDateMidnight(0, 3, 1),
      createDateMidnight(0, 12, 31),
      createDateMidnight(2007, 1, 1),
      createDateMidnight(2007, 2, 28),
      createDateMidnight(2007, 3, 1),
      createDateMidnight(2007, 12, 31),
      createDateMidnight(2008, 1, 1),
      createDateMidnight(2008, 2, 28),
      createDateMidnight(2008, 2, 29),
      createDateMidnight(2008, 3, 1),
      createDateMidnight(2008, 12, 31),
      createDateMidnight(2099, 1, 1),
      createDateMidnight(2099, 2, 28),
      createDateMidnight(2099, 3, 1),
      createDateMidnight(2099, 12, 31),
      createDateMidnight(2100, 1, 1),
      createDateMidnight(2100, 2, 28),
      createDateMidnight(2100, 3, 1),
      createDateMidnight(2100, 12, 31)
    )

  test("minusDays_symmetry") {
    provider_sampleMinusDaysSymmetry.foreach { reference =>
      var days: Int = 0
      while (days < 365 * 8) {
        {
          var t: LocalDateTime = reference.minusDays(days).minusDays(-days)
          assertEquals(t, reference)
          t = reference.minusDays(-days).minusDays(days)
          assertEquals(t, reference)
        }
        {
          days += 1
          days - 1
        }
      }
    }
  }

  test("minusDays_normal") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.minusDays(1)
    check(t, 2007, 7, 14, 12, 30, 40, 987654321)
  }

  test("minusDays_overMonths") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.minusDays(62)
    check(t, 2007, 5, 14, 12, 30, 40, 987654321)
  }

  test("minusDays_overYears") {
    val t: LocalDateTime = LocalDateTime.of(2008, 7, 16, 12, 30, 40, 987654321).minusDays(367)
    assertEquals(t, TEST_2007_07_15_12_30_40_987654321)
  }

  test("minusDays_overLeapYears") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.plusYears(2).minusDays(365 + 366)
    assertEquals(t, TEST_2007_07_15_12_30_40_987654321)
  }

  test("minusDays_negative") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.minusDays(-1)
    check(t, 2007, 7, 16, 12, 30, 40, 987654321)
  }

  test("minusDays_negativeAcrossYear") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.minusDays(-169)
    check(t, 2007, 12, 31, 12, 30, 40, 987654321)
  }

  test("minusDays_negativeOverYears") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.minusDays(-731)
    check(t, 2009, 7, 15, 12, 30, 40, 987654321)
  }

  test("minusDays_maximum") {
    val t: LocalDateTime = createDateMidnight(Year.MAX_VALUE, 12, 30).minusDays(-1)
    check(t, Year.MAX_VALUE, 12, 31, 0, 0, 0, 0)
  }

  test("minusDays_minimum") {
    val t: LocalDateTime = createDateMidnight(Year.MIN_VALUE, 1, 2).minusDays(1)
    check(t, Year.MIN_VALUE, 1, 1, 0, 0, 0, 0)
  }

  test("minusDays_invalidTooLarge") {
    assertThrows[DateTimeException] {
      createDateMidnight(Year.MAX_VALUE, 12, 31).minusDays(-1)
    }
  }

  test("minusDays_invalidTooSmall") {
    assertThrows[DateTimeException] {
      createDateMidnight(Year.MIN_VALUE, 1, 1).minusDays(1)
    }
  }

  test("minusDays_overflowTooLarge") {
    assertThrows[ArithmeticException] {
      createDateMidnight(Year.MAX_VALUE, 12, 31).minusDays(Long.MinValue)
    }
  }

  test("minusDays_overflowTooSmall") {
    assertThrows[ArithmeticException] {
      createDateMidnight(Year.MIN_VALUE, 1, 1).minusDays(Long.MaxValue)
    }
  }

  test("minusHours_one") {
    var t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.`with`(LocalTime.MIDNIGHT)
    var d: LocalDate     = t.toLocalDate

    {
      var i: Int = 0
      while (i < 50) {
        {
          t = t.minusHours(1)
          if (i % 24 == 0)
            d = d.minusDays(1)
          assertEquals(t.toLocalDate, d)
          assertEquals(t.getHour, (((-i + 23) % 24) + 24) % 24)
        }
        {
          i += 1
          i - 1
        }
      }
    }
  }

  test("minusHours_fromZero") {
    val base: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.`with`(LocalTime.MIDNIGHT)
    var d: LocalDate        = base.toLocalDate.plusDays(2)
    var t: LocalTime        = LocalTime.of(3, 0)

    {
      var i: Int = -50
      while (i < 50) {
        {
          val dt: LocalDateTime = base.minusHours(i)
          t = t.minusHours(1)
          if (t.getHour == 23)
            d = d.minusDays(1)
          assertEquals(dt.toLocalDate, d, String.valueOf(i))
          assertEquals(dt.toLocalTime, t)
        }
        {
          i += 1
          i - 1
        }
      }
    }
  }

  test("minusHours_fromOne") {
    val base: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.`with`(LocalTime.of(1, 0))
    var d: LocalDate        = base.toLocalDate.plusDays(2)
    var t: LocalTime        = LocalTime.of(4, 0)

    {
      var i: Int = -50
      while (i < 50) {
        {
          val dt: LocalDateTime = base.minusHours(i)
          t = t.minusHours(1)
          if (t.getHour == 23)
            d = d.minusDays(1)
          assertEquals(dt.toLocalDate, d, String.valueOf(i))
          assertEquals(dt.toLocalTime, t)
        }
        {
          i += 1
          i - 1
        }
      }
    }
  }

  test("minusMinutes_one") {
    var t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.`with`(LocalTime.MIDNIGHT)
    val d: LocalDate     = t.toLocalDate.minusDays(1)
    var hour: Int        = 0
    var min: Int         = 0

    {
      var i: Int = 0
      while (i < 70) {
        {
          t = t.minusMinutes(1)
          min -= 1
          if (min == -1) {
            hour -= 1
            min = 59
            if (hour == -1)
              hour = 23
          }
          assertEquals(t.toLocalDate, d)
          assertEquals(t.getHour, hour)
          assertEquals(t.getMinute, min)
        }
        {
          i += 1
          i - 1
        }
      }
    }
  }

  test("minusMinutes_fromZero") {
    val base: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.`with`(LocalTime.MIDNIGHT)
    var d: LocalDate        = base.toLocalDate.minusDays(1)
    var t: LocalTime        = LocalTime.of(22, 49)

    {
      var i: Int = 70
      while (i > -70) {
        {
          val dt: LocalDateTime = base.minusMinutes(i)
          t = t.plusMinutes(1)
          if (t eq LocalTime.MIDNIGHT)
            d = d.plusDays(1)
          assertEquals(dt.toLocalDate, d)
          assertEquals(dt.toLocalTime, t)
        }
        {
          i -= 1
          i + 1
        }
      }
    }
  }

  test("minusMinutes_noChange_oneDay") {
    val t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.minusMinutes(24 * 60)
    assertEquals(t.toLocalDate, TEST_2007_07_15_12_30_40_987654321.toLocalDate.minusDays(1))
  }

  test("minusSeconds_one") {
    var t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.`with`(LocalTime.MIDNIGHT)
    val d: LocalDate     = t.toLocalDate.minusDays(1)
    var hour: Int        = 0
    var min: Int         = 0
    var sec: Int         = 0

    {
      var i: Int = 0
      while (i < 3700) {
        {
          t = t.minusSeconds(1)
          sec -= 1
          if (sec == -1) {
            min -= 1
            sec = 59
            if (min == -1) {
              hour -= 1
              min = 59
              if (hour == -1)
                hour = 23
            }
          }
          assertEquals(t.toLocalDate, d)
          assertEquals(t.getHour, hour)
          assertEquals(t.getMinute, min)
          assertEquals(t.getSecond, sec)
        }
        {
          i += 1
          i - 1
        }
      }
    }
  }

  val minusSeconds_fromZero: java.util.Iterator[List[Any]] =
    new java.util.Iterator[List[Any]]() {
      private[bp] var delta: Int      = 30
      private[bp] var i: Int          = 3660
      private[bp] var date: LocalDate = TEST_2007_07_15_12_30_40_987654321.toLocalDate.minusDays(1)
      private[bp] var hour: Int       = 22
      private[bp] var min: Int        = 59
      private[bp] var sec: Int        = 0

      def hasNext: Boolean = i >= -3660

      def next: List[Any] = {
        i -= delta
        sec += delta
        if (sec >= 60) {
          min += 1
          sec -= 60
          if (min == 60) {
            hour += 1
            min = 0
            if (hour == 24)
              hour = 0
          }
        }
        if (i == 0)
          date = date.plusDays(1)
        List[Any](i, date, hour, min, sec)
      }

      override def remove(): Unit = throw new UnsupportedOperationException
    }

  test("minusSeconds_fromZero") {
    import scala.collection.JavaConverters._
    minusSeconds_fromZero.asScala.foreach {
      case (seconds: Int) :: (date: LocalDate) :: (hour: Int) :: (min: Int) :: (sec: Int) :: Nil =>
        val base: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.`with`(LocalTime.MIDNIGHT)
        val t: LocalDateTime    = base.minusSeconds(seconds)
        assertEquals(date, t.toLocalDate)
        assertEquals(hour, t.getHour)
        assertEquals(min, t.getMinute)
        assertEquals(sec, t.getSecond)
      case _                                                                                     =>
        fail()
    }
  }

  test("minusNanos_halfABillion") {
    var t: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.`with`(LocalTime.MIDNIGHT)
    val d: LocalDate     = t.toLocalDate.minusDays(1)
    var hour: Int        = 0
    var min: Int         = 0
    var sec: Int         = 0
    var nanos: Int       = 0
    var i: Long          = 0
    while (i < 3700 * 1000000000L) {
      t = t.minusNanos(500000000)
      nanos -= 500000000
      if (nanos < 0) {
        sec -= 1
        nanos += 1000000000
        if (sec == -1) {
          min -= 1
          sec += 60
          if (min == -1) {
            hour -= 1
            min += 60
            if (hour == -1)
              hour += 24
          }
        }
      }
      assertEquals(t.toLocalDate, d)
      assertEquals(t.getHour, hour)
      assertEquals(t.getMinute, min)
      assertEquals(t.getSecond, sec)
      assertEquals(t.getNano, nanos)
      i += 500000000
    }
  }

  val minusNanos_fromZero: java.util.Iterator[List[Any]] =
    new java.util.Iterator[List[Any]]() {
      private[bp] var delta: Long     = 7500000000L
      private[bp] var i: Long         = 3660 * 1000000000L
      private[bp] var date: LocalDate = TEST_2007_07_15_12_30_40_987654321.toLocalDate.minusDays(1)
      private[bp] var hour: Int       = 22
      private[bp] var min: Int        = 59
      private[bp] var sec: Int        = 0
      private[bp] var nanos: Long     = 0

      def hasNext: Boolean = i >= -3660 * 1000000000L

      def next: List[Any] = {
        i -= delta
        nanos += delta
        if (nanos >= 1000000000L) {
          sec += (nanos / 1000000000L).toInt // !!!
          nanos %= 1000000000L
          if (sec >= 60) {
            min += 1
            sec %= 60
            if (min == 60) {
              hour += 1
              min = 0
              if (hour == 24) {
                hour = 0
                date = date.plusDays(1)
              }
            }
          }
        }
        List(i, date, hour, min, sec, nanos.toInt)
      }

      override def remove(): Unit = throw new UnsupportedOperationException
    }

  test("minusNanos_fromZero") {
    import scala.collection.JavaConverters._
    minusNanos_fromZero.asScala.toList.foreach {
      case (nanoseconds: Long) :: (date: LocalDate) :: (hour: Int) :: (min: Int) :: (sec: Int) :: (nanos: Int) :: Nil =>
        val base: LocalDateTime = TEST_2007_07_15_12_30_40_987654321.`with`(LocalTime.MIDNIGHT)
        val t: LocalDateTime    = base.minusNanos(nanoseconds)
        assertEquals(date, t.toLocalDate)
        assertEquals(hour, t.getHour)
        assertEquals(min, t.getMinute)
        assertEquals(sec, t.getSecond)
        assertEquals(nanos, t.getNano)
      case _                                                                                                          =>
        fail()
    }
  }

  val provider_until: List[List[Any]] =
    List(
      List("2012-06-15T00:00", "2012-06-15T00:00", NANOS, 0L),
      List("2012-06-15T00:00", "2012-06-15T00:00", MICROS, 0L),
      List("2012-06-15T00:00", "2012-06-15T00:00", MILLIS, 0L),
      List("2012-06-15T00:00", "2012-06-15T00:00", SECONDS, 0L),
      List("2012-06-15T00:00", "2012-06-15T00:00", MINUTES, 0L),
      List("2012-06-15T00:00", "2012-06-15T00:00", HOURS, 0L),
      List("2012-06-15T00:00", "2012-06-15T00:00", HALF_DAYS, 0L),
      List("2012-06-15T00:00", "2012-06-15T00:00:01", NANOS, 1000000000L),
      List("2012-06-15T00:00", "2012-06-15T00:00:01", MICROS, 1000000L),
      List("2012-06-15T00:00", "2012-06-15T00:00:01", MILLIS, 1000L),
      List("2012-06-15T00:00", "2012-06-15T00:00:01", SECONDS, 1L),
      List("2012-06-15T00:00", "2012-06-15T00:00:01", MINUTES, 0L),
      List("2012-06-15T00:00", "2012-06-15T00:00:01", HOURS, 0L),
      List("2012-06-15T00:00", "2012-06-15T00:00:01", HALF_DAYS, 0L),
      List("2012-06-15T00:00", "2012-06-15T00:01", NANOS, 60000000000L),
      List("2012-06-15T00:00", "2012-06-15T00:01", MICROS, 60000000L),
      List("2012-06-15T00:00", "2012-06-15T00:01", MILLIS, 60000L),
      List("2012-06-15T00:00", "2012-06-15T00:01", SECONDS, 60L),
      List("2012-06-15T00:00", "2012-06-15T00:01", MINUTES, 1L),
      List("2012-06-15T00:00", "2012-06-15T00:01", HOURS, 0L),
      List("2012-06-15T00:00", "2012-06-15T00:01", HALF_DAYS, 0L),
      List("2012-06-15T12:30:40.500", "2012-06-15T12:30:39.499", SECONDS, -1L),
      List("2012-06-15T12:30:40.500", "2012-06-15T12:30:39.500", SECONDS, -1L),
      List("2012-06-15T12:30:40.500", "2012-06-15T12:30:39.501", SECONDS, 0L),
      List("2012-06-15T12:30:40.500", "2012-06-15T12:30:40.499", SECONDS, 0L),
      List("2012-06-15T12:30:40.500", "2012-06-15T12:30:40.500", SECONDS, 0L),
      List("2012-06-15T12:30:40.500", "2012-06-15T12:30:40.501", SECONDS, 0L),
      List("2012-06-15T12:30:40.500", "2012-06-15T12:30:41.499", SECONDS, 0L),
      List("2012-06-15T12:30:40.500", "2012-06-15T12:30:41.500", SECONDS, 1L),
      List("2012-06-15T12:30:40.500", "2012-06-15T12:30:41.501", SECONDS, 1L),
      List("2012-06-15T12:30:40.500", "2012-06-16T12:30:39.499", SECONDS, 86400 - 2L),
      List("2012-06-15T12:30:40.500", "2012-06-16T12:30:39.500", SECONDS, 86400 - 1L),
      List("2012-06-15T12:30:40.500", "2012-06-16T12:30:39.501", SECONDS, 86400 - 1L),
      List("2012-06-15T12:30:40.500", "2012-06-16T12:30:40.499", SECONDS, 86400 - 1L),
      List("2012-06-15T12:30:40.500", "2012-06-16T12:30:40.500", SECONDS, 86400 + 0L),
      List("2012-06-15T12:30:40.500", "2012-06-16T12:30:40.501", SECONDS, 86400 + 0L),
      List("2012-06-15T12:30:40.500", "2012-06-16T12:30:41.499", SECONDS, 86400 + 0L),
      List("2012-06-15T12:30:40.500", "2012-06-16T12:30:41.500", SECONDS, 86400 + 1L),
      List("2012-06-15T12:30:40.500", "2012-06-16T12:30:41.501", SECONDS, 86400 + 1L)
    )

  test("until") {
    provider_until.foreach {
      case (startStr: String) :: (endStr: String) :: (unit: TemporalUnit) :: (expected: Long) :: Nil =>
        val start: LocalDateTime = LocalDateTime.parse(startStr)
        val end: LocalDateTime   = LocalDateTime.parse(endStr)
        assertEquals(start.until(end, unit), expected)
      case _                                                                                         =>
        fail()
    }
  }

  test("until_reveresed") {
    provider_until.foreach {
      case (startStr: String) :: (endStr: String) :: (unit: TemporalUnit) :: (expected: Long) :: Nil =>
        val start: LocalDateTime = LocalDateTime.parse(startStr)
        val end: LocalDateTime   = LocalDateTime.parse(endStr)
        assertEquals(end.until(start, unit), -expected)
      case _                                                                                         =>
        fail()
    }
  }

  test("atZone") {
    val t: LocalDateTime = LocalDateTime.of(2008, 6, 30, 11, 30)
    assertEquals(
      t.atZone(TestLocalDateTime.ZONE_PARIS),
      ZonedDateTime.of(LocalDateTime.of(2008, 6, 30, 11, 30), TestLocalDateTime.ZONE_PARIS)
    )
  }

  test("atZone_Offset") {
    val t: LocalDateTime = LocalDateTime.of(2008, 6, 30, 11, 30)
    assertEquals(
      t.atZone(TestLocalDateTime.OFFSET_PTWO),
      ZonedDateTime.of(LocalDateTime.of(2008, 6, 30, 11, 30), TestLocalDateTime.OFFSET_PTWO)
    )
  }

  test("atZone_dstGap") {
    val t: LocalDateTime = LocalDateTime.of(2007, 4, 1, 0, 0)
    assertEquals(t.atZone(TestLocalDateTime.ZONE_GAZA),
                 ZonedDateTime.of(LocalDateTime.of(2007, 4, 1, 1, 0), TestLocalDateTime.ZONE_GAZA)
    )
  }

  test("atZone_dstOverlap") {
    val t: LocalDateTime = LocalDateTime.of(2007, 10, 28, 2, 30)
    assertEquals(
      t.atZone(TestLocalDateTime.ZONE_PARIS),
      ZonedDateTime.ofStrict(LocalDateTime.of(2007, 10, 28, 2, 30),
                             TestLocalDateTime.OFFSET_PTWO,
                             TestLocalDateTime.ZONE_PARIS
      )
    )
  }

  test("atZone_nullTimeZone") {
    assertThrows[NullPointerException] {
      val t: LocalDateTime = LocalDateTime.of(2008, 6, 30, 11, 30)
      t.atZone(null.asInstanceOf[ZoneId])
    }
  }

  test("toEpochSecond_afterEpoch") {
    var i: Int = -5
    while (i < 5) {
      val offset: ZoneOffset = ZoneOffset.ofHours(i)
      var j: Int             = 0
      while (j < 100000) {
        val a: LocalDateTime = LocalDateTime.of(1970, 1, 1, 0, 0).plusSeconds(j)
        assertEquals(a.toEpochSecond(offset), j - i * 3600)
        j += 1
      }
      i += 1
    }
  }

  test("toEpochSecond_beforeEpoch") {
    var i: Int = 0
    while (i < 100000) {
      val a: LocalDateTime = LocalDateTime.of(1970, 1, 1, 0, 0).minusSeconds(i)
      assertEquals(a.toEpochSecond(ZoneOffset.UTC), -i)
      i += 1
    }
  }

  test("comparisons") {
    test_comparisons_LocalDateTime(
      LocalDate.of(Year.MIN_VALUE, 1, 1),
      LocalDate.of(Year.MIN_VALUE, 12, 31),
      LocalDate.of(-1, 1, 1),
      LocalDate.of(-1, 12, 31),
      LocalDate.of(0, 1, 1),
      LocalDate.of(0, 12, 31),
      LocalDate.of(1, 1, 1),
      LocalDate.of(1, 12, 31),
      LocalDate.of(2008, 1, 1),
      LocalDate.of(2008, 2, 29),
      LocalDate.of(2008, 12, 31),
      LocalDate.of(Year.MAX_VALUE, 1, 1),
      LocalDate.of(Year.MAX_VALUE, 12, 31)
    )
  }

  private def test_comparisons_LocalDateTime(localDates: LocalDate*): Unit =
    test_comparisons_LocalDateTime(
      localDates.toArray,
      LocalTime.MIDNIGHT,
      LocalTime.of(0, 0, 0, 999999999),
      LocalTime.of(0, 0, 59, 0),
      LocalTime.of(0, 0, 59, 999999999),
      LocalTime.of(0, 59, 0, 0),
      LocalTime.of(0, 59, 59, 999999999),
      LocalTime.NOON,
      LocalTime.of(12, 0, 0, 999999999),
      LocalTime.of(12, 0, 59, 0),
      LocalTime.of(12, 0, 59, 999999999),
      LocalTime.of(12, 59, 0, 0),
      LocalTime.of(12, 59, 59, 999999999),
      LocalTime.of(23, 0, 0, 0),
      LocalTime.of(23, 0, 0, 999999999),
      LocalTime.of(23, 0, 59, 0),
      LocalTime.of(23, 0, 59, 999999999),
      LocalTime.of(23, 59, 0, 0),
      LocalTime.of(23, 59, 59, 999999999)
    )

  private def test_comparisons_LocalDateTime(
    localDates: Array[LocalDate],
    localTimes: LocalTime*
  ): Unit = {
    val localDateTimes: Array[LocalDateTime] =
      new Array[LocalDateTime](localDates.length * localTimes.length)
    var i: Int                               = 0
    for (localDate <- localDates)
      for (localTime <- localTimes)
        localDateTimes {
          i += 1
          i - 1
        } = LocalDateTime.of(localDate, localTime)
    doTest_comparisons_LocalDateTime(localDateTimes)
  }

  private def doTest_comparisons_LocalDateTime(localDateTimes: Array[LocalDateTime]): Unit = {
    var i: Int = 0
    while (i < localDateTimes.length) {
      val a: LocalDateTime = localDateTimes(i)
      var j: Int           = 0
      while (j < localDateTimes.length) {
        val b: LocalDateTime = localDateTimes(j)
        if (i < j) {
          assertTrue(a.compareTo(b) < 0)
          assertEquals(a.isBefore(b), true, s"$a <=> $b")
          assertEquals(a.isAfter(b), false, s"$a <=> $b")
          assertEquals(a == b, false, s"$a <=> $b")
        } else if (i > j) {
          assertTrue(a.compareTo(b) > 0)
          assertEquals(a.isBefore(b), false, s"$a <=> $b")
          assertEquals(a.isAfter(b), true, s"$a <=> $b")
          assertEquals(a == b, false, s"$a <=> $b")
        } else {
          assertEquals(a.compareTo(b), 0, s"$a <=> $b")
          assertEquals(a.isBefore(b), false, s"$a <=> $b")
          assertEquals(a.isAfter(b), false, s"$a <=> $b")
          assertEquals(a == b, true, s"$a <=> $b")
        }
        j += 1
      }
      i += 1
    }
  }

  test("compareTo_ObjectNull") {
    assertThrows[Platform.NPE] {
      TEST_2007_07_15_12_30_40_987654321.compareTo(null)
    }
  }

  test("isBefore_ObjectNull") {
    assertThrows[Platform.NPE] {
      TEST_2007_07_15_12_30_40_987654321.isBefore(null)
    }
  }

  test("isAfter_ObjectNull") {
    assertThrows[Platform.NPE] {
      TEST_2007_07_15_12_30_40_987654321.isAfter(null)
    }
  }

  val provider_sampleDateTimes: List[List[Int]] =
    for {
      d <- provider_sampleDates
      h <- provider_sampleTimes
    } yield d ::: h

  test("test_equals_true") {
    provider_sampleDateTimes.foreach {
      case y :: m :: d :: h :: mi :: s :: n :: Nil =>
        val a: LocalDateTime = LocalDateTime.of(y, m, d, h, mi, s, n)
        val b: LocalDateTime = LocalDateTime.of(y, m, d, h, mi, s, n)
        assertTrue(a == b)
      case _                                       =>
        fail()
    }
  }

  test("test_equals_false_year_differs") {
    provider_sampleDateTimes.foreach {
      case y :: m :: d :: h :: mi :: s :: n :: Nil =>
        val a: LocalDateTime = LocalDateTime.of(y, m, d, h, mi, s, n)
        val b: LocalDateTime = LocalDateTime.of(y + 1, m, d, h, mi, s, n)
        assertFalse(a == b)
      case _                                       =>
        fail()
    }
  }

  test("test_equals_false_month_differs") {
    provider_sampleDateTimes.foreach {
      case y :: m :: d :: h :: mi :: s :: n :: Nil =>
        val a: LocalDateTime = LocalDateTime.of(y, m, d, h, mi, s, n)
        val b: LocalDateTime = LocalDateTime.of(y, m + 1, d, h, mi, s, n)
        assertFalse(a == b)
      case _                                       =>
        fail()
    }
  }

  test("test_equals_false_day_differs") {
    provider_sampleDateTimes.foreach {
      case y :: m :: d :: h :: mi :: s :: n :: Nil =>
        val a: LocalDateTime = LocalDateTime.of(y, m, d, h, mi, s, n)
        val b: LocalDateTime = LocalDateTime.of(y, m, d + 1, h, mi, s, n)
        assertFalse(a == b)
      case _                                       =>
        fail()
    }
  }

  test("test_equals_false_hour_differs") {
    provider_sampleDateTimes.foreach {
      case y :: m :: d :: h :: mi :: s :: n :: Nil =>
        val a: LocalDateTime = LocalDateTime.of(y, m, d, h, mi, s, n)
        val b: LocalDateTime = LocalDateTime.of(y, m, d, h + 1, mi, s, n)
        assertFalse(a == b)
      case _                                       =>
        fail()
    }
  }

  test("test_equals_false_minute_differs") {
    provider_sampleDateTimes.foreach {
      case y :: m :: d :: h :: mi :: s :: n :: Nil =>
        val a: LocalDateTime = LocalDateTime.of(y, m, d, h, mi, s, n)
        val b: LocalDateTime = LocalDateTime.of(y, m, d, h, mi + 1, s, n)
        assertFalse(a == b)
      case _                                       =>
        fail()
    }
  }

  test("test_equals_false_second_differs") {
    provider_sampleDateTimes.foreach {
      case y :: m :: d :: h :: mi :: s :: n :: Nil =>
        val a: LocalDateTime = LocalDateTime.of(y, m, d, h, mi, s, n)
        val b: LocalDateTime = LocalDateTime.of(y, m, d, h, mi, s + 1, n)
        assertFalse(a == b)
      case _                                       =>
        fail()
    }
  }

  test("test_equals_false_nano_differs") {
    provider_sampleDateTimes.foreach {
      case y :: m :: d :: h :: mi :: s :: n :: Nil =>
        val a: LocalDateTime = LocalDateTime.of(y, m, d, h, mi, s, n)
        val b: LocalDateTime = LocalDateTime.of(y, m, d, h, mi, s, n + 1)
        assertFalse(a == b)
      case _                                       =>
        fail()
    }
  }

  test("equals_itself_true") {
    assertEquals(TEST_2007_07_15_12_30_40_987654321 == TEST_2007_07_15_12_30_40_987654321, true)
  }

  test("equals_string_false") {
    assertNotEquals(TEST_2007_07_15_12_30_40_987654321, "2007-07-15T12:30:40.987654321")
  }

  test("equals_null_false") {
    assertEquals(TEST_2007_07_15_12_30_40_987654321 == null, false)
  }

  test("test_hashCode") {
    provider_sampleDateTimes.foreach {
      case y :: m :: d :: h :: mi :: s :: n :: Nil =>
        val a: LocalDateTime = LocalDateTime.of(y, m, d, h, mi, s, n)
        assertEquals(a.hashCode, a.hashCode)
        val b: LocalDateTime = LocalDateTime.of(y, m, d, h, mi, s, n)
        assertEquals(a.hashCode, b.hashCode)
      case _                                       =>
        fail()
    }
  }

  val provider_sampleToString: List[List[Any]] =
    List(
      List(2008, 7, 5, 2, 1, 0, 0, "2008-07-05T02:01"),
      List(2007, 12, 31, 23, 59, 1, 0, "2007-12-31T23:59:01"),
      List(999, 12, 31, 23, 59, 59, 990000000, "0999-12-31T23:59:59.990"),
      List(-1, 1, 2, 23, 59, 59, 999990000, "-0001-01-02T23:59:59.999990"),
      List(-2008, 1, 2, 23, 59, 59, 999999990, "-2008-01-02T23:59:59.999999990")
    )

  test("toString") {
    provider_sampleToString.foreach {
      case (y: Int) :: (m: Int) :: (d: Int) :: (h: Int) :: (mi: Int) :: (s: Int) :: (n: Int) :: (expected: String) :: Nil =>
        val t: LocalDateTime = LocalDateTime.of(y, m, d, h, mi, s, n)
        val str: String      = t.toString
        assertEquals(str, expected)
      case _                                                                                                              =>
        fail()
    }
  }

  test("format_formatter") {
    val f: DateTimeFormatter = DateTimeFormatter.ofPattern("y M d H m s")
    val t: String            = LocalDateTime.of(2010, 12, 3, 11, 30, 45).format(f)
    assertEquals(t, "2010 12 3 11 30 45")
  }

  test("format_formatter_null") {
    assertThrows[NullPointerException] {
      LocalDateTime.of(2010, 12, 3, 11, 30, 45).format(null)
    }
  }
}
