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

import org.threeten.bp.Month.JANUARY
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
import org.threeten.bp.temporal.ChronoField.INSTANT_SECONDS
import org.threeten.bp.temporal.ChronoField.MICRO_OF_DAY
import org.threeten.bp.temporal.ChronoField.MICRO_OF_SECOND
import org.threeten.bp.temporal.ChronoField.MILLI_OF_DAY
import org.threeten.bp.temporal.ChronoField.MILLI_OF_SECOND
import org.threeten.bp.temporal.ChronoField.MINUTE_OF_DAY
import org.threeten.bp.temporal.ChronoField.MINUTE_OF_HOUR
import org.threeten.bp.temporal.ChronoField.MONTH_OF_YEAR
import org.threeten.bp.temporal.ChronoField.NANO_OF_DAY
import org.threeten.bp.temporal.ChronoField.NANO_OF_SECOND
import org.threeten.bp.temporal.ChronoField.OFFSET_SECONDS
import org.threeten.bp.temporal.ChronoField.PROLEPTIC_MONTH
import org.threeten.bp.temporal.ChronoField.SECOND_OF_DAY
import org.threeten.bp.temporal.ChronoField.SECOND_OF_MINUTE
import org.threeten.bp.temporal.ChronoField.YEAR
import org.threeten.bp.temporal.ChronoField.YEAR_OF_ERA
import org.threeten.bp.temporal.ChronoUnit.HOURS
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
import org.threeten.bp.temporal.TemporalAccessor
import org.threeten.bp.temporal.TemporalAdjuster
import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.temporal.TemporalQueries
import org.threeten.bp.temporal.TemporalQuery
import org.threeten.bp.temporal.ValueRange
import org.threeten.bp.temporal.UnsupportedTemporalTypeException

/** Test ZonedDateTime. */
object TestZonedDateTime {
  val OFFSET_0100: ZoneOffset = ZoneOffset.ofHours(1)
  val OFFSET_0200: ZoneOffset = ZoneOffset.ofHours(2)
  val OFFSET_0130: ZoneOffset = ZoneOffset.of("+01:30")
  val OFFSET_MAX: ZoneOffset  = ZoneOffset.ofHours(18)
  val OFFSET_MIN: ZoneOffset  = ZoneOffset.ofHours(-18)
  val ZONE_0100: ZoneId       = OFFSET_0100
  val ZONE_0200: ZoneId       = OFFSET_0200
  val ZONE_M0100: ZoneId      = ZoneOffset.ofHours(-1)
  val ZONE_PARIS: ZoneId      = ZoneId.of("Europe/Paris")

  private def dateTime(
    year:       Int,
    month:      Int,
    dayOfMonth: Int,
    hour:       Int,
    minute:     Int
  ): LocalDateTime =
    LocalDateTime.of(year, month, dayOfMonth, hour, minute)

  private def dateTime(
    year:         Int,
    month:        Int,
    dayOfMonth:   Int,
    hour:         Int,
    minute:       Int,
    second:       Int,
    nanoOfSecond: Int
  ): LocalDateTime =
    LocalDateTime.of(year, month, dayOfMonth, hour, minute, second, nanoOfSecond)

  private def dateTime(
    year:         Int,
    month:        Int,
    dayOfMonth:   Int,
    hour:         Int,
    minute:       Int,
    second:       Int,
    nanoOfSecond: Int,
    offset:       ZoneOffset,
    zoneId:       ZoneId
  ): ZonedDateTime =
    ZonedDateTime.ofStrict(
      LocalDateTime.of(year, month, dayOfMonth, hour, minute, second, nanoOfSecond),
      offset,
      zoneId
    )
}

class TestZonedDateTime
    extends AnyFunSuite
    with GenDateTimeTest
    with AssertionsHelper
    with BeforeAndAfter {
  private var TEST_PARIS_GAP_2008_03_30_02_30: LocalDateTime     = null
  private var TEST_PARIS_OVERLAP_2008_10_26_02_30: LocalDateTime = null
  private var TEST_LOCAL_2008_06_30_11_30_59_500: LocalDateTime  = null
  private var TEST_DATE_TIME: ZonedDateTime                      = null
  private var TEST_DATE_TIME_PARIS: ZonedDateTime                = null

  before {
    TEST_LOCAL_2008_06_30_11_30_59_500 = LocalDateTime.of(2008, 6, 30, 11, 30, 59, 500)
    TEST_DATE_TIME =
      ZonedDateTime.of(TEST_LOCAL_2008_06_30_11_30_59_500, TestZonedDateTime.ZONE_0100)
    TEST_DATE_TIME_PARIS =
      ZonedDateTime.of(TEST_LOCAL_2008_06_30_11_30_59_500, TestZonedDateTime.ZONE_PARIS)
    TEST_PARIS_OVERLAP_2008_10_26_02_30 = LocalDateTime.of(2008, 10, 26, 2, 30)
    TEST_PARIS_GAP_2008_03_30_02_30 = LocalDateTime.of(2008, 3, 30, 2, 30)
  }

  protected def samples: List[TemporalAccessor] =
    List(TEST_DATE_TIME)

  protected def validFields: List[TemporalField] =
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
      OFFSET_SECONDS,
      INSTANT_SECONDS,
      JulianFields.JULIAN_DAY,
      JulianFields.MODIFIED_JULIAN_DAY,
      JulianFields.RATA_DIE
    )

  protected def invalidFields: List[TemporalField] =
    List(ChronoField.values: _*).filterNot(validFields.contains)

  test("now") {
    var expected: ZonedDateTime = ZonedDateTime.now(Clock.systemDefaultZone)
    var test: ZonedDateTime     = ZonedDateTime.now
    var diff: Long              = Math.abs(test.toLocalTime.toNanoOfDay - expected.toLocalTime.toNanoOfDay)
    if (diff >= 100000000) {
      expected = ZonedDateTime.now(Clock.systemDefaultZone)
      test = ZonedDateTime.now
      diff = Math.abs(test.toLocalTime.toNanoOfDay - expected.toLocalTime.toNanoOfDay)
    }
    assertTrue(diff < 100000000)
  }

  test("now_ZoneId_nullZoneId") {
    assertThrows[NullPointerException] {
      ZonedDateTime.now(null.asInstanceOf[ZoneId])
    }
  }

  test("now_ZoneId") {
    val zone: ZoneId            = ZoneId.of("UTC+01:02:03")
    var expected: ZonedDateTime = ZonedDateTime.now(Clock.system(zone))
    var test: ZonedDateTime     = ZonedDateTime.now(zone)

    {
      var i: Int = 0
      while (i < 100) {
        {
          if (expected == test)
            i = 99
          expected = ZonedDateTime.now(Clock.system(zone))
          test = ZonedDateTime.now(zone)
        }
        {
          i += 1
          i - 1
        }
      }
    }
    var diff: Long = Math.abs(test.toLocalTime.toNanoOfDay - expected.toLocalTime.toNanoOfDay)
    if (diff >= 100000000) {
      expected = ZonedDateTime.now(Clock.systemDefaultZone)
      test = ZonedDateTime.now
      diff = Math.abs(test.toLocalTime.toNanoOfDay - expected.toLocalTime.toNanoOfDay)
    }
    assertTrue(diff < 100000000)
  }

  test("now_Clock_nullClock") {
    assertThrows[NullPointerException] {
      ZonedDateTime.now(null.asInstanceOf[Clock])
    }
  }

  test("now_Clock_allSecsInDay_utc") {
    var i: Int = 0
    while (i < (2 * 24 * 60 * 60)) {
      {
        val instant: Instant    = Instant.ofEpochSecond(i).plusNanos(123456789L)
        val clock: Clock        = Clock.fixed(instant, ZoneOffset.UTC)
        val test: ZonedDateTime = ZonedDateTime.now(clock)
        assertEquals(test.getYear, 1970)
        assertEquals(test.getMonth, Month.JANUARY)
        assertEquals(test.getDayOfMonth, if (i < 24 * 60 * 60) 1 else 2)
        assertEquals(test.getHour, (i / (60 * 60)) % 24)
        assertEquals(test.getMinute, (i / 60)      % 60)
        assertEquals(test.getSecond, i             % 60)
        assertEquals(test.getNano, 123456789)
        assertEquals(test.getOffset, ZoneOffset.UTC)
        assertEquals(test.getZone, ZoneOffset.UTC)
      }
      {
        i += 1
        i - 1
      }
    }
  }

  test("now_Clock_allSecsInDay_zone") {
    val zone: ZoneId = ZoneId.of("Europe/London")

    {
      var i: Int = 0
      while (i < (2 * 24 * 60 * 60)) {
        {
          val instant: Instant        = Instant.ofEpochSecond(i).plusNanos(123456789L)
          val expected: ZonedDateTime = ZonedDateTime.ofInstant(instant, zone)
          val clock: Clock            = Clock.fixed(expected.toInstant, zone)
          val test: ZonedDateTime     = ZonedDateTime.now(clock)
          assertEquals(test, expected)
        }
        {
          i += 1
          i - 1
        }
      }
    }
  }

  test("now_Clock_allSecsInDay_beforeEpoch") {
    var expected: LocalTime = LocalTime.MIDNIGHT.plusNanos(123456789L)

    {
      var i: Int = -1
      while (i >= -(24 * 60 * 60)) {
        {
          val instant: Instant    = Instant.ofEpochSecond(i).plusNanos(123456789L)
          val clock: Clock        = Clock.fixed(instant, ZoneOffset.UTC)
          val test: ZonedDateTime = ZonedDateTime.now(clock)
          assertEquals(test.getYear, 1969)
          assertEquals(test.getMonth, Month.DECEMBER)
          assertEquals(test.getDayOfMonth, 31)
          expected = expected.minusSeconds(1)
          assertEquals(test.toLocalTime, expected)
          assertEquals(test.getOffset, ZoneOffset.UTC)
          assertEquals(test.getZone, ZoneOffset.UTC)
        }
        {
          i -= 1
          i + 1
        }
      }
    }
  }

  test("now_Clock_offsets") {
    val base: ZonedDateTime = ZonedDateTime.of(LocalDateTime.of(1970, 1, 1, 12, 0), ZoneOffset.UTC)

    {
      var i: Int = -9
      while (i < 15) {
        {
          val offset: ZoneOffset  = ZoneOffset.ofHours(i)
          val clock: Clock        = Clock.fixed(base.toInstant, offset)
          val test: ZonedDateTime = ZonedDateTime.now(clock)
          assertEquals(test.getHour, (12 + i) % 24)
          assertEquals(test.getMinute, 0)
          assertEquals(test.getSecond, 0)
          assertEquals(test.getNano, 0)
          assertEquals(test.getOffset, offset)
          assertEquals(test.getZone, offset)
        }
        {
          i += 1
          i - 1
        }
      }
    }
  }

  private def check(
    test:   ZonedDateTime,
    y:      Int,
    m:      Int,
    d:      Int,
    h:      Int,
    min:    Int,
    s:      Int,
    n:      Int,
    offset: ZoneOffset,
    zone:   ZoneId
  ): Unit = {
    assertEquals(test.getYear, y)
    assertEquals(test.getMonth.getValue, m)
    assertEquals(test.getDayOfMonth, d)
    assertEquals(test.getHour, h)
    assertEquals(test.getMinute, min)
    assertEquals(test.getSecond, s)
    assertEquals(test.getNano, n)
    assertEquals(test.getOffset, offset)
    assertEquals(test.getZone, zone)
  }

  test("factory_of_LocalDateTime") {
    val base: LocalDateTime = LocalDateTime.of(2008, 6, 30, 11, 30, 10, 500)
    val test: ZonedDateTime = ZonedDateTime.of(base, TestZonedDateTime.ZONE_PARIS)
    check(test,
          2008,
          6,
          30,
          11,
          30,
          10,
          500,
          TestZonedDateTime.OFFSET_0200,
          TestZonedDateTime.ZONE_PARIS
    )
  }

  test("factory_of_LocalDateTime_nullDateTime") {
    assertThrows[NullPointerException] {
      ZonedDateTime.of(null.asInstanceOf[LocalDateTime], TestZonedDateTime.ZONE_PARIS)
    }
  }

  test("factory_of_LocalDateTime_nullZone") {
    assertThrows[NullPointerException] {
      val base: LocalDateTime = LocalDateTime.of(2008, 6, 30, 11, 30, 10, 500)
      ZonedDateTime.of(base, null)
    }
  }

  test("factory_ofInstant_Instant_ZR") {
    val instant: Instant    =
      LocalDateTime.of(2008, 6, 30, 11, 30, 10, 35).toInstant(TestZonedDateTime.OFFSET_0200)
    val test: ZonedDateTime = ZonedDateTime.ofInstant(instant, TestZonedDateTime.ZONE_PARIS)
    check(test,
          2008,
          6,
          30,
          11,
          30,
          10,
          35,
          TestZonedDateTime.OFFSET_0200,
          TestZonedDateTime.ZONE_PARIS
    )
  }

  test("factory_ofInstant_Instant_ZO") {
    val instant: Instant    =
      LocalDateTime.of(2008, 6, 30, 11, 30, 10, 45).toInstant(TestZonedDateTime.OFFSET_0200)
    val test: ZonedDateTime = ZonedDateTime.ofInstant(instant, TestZonedDateTime.OFFSET_0200)
    check(test,
          2008,
          6,
          30,
          11,
          30,
          10,
          45,
          TestZonedDateTime.OFFSET_0200,
          TestZonedDateTime.OFFSET_0200
    )
  }

  test("factory_ofInstant_Instant_inGap") {
    val instant: Instant    = TEST_PARIS_GAP_2008_03_30_02_30.toInstant(TestZonedDateTime.OFFSET_0100)
    val test: ZonedDateTime = ZonedDateTime.ofInstant(instant, TestZonedDateTime.ZONE_PARIS)
    check(test,
          2008,
          3,
          30,
          3,
          30,
          0,
          0,
          TestZonedDateTime.OFFSET_0200,
          TestZonedDateTime.ZONE_PARIS
    )
  }

  test("factory_ofInstant_Instant_inOverlap_earlier") {
    val instant: Instant    =
      TEST_PARIS_OVERLAP_2008_10_26_02_30.toInstant(TestZonedDateTime.OFFSET_0200)
    val test: ZonedDateTime = ZonedDateTime.ofInstant(instant, TestZonedDateTime.ZONE_PARIS)
    check(test,
          2008,
          10,
          26,
          2,
          30,
          0,
          0,
          TestZonedDateTime.OFFSET_0200,
          TestZonedDateTime.ZONE_PARIS
    )
  }

  test("factory_ofInstant_Instant_inOverlap_later") {
    val instant: Instant    =
      TEST_PARIS_OVERLAP_2008_10_26_02_30.toInstant(TestZonedDateTime.OFFSET_0100)
    val test: ZonedDateTime = ZonedDateTime.ofInstant(instant, TestZonedDateTime.ZONE_PARIS)
    check(test,
          2008,
          10,
          26,
          2,
          30,
          0,
          0,
          TestZonedDateTime.OFFSET_0100,
          TestZonedDateTime.ZONE_PARIS
    )
  }

  test("factory_ofInstant_Instant_invalidOffset") {
    val instant: Instant    =
      LocalDateTime.of(2008, 6, 30, 11, 30, 10, 500).toInstant(TestZonedDateTime.OFFSET_0130)
    val test: ZonedDateTime = ZonedDateTime.ofInstant(instant, TestZonedDateTime.ZONE_PARIS)
    check(test,
          2008,
          6,
          30,
          12,
          0,
          10,
          500,
          TestZonedDateTime.OFFSET_0200,
          TestZonedDateTime.ZONE_PARIS
    )
  }

  test("factory_ofInstant_allSecsInDay") {
    var i: Int = 0
    while (i < (24 * 60 * 60)) {
      {
        val instant: Instant    = Instant.ofEpochSecond(i)
        val test: ZonedDateTime = ZonedDateTime.ofInstant(instant, TestZonedDateTime.OFFSET_0100)
        assertEquals(test.getYear, 1970)
        assertEquals(test.getMonth, Month.JANUARY)
        assertEquals(test.getDayOfMonth, 1 + (if (i >= 23 * 60 * 60) 1 else 0))
        assertEquals(test.getHour, ((i / (60 * 60)) + 1) % 24)
        assertEquals(test.getMinute, (i / 60)            % 60)
        assertEquals(test.getSecond, i                   % 60)
      }
      {
        i += 1
        i - 1
      }
    }
  }

  test("factory_ofInstant_allDaysInCycle") {
    var expected: ZonedDateTime = LocalDateTime.of(1970, 1, 1, 0, 0, 0, 0).atZone(ZoneOffset.UTC)

    {
      var i: Long = 0
      while (i < 146097) {
        {
          val instant: Instant    = Instant.ofEpochSecond(i * 24L * 60L * 60L)
          val test: ZonedDateTime = ZonedDateTime.ofInstant(instant, ZoneOffset.UTC)
          assertEquals(test, expected)
          expected = expected.plusDays(1)
        }
        {
          i += 1
          i - 1
        }
      }
    }
  }

  test("factory_ofInstant_minWithMinOffset") {
    val days_0000_to_1970: Long = (146097 * 5) - (30 * 365 + 7)
    val year: Int               = Year.MIN_VALUE
    val days: Long              = (year * 365L + (year / 4 - year / 100 + year / 400)) - days_0000_to_1970
    val instant: Instant        =
      Instant.ofEpochSecond(days * 24L * 60L * 60L - TestZonedDateTime.OFFSET_MIN.getTotalSeconds)
    val test: ZonedDateTime     = ZonedDateTime.ofInstant(instant, TestZonedDateTime.OFFSET_MIN)
    assertEquals(test.getYear, Year.MIN_VALUE)
    assertEquals(test.getMonth.getValue, 1)
    assertEquals(test.getDayOfMonth, 1)
    assertEquals(test.getOffset, TestZonedDateTime.OFFSET_MIN)
    assertEquals(test.getHour, 0)
    assertEquals(test.getMinute, 0)
    assertEquals(test.getSecond, 0)
    assertEquals(test.getNano, 0)
  }

  test("factory_ofInstant_minWithMaxOffset") {
    val days_0000_to_1970: Long = (146097 * 5) - (30 * 365 + 7)
    val year: Int               = Year.MIN_VALUE
    val days: Long              = (year * 365L + (year / 4 - year / 100 + year / 400)) - days_0000_to_1970
    val instant: Instant        =
      Instant.ofEpochSecond(days * 24L * 60L * 60L - TestZonedDateTime.OFFSET_MAX.getTotalSeconds)
    val test: ZonedDateTime     = ZonedDateTime.ofInstant(instant, TestZonedDateTime.OFFSET_MAX)
    assertEquals(test.getYear, Year.MIN_VALUE)
    assertEquals(test.getMonth.getValue, 1)
    assertEquals(test.getDayOfMonth, 1)
    assertEquals(test.getOffset, TestZonedDateTime.OFFSET_MAX)
    assertEquals(test.getHour, 0)
    assertEquals(test.getMinute, 0)
    assertEquals(test.getSecond, 0)
    assertEquals(test.getNano, 0)
  }

  test("factory_ofInstant_maxWithMinOffset") {
    val days_0000_to_1970: Long = (146097 * 5) - (30 * 365 + 7)
    val year: Int               = Year.MAX_VALUE
    val days: Long              = (year * 365L + (year / 4 - year / 100 + year / 400)) + 365 - days_0000_to_1970
    val instant: Instant        = Instant.ofEpochSecond(
      (days + 1) * 24L * 60L * 60L - 1 - TestZonedDateTime.OFFSET_MIN.getTotalSeconds
    )
    val test: ZonedDateTime     = ZonedDateTime.ofInstant(instant, TestZonedDateTime.OFFSET_MIN)
    assertEquals(test.getYear, Year.MAX_VALUE)
    assertEquals(test.getMonth.getValue, 12)
    assertEquals(test.getDayOfMonth, 31)
    assertEquals(test.getOffset, TestZonedDateTime.OFFSET_MIN)
    assertEquals(test.getHour, 23)
    assertEquals(test.getMinute, 59)
    assertEquals(test.getSecond, 59)
    assertEquals(test.getNano, 0)
  }

  test("factory_ofInstant_maxWithMaxOffset") {
    val days_0000_to_1970: Long = (146097 * 5) - (30 * 365 + 7)
    val year: Int               = Year.MAX_VALUE
    val days: Long              = (year * 365L + (year / 4 - year / 100 + year / 400)) + 365 - days_0000_to_1970
    val instant: Instant        = Instant.ofEpochSecond(
      (days + 1) * 24L * 60L * 60L - 1 - TestZonedDateTime.OFFSET_MAX.getTotalSeconds
    )
    val test: ZonedDateTime     = ZonedDateTime.ofInstant(instant, TestZonedDateTime.OFFSET_MAX)
    assertEquals(test.getYear, Year.MAX_VALUE)
    assertEquals(test.getMonth.getValue, 12)
    assertEquals(test.getDayOfMonth, 31)
    assertEquals(test.getOffset, TestZonedDateTime.OFFSET_MAX)
    assertEquals(test.getHour, 23)
    assertEquals(test.getMinute, 59)
    assertEquals(test.getSecond, 59)
    assertEquals(test.getNano, 0)
  }

  test("factory_ofInstant_maxInstantWithMaxOffset") {
    assertThrows[DateTimeException] {
      val instant: Instant = Instant.ofEpochSecond(Long.MaxValue)
      ZonedDateTime.ofInstant(instant, TestZonedDateTime.OFFSET_MAX)
    }
  }

  test("factory_ofInstant_maxInstantWithMinOffset") {
    assertThrows[DateTimeException] {
      val instant: Instant = Instant.ofEpochSecond(Long.MaxValue)
      ZonedDateTime.ofInstant(instant, TestZonedDateTime.OFFSET_MIN)
    }
  }

  test("factory_ofInstant_tooBig") {
    assertThrows[DateTimeException] {
      val days_0000_to_1970: Long = (146097 * 5) - (30 * 365 + 7)
      val year: Long              = Year.MAX_VALUE + 1L
      val days: Long              = (year * 365L + (year / 4 - year / 100 + year / 400)) - days_0000_to_1970
      val instant: Instant        = Instant.ofEpochSecond(days * 24L * 60L * 60L)
      ZonedDateTime.ofInstant(instant, ZoneOffset.UTC)
    }
  }

  test("factory_ofInstant_tooLow") {
    assertThrows[DateTimeException] {
      val days_0000_to_1970: Long = (146097 * 5) - (30 * 365 + 7)
      val year: Int               = Year.MIN_VALUE - 1
      val days: Long              = (year * 365L + (year / 4 - year / 100 + year / 400)) - days_0000_to_1970
      val instant: Instant        = Instant.ofEpochSecond(days * 24L * 60L * 60L)
      ZonedDateTime.ofInstant(instant, ZoneOffset.UTC)
    }
  }

  test("factory_ofInstant_Instant_nullInstant") {
    assertThrows[NullPointerException] {
      ZonedDateTime.ofInstant(null.asInstanceOf[Instant], TestZonedDateTime.ZONE_0100)
    }
  }

  test("factory_ofInstant_Instant_nullZone") {
    assertThrows[NullPointerException] {
      ZonedDateTime.ofInstant(Instant.EPOCH, null)
    }
  }

  test("factory_ofStrict_LDT_ZI_ZO") {
    val normal: LocalDateTime = LocalDateTime.of(2008, 6, 30, 11, 30, 10, 500)
    val test: ZonedDateTime   =
      ZonedDateTime.ofStrict(normal, TestZonedDateTime.OFFSET_0200, TestZonedDateTime.ZONE_PARIS)
    check(test,
          2008,
          6,
          30,
          11,
          30,
          10,
          500,
          TestZonedDateTime.OFFSET_0200,
          TestZonedDateTime.ZONE_PARIS
    )
  }

  test("factory_ofStrict_LDT_ZI_ZO_inGap") {
    assertThrows[DateTimeException] {
      try ZonedDateTime.ofStrict(TEST_PARIS_GAP_2008_03_30_02_30,
                                 TestZonedDateTime.OFFSET_0100,
                                 TestZonedDateTime.ZONE_PARIS
      )
      catch {
        case ex: DateTimeException =>
          assertEquals(ex.getMessage.contains(" gap"), true)
          throw ex
      }
    }
  }

  test("factory_ofStrict_LDT_ZI_ZO_inOverlap_invalidOfset") {
    assertThrows[DateTimeException] {
      try ZonedDateTime.ofStrict(TEST_PARIS_OVERLAP_2008_10_26_02_30,
                                 TestZonedDateTime.OFFSET_0130,
                                 TestZonedDateTime.ZONE_PARIS
      )
      catch {
        case ex: DateTimeException =>
          assertEquals(ex.getMessage.contains(" is not valid for "), true)
          throw ex
      }
    }
  }

  test("factory_ofStrict_LDT_ZI_ZO_invalidOffset") {
    assertThrows[DateTimeException] {
      try ZonedDateTime.ofStrict(TEST_LOCAL_2008_06_30_11_30_59_500,
                                 TestZonedDateTime.OFFSET_0130,
                                 TestZonedDateTime.ZONE_PARIS
      )
      catch {
        case ex: DateTimeException =>
          assertEquals(ex.getMessage.contains(" is not valid for "), true)
          throw ex
      }
    }
  }

  test("factory_ofStrict_LDT_ZI_ZO_nullLDT") {
    assertThrows[NullPointerException] {
      ZonedDateTime.ofStrict(null.asInstanceOf[LocalDateTime],
                             TestZonedDateTime.OFFSET_0100,
                             TestZonedDateTime.ZONE_PARIS
      )
    }
  }

  test("factory_ofStrict_LDT_ZI_ZO_nullZO") {
    assertThrows[NullPointerException] {
      ZonedDateTime.ofStrict(TEST_LOCAL_2008_06_30_11_30_59_500, null, TestZonedDateTime.ZONE_PARIS)
    }
  }

  test("factory_ofStrict_LDT_ZI_ZO_nullZI") {
    assertThrows[NullPointerException] {
      ZonedDateTime.ofStrict(TEST_LOCAL_2008_06_30_11_30_59_500,
                             TestZonedDateTime.OFFSET_0100,
                             null
      )
    }
  }

  test("factory_from_DateTimeAccessor_ZDT") {
    assertEquals(ZonedDateTime.from(TEST_DATE_TIME_PARIS), TEST_DATE_TIME_PARIS)
  }

  test("factory_from_DateTimeAccessor_LDT_ZoneId") {
    assertEquals(
      ZonedDateTime.from(new TemporalAccessor() {
        def isSupported(field: TemporalField): Boolean =
          TEST_DATE_TIME_PARIS.toLocalDateTime.isSupported(field)

        def getLong(field: TemporalField): Long =
          TEST_DATE_TIME_PARIS.toLocalDateTime.getLong(field)

        @SuppressWarnings(Array("unchecked")) override def query[R](query: TemporalQuery[R]): R = {
          if (query eq TemporalQueries.zoneId)
            return TEST_DATE_TIME_PARIS.getZone.asInstanceOf[R]
          query.queryFrom(this)
        }

        override def get(field: TemporalField): Int =
          range(field).checkValidIntValue(getLong(field), field)

        override def range(field: TemporalField): ValueRange =
          if (field.isInstanceOf[ChronoField])
            if (isSupported(field)) field.range
            else throw new UnsupportedTemporalTypeException(s"Unsupported field: $field")
          else
            field.rangeRefinedBy(this)

      }),
      TEST_DATE_TIME_PARIS
    )
  }

  test("factory_from_DateTimeAccessor_Instant_ZoneId") {
    assertEquals(
      ZonedDateTime.from(new TemporalAccessor() {
        def isSupported(field: TemporalField): Boolean =
          (field eq INSTANT_SECONDS) || (field eq NANO_OF_SECOND)

        def getLong(field: TemporalField): Long =
          TEST_DATE_TIME_PARIS.toInstant.getLong(field)

        @SuppressWarnings(Array("unchecked")) override def query[R](query: TemporalQuery[R]): R = {
          if (query eq TemporalQueries.zoneId)
            return TEST_DATE_TIME_PARIS.getZone.asInstanceOf[R]
          query.queryFrom(this)
        }

        override def get(field: TemporalField): Int =
          range(field).checkValidIntValue(getLong(field), field)

        override def range(field: TemporalField): ValueRange =
          if (field.isInstanceOf[ChronoField])
            if (isSupported(field)) field.range
            else throw new UnsupportedTemporalTypeException(s"Unsupported field: $field")
          else
            field.rangeRefinedBy(this)
      }),
      TEST_DATE_TIME_PARIS
    )
  }

  test("factory_from_DateTimeAccessor_invalid_noDerive") {
    assertThrows[DateTimeException] {
      ZonedDateTime.from(LocalTime.of(12, 30))
    }
  }

  test("factory_from_DateTimeAccessor_null") {
    assertThrows[Platform.NPE] {
      ZonedDateTime.from(null.asInstanceOf[TemporalAccessor])
    }
  }

  test("test_parse") {
    provider_sampleToString.foreach {
      case (y: Int) :: (o: Int) :: (d: Int) :: (h: Int) :: (m: Int) :: (s: Int) :: (n: Int) :: (zoneId: String) :: (text: String) :: Nil =>
        val t: ZonedDateTime = ZonedDateTime.parse(text)
        assertEquals(t.getYear, y)
        assertEquals(t.getMonth.getValue, o)
        assertEquals(t.getDayOfMonth, d)
        assertEquals(t.getHour, h)
        assertEquals(t.getMinute, m)
        assertEquals(t.getSecond, s)
        assertEquals(t.getNano, n)
        assertEquals(t.getZone.getId, zoneId)
      case _                                                                                                                             =>
        fail()
    }
  }

  val data_parseAdditional: List[List[Any]] =
    List(
      List("2012-06-30T12:30:40Z[GMT]", 2012, 6, 30, 12, 30, 40, 0, "GMT"),
      List("2012-06-30T12:30:40Z[UT]", 2012, 6, 30, 12, 30, 40, 0, "UT"),
      List("2012-06-30T12:30:40Z[UTC]", 2012, 6, 30, 12, 30, 40, 0, "UTC"),
      List("2012-06-30T12:30:40+01:00[+01:00]", 2012, 6, 30, 12, 30, 40, 0, "+01:00"),
      List("2012-06-30T12:30:40+01:00[GMT+01:00]", 2012, 6, 30, 12, 30, 40, 0, "GMT+01:00"),
      List("2012-06-30T12:30:40+01:00[UT+01:00]", 2012, 6, 30, 12, 30, 40, 0, "UT+01:00"),
      List("2012-06-30T12:30:40+01:00[UTC+01:00]", 2012, 6, 30, 12, 30, 40, 0, "UTC+01:00"),
      List("2012-06-30T12:30:40-01:00[-01:00]", 2012, 6, 30, 12, 30, 40, 0, "-01:00"),
      List("2012-06-30T12:30:40-01:00[GMT-01:00]", 2012, 6, 30, 12, 30, 40, 0, "GMT-01:00"),
      List("2012-06-30T12:30:40-01:00[UT-01:00]", 2012, 6, 30, 12, 30, 40, 0, "UT-01:00"),
      List("2012-06-30T12:30:40-01:00[UTC-01:00]", 2012, 6, 30, 12, 30, 40, 0, "UTC-01:00"),
      List("2012-06-30T12:30:40+01:00[Europe/London]", 2012, 6, 30, 12, 30, 40, 0, "Europe/London")
    )

  test("test_parseAdditional") {
    data_parseAdditional.foreach {
      case (text: String) :: (y: Int) :: (month: Int) :: (d: Int) :: (h: Int) :: (m: Int) :: (s: Int) :: (n: Int) :: (zoneId: String) :: Nil =>
        val t: ZonedDateTime = ZonedDateTime.parse(text)
        assertEquals(t.getYear, y)
        assertEquals(t.getMonth.getValue, month)
        assertEquals(t.getDayOfMonth, d)
        assertEquals(t.getHour, h)
        assertEquals(t.getMinute, m)
        assertEquals(t.getSecond, s)
        assertEquals(t.getNano, n)
        assertEquals(t.getZone.getId, zoneId)
      case _                                                                                                                                 =>
        fail()
    }
  }

  test("factory_parse_illegalValue") {
    assertThrows[DateTimeParseException] {
      ZonedDateTime.parse("2008-06-32T11:15+01:00[Europe/Paris]")
    }
  }

  test("factory_parse_invalidValue") {
    assertThrows[DateTimeParseException] {
      ZonedDateTime.parse("2008-06-31T11:15+01:00[Europe/Paris]")
    }
  }

  test("factory_parse_nullText") {
    assertThrows[NullPointerException] {
      ZonedDateTime.parse(null.asInstanceOf[String])
    }
  }

  val data_parseOverlapRoundtrip: List[String] =
    List("2016-11-06T01:00-04:00[America/New_York]", "2016-10-30T02:00+02:00[Europe/Berlin]")

  test("test_parseFormatRoundtripWithZoneAndOffset") {
    data_parseOverlapRoundtrip.foreach { s =>
      val start = ZonedDateTime.parse(s)
      for { min <- 0 to 60 by 15 } {
        val t = start.plusMinutes(min)
        assertEquals(t, ZonedDateTime.parse(t.toString()))
      }
    }
  }

  val data_parseOverlapToInstant: List[(String, String)] =
    List(
      ("2016-11-06T01:00-04:00[America/New_York]", "2016-11-06T05:00:00Z"),
      ("2016-11-06T01:30-04:00[America/New_York]", "2016-11-06T05:30:00Z"),
      ("2016-11-06T01:00-05:00[America/New_York]", "2016-11-06T06:00:00Z"),
      ("2016-11-06T01:30-05:00[America/New_York]", "2016-11-06T06:30:00Z"),
      ("2016-11-06T02:00-05:00[America/New_York]", "2016-11-06T07:00:00Z"),
      ("2016-10-30T02:00+02:00[Europe/Berlin]", "2016-10-30T00:00:00Z"),
      ("2016-10-30T02:30+02:00[Europe/Berlin]", "2016-10-30T00:30:00Z"),
      ("2016-10-30T02:00+01:00[Europe/Berlin]", "2016-10-30T01:00:00Z"),
      ("2016-10-30T02:30+01:00[Europe/Berlin]", "2016-10-30T01:30:00Z"),
      ("2016-10-30T03:00+01:00[Europe/Berlin]", "2016-10-30T02:00:00Z")
    )

  test("parseWithZoneAndOffsetToInstant") {
    data_parseOverlapToInstant.foreach { case (z, i) =>
      val zdt     = ZonedDateTime.parse(z)
      val instant = Instant.parse(i)
      assertEquals(zdt.toInstant, instant)
    }
  }

  test("factory_parse_formatter") {
    val f: DateTimeFormatter = DateTimeFormatter.ofPattern("u M d H m s VV")
    val test: ZonedDateTime  = ZonedDateTime.parse("2010 12 3 11 30 0 Europe/London", f)
    assertEquals(
      test,
      ZonedDateTime.of(LocalDateTime.of(2010, 12, 3, 11, 30), ZoneId.of("Europe/London"))
    )
  }

  test("factory_parse_formatter_nullText") {
    assertThrows[NullPointerException] {
      val f: DateTimeFormatter = DateTimeFormatter.ofPattern("y M d H m s")
      ZonedDateTime.parse(null.asInstanceOf[String], f)
    }
  }

  test("factory_parse_formatter_nullFormatter") {
    assertThrows[NullPointerException] {
      ZonedDateTime.parse("ANY", null)
    }
  }

  val provider_sampleTimes: List[List[Any]] =
    List(
      List(2008, 6, 30, 11, 30, 20, 500, TestZonedDateTime.ZONE_0100),
      List(2008, 6, 30, 11, 0, 0, 0, TestZonedDateTime.ZONE_0100),
      List(2008, 6, 30, 11, 30, 20, 500, TestZonedDateTime.ZONE_PARIS),
      List(2008, 6, 30, 11, 0, 0, 0, TestZonedDateTime.ZONE_PARIS),
      List(2008, 6, 30, 23, 59, 59, 999999999, TestZonedDateTime.ZONE_0100),
      List(-1, 1, 1, 0, 0, 0, 0, TestZonedDateTime.ZONE_0100)
    )

  test("test_get") {
    provider_sampleTimes.foreach {
      case (y: Int) :: (o: Int) :: (d: Int) :: (h: Int) :: (m: Int) :: (s: Int) :: (n: Int) :: (zone: ZoneId) :: Nil =>
        val localDate: LocalDate         = LocalDate.of(y, o, d)
        val localTime: LocalTime         = LocalTime.of(h, m, s, n)
        val localDateTime: LocalDateTime = LocalDateTime.of(localDate, localTime)
        val offset: ZoneOffset           = zone.getRules.getOffset(localDateTime)
        val a: ZonedDateTime             = ZonedDateTime.of(localDateTime, zone)
        assertEquals(a.getYear, localDate.getYear)
        assertEquals(a.getMonth, localDate.getMonth)
        assertEquals(a.getDayOfMonth, localDate.getDayOfMonth)
        assertEquals(a.getDayOfYear, localDate.getDayOfYear)
        assertEquals(a.getDayOfWeek, localDate.getDayOfWeek)
        assertEquals(a.getHour, localTime.getHour)
        assertEquals(a.getMinute, localTime.getMinute)
        assertEquals(a.getSecond, localTime.getSecond)
        assertEquals(a.getNano, localTime.getNano)
        assertEquals(a.toLocalDate, localDate)
        assertEquals(a.toLocalTime, localTime)
        assertEquals(a.toLocalDateTime, localDateTime)
        if (zone.isInstanceOf[ZoneOffset])
          assertEquals(a.toString, localDateTime.toString + offset.toString)
        else
          assertEquals(a.toString,
                       localDateTime.toString + offset.toString + "[" + zone.toString + "]"
          )
      case _                                                                                                         =>
        fail()
    }
  }

  test("test_get_DateTimeField") {
    val test: ZonedDateTime = ZonedDateTime.of(LocalDateTime.of(2008, 6, 30, 12, 30, 40, 987654321),
                                               TestZonedDateTime.ZONE_0100
    )
    assertEquals(test.get(ChronoField.YEAR), 2008)
    assertEquals(test.get(ChronoField.MONTH_OF_YEAR), 6)
    assertEquals(test.get(ChronoField.DAY_OF_MONTH), 30)
    assertEquals(test.get(ChronoField.DAY_OF_WEEK), 1)
    assertEquals(test.get(ChronoField.DAY_OF_YEAR), 182)
    assertEquals(test.get(ChronoField.HOUR_OF_DAY), 12)
    assertEquals(test.get(ChronoField.MINUTE_OF_HOUR), 30)
    assertEquals(test.get(ChronoField.SECOND_OF_MINUTE), 40)
    assertEquals(test.get(ChronoField.NANO_OF_SECOND), 987654321)
    assertEquals(test.get(ChronoField.HOUR_OF_AMPM), 0)
    assertEquals(test.get(ChronoField.AMPM_OF_DAY), 1)
    assertEquals(test.get(ChronoField.OFFSET_SECONDS), 3600)
  }

  test("test_get_DateTimeField_long") {
    assertThrows[DateTimeException] {
      TEST_DATE_TIME.get(ChronoField.INSTANT_SECONDS)
    }
  }

  test("test_get_DateTimeField_invalidField") {
    assertThrows[DateTimeException] {
      TEST_DATE_TIME.get(MockFieldNoValue.INSTANCE)
    }
  }

  test("test_get_DateTimeField_null") {
    assertThrows[Platform.NPE] {
      TEST_DATE_TIME.get(null.asInstanceOf[TemporalField])
    }
  }

  test("test_getLong_DateTimeField") {
    val test: ZonedDateTime = ZonedDateTime.of(LocalDateTime.of(2008, 6, 30, 12, 30, 40, 987654321),
                                               TestZonedDateTime.ZONE_0100
    )
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
    assertEquals(test.getLong(ChronoField.OFFSET_SECONDS), 3600)
    assertEquals(test.getLong(ChronoField.INSTANT_SECONDS), test.toEpochSecond)
  }

  test("test_getLong_DateTimeField_invalidField") {
    assertThrows[DateTimeException] {
      TEST_DATE_TIME.getLong(MockFieldNoValue.INSTANCE)
    }
  }

  test("test_getLong_DateTimeField_null") {
    assertThrows[Platform.NPE] {
      TEST_DATE_TIME.getLong(null.asInstanceOf[TemporalField])
    }
  }

  test("test_query") {
    assertEquals(TEST_DATE_TIME.query(TemporalQueries.chronology), IsoChronology.INSTANCE)
    assertEquals(TEST_DATE_TIME.query(TemporalQueries.localDate), TEST_DATE_TIME.toLocalDate)
    assertEquals(TEST_DATE_TIME.query(TemporalQueries.localTime), TEST_DATE_TIME.toLocalTime)
    assertEquals(TEST_DATE_TIME.query(TemporalQueries.offset), TEST_DATE_TIME.getOffset)
    assertEquals(TEST_DATE_TIME.query(TemporalQueries.precision), ChronoUnit.NANOS)
    assertEquals(TEST_DATE_TIME.query(TemporalQueries.zone), TEST_DATE_TIME.getZone)
    assertEquals(TEST_DATE_TIME.query(TemporalQueries.zoneId), TEST_DATE_TIME.getZone)
  }

  test("test_query_null") {
    assertThrows[Platform.NPE] {
      TEST_DATE_TIME.query(null)
    }
  }

  test("test_withEarlierOffsetAtOverlap_notAtOverlap") {
    val base: ZonedDateTime = ZonedDateTime.ofStrict(TEST_LOCAL_2008_06_30_11_30_59_500,
                                                     TestZonedDateTime.OFFSET_0200,
                                                     TestZonedDateTime.ZONE_PARIS
    )
    val test: ZonedDateTime = base.withEarlierOffsetAtOverlap
    assertEquals(test, base)
  }

  test("test_withEarlierOffsetAtOverlap_atOverlap") {
    val base: ZonedDateTime = ZonedDateTime.ofStrict(TEST_PARIS_OVERLAP_2008_10_26_02_30,
                                                     TestZonedDateTime.OFFSET_0100,
                                                     TestZonedDateTime.ZONE_PARIS
    )
    val test: ZonedDateTime = base.withEarlierOffsetAtOverlap
    assertEquals(test.getOffset, TestZonedDateTime.OFFSET_0200)
    assertEquals(test.toLocalDateTime, base.toLocalDateTime)
  }

  test("test_withEarlierOffsetAtOverlap_atOverlap_noChange") {
    val base: ZonedDateTime = ZonedDateTime.ofStrict(TEST_PARIS_OVERLAP_2008_10_26_02_30,
                                                     TestZonedDateTime.OFFSET_0200,
                                                     TestZonedDateTime.ZONE_PARIS
    )
    val test: ZonedDateTime = base.withEarlierOffsetAtOverlap
    assertEquals(test, base)
  }

  test("test_withLaterOffsetAtOverlap_notAtOverlap") {
    val base: ZonedDateTime = ZonedDateTime.ofStrict(TEST_LOCAL_2008_06_30_11_30_59_500,
                                                     TestZonedDateTime.OFFSET_0200,
                                                     TestZonedDateTime.ZONE_PARIS
    )
    val test: ZonedDateTime = base.withLaterOffsetAtOverlap
    assertEquals(test, base)
  }

  test("test_withLaterOffsetAtOverlap_atOverlap") {
    val base: ZonedDateTime = ZonedDateTime.ofStrict(TEST_PARIS_OVERLAP_2008_10_26_02_30,
                                                     TestZonedDateTime.OFFSET_0200,
                                                     TestZonedDateTime.ZONE_PARIS
    )
    val test: ZonedDateTime = base.withLaterOffsetAtOverlap
    assertEquals(test.getOffset, TestZonedDateTime.OFFSET_0100)
    assertEquals(test.toLocalDateTime, base.toLocalDateTime)
  }

  test("test_withLaterOffsetAtOverlap_atOverlap_noChange") {
    val base: ZonedDateTime = ZonedDateTime.ofStrict(TEST_PARIS_OVERLAP_2008_10_26_02_30,
                                                     TestZonedDateTime.OFFSET_0100,
                                                     TestZonedDateTime.ZONE_PARIS
    )
    val test: ZonedDateTime = base.withLaterOffsetAtOverlap
    assertEquals(test, base)
  }

  test("test_withZoneSameLocal") {
    val ldt: LocalDateTime  = LocalDateTime.of(2008, 6, 30, 23, 30, 59, 0)
    val base: ZonedDateTime = ZonedDateTime.of(ldt, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.withZoneSameLocal(TestZonedDateTime.ZONE_0200)
    assertEquals(test.toLocalDateTime, base.toLocalDateTime)
  }

  test("test_withZoneSameLocal_noChange") {
    val ldt: LocalDateTime  = LocalDateTime.of(2008, 6, 30, 23, 30, 59, 0)
    val base: ZonedDateTime = ZonedDateTime.of(ldt, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.withZoneSameLocal(TestZonedDateTime.ZONE_0100)
    assertEquals(test, base)
  }

  test("test_withZoneSameLocal_retainOffset1") {
    val ldt: LocalDateTime  = LocalDateTime.of(2008, 11, 2, 1, 30, 59, 0)
    val base: ZonedDateTime = ZonedDateTime.of(ldt, ZoneId.of("UTC-04:00"))
    val test: ZonedDateTime = base.withZoneSameLocal(ZoneId.of("America/New_York"))
    assertEquals(base.getOffset, ZoneOffset.ofHours(-4))
    assertEquals(test.getOffset, ZoneOffset.ofHours(-4))
  }

  test("test_withZoneSameLocal_retainOffset2") {
    val ldt: LocalDateTime  = LocalDateTime.of(2008, 11, 2, 1, 30, 59, 0)
    val base: ZonedDateTime = ZonedDateTime.of(ldt, ZoneId.of("UTC-05:00"))
    val test: ZonedDateTime = base.withZoneSameLocal(ZoneId.of("America/New_York"))
    assertEquals(base.getOffset, ZoneOffset.ofHours(-5))
    assertEquals(test.getOffset, ZoneOffset.ofHours(-5))
  }

  test("test_withZoneSameLocal_null") {
    assertThrows[NullPointerException] {
      val ldt: LocalDateTime  = LocalDateTime.of(2008, 6, 30, 23, 30, 59, 0)
      val base: ZonedDateTime = ZonedDateTime.of(ldt, TestZonedDateTime.ZONE_0100)
      base.withZoneSameLocal(null)
    }
  }

  test("test_withZoneSameInstant") {
    val base: ZonedDateTime     =
      ZonedDateTime.of(TEST_LOCAL_2008_06_30_11_30_59_500, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime     = base.withZoneSameInstant(TestZonedDateTime.ZONE_0200)
    val expected: ZonedDateTime =
      ZonedDateTime.of(TEST_LOCAL_2008_06_30_11_30_59_500.plusHours(1), TestZonedDateTime.ZONE_0200)
    assertEquals(test, expected)
  }

  test("test_withZoneSameInstant_noChange") {
    val base: ZonedDateTime =
      ZonedDateTime.of(TEST_LOCAL_2008_06_30_11_30_59_500, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.withZoneSameInstant(TestZonedDateTime.ZONE_0100)
    assertEquals(test, base)
  }

  test("test_withZoneSameInstant_null") {
    assertThrows[NullPointerException] {
      val base: ZonedDateTime =
        ZonedDateTime.of(TEST_LOCAL_2008_06_30_11_30_59_500, TestZonedDateTime.ZONE_0100)
      base.withZoneSameInstant(null)
    }
  }

  test("test_withZoneLocked") {
    val base: ZonedDateTime     =
      ZonedDateTime.of(TEST_LOCAL_2008_06_30_11_30_59_500, TestZonedDateTime.ZONE_PARIS)
    val test: ZonedDateTime     = base.withFixedOffsetZone
    val expected: ZonedDateTime =
      ZonedDateTime.of(TEST_LOCAL_2008_06_30_11_30_59_500, TestZonedDateTime.ZONE_0200)
    assertEquals(test, expected)
  }

  test("test_with_WithAdjuster_LocalDateTime_sameOffset") {
    val base: ZonedDateTime =
      ZonedDateTime.of(TEST_LOCAL_2008_06_30_11_30_59_500, TestZonedDateTime.ZONE_PARIS)
    val test: ZonedDateTime = base.`with`(LocalDateTime.of(2012, 7, 15, 14, 30))
    check(test,
          2012,
          7,
          15,
          14,
          30,
          0,
          0,
          TestZonedDateTime.OFFSET_0200,
          TestZonedDateTime.ZONE_PARIS
    )
  }

  test("test_with_WithAdjuster_LocalDateTime_adjustedOffset") {
    val base: ZonedDateTime =
      ZonedDateTime.of(TEST_LOCAL_2008_06_30_11_30_59_500, TestZonedDateTime.ZONE_PARIS)
    val test: ZonedDateTime = base.`with`(LocalDateTime.of(2012, 1, 15, 14, 30))
    check(test,
          2012,
          1,
          15,
          14,
          30,
          0,
          0,
          TestZonedDateTime.OFFSET_0100,
          TestZonedDateTime.ZONE_PARIS
    )
  }

  test("test_with_WithAdjuster_LocalDate") {
    val base: ZonedDateTime =
      ZonedDateTime.of(TEST_LOCAL_2008_06_30_11_30_59_500, TestZonedDateTime.ZONE_PARIS)
    val test: ZonedDateTime = base.`with`(LocalDate.of(2012, 7, 28))
    check(test,
          2012,
          7,
          28,
          11,
          30,
          59,
          500,
          TestZonedDateTime.OFFSET_0200,
          TestZonedDateTime.ZONE_PARIS
    )
  }

  test("test_with_WithAdjuster_LocalTime") {
    val base: ZonedDateTime =
      ZonedDateTime.of(TEST_PARIS_OVERLAP_2008_10_26_02_30, TestZonedDateTime.ZONE_PARIS)
    val test: ZonedDateTime = base.`with`(LocalTime.of(2, 29))
    check(test,
          2008,
          10,
          26,
          2,
          29,
          0,
          0,
          TestZonedDateTime.OFFSET_0200,
          TestZonedDateTime.ZONE_PARIS
    )
  }

  test("test_with_WithAdjuster_Year") {
    val ldt: LocalDateTime  = LocalDateTime.of(2008, 6, 30, 23, 30, 59, 0)
    val base: ZonedDateTime = ZonedDateTime.of(ldt, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.`with`(Year.of(2007))
    assertEquals(test, ZonedDateTime.of(ldt.withYear(2007), TestZonedDateTime.ZONE_0100))
  }

  test("test_with_WithAdjuster_Month_adjustedDayOfMonth") {
    val base: ZonedDateTime =
      ZonedDateTime.of(LocalDateTime.of(2012, 7, 31, 0, 0), TestZonedDateTime.ZONE_PARIS)
    val test: ZonedDateTime = base.`with`(Month.JUNE)
    check(test,
          2012,
          6,
          30,
          0,
          0,
          0,
          0,
          TestZonedDateTime.OFFSET_0200,
          TestZonedDateTime.ZONE_PARIS
    )
  }

  test("test_with_WithAdjuster_Offset_same") {
    val base: ZonedDateTime =
      ZonedDateTime.of(LocalDateTime.of(2012, 7, 31, 0, 0), TestZonedDateTime.ZONE_PARIS)
    val test: ZonedDateTime = base.`with`(ZoneOffset.ofHours(2))
    check(test,
          2012,
          7,
          31,
          0,
          0,
          0,
          0,
          TestZonedDateTime.OFFSET_0200,
          TestZonedDateTime.ZONE_PARIS
    )
  }

  test("test_with_WithAdjuster_Offset_ignored") {
    val base: ZonedDateTime =
      ZonedDateTime.of(LocalDateTime.of(2012, 7, 31, 0, 0), TestZonedDateTime.ZONE_PARIS)
    val test: ZonedDateTime = base.`with`(ZoneOffset.ofHours(1))
    check(test,
          2012,
          7,
          31,
          0,
          0,
          0,
          0,
          TestZonedDateTime.OFFSET_0200,
          TestZonedDateTime.ZONE_PARIS
    )
  }

  test("test_with_WithAdjuster_LocalDate_retainOffset1") {
    val newYork: ZoneId     = ZoneId.of("America/New_York")
    val ldt: LocalDateTime  = LocalDateTime.of(2008, 11, 1, 1, 30)
    val base: ZonedDateTime = ZonedDateTime.of(ldt, newYork)
    assertEquals(base.getOffset, ZoneOffset.ofHours(-4))
    val test: ZonedDateTime = base.`with`(LocalDate.of(2008, 11, 2))
    assertEquals(test.getOffset, ZoneOffset.ofHours(-4))
  }

  test("test_with_WithAdjuster_LocalDate_retainOffset2") {
    val newYork: ZoneId     = ZoneId.of("America/New_York")
    val ldt: LocalDateTime  = LocalDateTime.of(2008, 11, 3, 1, 30)
    val base: ZonedDateTime = ZonedDateTime.of(ldt, newYork)
    assertEquals(base.getOffset, ZoneOffset.ofHours(-5))
    val test: ZonedDateTime = base.`with`(LocalDate.of(2008, 11, 2))
    assertEquals(test.getOffset, ZoneOffset.ofHours(-5))
  }

  test("test_with_WithAdjuster_null") {
    assertThrows[Platform.NPE] {
      val base: ZonedDateTime =
        ZonedDateTime.of(TEST_LOCAL_2008_06_30_11_30_59_500, TestZonedDateTime.ZONE_0100)
      base.`with`(null.asInstanceOf[TemporalAdjuster])
    }
  }

  test("test_withYear_normal") {
    val base: ZonedDateTime =
      ZonedDateTime.of(TEST_LOCAL_2008_06_30_11_30_59_500, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.withYear(2007)
    assertEquals(test,
                 ZonedDateTime.of(TEST_LOCAL_2008_06_30_11_30_59_500.withYear(2007),
                                  TestZonedDateTime.ZONE_0100
                 )
    )
  }

  test("test_withYear_noChange") {
    val base: ZonedDateTime =
      ZonedDateTime.of(TEST_LOCAL_2008_06_30_11_30_59_500, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.withYear(2008)
    assertEquals(test, base)
  }

  test("test_withMonth_Month_normal") {
    val base: ZonedDateTime =
      ZonedDateTime.of(TEST_LOCAL_2008_06_30_11_30_59_500, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.`with`(JANUARY)
    assertEquals(
      test,
      ZonedDateTime.of(TEST_LOCAL_2008_06_30_11_30_59_500.withMonth(1), TestZonedDateTime.ZONE_0100)
    )
  }

  test("test_withMonth_Month_null") {
    assertThrows[Platform.NPE] {
      val base: ZonedDateTime =
        ZonedDateTime.of(TEST_LOCAL_2008_06_30_11_30_59_500, TestZonedDateTime.ZONE_0100)
      base.`with`(null.asInstanceOf[Month])
    }
  }

  test("test_withMonth_normal") {
    val base: ZonedDateTime =
      ZonedDateTime.of(TEST_LOCAL_2008_06_30_11_30_59_500, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.withMonth(1)
    assertEquals(
      test,
      ZonedDateTime.of(TEST_LOCAL_2008_06_30_11_30_59_500.withMonth(1), TestZonedDateTime.ZONE_0100)
    )
  }

  test("test_withMonth_noChange") {
    val base: ZonedDateTime =
      ZonedDateTime.of(TEST_LOCAL_2008_06_30_11_30_59_500, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.withMonth(6)
    assertEquals(test, base)
  }

  test("test_withMonth_tooBig") {
    assertThrows[DateTimeException] {
      TEST_DATE_TIME.withMonth(13)
    }
  }

  test("test_withMonth_tooSmall") {
    assertThrows[DateTimeException] {
      TEST_DATE_TIME.withMonth(0)
    }
  }

  test("test_withDayOfMonth_normal") {
    val base: ZonedDateTime =
      ZonedDateTime.of(TEST_LOCAL_2008_06_30_11_30_59_500, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.withDayOfMonth(15)
    assertEquals(test,
                 ZonedDateTime.of(TEST_LOCAL_2008_06_30_11_30_59_500.withDayOfMonth(15),
                                  TestZonedDateTime.ZONE_0100
                 )
    )
  }

  test("test_withDayOfMonth_noChange") {
    val base: ZonedDateTime =
      ZonedDateTime.of(TEST_LOCAL_2008_06_30_11_30_59_500, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.withDayOfMonth(30)
    assertEquals(test, base)
  }

  test("test_withDayOfMonth_tooBig") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, 7, 2, 11, 30).atZone(TestZonedDateTime.ZONE_PARIS).withDayOfMonth(32)
    }
  }

  test("test_withDayOfMonth_tooSmall") {
    assertThrows[DateTimeException] {
      TEST_DATE_TIME.withDayOfMonth(0)
    }
  }

  test("test_withDayOfMonth_invalid31") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, 6, 2, 11, 30).atZone(TestZonedDateTime.ZONE_PARIS).withDayOfMonth(31)
    }
  }

  test("test_withDayOfYear_normal") {
    val base: ZonedDateTime =
      ZonedDateTime.of(TEST_LOCAL_2008_06_30_11_30_59_500, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.withDayOfYear(33)
    assertEquals(test,
                 ZonedDateTime.of(TEST_LOCAL_2008_06_30_11_30_59_500.withDayOfYear(33),
                                  TestZonedDateTime.ZONE_0100
                 )
    )
  }

  test("test_withDayOfYear_noChange") {
    val ldt: LocalDateTime  = LocalDateTime.of(2008, 2, 5, 23, 30, 59, 0)
    val base: ZonedDateTime = ZonedDateTime.of(ldt, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.withDayOfYear(36)
    assertEquals(test, base)
  }

  test("test_withDayOfYear_tooBig") {
    assertThrows[DateTimeException] {
      TEST_DATE_TIME.withDayOfYear(367)
    }
  }

  test("test_withDayOfYear_tooSmall") {
    assertThrows[DateTimeException] {
      TEST_DATE_TIME.withDayOfYear(0)
    }
  }

  test("test_withDayOfYear_invalid366") {
    assertThrows[DateTimeException] {
      LocalDateTime.of(2007, 2, 2, 11, 30).atZone(TestZonedDateTime.ZONE_PARIS).withDayOfYear(366)
    }
  }

  test("test_withHour_normal") {
    val base: ZonedDateTime =
      ZonedDateTime.of(TEST_LOCAL_2008_06_30_11_30_59_500, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.withHour(15)
    assertEquals(
      test,
      ZonedDateTime.of(TEST_LOCAL_2008_06_30_11_30_59_500.withHour(15), TestZonedDateTime.ZONE_0100)
    )
  }

  test("test_withHour_noChange") {
    val base: ZonedDateTime =
      ZonedDateTime.of(TEST_LOCAL_2008_06_30_11_30_59_500, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.withHour(11)
    assertEquals(test, base)
  }

  test("test_withMinute_normal") {
    val base: ZonedDateTime =
      ZonedDateTime.of(TEST_LOCAL_2008_06_30_11_30_59_500, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.withMinute(15)
    assertEquals(test,
                 ZonedDateTime.of(TEST_LOCAL_2008_06_30_11_30_59_500.withMinute(15),
                                  TestZonedDateTime.ZONE_0100
                 )
    )
  }

  test("test_withMinute_noChange") {
    val base: ZonedDateTime =
      ZonedDateTime.of(TEST_LOCAL_2008_06_30_11_30_59_500, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.withMinute(30)
    assertEquals(test, base)
  }

  test("test_withSecond_normal") {
    val base: ZonedDateTime =
      ZonedDateTime.of(TEST_LOCAL_2008_06_30_11_30_59_500, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.withSecond(12)
    assertEquals(test,
                 ZonedDateTime.of(TEST_LOCAL_2008_06_30_11_30_59_500.withSecond(12),
                                  TestZonedDateTime.ZONE_0100
                 )
    )
  }

  test("test_withSecond_noChange") {
    val base: ZonedDateTime =
      ZonedDateTime.of(TEST_LOCAL_2008_06_30_11_30_59_500, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.withSecond(59)
    assertEquals(test, base)
  }

  test("test_withNanoOfSecond_normal") {
    val base: ZonedDateTime =
      ZonedDateTime.of(TEST_LOCAL_2008_06_30_11_30_59_500, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.withNano(15)
    assertEquals(
      test,
      ZonedDateTime.of(TEST_LOCAL_2008_06_30_11_30_59_500.withNano(15), TestZonedDateTime.ZONE_0100)
    )
  }

  test("test_withNanoOfSecond_noChange") {
    val base: ZonedDateTime =
      ZonedDateTime.of(TEST_LOCAL_2008_06_30_11_30_59_500, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.withNano(500)
    assertEquals(test, base)
  }

  val data_plusDays: List[List[Any]] = {
    List(
      List(
        TestZonedDateTime.dateTime(2008,
                                   6,
                                   30,
                                   23,
                                   30,
                                   59,
                                   0,
                                   TestZonedDateTime.OFFSET_0100,
                                   TestZonedDateTime.ZONE_0100
        ),
        0,
        TestZonedDateTime.dateTime(2008,
                                   6,
                                   30,
                                   23,
                                   30,
                                   59,
                                   0,
                                   TestZonedDateTime.OFFSET_0100,
                                   TestZonedDateTime.ZONE_0100
        )
      ),
      List(
        TestZonedDateTime.dateTime(2008,
                                   6,
                                   30,
                                   23,
                                   30,
                                   59,
                                   0,
                                   TestZonedDateTime.OFFSET_0100,
                                   TestZonedDateTime.ZONE_0100
        ),
        1,
        TestZonedDateTime.dateTime(2008,
                                   7,
                                   1,
                                   23,
                                   30,
                                   59,
                                   0,
                                   TestZonedDateTime.OFFSET_0100,
                                   TestZonedDateTime.ZONE_0100
        )
      ),
      List(
        TestZonedDateTime.dateTime(2008,
                                   6,
                                   30,
                                   23,
                                   30,
                                   59,
                                   0,
                                   TestZonedDateTime.OFFSET_0100,
                                   TestZonedDateTime.ZONE_0100
        ),
        -1,
        TestZonedDateTime.dateTime(2008,
                                   6,
                                   29,
                                   23,
                                   30,
                                   59,
                                   0,
                                   TestZonedDateTime.OFFSET_0100,
                                   TestZonedDateTime.ZONE_0100
        )
      ),
      List(
        TestZonedDateTime.dateTime(2008,
                                   3,
                                   30,
                                   1,
                                   30,
                                   0,
                                   0,
                                   TestZonedDateTime.OFFSET_0100,
                                   TestZonedDateTime.ZONE_PARIS
        ),
        1,
        TestZonedDateTime.dateTime(2008,
                                   3,
                                   31,
                                   1,
                                   30,
                                   0,
                                   0,
                                   TestZonedDateTime.OFFSET_0200,
                                   TestZonedDateTime.ZONE_PARIS
        )
      ),
      List(
        TestZonedDateTime.dateTime(2008,
                                   3,
                                   30,
                                   3,
                                   30,
                                   0,
                                   0,
                                   TestZonedDateTime.OFFSET_0200,
                                   TestZonedDateTime.ZONE_PARIS
        ),
        -1,
        TestZonedDateTime.dateTime(2008,
                                   3,
                                   29,
                                   3,
                                   30,
                                   0,
                                   0,
                                   TestZonedDateTime.OFFSET_0100,
                                   TestZonedDateTime.ZONE_PARIS
        )
      ),
      List(
        TestZonedDateTime.dateTime(2008,
                                   3,
                                   29,
                                   2,
                                   30,
                                   0,
                                   0,
                                   TestZonedDateTime.OFFSET_0100,
                                   TestZonedDateTime.ZONE_PARIS
        ),
        1,
        TestZonedDateTime.dateTime(2008,
                                   3,
                                   30,
                                   3,
                                   30,
                                   0,
                                   0,
                                   TestZonedDateTime.OFFSET_0200,
                                   TestZonedDateTime.ZONE_PARIS
        )
      ),
      List(
        TestZonedDateTime.dateTime(2008,
                                   3,
                                   31,
                                   2,
                                   30,
                                   0,
                                   0,
                                   TestZonedDateTime.OFFSET_0200,
                                   TestZonedDateTime.ZONE_PARIS
        ),
        -1,
        TestZonedDateTime.dateTime(2008,
                                   3,
                                   30,
                                   3,
                                   30,
                                   0,
                                   0,
                                   TestZonedDateTime.OFFSET_0200,
                                   TestZonedDateTime.ZONE_PARIS
        )
      ),
      List(
        TestZonedDateTime.dateTime(2008,
                                   10,
                                   26,
                                   1,
                                   30,
                                   0,
                                   0,
                                   TestZonedDateTime.OFFSET_0200,
                                   TestZonedDateTime.ZONE_PARIS
        ),
        1,
        TestZonedDateTime.dateTime(2008,
                                   10,
                                   27,
                                   1,
                                   30,
                                   0,
                                   0,
                                   TestZonedDateTime.OFFSET_0100,
                                   TestZonedDateTime.ZONE_PARIS
        )
      ),
      List(
        TestZonedDateTime.dateTime(2008,
                                   10,
                                   25,
                                   3,
                                   30,
                                   0,
                                   0,
                                   TestZonedDateTime.OFFSET_0200,
                                   TestZonedDateTime.ZONE_PARIS
        ),
        1,
        TestZonedDateTime.dateTime(2008,
                                   10,
                                   26,
                                   3,
                                   30,
                                   0,
                                   0,
                                   TestZonedDateTime.OFFSET_0100,
                                   TestZonedDateTime.ZONE_PARIS
        )
      ),
      List(
        TestZonedDateTime.dateTime(2008,
                                   10,
                                   25,
                                   2,
                                   30,
                                   0,
                                   0,
                                   TestZonedDateTime.OFFSET_0200,
                                   TestZonedDateTime.ZONE_PARIS
        ),
        1,
        TestZonedDateTime.dateTime(2008,
                                   10,
                                   26,
                                   2,
                                   30,
                                   0,
                                   0,
                                   TestZonedDateTime.OFFSET_0200,
                                   TestZonedDateTime.ZONE_PARIS
        )
      ),
      List(
        TestZonedDateTime.dateTime(2008,
                                   10,
                                   27,
                                   2,
                                   30,
                                   0,
                                   0,
                                   TestZonedDateTime.OFFSET_0100,
                                   TestZonedDateTime.ZONE_PARIS
        ),
        -1,
        TestZonedDateTime.dateTime(2008,
                                   10,
                                   26,
                                   2,
                                   30,
                                   0,
                                   0,
                                   TestZonedDateTime.OFFSET_0100,
                                   TestZonedDateTime.ZONE_PARIS
        )
      )
    )
  }

  val data_plusTime: List[List[Any]] = {
    List(
      List(
        TestZonedDateTime.dateTime(2008,
                                   6,
                                   30,
                                   23,
                                   30,
                                   59,
                                   0,
                                   TestZonedDateTime.OFFSET_0100,
                                   TestZonedDateTime.ZONE_0100
        ),
        0,
        TestZonedDateTime.dateTime(2008,
                                   6,
                                   30,
                                   23,
                                   30,
                                   59,
                                   0,
                                   TestZonedDateTime.OFFSET_0100,
                                   TestZonedDateTime.ZONE_0100
        )
      ),
      List(
        TestZonedDateTime.dateTime(2008,
                                   6,
                                   30,
                                   23,
                                   30,
                                   59,
                                   0,
                                   TestZonedDateTime.OFFSET_0100,
                                   TestZonedDateTime.ZONE_0100
        ),
        1,
        TestZonedDateTime.dateTime(2008,
                                   7,
                                   1,
                                   0,
                                   30,
                                   59,
                                   0,
                                   TestZonedDateTime.OFFSET_0100,
                                   TestZonedDateTime.ZONE_0100
        )
      ),
      List(
        TestZonedDateTime.dateTime(2008,
                                   6,
                                   30,
                                   23,
                                   30,
                                   59,
                                   0,
                                   TestZonedDateTime.OFFSET_0100,
                                   TestZonedDateTime.ZONE_0100
        ),
        -1,
        TestZonedDateTime.dateTime(2008,
                                   6,
                                   30,
                                   22,
                                   30,
                                   59,
                                   0,
                                   TestZonedDateTime.OFFSET_0100,
                                   TestZonedDateTime.ZONE_0100
        )
      ),
      List(
        TestZonedDateTime.dateTime(2008,
                                   3,
                                   30,
                                   1,
                                   30,
                                   0,
                                   0,
                                   TestZonedDateTime.OFFSET_0100,
                                   TestZonedDateTime.ZONE_PARIS
        ),
        1,
        TestZonedDateTime.dateTime(2008,
                                   3,
                                   30,
                                   3,
                                   30,
                                   0,
                                   0,
                                   TestZonedDateTime.OFFSET_0200,
                                   TestZonedDateTime.ZONE_PARIS
        )
      ),
      List(
        TestZonedDateTime.dateTime(2008,
                                   3,
                                   30,
                                   3,
                                   30,
                                   0,
                                   0,
                                   TestZonedDateTime.OFFSET_0200,
                                   TestZonedDateTime.ZONE_PARIS
        ),
        -1,
        TestZonedDateTime.dateTime(2008,
                                   3,
                                   30,
                                   1,
                                   30,
                                   0,
                                   0,
                                   TestZonedDateTime.OFFSET_0100,
                                   TestZonedDateTime.ZONE_PARIS
        )
      ),
      List(
        TestZonedDateTime.dateTime(2008,
                                   10,
                                   26,
                                   1,
                                   30,
                                   0,
                                   0,
                                   TestZonedDateTime.OFFSET_0200,
                                   TestZonedDateTime.ZONE_PARIS
        ),
        1,
        TestZonedDateTime.dateTime(2008,
                                   10,
                                   26,
                                   2,
                                   30,
                                   0,
                                   0,
                                   TestZonedDateTime.OFFSET_0200,
                                   TestZonedDateTime.ZONE_PARIS
        )
      ),
      List(
        TestZonedDateTime.dateTime(2008,
                                   10,
                                   26,
                                   1,
                                   30,
                                   0,
                                   0,
                                   TestZonedDateTime.OFFSET_0200,
                                   TestZonedDateTime.ZONE_PARIS
        ),
        2,
        TestZonedDateTime.dateTime(2008,
                                   10,
                                   26,
                                   2,
                                   30,
                                   0,
                                   0,
                                   TestZonedDateTime.OFFSET_0100,
                                   TestZonedDateTime.ZONE_PARIS
        )
      ),
      List(
        TestZonedDateTime.dateTime(2008,
                                   10,
                                   26,
                                   1,
                                   30,
                                   0,
                                   0,
                                   TestZonedDateTime.OFFSET_0200,
                                   TestZonedDateTime.ZONE_PARIS
        ),
        3,
        TestZonedDateTime.dateTime(2008,
                                   10,
                                   26,
                                   3,
                                   30,
                                   0,
                                   0,
                                   TestZonedDateTime.OFFSET_0100,
                                   TestZonedDateTime.ZONE_PARIS
        )
      ),
      List(
        TestZonedDateTime.dateTime(2008,
                                   10,
                                   26,
                                   2,
                                   30,
                                   0,
                                   0,
                                   TestZonedDateTime.OFFSET_0200,
                                   TestZonedDateTime.ZONE_PARIS
        ),
        1,
        TestZonedDateTime.dateTime(2008,
                                   10,
                                   26,
                                   2,
                                   30,
                                   0,
                                   0,
                                   TestZonedDateTime.OFFSET_0100,
                                   TestZonedDateTime.ZONE_PARIS
        )
      ),
      List(
        TestZonedDateTime.dateTime(2008,
                                   10,
                                   26,
                                   2,
                                   30,
                                   0,
                                   0,
                                   TestZonedDateTime.OFFSET_0200,
                                   TestZonedDateTime.ZONE_PARIS
        ),
        2,
        TestZonedDateTime.dateTime(2008,
                                   10,
                                   26,
                                   3,
                                   30,
                                   0,
                                   0,
                                   TestZonedDateTime.OFFSET_0100,
                                   TestZonedDateTime.ZONE_PARIS
        )
      )
    )
  }

  test("test_plus_adjuster_Period_days") {
    data_plusDays.foreach {
      case (base: ZonedDateTime) :: (amount: Int) :: (expected: ZonedDateTime) :: Nil =>
        assertEquals(base.plus(Period.ofDays(amount.toInt)), expected)
      case _                                                                          =>
        fail()
    }
  }

  test("test_plus_adjuster_Period_hours") {
    data_plusTime.foreach {
      case (base: ZonedDateTime) :: (amount: Int) :: (expected: ZonedDateTime) :: Nil =>
        assertEquals(base.plus(Duration.ofHours(amount)), expected)
      case _                                                                          =>
        fail()
    }
  }

  test("test_plus_adjuster_Duration_hours") {
    data_plusTime.foreach {
      case (base: ZonedDateTime) :: (amount: Int) :: (expected: ZonedDateTime) :: Nil =>
        assertEquals(base.plus(Duration.ofHours(amount)), expected)
      case _                                                                          =>
        fail()
    }
  }

  test("test_plus_adjuster") {
    val period: MockSimplePeriod = MockSimplePeriod.of(7, ChronoUnit.MONTHS)
    val t: ZonedDateTime         =
      ZonedDateTime.of(LocalDateTime.of(2008, 6, 1, 12, 30, 59, 500), TestZonedDateTime.ZONE_0100)
    val expected: ZonedDateTime  =
      ZonedDateTime.of(LocalDateTime.of(2009, 1, 1, 12, 30, 59, 500), TestZonedDateTime.ZONE_0100)
    assertEquals(t.plus(period), expected)
  }

  test("test_plus_adjuster_Duration") {
    val duration: Duration      = Duration.ofSeconds(4L * 60 * 60 + 5L * 60 + 6L)
    val t: ZonedDateTime        =
      ZonedDateTime.of(LocalDateTime.of(2008, 6, 1, 12, 30, 59, 500), TestZonedDateTime.ZONE_0100)
    val expected: ZonedDateTime =
      ZonedDateTime.of(LocalDateTime.of(2008, 6, 1, 16, 36, 5, 500), TestZonedDateTime.ZONE_0100)
    assertEquals(t.plus(duration), expected)
  }

  test("test_plus_adjuster_Period_zero") {
    val t: ZonedDateTime = TEST_DATE_TIME.plus(MockSimplePeriod.ZERO_DAYS)
    assertEquals(t, TEST_DATE_TIME)
  }

  test("test_plus_adjuster_Duration_zero") {
    val t: ZonedDateTime = TEST_DATE_TIME.plus(Duration.ZERO)
    assertEquals(t, TEST_DATE_TIME)
  }

  test("test_plus_adjuster_null") {
    assertThrows[Platform.NPE] {
      TEST_DATE_TIME.plus(null)
    }
  }

  test("test_plus_longUnit_hours") {
    data_plusTime.foreach {
      case (base: ZonedDateTime) :: (amount: Int) :: (expected: ZonedDateTime) :: Nil =>
        assertEquals(base.plus(amount, HOURS), expected)
      case _                                                                          =>
    }
  }

  test("test_plus_longUnit_minutes") {
    data_plusTime.foreach {
      case (base: ZonedDateTime) :: (amount: Int) :: (expected: ZonedDateTime) :: Nil =>
        assertEquals(base.plus(amount * 60, MINUTES), expected)
      case _                                                                          =>
    }
  }

  test("test_plus_longUnit_seconds") {
    data_plusTime.foreach {
      case (base: ZonedDateTime) :: (amount: Int) :: (expected: ZonedDateTime) :: Nil =>
        assertEquals(base.plus(amount * 3600, SECONDS), expected)
      case _                                                                          =>
    }
  }

  test("test_plus_longUnit_nanos") {
    data_plusTime.foreach {
      case (base: ZonedDateTime) :: (amount: Int) :: (expected: ZonedDateTime) :: Nil =>
        assertEquals(base.plus(amount * 3600000000000L, NANOS), expected)
      case _                                                                          =>
    }
  }

  test("test_plus_longUnit_null") {
    assertThrows[Platform.NPE] {
      TEST_DATE_TIME_PARIS.plus(0, null)
    }
  }

  test("test_plusYears") {
    val ldt: LocalDateTime  = LocalDateTime.of(2008, 6, 30, 23, 30, 59, 0)
    val base: ZonedDateTime = ZonedDateTime.of(ldt, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.plusYears(1)
    assertEquals(test, ZonedDateTime.of(ldt.plusYears(1), TestZonedDateTime.ZONE_0100))
  }

  test("test_plusYears_zero") {
    val ldt: LocalDateTime  = LocalDateTime.of(2008, 6, 30, 23, 30, 59, 0)
    val base: ZonedDateTime = ZonedDateTime.of(ldt, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.plusYears(0)
    assertEquals(test, base)
  }

  test("test_plusMonths") {
    val ldt: LocalDateTime  = LocalDateTime.of(2008, 6, 30, 23, 30, 59, 0)
    val base: ZonedDateTime = ZonedDateTime.of(ldt, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.plusMonths(1)
    assertEquals(test, ZonedDateTime.of(ldt.plusMonths(1), TestZonedDateTime.ZONE_0100))
  }

  test("test_plusMonths_zero") {
    val ldt: LocalDateTime  = LocalDateTime.of(2008, 6, 30, 23, 30, 59, 0)
    val base: ZonedDateTime = ZonedDateTime.of(ldt, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.plusMonths(0)
    assertEquals(test, base)
  }

  test("test_plusWeeks") {
    val ldt: LocalDateTime  = LocalDateTime.of(2008, 6, 30, 23, 30, 59, 0)
    val base: ZonedDateTime = ZonedDateTime.of(ldt, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.plusWeeks(1)
    assertEquals(test, ZonedDateTime.of(ldt.plusWeeks(1), TestZonedDateTime.ZONE_0100))
  }

  test("test_plusWeeks_zero") {
    val ldt: LocalDateTime  = LocalDateTime.of(2008, 6, 30, 23, 30, 59, 0)
    val base: ZonedDateTime = ZonedDateTime.of(ldt, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.plusWeeks(0)
    assertEquals(test, base)
  }

  test("test_plusDays") {
    data_plusDays.foreach {
      case (base: ZonedDateTime) :: (amount: Int) :: (expected: ZonedDateTime) :: Nil =>
        assertEquals(base.plusDays(amount), expected)
      case _                                                                          =>
        fail()
    }
  }

  test("test_plusHours") {
    data_plusTime.foreach {
      case (base: ZonedDateTime) :: (amount: Int) :: (expected: ZonedDateTime) :: Nil =>
        assertEquals(base.plusHours(amount), expected)
      case _                                                                          =>
        fail()
    }
  }

  test("test_plusMinutes") {
    data_plusTime.foreach {
      case (base: ZonedDateTime) :: (amount: Int) :: (expected: ZonedDateTime) :: Nil =>
        assertEquals(base.plusMinutes(amount * 60), expected)
      case _                                                                          =>
        fail()
    }
  }

  test("test_plusMinutes_minutes") {
    val ldt: LocalDateTime  = LocalDateTime.of(2008, 6, 30, 23, 30, 59, 0)
    val base: ZonedDateTime = ZonedDateTime.of(ldt, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.plusMinutes(30)
    assertEquals(test, ZonedDateTime.of(ldt.plusMinutes(30), TestZonedDateTime.ZONE_0100))
  }

  test("test_plusSeconds") {
    data_plusTime.foreach {
      case (base: ZonedDateTime) :: (amount: Int) :: (expected: ZonedDateTime) :: Nil =>
        assertEquals(base.plusSeconds(amount * 3600), expected)
      case _                                                                          =>
        fail()
    }
  }

  test("test_plusSeconds_seconds") {
    val ldt: LocalDateTime  = LocalDateTime.of(2008, 6, 30, 23, 30, 59, 0)
    val base: ZonedDateTime = ZonedDateTime.of(ldt, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.plusSeconds(1)
    assertEquals(test, ZonedDateTime.of(ldt.plusSeconds(1), TestZonedDateTime.ZONE_0100))
  }

  test("test_plusNanos") {
    data_plusTime.foreach {
      case (base: ZonedDateTime) :: (amount: Int) :: (expected: ZonedDateTime) :: Nil =>
        assertEquals(base.plusNanos(amount * 3600000000000L), expected)
      case _                                                                          =>
        fail()
    }
  }

  test("test_plusNanos_nanos") {
    val ldt: LocalDateTime  = LocalDateTime.of(2008, 6, 30, 23, 30, 59, 0)
    val base: ZonedDateTime = ZonedDateTime.of(ldt, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.plusNanos(1)
    assertEquals(test, ZonedDateTime.of(ldt.plusNanos(1), TestZonedDateTime.ZONE_0100))
  }

  test("test_minus_adjuster_Period_days") {
    data_plusDays.foreach {
      case (base: ZonedDateTime) :: (amount: Int) :: (expected: ZonedDateTime) :: Nil =>
        assertEquals(base.minus(Period.ofDays(-amount.toInt)), expected)
      case _                                                                          =>
        fail()
    }
  }

  test("test_minus_adjuster_Period_hours") {
    data_plusTime.foreach {
      case (base: ZonedDateTime) :: (amount: Int) :: (expected: ZonedDateTime) :: Nil =>
        assertEquals(base.minus(Duration.ofHours(-amount)), expected)
      case _                                                                          =>
        fail()
    }
  }

  test("test_minus_adjuster_Duration_hours") {
    data_plusTime.foreach {
      case (base: ZonedDateTime) :: (amount: Int) :: (expected: ZonedDateTime) :: Nil =>
        assertEquals(base.minus(Duration.ofHours(-amount)), expected)
      case _                                                                          =>
        fail()
    }
  }

  test("test_minus_adjuster") {
    val period: MockSimplePeriod = MockSimplePeriod.of(7, ChronoUnit.MONTHS)
    val t: ZonedDateTime         =
      ZonedDateTime.of(LocalDateTime.of(2008, 6, 1, 12, 30, 59, 500), TestZonedDateTime.ZONE_0100)
    val expected: ZonedDateTime  =
      ZonedDateTime.of(LocalDateTime.of(2007, 11, 1, 12, 30, 59, 500), TestZonedDateTime.ZONE_0100)
    assertEquals(t.minus(period), expected)
  }

  test("test_minus_adjuster_Duration") {
    val duration: Duration      = Duration.ofSeconds(4L * 60 * 60 + 5L * 60 + 6L)
    val t: ZonedDateTime        =
      ZonedDateTime.of(LocalDateTime.of(2008, 6, 1, 12, 30, 59, 500), TestZonedDateTime.ZONE_0100)
    val expected: ZonedDateTime =
      ZonedDateTime.of(LocalDateTime.of(2008, 6, 1, 8, 25, 53, 500), TestZonedDateTime.ZONE_0100)
    assertEquals(t.minus(duration), expected)
  }

  test("test_minus_adjuster_Period_zero") {
    val t: ZonedDateTime = TEST_DATE_TIME.minus(MockSimplePeriod.ZERO_DAYS)
    assertEquals(t, TEST_DATE_TIME)
  }

  test("test_minus_adjuster_Duration_zero") {
    val t: ZonedDateTime = TEST_DATE_TIME.minus(Duration.ZERO)
    assertEquals(t, TEST_DATE_TIME)
  }

  test("test_minus_adjuster_null") {
    assertThrows[Platform.NPE] {
      TEST_DATE_TIME.minus(null)
    }
  }

  test("test_minusYears") {
    val ldt: LocalDateTime  = LocalDateTime.of(2008, 6, 30, 23, 30, 59, 0)
    val base: ZonedDateTime = ZonedDateTime.of(ldt, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.minusYears(1)
    assertEquals(test, ZonedDateTime.of(ldt.minusYears(1), TestZonedDateTime.ZONE_0100))
  }

  test("test_minusYears_zero") {
    val ldt: LocalDateTime  = LocalDateTime.of(2008, 6, 30, 23, 30, 59, 0)
    val base: ZonedDateTime = ZonedDateTime.of(ldt, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.minusYears(0)
    assertEquals(test, base)
  }

  test("test_minusMonths") {
    val ldt: LocalDateTime  = LocalDateTime.of(2008, 6, 30, 23, 30, 59, 0)
    val base: ZonedDateTime = ZonedDateTime.of(ldt, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.minusMonths(1)
    assertEquals(test, ZonedDateTime.of(ldt.minusMonths(1), TestZonedDateTime.ZONE_0100))
  }

  test("test_minusMonths_zero") {
    val ldt: LocalDateTime  = LocalDateTime.of(2008, 6, 30, 23, 30, 59, 0)
    val base: ZonedDateTime = ZonedDateTime.of(ldt, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.minusMonths(0)
    assertEquals(test, base)
  }

  test("test_minusWeeks") {
    val ldt: LocalDateTime  = LocalDateTime.of(2008, 6, 30, 23, 30, 59, 0)
    val base: ZonedDateTime = ZonedDateTime.of(ldt, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.minusWeeks(1)
    assertEquals(test, ZonedDateTime.of(ldt.minusWeeks(1), TestZonedDateTime.ZONE_0100))
  }

  test("test_minusWeeks_zero") {
    val ldt: LocalDateTime  = LocalDateTime.of(2008, 6, 30, 23, 30, 59, 0)
    val base: ZonedDateTime = ZonedDateTime.of(ldt, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.minusWeeks(0)
    assertEquals(test, base)
  }

  test("test_minusDays") {
    data_plusDays.foreach {
      case (base: ZonedDateTime) :: (amount: Int) :: (expected: ZonedDateTime) :: Nil =>
        assertEquals(base.minusDays(-amount), expected)
      case _                                                                          =>
        fail()
    }
  }

  test("test_minusHours") {
    data_plusTime.foreach {
      case (base: ZonedDateTime) :: (amount: Int) :: (expected: ZonedDateTime) :: Nil =>
        assertEquals(base.minusHours(-amount), expected)
      case _                                                                          =>
        fail()
    }
  }

  test("test_minusMinutes") {
    data_plusTime.foreach {
      case (base: ZonedDateTime) :: (amount: Int) :: (expected: ZonedDateTime) :: Nil =>
        assertEquals(base.minusMinutes(-amount * 60), expected)
      case _                                                                          =>
        fail()
    }
  }

  test("test_minusMinutes_minutes") {
    val ldt: LocalDateTime  = LocalDateTime.of(2008, 6, 30, 23, 30, 59, 0)
    val base: ZonedDateTime = ZonedDateTime.of(ldt, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.minusMinutes(30)
    assertEquals(test, ZonedDateTime.of(ldt.minusMinutes(30), TestZonedDateTime.ZONE_0100))
  }

  test("test_minusSeconds") {
    data_plusTime.foreach {
      case (base: ZonedDateTime) :: (amount: Int) :: (expected: ZonedDateTime) :: Nil =>
        assertEquals(base.minusSeconds(-amount * 3600), expected)
      case _                                                                          =>
        fail()
    }
  }

  test("test_minusSeconds_seconds") {
    val ldt: LocalDateTime  = LocalDateTime.of(2008, 6, 30, 23, 30, 59, 0)
    val base: ZonedDateTime = ZonedDateTime.of(ldt, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.minusSeconds(1)
    assertEquals(test, ZonedDateTime.of(ldt.minusSeconds(1), TestZonedDateTime.ZONE_0100))
  }

  test("test_minusNanos") {
    data_plusTime.foreach {
      case (base: ZonedDateTime) :: (amount: Int) :: (expected: ZonedDateTime) :: Nil =>
        assertEquals(base.minusNanos(-amount * 3600000000000L), expected)
      case _                                                                          =>
        fail()
    }
  }

  test("test_minusNanos_nanos") {
    val ldt: LocalDateTime  = LocalDateTime.of(2008, 6, 30, 23, 30, 59, 0)
    val base: ZonedDateTime = ZonedDateTime.of(ldt, TestZonedDateTime.ZONE_0100)
    val test: ZonedDateTime = base.minusNanos(1)
    assertEquals(test, ZonedDateTime.of(ldt.minusNanos(1), TestZonedDateTime.ZONE_0100))
  }

  val data_toInstant: List[List[Any]] =
    List(
      List(LocalDateTime.of(1970, 1, 1, 0, 0, 0, 0), 0L, 0),
      List(LocalDateTime.of(1970, 1, 1, 0, 0, 0, 1), 0L, 1),
      List(LocalDateTime.of(1970, 1, 1, 0, 0, 0, 999999999), 0L, 999999999),
      List(LocalDateTime.of(1970, 1, 1, 0, 0, 1, 0), 1L, 0),
      List(LocalDateTime.of(1970, 1, 1, 0, 0, 1, 1), 1L, 1),
      List(LocalDateTime.of(1969, 12, 31, 23, 59, 59, 999999999), -1L, 999999999),
      List(LocalDateTime.of(1970, 1, 2, 0, 0), 24L * 60L * 60L, 0),
      List(LocalDateTime.of(1969, 12, 31, 0, 0), -24L * 60L * 60L, 0)
    )

  test("test_toInstant_UTC") {
    data_toInstant.foreach {
      case (ldt: LocalDateTime) :: (expectedEpSec: Long) :: (expectedNos: Int) :: Nil =>
        val dt: ZonedDateTime = ldt.atZone(ZoneOffset.UTC)
        val test: Instant     = dt.toInstant
        assertEquals(test.getEpochSecond, expectedEpSec)
        assertEquals(test.getNano, expectedNos)
      case _                                                                          =>
        fail()
    }
  }

  test("test_toInstant_P0100") {
    data_toInstant.foreach {
      case (ldt: LocalDateTime) :: (expectedEpSec: Long) :: (expectedNos: Int) :: Nil =>
        val dt: ZonedDateTime = ldt.atZone(TestZonedDateTime.ZONE_0100)
        val test: Instant     = dt.toInstant
        assertEquals(test.getEpochSecond, expectedEpSec - 3600)
        assertEquals(test.getNano, expectedNos)
      case _                                                                          =>
        fail()
    }
  }

  test("test_toInstant_M0100") {
    data_toInstant.foreach {
      case (ldt: LocalDateTime) :: (expectedEpSec: Long) :: (expectedNos: Int) :: Nil =>
        val dt: ZonedDateTime = ldt.atZone(TestZonedDateTime.ZONE_M0100)
        val test: Instant     = dt.toInstant
        assertEquals(test.getEpochSecond, expectedEpSec + 3600)
        assertEquals(test.getNano, expectedNos)
      case _                                                                          =>
        fail()
    }
  }

  test("test_toEpochSecond_afterEpoch") {
    var ldt: LocalDateTime = LocalDateTime.of(1970, 1, 1, 0, 0).plusHours(1)
    var i: Int             = 0
    while (i < 100000) {
      val a: ZonedDateTime = ZonedDateTime.of(ldt, TestZonedDateTime.ZONE_PARIS)
      assertEquals(a.toEpochSecond, i)
      ldt = ldt.plusSeconds(1)
      i += 1
    }
  }

  test("test_toEpochSecond_beforeEpoch") {
    var ldt: LocalDateTime = LocalDateTime.of(1970, 1, 1, 0, 0).plusHours(1)
    var i: Int             = 0
    while (i < 100000) {
      val a: ZonedDateTime = ZonedDateTime.of(ldt, TestZonedDateTime.ZONE_PARIS)
      assertEquals(a.toEpochSecond, -i)
      ldt = ldt.minusSeconds(1)
      i += 1
    }
  }

  test("test_toEpochSecond_UTC") {
    data_toInstant.foreach {
      case (ldt: LocalDateTime) :: (expectedEpSec: Long) :: (_: Int) :: Nil =>
        val dt: ZonedDateTime = ldt.atZone(ZoneOffset.UTC)
        assertEquals(dt.toEpochSecond, expectedEpSec)
      case _                                                                =>
        fail()
    }
  }

  test("test_toEpochSecond_P0100") {
    data_toInstant.foreach {
      case (ldt: LocalDateTime) :: (expectedEpSec: Long) :: (_: Int) :: Nil =>
        val dt: ZonedDateTime = ldt.atZone(TestZonedDateTime.ZONE_0100)
        assertEquals(dt.toEpochSecond, expectedEpSec - 3600)
      case _                                                                =>
        fail()
    }
  }

  test("test_toEpochSecond_M0100") {
    data_toInstant.foreach {
      case (ldt: LocalDateTime) :: (expectedEpSec: Long) :: (_: Int) :: Nil =>
        val dt: ZonedDateTime = ldt.atZone(TestZonedDateTime.ZONE_M0100)
        assertEquals(dt.toEpochSecond, expectedEpSec + 3600)
      case _                                                                =>
        fail()
    }
  }

  test("test_compareTo_time1") {
    val a: ZonedDateTime =
      ZonedDateTime.of(LocalDateTime.of(2008, 6, 30, 11, 30, 39), TestZonedDateTime.ZONE_0100)
    val b: ZonedDateTime =
      ZonedDateTime.of(LocalDateTime.of(2008, 6, 30, 11, 30, 41), TestZonedDateTime.ZONE_0100)
    assertEquals(a.compareTo(b) < 0, true)
    assertEquals(b.compareTo(a) > 0, true)
    assertEquals(a.compareTo(a) == 0, true)
    assertEquals(b.compareTo(b) == 0, true)
  }

  test("test_compareTo_time2") {
    val a: ZonedDateTime =
      ZonedDateTime.of(LocalDateTime.of(2008, 6, 30, 11, 30, 40, 4), TestZonedDateTime.ZONE_0100)
    val b: ZonedDateTime =
      ZonedDateTime.of(LocalDateTime.of(2008, 6, 30, 11, 30, 40, 5), TestZonedDateTime.ZONE_0100)
    assertEquals(a.compareTo(b) < 0, true)
    assertEquals(b.compareTo(a) > 0, true)
    assertEquals(a.compareTo(a) == 0, true)
    assertEquals(b.compareTo(b) == 0, true)
  }

  test("test_compareTo_offset1") {
    val a: ZonedDateTime =
      ZonedDateTime.of(LocalDateTime.of(2008, 6, 30, 11, 30, 41), TestZonedDateTime.ZONE_0200)
    val b: ZonedDateTime =
      ZonedDateTime.of(LocalDateTime.of(2008, 6, 30, 11, 30, 39), TestZonedDateTime.ZONE_0100)
    assertEquals(a.compareTo(b) < 0, true)
    assertEquals(b.compareTo(a) > 0, true)
    assertEquals(a.compareTo(a) == 0, true)
    assertEquals(b.compareTo(b) == 0, true)
  }

  test("test_compareTo_offset2") {
    val a: ZonedDateTime =
      ZonedDateTime.of(LocalDateTime.of(2008, 6, 30, 11, 30, 40, 5), ZoneId.of("UTC+01:01"))
    val b: ZonedDateTime =
      ZonedDateTime.of(LocalDateTime.of(2008, 6, 30, 11, 30, 40, 4), TestZonedDateTime.ZONE_0100)
    assertEquals(a.compareTo(b) < 0, true)
    assertEquals(b.compareTo(a) > 0, true)
    assertEquals(a.compareTo(a) == 0, true)
    assertEquals(b.compareTo(b) == 0, true)
  }

  test("test_compareTo_both") {
    val a: ZonedDateTime =
      ZonedDateTime.of(LocalDateTime.of(2008, 6, 30, 11, 50), TestZonedDateTime.ZONE_0200)
    val b: ZonedDateTime =
      ZonedDateTime.of(LocalDateTime.of(2008, 6, 30, 11, 20), TestZonedDateTime.ZONE_0100)
    assertEquals(a.compareTo(b) < 0, true)
    assertEquals(b.compareTo(a) > 0, true)
    assertEquals(a.compareTo(a) == 0, true)
    assertEquals(b.compareTo(b) == 0, true)
  }

  test("test_compareTo_bothNanos") {
    val a: ZonedDateTime =
      ZonedDateTime.of(LocalDateTime.of(2008, 6, 30, 11, 20, 40, 5), TestZonedDateTime.ZONE_0200)
    val b: ZonedDateTime =
      ZonedDateTime.of(LocalDateTime.of(2008, 6, 30, 10, 20, 40, 6), TestZonedDateTime.ZONE_0100)
    assertEquals(a.compareTo(b) < 0, true)
    assertEquals(b.compareTo(a) > 0, true)
    assertEquals(a.compareTo(a) == 0, true)
    assertEquals(b.compareTo(b) == 0, true)
  }

  test("test_compareTo_hourDifference") {
    val a: ZonedDateTime =
      ZonedDateTime.of(LocalDateTime.of(2008, 6, 30, 10, 0), TestZonedDateTime.ZONE_0100)
    val b: ZonedDateTime =
      ZonedDateTime.of(LocalDateTime.of(2008, 6, 30, 11, 0), TestZonedDateTime.ZONE_0200)
    assertEquals(a.compareTo(b) < 0, true)
    assertEquals(b.compareTo(a) > 0, true)
    assertEquals(a.compareTo(a) == 0, true)
    assertEquals(b.compareTo(b) == 0, true)
  }

  test("test_compareTo_null") {
    assertThrows[Platform.NPE] {
      val ldt: LocalDateTime = LocalDateTime.of(2008, 6, 30, 23, 30, 59, 0)
      val a: ZonedDateTime   = ZonedDateTime.of(ldt, TestZonedDateTime.ZONE_0100)
      a.compareTo(null)
    }
  }

  val data_isBefore: List[List[Any]] =
    List(
      List(11, 30, TestZonedDateTime.ZONE_0100, 11, 31, TestZonedDateTime.ZONE_0100, true),
      List(11, 30, TestZonedDateTime.ZONE_0200, 11, 30, TestZonedDateTime.ZONE_0100, true),
      List(11, 30, TestZonedDateTime.ZONE_0200, 10, 30, TestZonedDateTime.ZONE_0100, false)
    )

  test("test_isBefore") {
    data_isBefore.foreach {
      case (hour1: Int) :: (minute1: Int) :: (zone1: ZoneId) :: (hour2: Int) :: (minute2: Int) :: (zone2: ZoneId) :: (expected: Boolean) :: Nil =>
        val a: ZonedDateTime =
          ZonedDateTime.of(LocalDateTime.of(2008, 6, 30, hour1, minute1), zone1)
        val b: ZonedDateTime =
          ZonedDateTime.of(LocalDateTime.of(2008, 6, 30, hour2, minute2), zone2)
        assertEquals(a.isBefore(b), expected)
        assertEquals(b.isBefore(a), false)
        assertEquals(a.isBefore(a), false)
        assertEquals(b.isBefore(b), false)
      case _                                                                                                                                    =>
        fail()
    }
  }

  test("test_isBefore_null") {
    assertThrows[Platform.NPE] {
      val ldt: LocalDateTime = LocalDateTime.of(2008, 6, 30, 23, 30, 59, 0)
      val a: ZonedDateTime   = ZonedDateTime.of(ldt, TestZonedDateTime.ZONE_0100)
      a.isBefore(null)
    }
  }

  val data_isAfter: List[List[Any]] =
    List(
      List(11, 31, TestZonedDateTime.ZONE_0100, 11, 30, TestZonedDateTime.ZONE_0100, true),
      List(11, 30, TestZonedDateTime.ZONE_0100, 11, 30, TestZonedDateTime.ZONE_0200, true),
      List(11, 30, TestZonedDateTime.ZONE_0200, 10, 30, TestZonedDateTime.ZONE_0100, false)
    )

  test("test_isAfter") {
    data_isAfter.foreach {
      case (hour1: Int) :: (minute1: Int) :: (zone1: ZoneId) :: (hour2: Int) :: (minute2: Int) :: (zone2: ZoneId) :: (expected: Boolean) :: Nil =>
        val a: ZonedDateTime =
          ZonedDateTime.of(LocalDateTime.of(2008, 6, 30, hour1, minute1), zone1)
        val b: ZonedDateTime =
          ZonedDateTime.of(LocalDateTime.of(2008, 6, 30, hour2, minute2), zone2)
        assertEquals(a.isAfter(b), expected)
        assertEquals(b.isAfter(a), false)
        assertEquals(a.isAfter(a), false)
        assertEquals(b.isAfter(b), false)
      case _                                                                                                                                    =>
        fail()
    }
  }

  test("test_isAfter_null") {
    assertThrows[Platform.NPE] {
      val ldt: LocalDateTime = LocalDateTime.of(2008, 6, 30, 23, 30, 59, 0)
      val a: ZonedDateTime   = ZonedDateTime.of(ldt, TestZonedDateTime.ZONE_0100)
      a.isAfter(null)
    }
  }

  test("test_equals_true") {
    provider_sampleTimes.foreach {
      case (y: Int) :: (o: Int) :: (d: Int) :: (h: Int) :: (m: Int) :: (s: Int) :: (n: Int) :: (_: ZoneId) :: Nil =>
        val a: ZonedDateTime = ZonedDateTime.of(TestZonedDateTime.dateTime(y, o, d, h, m, s, n),
                                                TestZonedDateTime.ZONE_0100
        )
        val b: ZonedDateTime = ZonedDateTime.of(TestZonedDateTime.dateTime(y, o, d, h, m, s, n),
                                                TestZonedDateTime.ZONE_0100
        )
        assertEquals(a == b, true)
        assertEquals(a.hashCode == b.hashCode, true)
      case _                                                                                                      =>
        fail()
    }
  }

  test("test_equals_false_year_differs") {
    provider_sampleTimes.foreach {
      case (y: Int) :: (o: Int) :: (d: Int) :: (h: Int) :: (m: Int) :: (s: Int) :: (n: Int) :: (_: ZoneId) :: Nil =>
        val a: ZonedDateTime = ZonedDateTime.of(TestZonedDateTime.dateTime(y, o, d, h, m, s, n),
                                                TestZonedDateTime.ZONE_0100
        )
        val b: ZonedDateTime = ZonedDateTime.of(TestZonedDateTime.dateTime(y + 1, o, d, h, m, s, n),
                                                TestZonedDateTime.ZONE_0100
        )
        assertEquals(a == b, false)
      case _                                                                                                      =>
        fail()
    }
  }

  test("test_equals_false_hour_differs") {
    provider_sampleTimes.foreach {
      case (y: Int) :: (o: Int) :: (d: Int) :: (h: Int) :: (m: Int) :: (s: Int) :: (n: Int) :: (_: ZoneId) :: Nil =>
        var _h               = h
        _h = if (_h == 23) 22 else _h
        val a: ZonedDateTime = ZonedDateTime.of(TestZonedDateTime.dateTime(y, o, d, _h, m, s, n),
                                                TestZonedDateTime.ZONE_0100
        )
        val b: ZonedDateTime =
          ZonedDateTime.of(TestZonedDateTime.dateTime(y, o, d, _h + 1, m, s, n),
                           TestZonedDateTime.ZONE_0100
          )
        assertEquals(a == b, false)
      case _                                                                                                      =>
        fail()
    }
  }

  test("test_equals_false_minute_differs") {
    provider_sampleTimes.foreach {
      case (y: Int) :: (o: Int) :: (d: Int) :: (h: Int) :: (m: Int) :: (s: Int) :: (n: Int) :: (_: ZoneId) :: Nil =>
        var _m               = m
        _m = if (_m == 59) 58 else _m
        val a: ZonedDateTime = ZonedDateTime.of(TestZonedDateTime.dateTime(y, o, d, h, _m, s, n),
                                                TestZonedDateTime.ZONE_0100
        )
        val b: ZonedDateTime =
          ZonedDateTime.of(TestZonedDateTime.dateTime(y, o, d, h, _m + 1, s, n),
                           TestZonedDateTime.ZONE_0100
          )
        assertEquals(a == b, false)
      case _                                                                                                      =>
        fail()
    }
  }

  test("test_equals_false_second_differs") {
    provider_sampleTimes.foreach {
      case (y: Int) :: (o: Int) :: (d: Int) :: (h: Int) :: (m: Int) :: (s: Int) :: (n: Int) :: (_: ZoneId) :: Nil =>
        var _s               = s
        _s = if (_s == 59) 58 else _s
        val a: ZonedDateTime = ZonedDateTime.of(TestZonedDateTime.dateTime(y, o, d, h, m, _s, n),
                                                TestZonedDateTime.ZONE_0100
        )
        val b: ZonedDateTime =
          ZonedDateTime.of(TestZonedDateTime.dateTime(y, o, d, h, m, _s + 1, n),
                           TestZonedDateTime.ZONE_0100
          )
        assertEquals(a == b, false)
      case _                                                                                                      =>
        fail()
    }
  }

  test("test_equals_false_nano_differs") {
    provider_sampleTimes.foreach {
      case (y: Int) :: (o: Int) :: (d: Int) :: (h: Int) :: (m: Int) :: (s: Int) :: (n: Int) :: (_: ZoneId) :: Nil =>
        var _n               = n
        _n = if (_n == 999999999) 999999998 else _n
        val a: ZonedDateTime = ZonedDateTime.of(TestZonedDateTime.dateTime(y, o, d, h, m, s, _n),
                                                TestZonedDateTime.ZONE_0100
        )
        val b: ZonedDateTime =
          ZonedDateTime.of(TestZonedDateTime.dateTime(y, o, d, h, m, s, _n + 1),
                           TestZonedDateTime.ZONE_0100
          )
        assertEquals(a == b, false)
      case _                                                                                                      =>
        fail()
    }
  }

  test("test_equals_false_offset_differs") {
    provider_sampleTimes.foreach {
      case (y: Int) :: (o: Int) :: (d: Int) :: (h: Int) :: (m: Int) :: (s: Int) :: (n: Int) :: (_: ZoneId) :: Nil =>
        val a: ZonedDateTime = ZonedDateTime.of(TestZonedDateTime.dateTime(y, o, d, h, m, s, n),
                                                TestZonedDateTime.ZONE_0100
        )
        val b: ZonedDateTime = ZonedDateTime.of(TestZonedDateTime.dateTime(y, o, d, h, m, s, n),
                                                TestZonedDateTime.ZONE_0200
        )
        assertEquals(a == b, false)
      case _                                                                                                      =>
        fail()
    }
  }

  test("test_equals_itself_true") {
    assertEquals(TEST_DATE_TIME == TEST_DATE_TIME, true)
  }

  test("test_equals_string_false") {
    assertNotEquals(TEST_DATE_TIME, "2007-07-15")
  }

  val provider_sampleToString: List[List[Any]] =
    List(
      List(2008, 6, 30, 11, 30, 59, 0, "Z", "2008-06-30T11:30:59Z"),
      List(2008, 6, 30, 11, 30, 59, 0, "+01:00", "2008-06-30T11:30:59+01:00"),
      List(2008, 6, 30, 11, 30, 59, 999000000, "Z", "2008-06-30T11:30:59.999Z"),
      List(2008, 6, 30, 11, 30, 59, 999000000, "+01:00", "2008-06-30T11:30:59.999+01:00"),
      List(2008, 6, 30, 11, 30, 59, 999000, "Z", "2008-06-30T11:30:59.000999Z"),
      List(2008, 6, 30, 11, 30, 59, 999000, "+01:00", "2008-06-30T11:30:59.000999+01:00"),
      List(2008, 6, 30, 11, 30, 59, 999, "Z", "2008-06-30T11:30:59.000000999Z"),
      List(2008, 6, 30, 11, 30, 59, 999, "+01:00", "2008-06-30T11:30:59.000000999+01:00"),
      List(2008,
           6,
           30,
           11,
           30,
           59,
           999,
           "Europe/London",
           "2008-06-30T11:30:59.000000999+01:00[Europe/London]"
      ),
      List(2008,
           6,
           30,
           11,
           30,
           59,
           999,
           "Europe/Paris",
           "2008-06-30T11:30:59.000000999+02:00[Europe/Paris]"
      )
    )

  test("test_toString") {
    provider_sampleToString.foreach {
      case (y: Int) :: (o: Int) :: (d: Int) :: (h: Int) :: (m: Int) :: (s: Int) :: (n: Int) :: (zoneId: String) :: (expected: String) :: Nil =>
        val t: ZonedDateTime =
          ZonedDateTime.of(TestZonedDateTime.dateTime(y, o, d, h, m, s, n), ZoneId.of(zoneId))
        val str: String      = t.toString
        assertEquals(str, expected)
      case _                                                                                                                                 =>
        fail()
    }
  }

  test("test_format_formatter") {
    val f: DateTimeFormatter = DateTimeFormatter.ofPattern("y M d H m s")
    val t: String            = ZonedDateTime
      .of(TestZonedDateTime.dateTime(2010, 12, 3, 11, 30), TestZonedDateTime.ZONE_PARIS)
      .format(f)
    assertEquals(t, "2010 12 3 11 30 0")
  }

  test("test_format_formatter_null") {
    assertThrows[NullPointerException] {
      ZonedDateTime
        .of(TestZonedDateTime.dateTime(2010, 12, 3, 11, 30), TestZonedDateTime.ZONE_PARIS)
        .format(null)
    }
  }
}
