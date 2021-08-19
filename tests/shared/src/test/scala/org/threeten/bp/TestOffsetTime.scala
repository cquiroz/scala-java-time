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

import org.threeten.bp.temporal.ChronoField.AMPM_OF_DAY
import org.threeten.bp.temporal.ChronoField.CLOCK_HOUR_OF_AMPM
import org.threeten.bp.temporal.ChronoField.CLOCK_HOUR_OF_DAY
import org.threeten.bp.temporal.ChronoField.HOUR_OF_AMPM
import org.threeten.bp.temporal.ChronoField.HOUR_OF_DAY
import org.threeten.bp.temporal.ChronoField.MICRO_OF_DAY
import org.threeten.bp.temporal.ChronoField.MICRO_OF_SECOND
import org.threeten.bp.temporal.ChronoField.MILLI_OF_DAY
import org.threeten.bp.temporal.ChronoField.MILLI_OF_SECOND
import org.threeten.bp.temporal.ChronoField.MINUTE_OF_DAY
import org.threeten.bp.temporal.ChronoField.MINUTE_OF_HOUR
import org.threeten.bp.temporal.ChronoField.NANO_OF_DAY
import org.threeten.bp.temporal.ChronoField.NANO_OF_SECOND
import org.threeten.bp.temporal.ChronoField.OFFSET_SECONDS
import org.threeten.bp.temporal.ChronoField.SECOND_OF_DAY
import org.threeten.bp.temporal.ChronoField.SECOND_OF_MINUTE
import org.threeten.bp.temporal.ChronoUnit.DAYS
import org.threeten.bp.temporal.ChronoUnit.NANOS
import org.threeten.bp.temporal.ChronoUnit.SECONDS
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.BeforeAndAfter
import org.threeten.bp.format.DateTimeFormatter
import org.threeten.bp.format.DateTimeParseException
import org.threeten.bp.temporal.ChronoField
import org.threeten.bp.temporal.ChronoUnit
import org.threeten.bp.temporal.Temporal
import org.threeten.bp.temporal.TemporalAccessor
import org.threeten.bp.temporal.TemporalAdjuster
import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.temporal.TemporalQueries

/** Test OffsetTime. */
object TestOffsetTime {
  val OFFSET_PONE: ZoneOffset = ZoneOffset.ofHours(1)
  val OFFSET_PTWO: ZoneOffset = ZoneOffset.ofHours(2)
  val DATE: LocalDate         = LocalDate.of(2008, 12, 3)
}

class TestOffsetTime
    extends AnyFunSuite
    with GenDateTimeTest
    with AssertionsHelper
    with BeforeAndAfter {
  private var TEST_11_30_59_500_PONE: OffsetTime = null

  before {
    TEST_11_30_59_500_PONE =
      OffsetTime.of(LocalTime.of(11, 30, 59, 500), TestOffsetTime.OFFSET_PONE)
  }

  protected def samples: List[TemporalAccessor] =
    List(TEST_11_30_59_500_PONE, OffsetTime.MIN, OffsetTime.MAX)

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
      OFFSET_SECONDS
    )

  protected def invalidFields: List[TemporalField] =
    List(ChronoField.values: _*).filterNot(validFields.contains)

  test("constant_MIN") {
    check(OffsetTime.MIN, 0, 0, 0, 0, ZoneOffset.MAX)
  }

  test("constant_MAX") {
    check(OffsetTime.MAX, 23, 59, 59, 999999999, ZoneOffset.MIN)
  }

  test("now") {
    val nowDT: ZonedDateTime = ZonedDateTime.now
    val expected: OffsetTime = OffsetTime.now(Clock.systemDefaultZone)
    val test: OffsetTime     = OffsetTime.now
    val diff: Long           = Math.abs(test.toLocalTime.toNanoOfDay - expected.toLocalTime.toNanoOfDay)
    assertTrue(diff < 100000000)
    assertEquals(test.getOffset, nowDT.getOffset)
  }

  test("now_Clock_allSecsInDay") {
    var i: Int = 0
    while (i < (2 * 24 * 60 * 60)) {
      {
        val instant: Instant = Instant.ofEpochSecond(i, 8)
        val clock: Clock     = Clock.fixed(instant, ZoneOffset.UTC)
        val test: OffsetTime = OffsetTime.now(clock)
        assertEquals(test.getHour, (i / (60 * 60)) % 24)
        assertEquals(test.getMinute, (i / 60)      % 60)
        assertEquals(test.getSecond, i             % 60)
        assertEquals(test.getNano, 8)
        assertEquals(test.getOffset, ZoneOffset.UTC)
      }
      {
        i += 1
        i - 1
      }
    }
  }

  test("now_Clock_beforeEpoch") {
    var i: Int = -1
    while (i >= -(24 * 60 * 60)) {
      {
        val instant: Instant = Instant.ofEpochSecond(i, 8)
        val clock: Clock     = Clock.fixed(instant, ZoneOffset.UTC)
        val test: OffsetTime = OffsetTime.now(clock)
        assertEquals(test.getHour, ((i + 24 * 60 * 60) / (60 * 60)) % 24)
        assertEquals(test.getMinute, ((i + 24 * 60 * 60) / 60)      % 60)
        assertEquals(test.getSecond, (i + 24 * 60 * 60)             % 60)
        assertEquals(test.getNano, 8)
        assertEquals(test.getOffset, ZoneOffset.UTC)
      }
      {
        i -= 1
        i + 1
      }
    }
  }

  test("now_Clock_offsets") {
    val base: Instant = LocalDateTime.of(1970, 1, 1, 12, 0).toInstant(ZoneOffset.UTC)

    {
      var i: Int = -9
      while (i < 15) {
        {
          val offset: ZoneOffset = ZoneOffset.ofHours(i)
          val clock: Clock       = Clock.fixed(base, offset)
          val test: OffsetTime   = OffsetTime.now(clock)
          assertEquals(test.getHour, (12 + i) % 24)
          assertEquals(test.getMinute, 0)
          assertEquals(test.getSecond, 0)
          assertEquals(test.getNano, 0)
          assertEquals(test.getOffset, offset)
        }
        {
          i += 1
          i - 1
        }
      }
    }
  }

  test("now_Clock_nullZoneId") {
    assertThrows[NullPointerException] {
      OffsetTime.now(null.asInstanceOf[ZoneId])
    }
  }

  test("now_Clock_nullClock") {
    assertThrows[NullPointerException] {
      OffsetTime.now(null.asInstanceOf[Clock])
    }
  }

  private def check(test: OffsetTime, h: Int, m: Int, s: Int, n: Int, offset: ZoneOffset): Unit = {
    assertEquals(test.toLocalTime, LocalTime.of(h, m, s, n))
    assertEquals(test.getOffset, offset)
    assertEquals(test.getHour, h)
    assertEquals(test.getMinute, m)
    assertEquals(test.getSecond, s)
    assertEquals(test.getNano, n)
    assertEquals(test, test)
    assertEquals(test.hashCode, test.hashCode)
    assertEquals(OffsetTime.of(LocalTime.of(h, m, s, n), offset), test)
  }

  test("factory_intsHM") {
    val test: OffsetTime = OffsetTime.of(LocalTime.of(11, 30), TestOffsetTime.OFFSET_PONE)
    check(test, 11, 30, 0, 0, TestOffsetTime.OFFSET_PONE)
  }

  test("factory_intsHMS") {
    val test: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 10), TestOffsetTime.OFFSET_PONE)
    check(test, 11, 30, 10, 0, TestOffsetTime.OFFSET_PONE)
  }

  test("factory_intsHMSN") {
    val test: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 10, 500), TestOffsetTime.OFFSET_PONE)
    check(test, 11, 30, 10, 500, TestOffsetTime.OFFSET_PONE)
  }

  test("factory_LocalTimeZoneOffset") {
    val localTime: LocalTime = LocalTime.of(11, 30, 10, 500)
    val test: OffsetTime     = OffsetTime.of(localTime, TestOffsetTime.OFFSET_PONE)
    check(test, 11, 30, 10, 500, TestOffsetTime.OFFSET_PONE)
  }

  test("factory_LocalTimeZoneOffset_nullTime") {
    assertThrows[NullPointerException] {
      OffsetTime.of(null.asInstanceOf[LocalTime], TestOffsetTime.OFFSET_PONE)
    }
  }

  test("factory_LocalTimeZoneOffset_nullOffset") {
    assertThrows[NullPointerException] {
      val localTime: LocalTime = LocalTime.of(11, 30, 10, 500)
      OffsetTime.of(localTime, null.asInstanceOf[ZoneOffset])
    }
  }

  test("factory_ofInstant_nullInstant") {
    assertThrows[NullPointerException] {
      OffsetTime.ofInstant(null.asInstanceOf[Instant], ZoneOffset.UTC)
    }
  }

  test("factory_ofInstant_nullOffset") {
    assertThrows[NullPointerException] {
      val instant: Instant = Instant.ofEpochSecond(0L)
      OffsetTime.ofInstant(instant, null.asInstanceOf[ZoneOffset])
    }
  }

  test("factory_ofInstant_allSecsInDay") {
    var i: Int = 0
    while (i < (2 * 24 * 60 * 60)) {
      {
        val instant: Instant = Instant.ofEpochSecond(i, 8)
        val test: OffsetTime = OffsetTime.ofInstant(instant, ZoneOffset.UTC)
        assertEquals(test.getHour, (i / (60 * 60)) % 24)
        assertEquals(test.getMinute, (i / 60)      % 60)
        assertEquals(test.getSecond, i             % 60)
        assertEquals(test.getNano, 8)
      }
      {
        i += 1
        i - 1
      }
    }
  }

  test("factory_ofInstant_beforeEpoch") {
    var i: Int = -1
    while (i >= -(24 * 60 * 60)) {
      {
        val instant: Instant = Instant.ofEpochSecond(i, 8)
        val test: OffsetTime = OffsetTime.ofInstant(instant, ZoneOffset.UTC)
        assertEquals(test.getHour, ((i + 24 * 60 * 60) / (60 * 60)) % 24)
        assertEquals(test.getMinute, ((i + 24 * 60 * 60) / 60)      % 60)
        assertEquals(test.getSecond, (i + 24 * 60 * 60)             % 60)
        assertEquals(test.getNano, 8)
      }
      {
        i -= 1
        i + 1
      }
    }
  }

  test("factory_ofInstant_maxYear") {
    val test: OffsetTime = OffsetTime.ofInstant(Instant.MAX, ZoneOffset.UTC)
    assertEquals(test.getHour, 23)
    assertEquals(test.getMinute, 59)
    assertEquals(test.getSecond, 59)
    assertEquals(test.getNano, 999999999)
  }

  test("factory_ofInstant_minYear") {
    val test: OffsetTime = OffsetTime.ofInstant(Instant.MIN, ZoneOffset.UTC)
    assertEquals(test.getHour, 0)
    assertEquals(test.getMinute, 0)
    assertEquals(test.getSecond, 0)
    assertEquals(test.getNano, 0)
  }

  test("factory_from_TemporalAccessor_OT") {
    assertEquals(OffsetTime.from(OffsetTime.of(LocalTime.of(17, 30), TestOffsetTime.OFFSET_PONE)),
                 OffsetTime.of(LocalTime.of(17, 30), TestOffsetTime.OFFSET_PONE)
    )
  }

  test("test_from_TemporalAccessor_ZDT") {
    val base: ZonedDateTime =
      LocalDateTime.of(2007, 7, 15, 11, 30, 59, 500).atZone(TestOffsetTime.OFFSET_PONE)
    assertEquals(OffsetTime.from(base), TEST_11_30_59_500_PONE)
  }

  test("factory_from_TemporalAccessor_invalid_noDerive") {
    assertThrows[DateTimeException] {
      OffsetTime.from(LocalDate.of(2007, 7, 15))
    }
  }

  test("factory_from_TemporalAccessor_null") {
    assertThrows[Platform.NPE] {
      OffsetTime.from(null.asInstanceOf[TemporalAccessor])
    }
  }

  test("factory_parse_validText") {
    provider_sampleToString.foreach {
      case (h: Int) :: (m: Int) :: (s: Int) :: (n: Int) :: (offsetId: String) :: (parsable: String) :: Nil =>
        val t: OffsetTime = OffsetTime.parse(parsable)
        assertNotNull(t, parsable)
        check(t, h, m, s, n, ZoneOffset.of(offsetId))
      case _                                                                                               =>
        fail()
    }
  }

  val provider_sampleBadParse: List[String] =
    List("00;00",
         "12-00",
         "-01:00",
         "00:00:00-09",
         "00:00:00,09",
         "00:00:abs",
         "11",
         "11:30",
         "11:30+01:00[Europe/Paris]"
    )

  test("factory_parse_invalidText") {
    provider_sampleBadParse.foreach { unparsable =>
      assertThrows[DateTimeException] {
        OffsetTime.parse(unparsable)
      }
    }
  }

  test("factory_parse_illegalHour") {
    assertThrows[DateTimeParseException] {
      OffsetTime.parse("25:00+01:00")
    }
  }

  test("factory_parse_illegalMinute") {
    assertThrows[DateTimeParseException] {
      OffsetTime.parse("12:60+01:00")
    }
  }

  test("factory_parse_illegalSecond") {
    assertThrows[DateTimeParseException] {
      OffsetTime.parse("12:12:60+01:00")
    }
  }

  test("factory_parse_formatter") {
    val f: DateTimeFormatter = DateTimeFormatter.ofPattern("H m s XXX")
    val test: OffsetTime     = OffsetTime.parse("11 30 0 +01:00", f)
    assertEquals(test, OffsetTime.of(LocalTime.of(11, 30), ZoneOffset.ofHours(1)))
  }

  test("factory_parse_formatter_nullText") {
    assertThrows[NullPointerException] {
      val f: DateTimeFormatter = DateTimeFormatter.ofPattern("y M d H m s")
      OffsetTime.parse(null.asInstanceOf[String], f)
    }
  }

  test("factory_parse_formatter_nullFormatter") {
    assertThrows[NullPointerException] {
      OffsetTime.parse("ANY", null)
    }
  }

  val provider_sampleTimes: List[List[Any]] =
    List(List(11, 30, 20, 500, TestOffsetTime.OFFSET_PONE),
         List(11, 0, 0, 0, TestOffsetTime.OFFSET_PONE),
         List(23, 59, 59, 999999999, TestOffsetTime.OFFSET_PONE)
    )

  test("test_get") {
    provider_sampleTimes.foreach {
      case (h: Int) :: (m: Int) :: (s: Int) :: (n: Int) :: (offset: ZoneOffset) :: Nil =>
        val localTime: LocalTime = LocalTime.of(h, m, s, n)
        val a: OffsetTime        = OffsetTime.of(localTime, offset)
        assertEquals(a.toLocalTime, localTime)
        assertEquals(a.getOffset, offset)
        assertEquals(a.toString, localTime.toString + offset.toString)
        assertEquals(a.getHour, localTime.getHour)
        assertEquals(a.getMinute, localTime.getMinute)
        assertEquals(a.getSecond, localTime.getSecond)
        assertEquals(a.getNano, localTime.getNano)
      case _                                                                           =>
        fail()
    }
  }

  test("test_get_TemporalField") {
    val test: OffsetTime =
      OffsetTime.of(LocalTime.of(12, 30, 40, 987654321), TestOffsetTime.OFFSET_PONE)
    assertEquals(test.get(ChronoField.HOUR_OF_DAY), 12)
    assertEquals(test.get(ChronoField.MINUTE_OF_HOUR), 30)
    assertEquals(test.get(ChronoField.SECOND_OF_MINUTE), 40)
    assertEquals(test.get(ChronoField.NANO_OF_SECOND), 987654321)
    assertEquals(test.get(ChronoField.HOUR_OF_AMPM), 0)
    assertEquals(test.get(ChronoField.AMPM_OF_DAY), 1)
    assertEquals(test.get(ChronoField.OFFSET_SECONDS), 3600)
  }

  test("test_getLong_TemporalField") {
    val test: OffsetTime =
      OffsetTime.of(LocalTime.of(12, 30, 40, 987654321), TestOffsetTime.OFFSET_PONE)
    assertEquals(test.getLong(ChronoField.HOUR_OF_DAY), 12)
    assertEquals(test.getLong(ChronoField.MINUTE_OF_HOUR), 30)
    assertEquals(test.getLong(ChronoField.SECOND_OF_MINUTE), 40)
    assertEquals(test.getLong(ChronoField.NANO_OF_SECOND), 987654321)
    assertEquals(test.getLong(ChronoField.HOUR_OF_AMPM), 0)
    assertEquals(test.getLong(ChronoField.AMPM_OF_DAY), 1)
    assertEquals(test.getLong(ChronoField.OFFSET_SECONDS), 3600)
  }

  test("test_query") {
    assertEquals(TEST_11_30_59_500_PONE.query(TemporalQueries.chronology), null)
    assertEquals(TEST_11_30_59_500_PONE.query(TemporalQueries.localDate), null)
    assertEquals(TEST_11_30_59_500_PONE.query(TemporalQueries.localTime),
                 TEST_11_30_59_500_PONE.toLocalTime
    )
    assertEquals(TEST_11_30_59_500_PONE.query(TemporalQueries.offset),
                 TEST_11_30_59_500_PONE.getOffset
    )
    assertEquals(TEST_11_30_59_500_PONE.query(TemporalQueries.precision), ChronoUnit.NANOS)
    assertEquals(TEST_11_30_59_500_PONE.query(TemporalQueries.zone),
                 TEST_11_30_59_500_PONE.getOffset
    )
    assertEquals(TEST_11_30_59_500_PONE.query(TemporalQueries.zoneId), null)
  }

  test("test_query_null") {
    assertThrows[Platform.NPE] {
      TEST_11_30_59_500_PONE.query(null)
    }
  }

  test("test_withOffsetSameLocal") {
    val base: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59), TestOffsetTime.OFFSET_PONE)
    val test: OffsetTime = base.withOffsetSameLocal(TestOffsetTime.OFFSET_PTWO)
    assertEquals(test.toLocalTime, base.toLocalTime)
    assertEquals(test.getOffset, TestOffsetTime.OFFSET_PTWO)
  }

  test("test_withOffsetSameLocal_noChange") {
    val base: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59), TestOffsetTime.OFFSET_PONE)
    val test: OffsetTime = base.withOffsetSameLocal(TestOffsetTime.OFFSET_PONE)
    assertEquals(test, base)
  }

  test("test_withOffsetSameLocal_null") {
    assertThrows[NullPointerException] {
      val base: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59), TestOffsetTime.OFFSET_PONE)
      base.withOffsetSameLocal(null)
    }
  }

  test("test_withOffsetSameInstant") {
    val base: OffsetTime     = OffsetTime.of(LocalTime.of(11, 30, 59), TestOffsetTime.OFFSET_PONE)
    val test: OffsetTime     = base.withOffsetSameInstant(TestOffsetTime.OFFSET_PTWO)
    val expected: OffsetTime = OffsetTime.of(LocalTime.of(12, 30, 59), TestOffsetTime.OFFSET_PTWO)
    assertEquals(test, expected)
  }

  test("test_withOffsetSameInstant_noChange") {
    val base: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59), TestOffsetTime.OFFSET_PONE)
    val test: OffsetTime = base.withOffsetSameInstant(TestOffsetTime.OFFSET_PONE)
    assertEquals(test, base)
  }

  test("test_withOffsetSameInstant_null") {
    assertThrows[Platform.NPE] {
      val base: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59), TestOffsetTime.OFFSET_PONE)
      base.withOffsetSameInstant(null)
    }
  }

  test("test_with_adjustment") {
    val sample: OffsetTime         = OffsetTime.of(LocalTime.of(23, 5), TestOffsetTime.OFFSET_PONE)
    val adjuster: TemporalAdjuster = new TemporalAdjuster {
      override def adjustInto(temporal: Temporal): Temporal = sample
    }
    assertEquals(TEST_11_30_59_500_PONE.`with`(adjuster), sample)
  }

  test("test_with_adjustment_LocalTime") {
    val test: OffsetTime = TEST_11_30_59_500_PONE.`with`(LocalTime.of(13, 30))
    assertEquals(test, OffsetTime.of(LocalTime.of(13, 30), TestOffsetTime.OFFSET_PONE))
  }

  test("test_with_adjustment_OffsetTime") {
    val test: OffsetTime =
      TEST_11_30_59_500_PONE.`with`(OffsetTime.of(LocalTime.of(13, 35), TestOffsetTime.OFFSET_PTWO))
    assertEquals(test, OffsetTime.of(LocalTime.of(13, 35), TestOffsetTime.OFFSET_PTWO))
  }

  test("test_with_adjustment_ZoneOffset") {
    val test: OffsetTime = TEST_11_30_59_500_PONE.`with`(TestOffsetTime.OFFSET_PTWO)
    assertEquals(test, OffsetTime.of(LocalTime.of(11, 30, 59, 500), TestOffsetTime.OFFSET_PTWO))
  }

  test("test_with_adjustment_AmPm") {
    val adjuster: TemporalAdjuster = new TemporalAdjuster {
      override def adjustInto(dateTime: Temporal): Temporal = dateTime.`with`(HOUR_OF_DAY, 23)
    }
    val test: OffsetTime           = TEST_11_30_59_500_PONE.`with`(adjuster)
    assertEquals(test, OffsetTime.of(LocalTime.of(23, 30, 59, 500), TestOffsetTime.OFFSET_PONE))
  }

  test("test_with_adjustment_null") {
    assertThrows[Platform.NPE] {
      TEST_11_30_59_500_PONE.`with`(null.asInstanceOf[TemporalAdjuster])
    }
  }

  test("test_with_TemporalField") {
    val test: OffsetTime =
      OffsetTime.of(LocalTime.of(12, 30, 40, 987654321), TestOffsetTime.OFFSET_PONE)
    assertEquals(test.`with`(ChronoField.HOUR_OF_DAY, 15),
                 OffsetTime.of(LocalTime.of(15, 30, 40, 987654321), TestOffsetTime.OFFSET_PONE)
    )
    assertEquals(test.`with`(ChronoField.MINUTE_OF_HOUR, 50),
                 OffsetTime.of(LocalTime.of(12, 50, 40, 987654321), TestOffsetTime.OFFSET_PONE)
    )
    assertEquals(test.`with`(ChronoField.SECOND_OF_MINUTE, 50),
                 OffsetTime.of(LocalTime.of(12, 30, 50, 987654321), TestOffsetTime.OFFSET_PONE)
    )
    assertEquals(test.`with`(ChronoField.NANO_OF_SECOND, 12345),
                 OffsetTime.of(LocalTime.of(12, 30, 40, 12345), TestOffsetTime.OFFSET_PONE)
    )
    assertEquals(test.`with`(ChronoField.HOUR_OF_AMPM, 6),
                 OffsetTime.of(LocalTime.of(18, 30, 40, 987654321), TestOffsetTime.OFFSET_PONE)
    )
    assertEquals(test.`with`(ChronoField.AMPM_OF_DAY, 0),
                 OffsetTime.of(LocalTime.of(0, 30, 40, 987654321), TestOffsetTime.OFFSET_PONE)
    )
    assertEquals(
      test.`with`(ChronoField.OFFSET_SECONDS, 7205),
      OffsetTime.of(LocalTime.of(12, 30, 40, 987654321), ZoneOffset.ofHoursMinutesSeconds(2, 0, 5))
    )
  }

  test("test_with_TemporalField_null") {
    assertThrows[Platform.NPE] {
      TEST_11_30_59_500_PONE.`with`(null.asInstanceOf[TemporalField], 0)
    }
  }

  test("test_with_TemporalField_invalidField") {
    assertThrows[DateTimeException] {
      TEST_11_30_59_500_PONE.`with`(ChronoField.YEAR, 0)
    }
  }

  test("test_withHour_normal") {
    val base: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59), TestOffsetTime.OFFSET_PONE)
    val test: OffsetTime = base.withHour(15)
    assertEquals(test, OffsetTime.of(LocalTime.of(15, 30, 59), TestOffsetTime.OFFSET_PONE))
  }

  test("test_withHour_noChange") {
    val base: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59), TestOffsetTime.OFFSET_PONE)
    val test: OffsetTime = base.withHour(11)
    assertEquals(test, base)
  }

  test("test_withMinute_normal") {
    val base: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59), TestOffsetTime.OFFSET_PONE)
    val test: OffsetTime = base.withMinute(15)
    assertEquals(test, OffsetTime.of(LocalTime.of(11, 15, 59), TestOffsetTime.OFFSET_PONE))
  }

  test("test_withMinute_noChange") {
    val base: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59), TestOffsetTime.OFFSET_PONE)
    val test: OffsetTime = base.withMinute(30)
    assertEquals(test, base)
  }

  test("test_withSecond_normal") {
    val base: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59), TestOffsetTime.OFFSET_PONE)
    val test: OffsetTime = base.withSecond(15)
    assertEquals(test, OffsetTime.of(LocalTime.of(11, 30, 15), TestOffsetTime.OFFSET_PONE))
  }

  test("test_withSecond_noChange") {
    val base: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59), TestOffsetTime.OFFSET_PONE)
    val test: OffsetTime = base.withSecond(59)
    assertEquals(test, base)
  }

  test("test_withNanoOfSecond_normal") {
    val base: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59, 1), TestOffsetTime.OFFSET_PONE)
    val test: OffsetTime = base.withNano(15)
    assertEquals(test, OffsetTime.of(LocalTime.of(11, 30, 59, 15), TestOffsetTime.OFFSET_PONE))
  }

  test("test_withNanoOfSecond_noChange") {
    val base: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59, 1), TestOffsetTime.OFFSET_PONE)
    val test: OffsetTime = base.withNano(1)
    assertEquals(test, base)
  }

  test("test_truncatedTo_normal") {
    assertEquals(TEST_11_30_59_500_PONE.truncatedTo(NANOS), TEST_11_30_59_500_PONE)
    assertEquals(TEST_11_30_59_500_PONE.truncatedTo(SECONDS), TEST_11_30_59_500_PONE.withNano(0))
    assertEquals(TEST_11_30_59_500_PONE.truncatedTo(DAYS),
                 TEST_11_30_59_500_PONE.`with`(LocalTime.MIDNIGHT)
    )
  }

  test("test_truncatedTo_null") {
    assertThrows[Platform.NPE] {
      TEST_11_30_59_500_PONE.truncatedTo(null)
    }
  }

  test("test_plus_PlusAdjuster") {
    val period: MockSimplePeriod = MockSimplePeriod.of(7, ChronoUnit.MINUTES)
    val t: OffsetTime            = TEST_11_30_59_500_PONE.plus(period)
    assertEquals(t, OffsetTime.of(LocalTime.of(11, 37, 59, 500), TestOffsetTime.OFFSET_PONE))
  }

  test("test_plus_PlusAdjuster_noChange") {
    val t: OffsetTime = TEST_11_30_59_500_PONE.plus(MockSimplePeriod.of(0, SECONDS))
    assertEquals(t, TEST_11_30_59_500_PONE)
  }

  test("test_plus_PlusAdjuster_zero") {
    val t: OffsetTime = TEST_11_30_59_500_PONE.plus(Period.ZERO)
    assertEquals(t, TEST_11_30_59_500_PONE)
  }

  test("test_plus_PlusAdjuster_null") {
    assertThrows[Platform.NPE] {
      TEST_11_30_59_500_PONE.plus(null)
    }
  }

  test("test_plusHours") {
    val base: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59), TestOffsetTime.OFFSET_PONE)
    val test: OffsetTime = base.plusHours(13)
    assertEquals(test, OffsetTime.of(LocalTime.of(0, 30, 59), TestOffsetTime.OFFSET_PONE))
  }

  test("test_plusHours_zero") {
    val base: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59), TestOffsetTime.OFFSET_PONE)
    val test: OffsetTime = base.plusHours(0)
    assertEquals(test, base)
  }

  test("test_plusMinutes") {
    val base: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59), TestOffsetTime.OFFSET_PONE)
    val test: OffsetTime = base.plusMinutes(30)
    assertEquals(test, OffsetTime.of(LocalTime.of(12, 0, 59), TestOffsetTime.OFFSET_PONE))
  }

  test("test_plusMinutes_zero") {
    val base: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59), TestOffsetTime.OFFSET_PONE)
    val test: OffsetTime = base.plusMinutes(0)
    assertEquals(test, base)
  }

  test("test_plusSeconds") {
    val base: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59), TestOffsetTime.OFFSET_PONE)
    val test: OffsetTime = base.plusSeconds(1)
    assertEquals(test, OffsetTime.of(LocalTime.of(11, 31, 0), TestOffsetTime.OFFSET_PONE))
  }

  test("test_plusSeconds_zero") {
    val base: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59), TestOffsetTime.OFFSET_PONE)
    val test: OffsetTime = base.plusSeconds(0)
    assertEquals(test, base)
  }

  test("test_plusNanos") {
    val base: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59, 0), TestOffsetTime.OFFSET_PONE)
    val test: OffsetTime = base.plusNanos(1)
    assertEquals(test, OffsetTime.of(LocalTime.of(11, 30, 59, 1), TestOffsetTime.OFFSET_PONE))
  }

  test("test_plusNanos_zero") {
    val base: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59), TestOffsetTime.OFFSET_PONE)
    val test: OffsetTime = base.plusNanos(0)
    assertEquals(test, base)
  }

  test("test_minus_MinusAdjuster") {
    val period: MockSimplePeriod = MockSimplePeriod.of(7, ChronoUnit.MINUTES)
    val t: OffsetTime            = TEST_11_30_59_500_PONE.minus(period)
    assertEquals(t, OffsetTime.of(LocalTime.of(11, 23, 59, 500), TestOffsetTime.OFFSET_PONE))
  }

  test("test_minus_MinusAdjuster_noChange") {
    val t: OffsetTime = TEST_11_30_59_500_PONE.minus(MockSimplePeriod.of(0, SECONDS))
    assertEquals(t, TEST_11_30_59_500_PONE)
  }

  test("test_minus_MinusAdjuster_zero") {
    val t: OffsetTime = TEST_11_30_59_500_PONE.minus(Period.ZERO)
    assertEquals(t, TEST_11_30_59_500_PONE)
  }

  test("test_minus_MinusAdjuster_null") {
    assertThrows[Platform.NPE] {
      TEST_11_30_59_500_PONE.minus(null)
    }
  }

  test("test_minusHours") {
    val base: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59), TestOffsetTime.OFFSET_PONE)
    val test: OffsetTime = base.minusHours(-13)
    assertEquals(test, OffsetTime.of(LocalTime.of(0, 30, 59), TestOffsetTime.OFFSET_PONE))
  }

  test("test_minusHours_zero") {
    val base: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59), TestOffsetTime.OFFSET_PONE)
    val test: OffsetTime = base.minusHours(0)
    assertEquals(test, base)
  }

  test("test_minusMinutes") {
    val base: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59), TestOffsetTime.OFFSET_PONE)
    val test: OffsetTime = base.minusMinutes(50)
    assertEquals(test, OffsetTime.of(LocalTime.of(10, 40, 59), TestOffsetTime.OFFSET_PONE))
  }

  test("test_minusMinutes_zero") {
    val base: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59), TestOffsetTime.OFFSET_PONE)
    val test: OffsetTime = base.minusMinutes(0)
    assertEquals(test, base)
  }

  test("test_minusSeconds") {
    val base: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59), TestOffsetTime.OFFSET_PONE)
    val test: OffsetTime = base.minusSeconds(60)
    assertEquals(test, OffsetTime.of(LocalTime.of(11, 29, 59), TestOffsetTime.OFFSET_PONE))
  }

  test("test_minusSeconds_zero") {
    val base: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59), TestOffsetTime.OFFSET_PONE)
    val test: OffsetTime = base.minusSeconds(0)
    assertEquals(test, base)
  }

  test("test_minusNanos") {
    val base: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59, 0), TestOffsetTime.OFFSET_PONE)
    val test: OffsetTime = base.minusNanos(1)
    assertEquals(test,
                 OffsetTime.of(LocalTime.of(11, 30, 58, 999999999), TestOffsetTime.OFFSET_PONE)
    )
  }

  test("test_minusNanos_zero") {
    val base: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59), TestOffsetTime.OFFSET_PONE)
    val test: OffsetTime = base.minusNanos(0)
    assertEquals(test, base)
  }

  test("test_compareTo_time") {
    val a: OffsetTime = OffsetTime.of(LocalTime.of(11, 29), TestOffsetTime.OFFSET_PONE)
    val b: OffsetTime = OffsetTime.of(LocalTime.of(11, 30), TestOffsetTime.OFFSET_PONE)
    assertEquals(a.compareTo(b) < 0, true)
    assertEquals(b.compareTo(a) > 0, true)
    assertEquals(a.compareTo(a) == 0, true)
    assertEquals(b.compareTo(b) == 0, true)
    assertEquals(convertInstant(a).compareTo(convertInstant(b)) < 0, true)
  }

  test("test_compareTo_offset") {
    val a: OffsetTime = OffsetTime.of(LocalTime.of(11, 30), TestOffsetTime.OFFSET_PTWO)
    val b: OffsetTime = OffsetTime.of(LocalTime.of(11, 30), TestOffsetTime.OFFSET_PONE)
    assertEquals(a.compareTo(b) < 0, true)
    assertEquals(b.compareTo(a) > 0, true)
    assertEquals(a.compareTo(a) == 0, true)
    assertEquals(b.compareTo(b) == 0, true)
    assertEquals(convertInstant(a).compareTo(convertInstant(b)) < 0, true)
  }

  test("test_compareTo_both") {
    val a: OffsetTime = OffsetTime.of(LocalTime.of(11, 50), TestOffsetTime.OFFSET_PTWO)
    val b: OffsetTime = OffsetTime.of(LocalTime.of(11, 20), TestOffsetTime.OFFSET_PONE)
    assertEquals(a.compareTo(b) < 0, true)
    assertEquals(b.compareTo(a) > 0, true)
    assertEquals(a.compareTo(a) == 0, true)
    assertEquals(b.compareTo(b) == 0, true)
    assertEquals(convertInstant(a).compareTo(convertInstant(b)) < 0, true)
  }

  test("test_compareTo_bothNearStartOfDay") {
    val a: OffsetTime = OffsetTime.of(LocalTime.of(0, 10), TestOffsetTime.OFFSET_PONE)
    val b: OffsetTime = OffsetTime.of(LocalTime.of(2, 30), TestOffsetTime.OFFSET_PTWO)
    assertEquals(a.compareTo(b) < 0, true)
    assertEquals(b.compareTo(a) > 0, true)
    assertEquals(a.compareTo(a) == 0, true)
    assertEquals(b.compareTo(b) == 0, true)
    assertEquals(convertInstant(a).compareTo(convertInstant(b)) < 0, true)
  }

  test("test_compareTo_hourDifference") {
    val a: OffsetTime = OffsetTime.of(LocalTime.of(10, 0), TestOffsetTime.OFFSET_PONE)
    val b: OffsetTime = OffsetTime.of(LocalTime.of(11, 0), TestOffsetTime.OFFSET_PTWO)
    assertEquals(a.compareTo(b) < 0, true)
    assertEquals(b.compareTo(a) > 0, true)
    assertEquals(a.compareTo(a) == 0, true)
    assertEquals(b.compareTo(b) == 0, true)
    assertEquals(convertInstant(a).compareTo(convertInstant(b)) == 0, true)
  }

  test("test_compareTo_null") {
    assertThrows[Platform.NPE] {
      val a: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59), TestOffsetTime.OFFSET_PONE)
      a.compareTo(null)
    }
  }

  private def convertInstant(ot: OffsetTime): Instant =
    TestOffsetTime.DATE.atTime(ot.toLocalTime).toInstant(ot.getOffset)

  test("test_isBeforeIsAfterIsEqual1") {
    val a: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 58), TestOffsetTime.OFFSET_PONE)
    val b: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59), TestOffsetTime.OFFSET_PONE)
    assertEquals(a.isBefore(b), true)
    assertEquals(a.isEqual(b), false)
    assertEquals(a.isAfter(b), false)
    assertEquals(b.isBefore(a), false)
    assertEquals(b.isEqual(a), false)
    assertEquals(b.isAfter(a), true)
    assertEquals(a.isBefore(a), false)
    assertEquals(b.isBefore(b), false)
    assertEquals(a.isEqual(a), true)
    assertEquals(b.isEqual(b), true)
    assertEquals(a.isAfter(a), false)
    assertEquals(b.isAfter(b), false)
  }

  test("test_isBeforeIsAfterIsEqual1nanos") {
    val a: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59, 3), TestOffsetTime.OFFSET_PONE)
    val b: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59, 4), TestOffsetTime.OFFSET_PONE)
    assertEquals(a.isBefore(b), true)
    assertEquals(a.isEqual(b), false)
    assertEquals(a.isAfter(b), false)
    assertEquals(b.isBefore(a), false)
    assertEquals(b.isEqual(a), false)
    assertEquals(b.isAfter(a), true)
    assertEquals(a.isBefore(a), false)
    assertEquals(b.isBefore(b), false)
    assertEquals(a.isEqual(a), true)
    assertEquals(b.isEqual(b), true)
    assertEquals(a.isAfter(a), false)
    assertEquals(b.isAfter(b), false)
  }

  test("test_isBeforeIsAfterIsEqual2") {
    val a: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59), TestOffsetTime.OFFSET_PTWO)
    val b: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 58), TestOffsetTime.OFFSET_PONE)
    assertEquals(a.isBefore(b), true)
    assertEquals(a.isEqual(b), false)
    assertEquals(a.isAfter(b), false)
    assertEquals(b.isBefore(a), false)
    assertEquals(b.isEqual(a), false)
    assertEquals(b.isAfter(a), true)
    assertEquals(a.isBefore(a), false)
    assertEquals(b.isBefore(b), false)
    assertEquals(a.isEqual(a), true)
    assertEquals(b.isEqual(b), true)
    assertEquals(a.isAfter(a), false)
    assertEquals(b.isAfter(b), false)
  }

  test("test_isBeforeIsAfterIsEqual2nanos") {
    val a: OffsetTime =
      OffsetTime.of(LocalTime.of(11, 30, 59, 4),
                    ZoneOffset.ofTotalSeconds(TestOffsetTime.OFFSET_PONE.getTotalSeconds + 1)
      )
    val b: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59, 3), TestOffsetTime.OFFSET_PONE)
    assertEquals(a.isBefore(b), true)
    assertEquals(a.isEqual(b), false)
    assertEquals(a.isAfter(b), false)
    assertEquals(b.isBefore(a), false)
    assertEquals(b.isEqual(a), false)
    assertEquals(b.isAfter(a), true)
    assertEquals(a.isBefore(a), false)
    assertEquals(b.isBefore(b), false)
    assertEquals(a.isEqual(a), true)
    assertEquals(b.isEqual(b), true)
    assertEquals(a.isAfter(a), false)
    assertEquals(b.isAfter(b), false)
  }

  test("test_isBeforeIsAfterIsEqual_instantComparison") {
    val a: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59), TestOffsetTime.OFFSET_PTWO)
    val b: OffsetTime = OffsetTime.of(LocalTime.of(10, 30, 59), TestOffsetTime.OFFSET_PONE)
    assertEquals(a.isBefore(b), false)
    assertEquals(a.isEqual(b), true)
    assertEquals(a.isAfter(b), false)
    assertEquals(b.isBefore(a), false)
    assertEquals(b.isEqual(a), true)
    assertEquals(b.isAfter(a), false)
    assertEquals(a.isBefore(a), false)
    assertEquals(b.isBefore(b), false)
    assertEquals(a.isEqual(a), true)
    assertEquals(b.isEqual(b), true)
    assertEquals(a.isAfter(a), false)
    assertEquals(b.isAfter(b), false)
  }

  test("test_isBefore_null") {
    assertThrows[Platform.NPE] {
      val a: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59), TestOffsetTime.OFFSET_PONE)
      a.isBefore(null)
    }
  }

  test("test_isAfter_null") {
    assertThrows[Platform.NPE] {
      val a: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59), TestOffsetTime.OFFSET_PONE)
      a.isAfter(null)
    }
  }

  test("test_isEqual_null") {
    assertThrows[Platform.NPE] {
      val a: OffsetTime = OffsetTime.of(LocalTime.of(11, 30, 59), TestOffsetTime.OFFSET_PONE)
      a.isEqual(null)
    }
  }

  test("test_equals_true") {
    provider_sampleTimes.foreach {
      case (h: Int) :: (m: Int) :: (s: Int) :: (n: Int) :: (_: ZoneOffset) :: Nil =>
        val a: OffsetTime = OffsetTime.of(LocalTime.of(h, m, s, n), TestOffsetTime.OFFSET_PONE)
        val b: OffsetTime = OffsetTime.of(LocalTime.of(h, m, s, n), TestOffsetTime.OFFSET_PONE)
        assertEquals(a == b, true)
        assertEquals(a.hashCode == b.hashCode, true)
      case _                                                                      =>
        fail()
    }
  }

  test("test_equals_false_hour_differs") {
    provider_sampleTimes.foreach {
      case (h: Int) :: (m: Int) :: (s: Int) :: (n: Int) :: (_: ZoneOffset) :: Nil =>
        var _h            = h
        _h = if (_h == 23) 22 else _h
        val a: OffsetTime = OffsetTime.of(LocalTime.of(_h, m, s, n), TestOffsetTime.OFFSET_PONE)
        val b: OffsetTime = OffsetTime.of(LocalTime.of(_h + 1, m, s, n), TestOffsetTime.OFFSET_PONE)
        assertEquals(a == b, false)
      case _                                                                      =>
        fail()
    }
  }

  test("test_equals_false_minute_differs") {
    provider_sampleTimes.foreach {
      case (h: Int) :: (m: Int) :: (s: Int) :: (n: Int) :: (_: ZoneOffset) :: Nil =>
        var _m            = m
        _m = if (_m == 59) 58 else _m
        val a: OffsetTime = OffsetTime.of(LocalTime.of(h, _m, s, n), TestOffsetTime.OFFSET_PONE)
        val b: OffsetTime = OffsetTime.of(LocalTime.of(h, _m + 1, s, n), TestOffsetTime.OFFSET_PONE)
        assertEquals(a == b, false)
      case _                                                                      =>
        fail()
    }
  }

  test("test_equals_false_second_differs") {
    provider_sampleTimes.foreach {
      case (h: Int) :: (m: Int) :: (s: Int) :: (n: Int) :: (_: ZoneOffset) :: Nil =>
        var _s            = s
        _s = if (_s == 59) 58 else _s
        val a: OffsetTime = OffsetTime.of(LocalTime.of(h, m, _s, n), TestOffsetTime.OFFSET_PONE)
        val b: OffsetTime = OffsetTime.of(LocalTime.of(h, m, _s + 1, n), TestOffsetTime.OFFSET_PONE)
        assertEquals(a == b, false)
      case _                                                                      =>
        fail()
    }
  }

  test("test_equals_false_nano_differs") {
    provider_sampleTimes.foreach {
      case (h: Int) :: (m: Int) :: (s: Int) :: (n: Int) :: (_: ZoneOffset) :: Nil =>
        var _n            = n
        _n = if (_n == 999999999) 999999998 else _n
        val a: OffsetTime = OffsetTime.of(LocalTime.of(h, m, s, _n), TestOffsetTime.OFFSET_PONE)
        val b: OffsetTime = OffsetTime.of(LocalTime.of(h, m, s, _n + 1), TestOffsetTime.OFFSET_PONE)
        assertEquals(a == b, false)
      case _                                                                      =>
        fail()
    }
  }

  test("test_equals_false_offset_differs") {
    provider_sampleTimes.foreach {
      case (h: Int) :: (m: Int) :: (s: Int) :: (n: Int) :: (_: ZoneOffset) :: Nil =>
        val a: OffsetTime = OffsetTime.of(LocalTime.of(h, m, s, n), TestOffsetTime.OFFSET_PONE)
        val b: OffsetTime = OffsetTime.of(LocalTime.of(h, m, s, n), TestOffsetTime.OFFSET_PTWO)
        assertEquals(a == b, false)
      case _                                                                      =>
        fail()
    }
  }

  test("test_equals_itself_true") {
    assertEquals(TEST_11_30_59_500_PONE == TEST_11_30_59_500_PONE, true)
  }

  test("test_equals_string_false") {
    assertNotEquals(TEST_11_30_59_500_PONE, "2007-07-15")
  }

  test("test_equals_null_false") {
    assertEquals(TEST_11_30_59_500_PONE == null, false)
  }

  val provider_sampleToString: List[List[Any]] =
    List(
      List(11, 30, 59, 0, "Z", "11:30:59Z"),
      List(11, 30, 59, 0, "+01:00", "11:30:59+01:00"),
      List(11, 30, 59, 999000000, "Z", "11:30:59.999Z"),
      List(11, 30, 59, 999000000, "+01:00", "11:30:59.999+01:00"),
      List(11, 30, 59, 999000, "Z", "11:30:59.000999Z"),
      List(11, 30, 59, 999000, "+01:00", "11:30:59.000999+01:00"),
      List(11, 30, 59, 999, "Z", "11:30:59.000000999Z"),
      List(11, 30, 59, 999, "+01:00", "11:30:59.000000999+01:00")
    )

  test("test_toString") {
    provider_sampleToString.foreach {
      case (h: Int) :: (m: Int) :: (s: Int) :: (n: Int) :: (offsetId: String) :: (expected: String) :: Nil =>
        val t: OffsetTime = OffsetTime.of(LocalTime.of(h, m, s, n), ZoneOffset.of(offsetId))
        val str: String   = t.toString
        assertEquals(str, expected)
      case _                                                                                               =>
        fail()
    }
  }

  test("test_format_formatter") {
    val f: DateTimeFormatter = DateTimeFormatter.ofPattern("H m s")
    val t: String            = OffsetTime.of(LocalTime.of(11, 30), TestOffsetTime.OFFSET_PONE).format(f)
    assertEquals(t, "11 30 0")
  }

  test("test_format_formatter_null") {
    assertThrows[NullPointerException] {
      OffsetTime.of(LocalTime.of(11, 30), TestOffsetTime.OFFSET_PONE).format(null)
    }
  }
}
