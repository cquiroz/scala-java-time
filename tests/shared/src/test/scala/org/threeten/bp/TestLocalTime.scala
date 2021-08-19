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
import org.threeten.bp.temporal.ChronoField.SECOND_OF_DAY
import org.threeten.bp.temporal.ChronoField.SECOND_OF_MINUTE
import org.threeten.bp.temporal.ChronoUnit.DAYS
import org.threeten.bp.temporal.ChronoUnit.FOREVER
import org.threeten.bp.temporal.ChronoUnit.HALF_DAYS
import org.threeten.bp.temporal.ChronoUnit.HOURS
import org.threeten.bp.temporal.ChronoUnit.MICROS
import org.threeten.bp.temporal.ChronoUnit.MILLIS
import org.threeten.bp.temporal.ChronoUnit.MINUTES
import org.threeten.bp.temporal.ChronoUnit.MONTHS
import org.threeten.bp.temporal.ChronoUnit.NANOS
import org.threeten.bp.temporal.ChronoUnit.SECONDS
import org.threeten.bp.temporal.ChronoUnit.WEEKS
import org.threeten.bp.temporal.ChronoUnit.YEARS
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.BeforeAndAfter
import org.threeten.bp.format.DateTimeFormatter
import org.threeten.bp.format.DateTimeParseException
import org.threeten.bp.temporal.ChronoField
import org.threeten.bp.temporal.ChronoUnit
import org.threeten.bp.temporal.JulianFields
import org.threeten.bp.temporal.MockFieldNoValue
import org.threeten.bp.temporal.Temporal
import org.threeten.bp.temporal.TemporalAccessor
import org.threeten.bp.temporal.TemporalAdjuster
import org.threeten.bp.temporal.TemporalAmount
import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.temporal.TemporalQueries
import org.threeten.bp.temporal.TemporalUnit
import org.threeten.bp.temporal.UnsupportedTemporalTypeException

import scala.collection.JavaConverters._

/** Test LocalTime. */
object TestLocalTime {
  private val INVALID_UNITS: Array[TemporalUnit] =
    //val set: java.util.EnumSet[ChronoUnit] = EnumSet.range(WEEKS, FOREVER)
    //set.toArray(new Array[TemporalUnit](set.size)).asInstanceOf[Array[TemporalUnit]]
    // We can't use the code above, because ChronoUnit is not an enum (yet), because we can't define enums in Scala (yet).
    ChronoUnit.values
      .filter(unit => unit.ordinal >= WEEKS.ordinal && unit.ordinal <= FOREVER.ordinal)
      .asInstanceOf[Array[TemporalUnit]]
}

class TestLocalTime
    extends AnyFunSuite
    with GenDateTimeTest
    with AssertionsHelper
    with BeforeAndAfter {
  private var TEST_12_30_40_987654321: LocalTime = null

  before {
    TEST_12_30_40_987654321 = LocalTime.of(12, 30, 40, 987654321)
  }

  override protected def samples: List[TemporalAccessor] =
    List(TEST_12_30_40_987654321, LocalTime.MIN, LocalTime.MAX, LocalTime.MIDNIGHT, LocalTime.NOON)

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
      AMPM_OF_DAY
    )

  override protected def invalidFields: List[TemporalField] = {
    val list: List[TemporalField] = List(ChronoField.values: _*)
    (list :+ JulianFields.JULIAN_DAY :+ JulianFields.MODIFIED_JULIAN_DAY :+ JulianFields.RATA_DIE)
      .filterNot(validFields.contains)
  }

  private def check(time: LocalTime, h: Int, m: Int, s: Int, n: Int): Unit = {
    assertEquals(time.getHour, h)
    assertEquals(time.getMinute, m)
    assertEquals(time.getSecond, s)
    assertEquals(time.getNano, n)
  }

  test("constant_MIDNIGHT") {
    check(LocalTime.MIDNIGHT, 0, 0, 0, 0)
  }

  test("constant_MIDNIGHT_equal") {
    assertEquals(LocalTime.MIDNIGHT, LocalTime.MIDNIGHT)
    assertEquals(LocalTime.MIDNIGHT, LocalTime.of(0, 0))
  }

  test("constant_MIDDAY") {
    check(LocalTime.NOON, 12, 0, 0, 0)
  }

  test("constant_MIDDAY_equal") {
    assertEquals(LocalTime.NOON, LocalTime.NOON)
    assertEquals(LocalTime.NOON, LocalTime.of(12, 0))
  }

  test("constant_MIN_TIME") {
    check(LocalTime.MIN, 0, 0, 0, 0)
  }

  test("constant_MIN_TIME_equal") {
    assertEquals(LocalTime.MIN, LocalTime.of(0, 0))
  }

  test("constant_MAX_TIME") {
    check(LocalTime.MAX, 23, 59, 59, 999999999)
  }

  test("constant_MAX_TIME_equal") {
    assertEquals(LocalTime.NOON, LocalTime.NOON)
    assertEquals(LocalTime.NOON, LocalTime.of(12, 0))
  }

  test("now") {
    val expected: LocalTime = LocalTime.now(Clock.systemDefaultZone)
    val test: LocalTime     = LocalTime.now
    val diff: Long          = Math.abs(test.toNanoOfDay - expected.toNanoOfDay)
    assertTrue(diff < 100000000)
  }

  test("now_ZoneId_nullZoneId") {
    assertThrows[NullPointerException] {
      LocalTime.now(null.asInstanceOf[ZoneId])
    }
  }

  test("now_ZoneId") {
    val zone: ZoneId        = ZoneId.of("UTC+01:02:03")
    var expected: LocalTime = LocalTime.now(Clock.system(zone))
    var test: LocalTime     = LocalTime.now(zone)

    {
      var i: Int = 0
      while (i < 100) {
        {
          if (expected == test)
            i = 99
          else {
            expected = LocalTime.now(Clock.system(zone))
            test = LocalTime.now(zone)
          }
        }
        {
          i += 1
          i - 1
        }
      }
    }
    assertEquals(test, expected)
  }

  test("now_Clock_nullClock") {
    assertThrows[NullPointerException] {
      LocalTime.now(null.asInstanceOf[Clock])
    }
  }

  test("now_Clock_allSecsInDay") {
    var i: Int = 0
    while (i < (2 * 24 * 60 * 60)) {
      {
        val instant: Instant = Instant.ofEpochSecond(i, 8)
        val clock: Clock     = Clock.fixed(instant, ZoneOffset.UTC)
        val test: LocalTime  = LocalTime.now(clock)
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

  test("now_Clock_beforeEpoch") {
    var i: Int = -1
    while (i >= -(24 * 60 * 60)) {
      {
        val instant: Instant = Instant.ofEpochSecond(i, 8)
        val clock: Clock     = Clock.fixed(instant, ZoneOffset.UTC)
        val test: LocalTime  = LocalTime.now(clock)
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

  test("now_Clock_max") {
    val clock: Clock    = Clock.fixed(Instant.MAX, ZoneOffset.UTC)
    val test: LocalTime = LocalTime.now(clock)
    assertEquals(test.getHour, 23)
    assertEquals(test.getMinute, 59)
    assertEquals(test.getSecond, 59)
    assertEquals(test.getNano, 999999999)
  }

  test("now_Clock_min") {
    val clock: Clock    = Clock.fixed(Instant.MIN, ZoneOffset.UTC)
    val test: LocalTime = LocalTime.now(clock)
    assertEquals(test.getHour, 0)
    assertEquals(test.getMinute, 0)
    assertEquals(test.getSecond, 0)
    assertEquals(test.getNano, 0)
  }

  test("factory_time_2ints") {
    val test: LocalTime = LocalTime.of(12, 30)
    check(test, 12, 30, 0, 0)
  }

  test("factory_time_2ints_hourTooLow") {
    assertThrows[DateTimeException] {
      LocalTime.of(-1, 0)
    }
  }

  test("factory_time_2ints_hourTooHigh") {
    assertThrows[DateTimeException] {
      LocalTime.of(24, 0)
    }
  }

  test("factory_time_2ints_minuteTooLow") {
    assertThrows[DateTimeException] {
      LocalTime.of(0, -1)
    }
  }

  test("factory_time_2ints_minuteTooHigh") {
    assertThrows[DateTimeException] {
      LocalTime.of(0, 60)
    }
  }

  test("factory_time_3ints") {
    val test: LocalTime = LocalTime.of(12, 30, 40)
    check(test, 12, 30, 40, 0)
  }

  test("factory_time_3ints_hourTooLow") {
    assertThrows[DateTimeException] {
      LocalTime.of(-1, 0, 0)
    }
  }

  test("factory_time_3ints_hourTooHigh") {
    assertThrows[DateTimeException] {
      LocalTime.of(24, 0, 0)
    }
  }

  test("factory_time_3ints_minuteTooLow") {
    assertThrows[DateTimeException] {
      LocalTime.of(0, -1, 0)
    }
  }

  test("factory_time_3ints_minuteTooHigh") {
    assertThrows[DateTimeException] {
      LocalTime.of(0, 60, 0)
    }
  }

  test("factory_time_3ints_secondTooLow") {
    assertThrows[DateTimeException] {
      LocalTime.of(0, 0, -1)
    }
  }

  test("factory_time_3ints_secondTooHigh") {
    assertThrows[DateTimeException] {
      LocalTime.of(0, 0, 60)
    }
  }

  test("factory_time_4ints") {
    var test: LocalTime = LocalTime.of(12, 30, 40, 987654321)
    check(test, 12, 30, 40, 987654321)
    test = LocalTime.of(12, 0, 40, 987654321)
    check(test, 12, 0, 40, 987654321)
  }

  test("factory_time_4ints_hourTooLow") {
    assertThrows[DateTimeException] {
      LocalTime.of(-1, 0, 0, 0)
    }
  }

  test("factory_time_4ints_hourTooHigh") {
    assertThrows[DateTimeException] {
      LocalTime.of(24, 0, 0, 0)
    }
  }

  test("factory_time_4ints_minuteTooLow") {
    assertThrows[DateTimeException] {
      LocalTime.of(0, -1, 0, 0)
    }
  }

  test("factory_time_4ints_minuteTooHigh") {
    assertThrows[DateTimeException] {
      LocalTime.of(0, 60, 0, 0)
    }
  }

  test("factory_time_4ints_secondTooLow") {
    assertThrows[DateTimeException] {
      LocalTime.of(0, 0, -1, 0)
    }
  }

  test("factory_time_4ints_secondTooHigh") {
    assertThrows[DateTimeException] {
      LocalTime.of(0, 0, 60, 0)
    }
  }

  test("factory_time_4ints_nanoTooLow") {
    assertThrows[DateTimeException] {
      LocalTime.of(0, 0, 0, -1)
    }
  }

  test("factory_time_4ints_nanoTooHigh") {
    assertThrows[DateTimeException] {
      LocalTime.of(0, 0, 0, 1000000000)
    }
  }

  test("factory_ofSecondOfDay") {
    val localTime: LocalTime = LocalTime.ofSecondOfDay(2 * 60 * 60 + 17 * 60 + 23)
    check(localTime, 2, 17, 23, 0)
  }

  test("factory_ofSecondOfDay_tooLow") {
    assertThrows[DateTimeException] {
      LocalTime.ofSecondOfDay(-1)
    }
  }

  test("factory_ofSecondOfDay_tooHigh") {
    assertThrows[DateTimeException] {
      LocalTime.ofSecondOfDay(24 * 60 * 60)
    }
  }

  test("factory_ofSecondOfDay_long_int") {
    val localTime: LocalTime = LocalTime.ofSecondOfDay(2 * 60 * 60 + 17 * 60 + 23).withNano(987)
    check(localTime, 2, 17, 23, 987)
  }

  test("factory_ofSecondOfDay_long_int_tooLowSecs") {
    assertThrows[DateTimeException] {
      LocalTime.ofSecondOfDay(-1).withNano(0)
    }
  }

  test("factory_ofSecondOfDay_long_int_tooHighSecs") {
    assertThrows[DateTimeException] {
      LocalTime.ofSecondOfDay(24 * 60 * 60).withNano(0)
    }
  }

  test("factory_ofSecondOfDay_long_int_tooLowNanos") {
    assertThrows[DateTimeException] {
      LocalTime.ofSecondOfDay(0).withNano(-1)
    }
  }

  test("factory_ofSecondOfDay_long_int_tooHighNanos") {
    assertThrows[DateTimeException] {
      LocalTime.ofSecondOfDay(0).withNano(1000000000)
    }
  }

  test("factory_ofNanoOfDay") {
    val localTime: LocalTime = LocalTime.ofNanoOfDay(60 * 60 * 1000000000L + 17)
    check(localTime, 1, 0, 0, 17)
  }

  test("factory_ofNanoOfDay_tooLow") {
    assertThrows[DateTimeException] {
      LocalTime.ofNanoOfDay(-1)
    }
  }

  test("factory_ofNanoOfDay_tooHigh") {
    assertThrows[DateTimeException] {
      LocalTime.ofNanoOfDay(24 * 60 * 60 * 1000000000L)
    }
  }

  test("factory_from_DateTimeAccessor") {
    assertEquals(LocalTime.from(LocalTime.of(17, 30)), LocalTime.of(17, 30))
    assertEquals(LocalTime.from(LocalDateTime.of(2012, 5, 1, 17, 30)), LocalTime.of(17, 30))
  }

  test("factory_from_DateTimeAccessor_invalid_noDerive") {
    assertThrows[DateTimeException] {
      LocalTime.from(LocalDate.of(2007, 7, 15))
    }
  }

  test("factory_from_DateTimeAccessor_null") {
    assertThrows[Platform.NPE] {
      LocalTime.from(null.asInstanceOf[TemporalAccessor])
    }
  }

  test("factory_parse_validText") {
    provider_sampleToString.foreach {
      case (h: Int) :: (m: Int) :: (s: Int) :: (n: Int) :: (parsable: String) :: Nil =>
        val t: LocalTime = LocalTime.parse(parsable)
        assertNotNull(t, parsable)
        assertEquals(t.getHour, h)
        assertEquals(t.getMinute, m)
        assertEquals(t.getSecond, s)
        assertEquals(t.getNano, n)
      case _                                                                         =>
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
         "11:30+01:00",
         "11:30+01:00[Europe/Paris]"
    )

  test("factory_parse_invalidText") {
    provider_sampleBadParse.foreach { unparsable =>
      assertThrows[DateTimeException] {
        LocalTime.parse(unparsable)
      }
    }
  }

  test("factory_parse_illegalHour") {
    assertThrows[DateTimeParseException] {
      LocalTime.parse("25:00")
    }
  }

  test("factory_parse_illegalMinute") {
    assertThrows[DateTimeParseException] {
      LocalTime.parse("12:60")
    }
  }

  test("factory_parse_illegalSecond") {
    assertThrows[DateTimeParseException] {
      LocalTime.parse("12:12:60")
    }
  }

  test("factory_parse_nullTest") {
    assertThrows[NullPointerException] {
      LocalTime.parse(null.asInstanceOf[String])
    }
  }

  test("factory_parse_formatter") {
    val f: DateTimeFormatter = DateTimeFormatter.ofPattern("H m s")
    val test: LocalTime      = LocalTime.parse("14 30 40", f)
    assertEquals(test, LocalTime.of(14, 30, 40))
  }

  test("factory_parse_formatter_nullText") {
    assertThrows[NullPointerException] {
      val f: DateTimeFormatter = DateTimeFormatter.ofPattern("H m s")
      LocalTime.parse(null.asInstanceOf[String], f)
    }
  }

  test("factory_parse_formatter_nullFormatter") {
    assertThrows[NullPointerException] {
      LocalTime.parse("ANY", null)
    }
  }

  test("test_get_TemporalField") {
    val test: LocalTime = TEST_12_30_40_987654321
    assertEquals(test.get(ChronoField.HOUR_OF_DAY), 12)
    assertEquals(test.get(ChronoField.MINUTE_OF_HOUR), 30)
    assertEquals(test.get(ChronoField.SECOND_OF_MINUTE), 40)
    assertEquals(test.get(ChronoField.NANO_OF_SECOND), 987654321)
    assertEquals(test.get(ChronoField.SECOND_OF_DAY), 12 * 3600 + 30 * 60 + 40)
    assertEquals(test.get(ChronoField.MINUTE_OF_DAY), 12 * 60 + 30)
    assertEquals(test.get(ChronoField.HOUR_OF_AMPM), 0)
    assertEquals(test.get(ChronoField.CLOCK_HOUR_OF_AMPM), 12)
    assertEquals(test.get(ChronoField.CLOCK_HOUR_OF_DAY), 12)
    assertEquals(test.get(ChronoField.AMPM_OF_DAY), 1)
  }

  test("test_get_TemporalField_tooBig") {
    assertThrows[DateTimeException] {
      TEST_12_30_40_987654321.get(NANO_OF_DAY)
    }
  }

  test("test_get_TemporalField_null") {
    assertThrows[Platform.NPE] {
      TEST_12_30_40_987654321.get(null.asInstanceOf[TemporalField])
    }
  }

  test("test_get_TemporalField_invalidField") {
    assertThrows[DateTimeException] {
      TEST_12_30_40_987654321.get(MockFieldNoValue.INSTANCE)
    }
  }

  test("test_get_TemporalField_dateField") {
    assertThrows[DateTimeException] {
      TEST_12_30_40_987654321.get(ChronoField.DAY_OF_MONTH)
    }
  }

  test("test_getLong_TemporalField") {
    val test: LocalTime = TEST_12_30_40_987654321
    assertEquals(test.getLong(ChronoField.HOUR_OF_DAY), 12)
    assertEquals(test.getLong(ChronoField.MINUTE_OF_HOUR), 30)
    assertEquals(test.getLong(ChronoField.SECOND_OF_MINUTE), 40)
    assertEquals(test.getLong(ChronoField.NANO_OF_SECOND), 987654321)
    assertEquals(test.getLong(ChronoField.NANO_OF_DAY),
                 ((12 * 3600 + 30 * 60 + 40) * 1000000000L) + 987654321
    )
    assertEquals(test.getLong(ChronoField.SECOND_OF_DAY), 12 * 3600 + 30 * 60 + 40)
    assertEquals(test.getLong(ChronoField.MINUTE_OF_DAY), 12 * 60 + 30)
    assertEquals(test.getLong(ChronoField.HOUR_OF_AMPM), 0)
    assertEquals(test.getLong(ChronoField.CLOCK_HOUR_OF_AMPM), 12)
    assertEquals(test.getLong(ChronoField.CLOCK_HOUR_OF_DAY), 12)
    assertEquals(test.getLong(ChronoField.AMPM_OF_DAY), 1)
  }

  test("test_getLong_TemporalField_null") {
    assertThrows[Platform.NPE] {
      TEST_12_30_40_987654321.getLong(null.asInstanceOf[TemporalField])
    }
  }

  test("test_getLong_TemporalField_invalidField") {
    assertThrows[DateTimeException] {
      TEST_12_30_40_987654321.getLong(MockFieldNoValue.INSTANCE)
    }
  }

  test("test_getLong_TemporalField_dateField") {
    assertThrows[DateTimeException] {
      TEST_12_30_40_987654321.getLong(ChronoField.DAY_OF_MONTH)
    }
  }

  test("test_query") {
    assertEquals(TEST_12_30_40_987654321.query(TemporalQueries.chronology), null)
    assertEquals(TEST_12_30_40_987654321.query(TemporalQueries.localDate), null)
    assertEquals(TEST_12_30_40_987654321.query(TemporalQueries.localTime), TEST_12_30_40_987654321)
    assertEquals(TEST_12_30_40_987654321.query(TemporalQueries.offset), null)
    assertEquals(TEST_12_30_40_987654321.query(TemporalQueries.precision), ChronoUnit.NANOS)
    assertEquals(TEST_12_30_40_987654321.query(TemporalQueries.zone), null)
    assertEquals(TEST_12_30_40_987654321.query(TemporalQueries.zoneId), null)
  }

  test("test_query_null") {
    assertThrows[Platform.NPE] {
      TEST_12_30_40_987654321.query(null)
    }
  }

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

  test("test_get") {
    provider_sampleTimes.foreach {
      case (h: Int) :: (m: Int) :: (s: Int) :: (ns: Int) :: Nil =>
        val a: LocalTime = LocalTime.of(h, m, s, ns)
        assertEquals(a.getHour, h)
        assertEquals(a.getMinute, m)
        assertEquals(a.getSecond, s)
        assertEquals(a.getNano, ns)
      case _                                                    =>
        fail()
    }
  }

  test("test_with_adjustment") {
    val sample: LocalTime          = LocalTime.of(23, 5)
    val adjuster: TemporalAdjuster = new TemporalAdjuster {
      override def adjustInto(temporal: Temporal): Temporal = sample
    }
    assertEquals(TEST_12_30_40_987654321.`with`(adjuster), sample)
  }

  test("test_with_adjustment_null") {
    assertThrows[Platform.NPE] {
      TEST_12_30_40_987654321.`with`(null.asInstanceOf[TemporalAdjuster])
    }
  }

  test("test_withHour_normal") {
    var t: LocalTime = TEST_12_30_40_987654321

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

  test("test_withHour_noChange_equal") {
    val t: LocalTime = TEST_12_30_40_987654321.withHour(12)
    assertEquals(t, TEST_12_30_40_987654321)
  }

  test("test_withHour_toMidnight_equal") {
    val t: LocalTime = LocalTime.of(1, 0).withHour(0)
    assertEquals(t, LocalTime.MIDNIGHT)
  }

  test("test_withHour_toMidday_equal") {
    val t: LocalTime = LocalTime.of(1, 0).withHour(12)
    assertEquals(t, LocalTime.NOON)
  }

  test("test_withHour_hourTooLow") {
    assertThrows[DateTimeException] {
      TEST_12_30_40_987654321.withHour(-1)
    }
  }

  test("test_withHour_hourTooHigh") {
    assertThrows[DateTimeException] {
      TEST_12_30_40_987654321.withHour(24)
    }
  }

  test("test_withMinute_normal") {
    var t: LocalTime = TEST_12_30_40_987654321

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

  test("test_withMinute_noChange_equal") {
    val t: LocalTime = TEST_12_30_40_987654321.withMinute(30)
    assertEquals(t, TEST_12_30_40_987654321)
  }

  test("test_withMinute_toMidnight_equal") {
    val t: LocalTime = LocalTime.of(0, 1).withMinute(0)
    assertEquals(t, LocalTime.MIDNIGHT)
  }

  test("test_withMinute_toMidday_equals") {
    val t: LocalTime = LocalTime.of(12, 1).withMinute(0)
    assertEquals(t, LocalTime.NOON)
  }

  test("test_withMinute_minuteTooLow") {
    assertThrows[DateTimeException] {
      TEST_12_30_40_987654321.withMinute(-1)
    }
  }

  test("test_withMinute_minuteTooHigh") {
    assertThrows[DateTimeException] {
      TEST_12_30_40_987654321.withMinute(60)
    }
  }

  test("test_withSecond_normal") {
    var t: LocalTime = TEST_12_30_40_987654321

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

  test("test_withSecond_noChange_equal") {
    val t: LocalTime = TEST_12_30_40_987654321.withSecond(40)
    assertEquals(t, TEST_12_30_40_987654321)
  }

  test("test_withSecond_toMidnight_equal") {
    val t: LocalTime = LocalTime.of(0, 0, 1).withSecond(0)
    assertEquals(t, LocalTime.MIDNIGHT)
  }

  test("test_withSecond_toMidday_equal") {
    val t: LocalTime = LocalTime.of(12, 0, 1).withSecond(0)
    assertEquals(t, LocalTime.NOON)
  }

  test("test_withSecond_secondTooLow") {
    assertThrows[DateTimeException] {
      TEST_12_30_40_987654321.withSecond(-1)
    }
  }

  test("test_withSecond_secondTooHigh") {
    assertThrows[DateTimeException] {
      TEST_12_30_40_987654321.withSecond(60)
    }
  }

  test("test_withNanoOfSecond_normal") {
    var t: LocalTime = TEST_12_30_40_987654321
    t = t.withNano(1)
    assertEquals(t.getNano, 1)
    t = t.withNano(10)
    assertEquals(t.getNano, 10)
    t = t.withNano(100)
    assertEquals(t.getNano, 100)
    t = t.withNano(999999999)
    assertEquals(t.getNano, 999999999)
  }

  test("test_withNanoOfSecond_noChange_equal") {
    val t: LocalTime = TEST_12_30_40_987654321.withNano(987654321)
    assertEquals(t, TEST_12_30_40_987654321)
  }

  test("test_withNanoOfSecond_toMidnight_equal") {
    val t: LocalTime = LocalTime.of(0, 0, 0, 1).withNano(0)
    assertEquals(t, LocalTime.MIDNIGHT)
  }

  test("test_withNanoOfSecond_toMidday_equal") {
    val t: LocalTime = LocalTime.of(12, 0, 0, 1).withNano(0)
    assertEquals(t, LocalTime.NOON)
  }

  test("test_withNanoOfSecond_nanoTooLow") {
    assertThrows[DateTimeException] {
      TEST_12_30_40_987654321.withNano(-1)
    }
  }

  test("test_withNanoOfSecond_nanoTooHigh") {
    assertThrows[DateTimeException] {
      TEST_12_30_40_987654321.withNano(1000000000)
    }
  }

  private val NINETY_MINS: TemporalUnit      = new TemporalUnit() {
    override def toString: String =
      "NinetyMins"

    def getDuration: Duration =
      Duration.ofMinutes(90)

    def isDurationEstimated: Boolean =
      false

    def isDateBased: Boolean =
      false

    def isTimeBased: Boolean =
      true

    override def isSupportedBy(temporal: Temporal): Boolean =
      false

    def addTo[R <: Temporal](r: R, l: Long): R =
      throw new UnsupportedOperationException

    def between(r: Temporal, r2: Temporal): Long =
      throw new UnsupportedOperationException
  }
  private val NINETY_FIVE_MINS: TemporalUnit = new TemporalUnit() {
    override def toString: String =
      "NinetyFiveMins"

    def getDuration: Duration =
      Duration.ofMinutes(95)

    def isDurationEstimated: Boolean =
      false

    def isDateBased: Boolean =
      false

    def isTimeBased: Boolean =
      true

    override def isSupportedBy(temporal: Temporal): Boolean =
      false

    def addTo[R <: Temporal](r: R, l: Long): R =
      throw new UnsupportedOperationException

    def between(r: Temporal, r2: Temporal): Long =
      throw new UnsupportedOperationException
  }

  val data_truncatedToValid: List[List[AnyRef]] =
    List(
      List(LocalTime.of(1, 2, 3, 123456789), NANOS, LocalTime.of(1, 2, 3, 123456789)),
      List(LocalTime.of(1, 2, 3, 123456789), MICROS, LocalTime.of(1, 2, 3, 123456000)),
      List(LocalTime.of(1, 2, 3, 123456789), MILLIS, LocalTime.of(1, 2, 3, 123000000)),
      List(LocalTime.of(1, 2, 3, 123456789), SECONDS, LocalTime.of(1, 2, 3)),
      List(LocalTime.of(1, 2, 3, 123456789), MINUTES, LocalTime.of(1, 2)),
      List(LocalTime.of(1, 2, 3, 123456789), HOURS, LocalTime.of(1, 0)),
      List(LocalTime.of(1, 2, 3, 123456789), DAYS, LocalTime.MIDNIGHT),
      List(LocalTime.of(1, 1, 1, 123456789), NINETY_MINS, LocalTime.of(0, 0)),
      List(LocalTime.of(2, 1, 1, 123456789), NINETY_MINS, LocalTime.of(1, 30)),
      List(LocalTime.of(3, 1, 1, 123456789), NINETY_MINS, LocalTime.of(3, 0))
    )

  test("test_truncatedTo_valid") {
    data_truncatedToValid.foreach {
      case (input: LocalTime) :: (unit: TemporalUnit) :: (expected: LocalTime) :: Nil =>
        assertEquals(input.truncatedTo(unit), expected)
      case _                                                                          =>
    }
  }

  val data_truncatedToInvalid: List[List[AnyRef]] =
    List(
      List(LocalTime.of(1, 2, 3, 123456789), NINETY_FIVE_MINS),
      List(LocalTime.of(1, 2, 3, 123456789), WEEKS),
      List(LocalTime.of(1, 2, 3, 123456789), MONTHS),
      List(LocalTime.of(1, 2, 3, 123456789), YEARS)
    )

  test("test_truncatedTo_invalid") {
    data_truncatedToInvalid.foreach {
      case (input: LocalTime) :: (unit: TemporalUnit) :: Nil =>
        assertThrows[DateTimeException] {
          input.truncatedTo(unit)
        }
      case _                                                 =>
        fail()
    }
  }

  test("test_truncatedTo_null") {
    assertThrows[Platform.NPE] {
      TEST_12_30_40_987654321.truncatedTo(null)
    }
  }

  test("test_plus_Adjuster_positiveHours") {
    val period: TemporalAmount = MockSimplePeriod.of(7, ChronoUnit.HOURS)
    val t: LocalTime           = TEST_12_30_40_987654321.plus(period)
    assertEquals(t, LocalTime.of(19, 30, 40, 987654321))
  }

  test("test_plus_Adjuster_negativeMinutes") {
    val period: TemporalAmount = MockSimplePeriod.of(-25, ChronoUnit.MINUTES)
    val t: LocalTime           = TEST_12_30_40_987654321.plus(period)
    assertEquals(t, LocalTime.of(12, 5, 40, 987654321))
  }

  test("test_plus_Adjuster_zero") {
    val period: TemporalAmount = Period.ZERO
    val t: LocalTime           = TEST_12_30_40_987654321.plus(period)
    assertEquals(t, TEST_12_30_40_987654321)
  }

  test("test_plus_Adjuster_wrap") {
    val p: TemporalAmount = Duration.ofHours(1)
    val t: LocalTime      = LocalTime.of(23, 30).plus(p)
    assertEquals(t, LocalTime.of(0, 30))
  }

  test("test_plus_Adjuster_dateNotAllowed") {
    assertThrows[UnsupportedTemporalTypeException] {
      val period: TemporalAmount = MockSimplePeriod.of(7, ChronoUnit.MONTHS)
      TEST_12_30_40_987654321.plus(period)
    }
  }

  test("test_plus_Adjuster_null") {
    assertThrows[Platform.NPE] {
      TEST_12_30_40_987654321.plus(null.asInstanceOf[TemporalAmount])
    }
  }

  test("test_plus_longPeriodUnit_positiveHours") {
    val t: LocalTime = TEST_12_30_40_987654321.plus(7, ChronoUnit.HOURS)
    assertEquals(t, LocalTime.of(19, 30, 40, 987654321))
  }

  test("test_plus_longPeriodUnit_negativeMinutes") {
    val t: LocalTime = TEST_12_30_40_987654321.plus(-25, ChronoUnit.MINUTES)
    assertEquals(t, LocalTime.of(12, 5, 40, 987654321))
  }

  test("test_plus_longPeriodUnit_zero") {
    val t: LocalTime = TEST_12_30_40_987654321.plus(0, ChronoUnit.MINUTES)
    assertEquals(t, TEST_12_30_40_987654321)
  }

  test("test_plus_long_unit_invalidUnit") {
    for (unit <- TestLocalTime.INVALID_UNITS)
      try {
        TEST_12_30_40_987654321.plus(1, unit)
        fail("Unit should not be allowed " + unit)
      } catch {
        case _: DateTimeException =>
      }
  }

  test("test_plus_long_multiples") {
    assertThrows[UnsupportedTemporalTypeException] {
      TEST_12_30_40_987654321.plus(0, DAYS)
    }
  }

  test("test_plus_longPeriodUnit_null") {
    assertThrows[Platform.NPE] {
      TEST_12_30_40_987654321.plus(1, null.asInstanceOf[TemporalUnit])
    }
  }

  test("test_plus_adjuster") {
    val p: Duration  = Duration.ofSeconds(62, 3)
    val t: LocalTime = TEST_12_30_40_987654321.plus(p)
    assertEquals(t, LocalTime.of(12, 31, 42, 987654324))
  }

  test("test_plus_adjuster_big") {
    val p: Duration  = Duration.ofNanos(Long.MaxValue)
    val t: LocalTime = TEST_12_30_40_987654321.plus(p)
    assertEquals(t, TEST_12_30_40_987654321.plusNanos(Long.MaxValue))
  }

  test("test_plus_adjuster_zero_equal") {
    val t: LocalTime = TEST_12_30_40_987654321.plus(Period.ZERO)
    assertEquals(t, TEST_12_30_40_987654321)
  }

  test("test_plus_adjuster_wrap") {
    val p: Duration  = Duration.ofHours(1)
    val t: LocalTime = LocalTime.of(23, 30).plus(p)
    assertEquals(t, LocalTime.of(0, 30))
  }

  test("test_plus_adjuster_null") {
    assertThrows[Platform.NPE] {
      TEST_12_30_40_987654321.plus(null.asInstanceOf[TemporalAmount])
    }
  }

  test("test_plusHours_one") {
    var t: LocalTime = LocalTime.MIDNIGHT

    {
      var i: Int = 0
      while (i < 50) {
        {
          t = t.plusHours(1)
          assertEquals(t.getHour, (i + 1) % 24)
        }
        {
          i += 1
          i - 1
        }
      }
    }
  }

  test("test_plusHours_fromZero") {
    val base: LocalTime = LocalTime.MIDNIGHT

    {
      var i: Int = -50
      while (i < 50) {
        {
          val t: LocalTime = base.plusHours(i)
          assertEquals(t.getHour, (i + 72) % 24)
        }
        {
          i += 1
          i - 1
        }
      }
    }
  }

  test("test_plusHours_fromOne") {
    val base: LocalTime = LocalTime.of(1, 0)

    {
      var i: Int = -50
      while (i < 50) {
        {
          val t: LocalTime = base.plusHours(i)
          assertEquals(t.getHour, (1 + i + 72) % 24)
        }
        {
          i += 1
          i - 1
        }
      }
    }
  }

  test("test_plusHours_noChange_equal") {
    val t: LocalTime = TEST_12_30_40_987654321.plusHours(0)
    assertEquals(t, TEST_12_30_40_987654321)
  }

  test("test_plusHours_toMidnight_equal") {
    val t: LocalTime = LocalTime.of(23, 0).plusHours(1)
    assertEquals(t, LocalTime.MIDNIGHT)
  }

  test("test_plusHours_toMidday_equal") {
    val t: LocalTime = LocalTime.of(11, 0).plusHours(1)
    assertEquals(t, LocalTime.NOON)
  }

  test("test_plusHours_big") {
    val t: LocalTime = LocalTime.of(2, 30).plusHours(Long.MaxValue)
    val hours: Int   = (Long.MaxValue % 24L).toInt
    assertEquals(t, LocalTime.of(2, 30).plusHours(hours))
  }

  test("test_plusMinutes_one") {
    var t: LocalTime = LocalTime.MIDNIGHT
    var hour: Int    = 0
    var min: Int     = 0

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

  test("test_plusMinutes_fromZero") {
    val base: LocalTime = LocalTime.MIDNIGHT
    var hour: Int       = 0
    var min: Int        = 0

    {
      var i: Int = -70
      while (i < 70) {
        {
          val t: LocalTime = base.plusMinutes(i)
          if (i < -60) {
            hour = 22
            min = i + 120
          } else if (i < 0) {
            hour = 23
            min = i + 60
          } else if (i >= 60) {
            hour = 1
            min = i - 60
          } else {
            hour = 0
            min = i
          }
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

  test("test_plusMinutes_noChange_equal") {
    val t: LocalTime = TEST_12_30_40_987654321.plusMinutes(0)
    assertEquals(t, TEST_12_30_40_987654321)
  }

  test("test_plusMinutes_noChange_oneDay_equal") {
    val t: LocalTime = TEST_12_30_40_987654321.plusMinutes(24 * 60)
    assertEquals(t, TEST_12_30_40_987654321)
  }

  test("test_plusMinutes_toMidnight_equal") {
    val t: LocalTime = LocalTime.of(23, 59).plusMinutes(1)
    assertEquals(t, LocalTime.MIDNIGHT)
  }

  test("test_plusMinutes_toMidday_equal") {
    val t: LocalTime = LocalTime.of(11, 59).plusMinutes(1)
    assertEquals(t, LocalTime.NOON)
  }

  test("test_plusMinutes_big") {
    val t: LocalTime = LocalTime.of(2, 30).plusMinutes(Long.MaxValue)
    val mins: Int    = (Long.MaxValue % (24L * 60L)).toInt
    assertEquals(t, LocalTime.of(2, 30).plusMinutes(mins))
  }

  test("test_plusSeconds_one") {
    var t: LocalTime = LocalTime.MIDNIGHT
    var hour: Int    = 0
    var min: Int     = 0
    var sec: Int     = 0

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

  val plusSeconds_fromZero: java.util.Iterator[List[Int]] =
    new java.util.Iterator[List[Int]]() {
      private[bp] var delta: Int = 30
      private[bp] var i: Int     = -3660
      private[bp] var hour: Int  = 22
      private[bp] var min: Int   = 59
      private[bp] var sec: Int   = 0

      def hasNext: Boolean = i <= 3660

      def next: List[Int] = {
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
        List[Int](i, hour, min, sec)
      }

      override def remove(): Unit = throw new UnsupportedOperationException
    }

  test("test_plusSeconds_fromZero") {
    plusSeconds_fromZero.asScala.toList.foreach {
      case (seconds: Int) :: (hour: Int) :: (min: Int) :: (sec: Int) :: Nil =>
        val base: LocalTime = LocalTime.MIDNIGHT
        val t: LocalTime    = base.plusSeconds(seconds)
        assertEquals(hour, t.getHour)
        assertEquals(min, t.getMinute)
        assertEquals(sec, t.getSecond)
      case _                                                                =>
        fail()
    }
  }

  test("test_plusSeconds_noChange_equal") {
    val t: LocalTime = TEST_12_30_40_987654321.plusSeconds(0)
    assertEquals(t, TEST_12_30_40_987654321)
  }

  test("test_plusSeconds_noChange_oneDay_equal") {
    val t: LocalTime = TEST_12_30_40_987654321.plusSeconds(24 * 60 * 60)
    assertEquals(t, TEST_12_30_40_987654321)
  }

  test("test_plusSeconds_toMidnight_equal") {
    val t: LocalTime = LocalTime.of(23, 59, 59).plusSeconds(1)
    assertEquals(t, LocalTime.MIDNIGHT)
  }

  test("test_plusSeconds_toMidday_equal") {
    val t: LocalTime = LocalTime.of(11, 59, 59).plusSeconds(1)
    assertEquals(t, LocalTime.NOON)
  }

  test("test_plusNanos_halfABillion") {
    var t: LocalTime = LocalTime.MIDNIGHT
    var hour: Int    = 0
    var min: Int     = 0
    var sec: Int     = 0
    var nanos: Int   = 0
    var i: Long      = 0
    while (i < 3700 * 1000000000L) {
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
      assertEquals(t.getHour, hour)
      assertEquals(t.getMinute, min)
      assertEquals(t.getSecond, sec)
      assertEquals(t.getNano, nanos)
      i += 500000000
    }
  }

  val plusNanos_fromZero: java.util.Iterator[List[Any]] =
    new java.util.Iterator[List[Any]]() {
      private[bp] var delta: Long = 7500000000L
      private[bp] var i: Long     = -3660 * 1000000000L
      private[bp] var hour: Int   = 22
      private[bp] var min: Int    = 59
      private[bp] var sec: Int    = 0
      private[bp] var nanos: Long = 0

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
              if (hour == 24)
                hour = 0
            }
          }
        }
        List[Any](i, hour, min, sec, nanos.toInt)
      }

      override def remove(): Unit = throw new UnsupportedOperationException
    }

  test("test_plusNanos_fromZero") {
    plusNanos_fromZero.asScala.toList.foreach {
      case (nanoseconds: Long) :: (hour: Int) :: (min: Int) :: (sec: Int) :: (nanos: Int) :: Nil =>
        val base: LocalTime = LocalTime.MIDNIGHT
        val t: LocalTime    = base.plusNanos(nanoseconds)
        assertEquals(hour, t.getHour)
        assertEquals(min, t.getMinute)
        assertEquals(sec, t.getSecond)
        assertEquals(nanos, t.getNano)
      case _                                                                                     =>
        fail()
    }
  }

  test("test_plusNanos_noChange_equal") {
    val t: LocalTime = TEST_12_30_40_987654321.plusNanos(0)
    assertEquals(t, TEST_12_30_40_987654321)
  }

  test("test_plusNanos_noChange_oneDay_equal") {
    val t: LocalTime = TEST_12_30_40_987654321.plusNanos(24 * 60 * 60 * 1000000000L)
    assertEquals(t, TEST_12_30_40_987654321)
  }

  test("test_plusNanos_toMidnight_equal") {
    val t: LocalTime = LocalTime.of(23, 59, 59, 999999999).plusNanos(1)
    assertEquals(t, LocalTime.MIDNIGHT)
  }

  test("test_plusNanos_toMidday_equal") {
    val t: LocalTime = LocalTime.of(11, 59, 59, 999999999).plusNanos(1)
    assertEquals(t, LocalTime.NOON)
  }

  test("test_minus_Adjuster") {
    val p: TemporalAmount = Duration.ofSeconds(62, 3)
    val t: LocalTime      = TEST_12_30_40_987654321.minus(p)
    assertEquals(t, LocalTime.of(12, 29, 38, 987654318))
  }

  test("test_minus_Adjuster_positiveHours") {
    val period: TemporalAmount = MockSimplePeriod.of(7, ChronoUnit.HOURS)
    val t: LocalTime           = TEST_12_30_40_987654321.minus(period)
    assertEquals(t, LocalTime.of(5, 30, 40, 987654321))
  }

  test("test_minus_Adjuster_negativeMinutes") {
    val period: TemporalAmount = MockSimplePeriod.of(-25, ChronoUnit.MINUTES)
    val t: LocalTime           = TEST_12_30_40_987654321.minus(period)
    assertEquals(t, LocalTime.of(12, 55, 40, 987654321))
  }

  test("test_minus_Adjuster_big1") {
    val p: TemporalAmount = Duration.ofNanos(Long.MaxValue)
    val t: LocalTime      = TEST_12_30_40_987654321.minus(p)
    assertEquals(t, TEST_12_30_40_987654321.minusNanos(Long.MaxValue))
  }

  test("test_minus_Adjuster_zero") {
    val p: TemporalAmount = Period.ZERO
    val t: LocalTime      = TEST_12_30_40_987654321.minus(p)
    assertEquals(t, TEST_12_30_40_987654321)
  }

  test("test_minus_Adjuster_wrap") {
    val p: TemporalAmount = Duration.ofHours(1)
    val t: LocalTime      = LocalTime.of(0, 30).minus(p)
    assertEquals(t, LocalTime.of(23, 30))
  }

  test("test_minus_Adjuster_dateNotAllowed") {
    assertThrows[UnsupportedTemporalTypeException] {
      val period: TemporalAmount = MockSimplePeriod.of(7, ChronoUnit.MONTHS)
      TEST_12_30_40_987654321.minus(period)
    }
  }

  test("test_minus_Adjuster_null") {
    assertThrows[Platform.NPE] {
      TEST_12_30_40_987654321.minus(null.asInstanceOf[TemporalAmount])
    }
  }

  test("test_minus_longPeriodUnit_positiveHours") {
    val t: LocalTime = TEST_12_30_40_987654321.minus(7, ChronoUnit.HOURS)
    assertEquals(t, LocalTime.of(5, 30, 40, 987654321))
  }

  test("test_minus_longPeriodUnit_negativeMinutes") {
    val t: LocalTime = TEST_12_30_40_987654321.minus(-25, ChronoUnit.MINUTES)
    assertEquals(t, LocalTime.of(12, 55, 40, 987654321))
  }

  test("test_minus_longPeriodUnit_zero") {
    val t: LocalTime = TEST_12_30_40_987654321.minus(0, ChronoUnit.MINUTES)
    assertEquals(t, TEST_12_30_40_987654321)
  }

  test("test_minus_long_unit_invalidUnit") {
    for (unit <- TestLocalTime.INVALID_UNITS)
      try {
        TEST_12_30_40_987654321.minus(1, unit)
        fail("Unit should not be allowed " + unit)
      } catch {
        case _: DateTimeException =>
      }
  }

  test("test_minus_long_multiples") {
    assertThrows[UnsupportedTemporalTypeException] {
      TEST_12_30_40_987654321.minus(0, DAYS)
    }
  }

  test("test_minus_longPeriodUnit_null") {
    assertThrows[Platform.NPE] {
      TEST_12_30_40_987654321.minus(1, null.asInstanceOf[TemporalUnit])
    }
  }

  test("test_minusHours_one") {
    var t: LocalTime = LocalTime.MIDNIGHT

    {
      var i: Int = 0
      while (i < 50) {
        {
          t = t.minusHours(1)
          assertEquals(t.getHour, (((-i + 23) % 24) + 24) % 24, String.valueOf(i))
        }
        {
          i += 1
          i - 1
        }
      }
    }
  }

  test("test_minusHours_fromZero") {
    val base: LocalTime = LocalTime.MIDNIGHT

    {
      var i: Int = -50
      while (i < 50) {
        {
          val t: LocalTime = base.minusHours(i)
          assertEquals(t.getHour, ((-i % 24) + 24) % 24)
        }
        {
          i += 1
          i - 1
        }
      }
    }
  }

  test("test_minusHours_fromOne") {
    val base: LocalTime = LocalTime.of(1, 0)

    {
      var i: Int = -50
      while (i < 50) {
        {
          val t: LocalTime = base.minusHours(i)
          assertEquals(t.getHour, (1 + (-i % 24) + 24) % 24)
        }
        {
          i += 1
          i - 1
        }
      }
    }
  }

  test("test_minusHours_noChange_equal") {
    val t: LocalTime = TEST_12_30_40_987654321.minusHours(0)
    assertEquals(t, TEST_12_30_40_987654321)
  }

  test("test_minusHours_toMidnight_equal") {
    val t: LocalTime = LocalTime.of(1, 0).minusHours(1)
    assertEquals(t, LocalTime.MIDNIGHT)
  }

  test("test_minusHours_toMidday_equal") {
    val t: LocalTime = LocalTime.of(13, 0).minusHours(1)
    assertEquals(t, LocalTime.NOON)
  }

  test("test_minusHours_big") {
    val t: LocalTime = LocalTime.of(2, 30).minusHours(Long.MaxValue)
    val hours: Int   = (Long.MaxValue % 24L).toInt
    assertEquals(t, LocalTime.of(2, 30).minusHours(hours))
  }

  test("test_minusMinutes_one") {
    var t: LocalTime = LocalTime.MIDNIGHT
    var hour: Int    = 0
    var min: Int     = 0

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

  test("test_minusMinutes_fromZero") {
    val base: LocalTime = LocalTime.MIDNIGHT
    var hour: Int       = 22
    var min: Int        = 49

    {
      var i: Int = 70
      while (i > -70) {
        {
          val t: LocalTime = base.minusMinutes(i)
          min += 1
          if (min == 60) {
            hour += 1
            min = 0
            if (hour == 24)
              hour = 0
          }
          assertEquals(t.getHour, hour)
          assertEquals(t.getMinute, min)
        }
        {
          i -= 1
          i + 1
        }
      }
    }
  }

  test("test_minusMinutes_noChange_equal") {
    val t: LocalTime = TEST_12_30_40_987654321.minusMinutes(0)
    assertEquals(t, TEST_12_30_40_987654321)
  }

  test("test_minusMinutes_noChange_oneDay_equal") {
    val t: LocalTime = TEST_12_30_40_987654321.minusMinutes(24 * 60)
    assertEquals(t, TEST_12_30_40_987654321)
  }

  test("test_minusMinutes_toMidnight_equal") {
    val t: LocalTime = LocalTime.of(0, 1).minusMinutes(1)
    assertEquals(t, LocalTime.MIDNIGHT)
  }

  test("test_minusMinutes_toMidday_equals") {
    val t: LocalTime = LocalTime.of(12, 1).minusMinutes(1)
    assertEquals(t, LocalTime.NOON)
  }

  test("test_minusMinutes_big") {
    val t: LocalTime = LocalTime.of(2, 30).minusMinutes(Long.MaxValue)
    val mins: Int    = (Long.MaxValue % (24L * 60L)).toInt
    assertEquals(t, LocalTime.of(2, 30).minusMinutes(mins))
  }

  test("test_minusSeconds_one") {
    var t: LocalTime = LocalTime.MIDNIGHT
    var hour: Int    = 0
    var min: Int     = 0
    var sec: Int     = 0

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

  val minusSeconds_fromZero: java.util.Iterator[List[Int]] =
    new java.util.Iterator[List[Int]]() {
      private[bp] var delta: Int = 30
      private[bp] var i: Int     = 3660
      private[bp] var hour: Int  = 22
      private[bp] var min: Int   = 59
      private[bp] var sec: Int   = 0

      def hasNext: Boolean = i >= -3660

      def next: List[Int] = {
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
        List[Int](i, hour, min, sec)
      }

      override def remove(): Unit = throw new UnsupportedOperationException
    }

  test("test_minusSeconds_fromZero") {
    minusSeconds_fromZero.asScala.toList.foreach {
      case (seconds: Int) :: (hour: Int) :: (min: Int) :: (sec: Int) :: Nil =>
        val base: LocalTime = LocalTime.MIDNIGHT
        val t: LocalTime    = base.minusSeconds(seconds)
        assertEquals(t.getHour, hour)
        assertEquals(t.getMinute, min)
        assertEquals(t.getSecond, sec)
      case _                                                                =>
        fail()
    }
  }

  test("test_minusSeconds_noChange_equal") {
    val t: LocalTime = TEST_12_30_40_987654321.minusSeconds(0)
    assertEquals(t, TEST_12_30_40_987654321)
  }

  test("test_minusSeconds_noChange_oneDay_equal") {
    val t: LocalTime = TEST_12_30_40_987654321.minusSeconds(24 * 60 * 60)
    assertEquals(t, TEST_12_30_40_987654321)
  }

  test("test_minusSeconds_toMidnight_equal") {
    val t: LocalTime = LocalTime.of(0, 0, 1).minusSeconds(1)
    assertEquals(t, LocalTime.MIDNIGHT)
  }

  test("test_minusSeconds_toMidday_equal") {
    val t: LocalTime = LocalTime.of(12, 0, 1).minusSeconds(1)
    assertEquals(t, LocalTime.NOON)
  }

  test("test_minusSeconds_big") {
    val t: LocalTime = LocalTime.of(2, 30).minusSeconds(Long.MaxValue)
    val secs: Int    = (Long.MaxValue % (24L * 60L * 60L)).toInt
    assertEquals(t, LocalTime.of(2, 30).minusSeconds(secs))
  }

  test("test_minusNanos_halfABillion") {
    var t: LocalTime = LocalTime.MIDNIGHT
    var hour: Int    = 0
    var min: Int     = 0
    var sec: Int     = 0
    var nanos: Int   = 0

    {
      var i: Long = 0
      while (i < 3700 * 1000000000L) {
        {
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
          assertEquals(t.getHour, hour)
          assertEquals(t.getMinute, min)
          assertEquals(t.getSecond, sec)
          assertEquals(t.getNano, nanos)
        }
        i += 500000000
      }
    }
  }

  val minusNanos_fromZero: java.util.Iterator[List[Any]] =
    new java.util.Iterator[List[Any]]() {
      private[bp] var delta: Long = 7500000000L
      private[bp] var i: Long     = 3660 * 1000000000L
      private[bp] var hour: Int   = 22
      private[bp] var min: Int    = 59
      private[bp] var sec: Int    = 0
      private[bp] var nanos: Long = 0

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
              if (hour == 24)
                hour = 0
            }
          }
        }
        List(i, hour, min, sec, nanos)
      }

      override def remove(): Unit = throw new UnsupportedOperationException
    }

  test("test_minusNanos_fromZero") {
    minusNanos_fromZero.asScala.toList.foreach {
      case (nanoseconds: Long) :: (hour: Long) :: (min: Long) :: (sec: Long) :: (nanos: Long) :: Nil =>
        val base: LocalTime = LocalTime.MIDNIGHT
        val t: LocalTime    = base.minusNanos(nanoseconds)
        assertEquals(hour, t.getHour)
        assertEquals(min, t.getMinute)
        assertEquals(sec, t.getSecond)
        assertEquals(nanos, t.getNano)
      case _                                                                                         =>
        fail()
    }
  }

  test("test_minusNanos_noChange_equal") {
    val t: LocalTime = TEST_12_30_40_987654321.minusNanos(0)
    assertEquals(t, TEST_12_30_40_987654321)
  }

  test("test_minusNanos_noChange_oneDay_equal") {
    val t: LocalTime = TEST_12_30_40_987654321.minusNanos(24 * 60 * 60 * 1000000000L)
    assertEquals(t, TEST_12_30_40_987654321)
  }

  test("test_minusNanos_toMidnight_equal") {
    val t: LocalTime = LocalTime.of(0, 0, 0, 1).minusNanos(1)
    assertEquals(t, LocalTime.MIDNIGHT)
  }

  test("test_minusNanos_toMidday_equal") {
    val t: LocalTime = LocalTime.of(12, 0, 0, 1).minusNanos(1)
    assertEquals(t, LocalTime.NOON)
  }

  val provider_until: List[List[Any]] =
    List(
      List("00:00", "00:00", NANOS, 0L),
      List("00:00", "00:00", MICROS, 0L),
      List("00:00", "00:00", MILLIS, 0L),
      List("00:00", "00:00", SECONDS, 0L),
      List("00:00", "00:00", MINUTES, 0L),
      List("00:00", "00:00", HOURS, 0L),
      List("00:00", "00:00", HALF_DAYS, 0L),
      List("00:00", "00:00:01", NANOS, 1000000000L),
      List("00:00", "00:00:01", MICROS, 1000000L),
      List("00:00", "00:00:01", MILLIS, 1000L),
      List("00:00", "00:00:01", SECONDS, 1L),
      List("00:00", "00:00:01", MINUTES, 0L),
      List("00:00", "00:00:01", HOURS, 0L),
      List("00:00", "00:00:01", HALF_DAYS, 0L),
      List("00:00", "00:01", NANOS, 60000000000L),
      List("00:00", "00:01", MICROS, 60000000L),
      List("00:00", "00:01", MILLIS, 60000L),
      List("00:00", "00:01", SECONDS, 60L),
      List("00:00", "00:01", MINUTES, 1L),
      List("00:00", "00:01", HOURS, 0L),
      List("00:00", "00:01", HALF_DAYS, 0L)
    )

  test("test_until") {
    provider_until.foreach {
      case (startStr: String) :: (endStr: String) :: (unit: TemporalUnit) :: (expected: Long) :: Nil =>
        val start: LocalTime = LocalTime.parse(startStr)
        val end: LocalTime   = LocalTime.parse(endStr)
        assertEquals(start.until(end, unit), expected)
        assertEquals(end.until(start, unit), -expected)
      case _                                                                                         =>
        fail()
    }
  }

  test("test_atDate") {
    val t: LocalTime = LocalTime.of(11, 30)
    assertEquals(t.atDate(LocalDate.of(2012, 6, 30)), LocalDateTime.of(2012, 6, 30, 11, 30))
  }

  test("test_atDate_nullDate") {
    assertThrows[NullPointerException] {
      TEST_12_30_40_987654321.atDate(null.asInstanceOf[LocalDate])
    }
  }

  test("test_toSecondOfDay") {
    var t: LocalTime = LocalTime.of(0, 0)
    var i: Int       = 0
    while (i < 24 * 60 * 60) {
      assertEquals(t.toSecondOfDay, i)
      t = t.plusSeconds(1)
      i += 1
    }
  }

  test("test_toSecondOfDay_fromNanoOfDay_symmetry") {
    var t: LocalTime = LocalTime.of(0, 0)
    var i: Int       = 0
    while (i < 24 * 60 * 60) {
      assertEquals(LocalTime.ofSecondOfDay(t.toSecondOfDay), t)
      t = t.plusSeconds(1)
      i += 1
    }
  }

  test("test_toNanoOfDay") {
    var t: LocalTime = LocalTime.of(0, 0)

    {
      var i: Int = 0
      while (i < 1000000) {
        assertEquals(t.toNanoOfDay, i)
        t = t.plusNanos(1)
        i += 1
      }
    }
    t = LocalTime.of(0, 0)

    {
      var i: Int = 1
      while (i <= 1000000) {
        t = t.minusNanos(1)
        assertEquals(t.toNanoOfDay, 24 * 60 * 60 * 1000000000L - i)
        i += 1
      }
    }
  }

  test("test_toNanoOfDay_fromNanoOfDay_symmetry") {
    var t: LocalTime = LocalTime.of(0, 0)

    {
      var i: Int = 0
      while (i < 1000000) {
        assertEquals(LocalTime.ofNanoOfDay(t.toNanoOfDay), t)
        t = t.plusNanos(1)
        i += 1
      }
    }
    t = LocalTime.of(0, 0)

    {
      var i: Int = 1
      while (i <= 1000000) {
        t = t.minusNanos(1)
        assertEquals(LocalTime.ofNanoOfDay(t.toNanoOfDay), t)
        i += 1
      }
    }
  }

  test("test_comparisons") {
    doTest_comparisons_LocalTime(
      LocalTime.MIDNIGHT,
      LocalTime.of(0, 0, 0, 999999999),
      LocalTime.of(0, 0, 59, 0),
      LocalTime.of(0, 0, 59, 999999999),
      LocalTime.of(0, 59, 0, 0),
      LocalTime.of(0, 59, 0, 999999999),
      LocalTime.of(0, 59, 59, 0),
      LocalTime.of(0, 59, 59, 999999999),
      LocalTime.NOON,
      LocalTime.of(12, 0, 0, 999999999),
      LocalTime.of(12, 0, 59, 0),
      LocalTime.of(12, 0, 59, 999999999),
      LocalTime.of(12, 59, 0, 0),
      LocalTime.of(12, 59, 0, 999999999),
      LocalTime.of(12, 59, 59, 0),
      LocalTime.of(12, 59, 59, 999999999),
      LocalTime.of(23, 0, 0, 0),
      LocalTime.of(23, 0, 0, 999999999),
      LocalTime.of(23, 0, 59, 0),
      LocalTime.of(23, 0, 59, 999999999),
      LocalTime.of(23, 59, 0, 0),
      LocalTime.of(23, 59, 0, 999999999),
      LocalTime.of(23, 59, 59, 0),
      LocalTime.of(23, 59, 59, 999999999)
    )
  }

  private def doTest_comparisons_LocalTime(localTimes: LocalTime*): Unit = {
    var i: Int = 0
    while (i < localTimes.length) {
      val a: LocalTime = localTimes(i)
      var j: Int       = 0
      while (j < localTimes.length) {
        val b: LocalTime = localTimes(j)
        if (i < j) {
          assertTrue(a.compareTo(b) < 0)
          assertEquals(a.isBefore(b), true, a + " <=> " + b)
          assertEquals(a.isAfter(b), false, a + " <=> " + b)
          assertEquals(a == b, false, a + " <=> " + b)
        } else if (i > j) {
          assertTrue(a.compareTo(b) > 0)
          assertEquals(a.isBefore(b), false, a + " <=> " + b)
          assertEquals(a.isAfter(b), true, a + " <=> " + b)
          assertEquals(a == b, false, a + " <=> " + b)
        } else {
          assertEquals(a.compareTo(b), 0, a + " <=> " + b)
          assertEquals(a.isBefore(b), false, a + " <=> " + b)
          assertEquals(a.isAfter(b), false, a + " <=> " + b)
          assertEquals(a == b, true, a + " <=> " + b)
        }
        j += 1
      }

      i += 1
    }
  }

  test("test_compareTo_ObjectNull") {
    assertThrows[Platform.NPE] {
      TEST_12_30_40_987654321.compareTo(null)
    }
  }

  test("test_isBefore_ObjectNull") {
    assertThrows[Platform.NPE] {
      TEST_12_30_40_987654321.isBefore(null)
    }
  }

  test("test_isAfter_ObjectNull") {
    assertThrows[Platform.NPE] {
      TEST_12_30_40_987654321.isAfter(null)
    }
  }

  test("test_equals_true") {
    provider_sampleTimes.foreach {
      case (h: Int) :: (m: Int) :: (s: Int) :: (n: Int) :: Nil =>
        val a: LocalTime = LocalTime.of(h, m, s, n)
        val b: LocalTime = LocalTime.of(h, m, s, n)
        assertEquals(a == b, true)
      case _                                                   =>
        fail()
    }
  }

  test("test_equals_false_hour_differs") {
    provider_sampleTimes.foreach {
      case (h: Int) :: (m: Int) :: (s: Int) :: (n: Int) :: Nil =>
        val a: LocalTime = LocalTime.of(h, m, s, n)
        val b: LocalTime = LocalTime.of(h + 1, m, s, n)
        assertEquals(a == b, false)
      case _                                                   =>
        fail()
    }
  }

  test("test_equals_false_minute_differs") {
    provider_sampleTimes.foreach {
      case (h: Int) :: (m: Int) :: (s: Int) :: (n: Int) :: Nil =>
        val a: LocalTime = LocalTime.of(h, m, s, n)
        val b: LocalTime = LocalTime.of(h, m + 1, s, n)
        assertEquals(a == b, false)
      case _                                                   =>
        fail()
    }
  }

  test("test_equals_false_second_differs") {
    provider_sampleTimes.foreach {
      case (h: Int) :: (m: Int) :: (s: Int) :: (n: Int) :: Nil =>
        val a: LocalTime = LocalTime.of(h, m, s, n)
        val b: LocalTime = LocalTime.of(h, m, s + 1, n)
        assertEquals(a == b, false)
      case _                                                   =>
        fail()
    }
  }

  test("test_equals_false_nano_differs") {
    provider_sampleTimes.foreach {
      case (h: Int) :: (m: Int) :: (s: Int) :: (n: Int) :: Nil =>
        val a: LocalTime = LocalTime.of(h, m, s, n)
        val b: LocalTime = LocalTime.of(h, m, s, n + 1)
        assertEquals(a == b, false)
      case _                                                   =>
        fail()
    }
  }

  test("test_equals_itself_true") {
    assertEquals(TEST_12_30_40_987654321 == TEST_12_30_40_987654321, true)
  }

  test("test_equals_string_false") {
    assertNotEquals(TEST_12_30_40_987654321, "2007-07-15")
  }

  test("test_equals_null_false") {
    assertEquals(TEST_12_30_40_987654321 == null, false)
  }

  test("test_hashCode_same") {
    provider_sampleTimes.foreach {
      case (h: Int) :: (m: Int) :: (s: Int) :: (n: Int) :: Nil =>
        val a: LocalTime = LocalTime.of(h, m, s, n)
        val b: LocalTime = LocalTime.of(h, m, s, n)
        assertEquals(a.hashCode, b.hashCode)
      case _                                                   =>
        fail()
    }
  }

  test("test_hashCode_hour_differs") {
    provider_sampleTimes.foreach {
      case (h: Int) :: (m: Int) :: (s: Int) :: (n: Int) :: Nil =>
        val a: LocalTime = LocalTime.of(h, m, s, n)
        val b: LocalTime = LocalTime.of(h + 1, m, s, n)
        assertEquals(a.hashCode == b.hashCode, false)
      case _                                                   =>
        fail()
    }
  }

  test("test_hashCode_minute_differs") {
    provider_sampleTimes.foreach {
      case (h: Int) :: (m: Int) :: (s: Int) :: (n: Int) :: Nil =>
        val a: LocalTime = LocalTime.of(h, m, s, n)
        val b: LocalTime = LocalTime.of(h, m + 1, s, n)
        assertEquals(a.hashCode == b.hashCode, false)
      case _                                                   =>
        fail()
    }
  }

  test("test_hashCode_second_differs") {
    provider_sampleTimes.foreach {
      case (h: Int) :: (m: Int) :: (s: Int) :: (n: Int) :: Nil =>
        val a: LocalTime = LocalTime.of(h, m, s, n)
        val b: LocalTime = LocalTime.of(h, m, s + 1, n)
        assertEquals(a.hashCode == b.hashCode, false)
      case _                                                   =>
        fail()
    }
  }

  test("test_hashCode_nano_differs") {
    provider_sampleTimes.foreach {
      case (h: Int) :: (m: Int) :: (s: Int) :: (n: Int) :: Nil =>
        val a: LocalTime = LocalTime.of(h, m, s, n)
        val b: LocalTime = LocalTime.of(h, m, s, n + 1)
        assertEquals(a.hashCode == b.hashCode, false)
      case _                                                   =>
        fail()
    }
  }

  val provider_sampleToString: List[List[Any]] =
    List(
      List(0, 0, 0, 0, "00:00"),
      List(1, 0, 0, 0, "01:00"),
      List(23, 0, 0, 0, "23:00"),
      List(0, 1, 0, 0, "00:01"),
      List(12, 30, 0, 0, "12:30"),
      List(23, 59, 0, 0, "23:59"),
      List(0, 0, 1, 0, "00:00:01"),
      List(0, 0, 59, 0, "00:00:59"),
      List(0, 0, 0, 100000000, "00:00:00.100"),
      List(0, 0, 0, 10000000, "00:00:00.010"),
      List(0, 0, 0, 1000000, "00:00:00.001"),
      List(0, 0, 0, 100000, "00:00:00.000100"),
      List(0, 0, 0, 10000, "00:00:00.000010"),
      List(0, 0, 0, 1000, "00:00:00.000001"),
      List(0, 0, 0, 100, "00:00:00.000000100"),
      List(0, 0, 0, 10, "00:00:00.000000010"),
      List(0, 0, 0, 1, "00:00:00.000000001"),
      List(0, 0, 0, 999999999, "00:00:00.999999999"),
      List(0, 0, 0, 99999999, "00:00:00.099999999"),
      List(0, 0, 0, 9999999, "00:00:00.009999999"),
      List(0, 0, 0, 999999, "00:00:00.000999999"),
      List(0, 0, 0, 99999, "00:00:00.000099999"),
      List(0, 0, 0, 9999, "00:00:00.000009999"),
      List(0, 0, 0, 999, "00:00:00.000000999"),
      List(0, 0, 0, 99, "00:00:00.000000099"),
      List(0, 0, 0, 9, "00:00:00.000000009")
    )

  test("test_toString") {
    provider_sampleToString.foreach {
      case (h: Int) :: (m: Int) :: (s: Int) :: (n: Int) :: (expected: String) :: Nil =>
        val t: LocalTime = LocalTime.of(h, m, s, n)
        val str: String  = t.toString
        assertEquals(str, expected)
      case _                                                                         =>
        fail()
    }
  }

  test("test_format_formatter") {
    val f: DateTimeFormatter = DateTimeFormatter.ofPattern("H m s")
    val t: String            = LocalTime.of(11, 30, 45).format(f)
    assertEquals(t, "11 30 45")
  }

  test("test_format_formatter_null") {
    assertThrows[NullPointerException] {
      LocalTime.of(11, 30, 45).format(null)
    }
  }
}
