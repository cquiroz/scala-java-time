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

import java.util.Arrays

import org.scalatest.funsuite.AnyFunSuite
import org.threeten.bp.temporal.ChronoField.OFFSET_SECONDS
import org.threeten.bp.temporal._

/** Test ZoneOffset. */
class TestZoneOffset extends AnyFunSuite with GenDateTimeTest with AssertionsHelper {
  protected def samples: List[TemporalAccessor] =
    List(ZoneOffset.ofHours(1), ZoneOffset.ofHoursMinutesSeconds(-5, -6, -30))

  protected def validFields: List[TemporalField] =
    List(OFFSET_SECONDS)

  protected def invalidFields: List[TemporalField] = {
    val list: List[TemporalField] = List(ChronoField.values: _*)
    (list :+ JulianFields.JULIAN_DAY :+ JulianFields.MODIFIED_JULIAN_DAY :+ JulianFields.RATA_DIE)
      .filterNot(validFields.contains)
  }

  test("constant_UTC") {
    val test: ZoneOffset = ZoneOffset.UTC
    doTestOffset(test, 0, 0, 0)
  }

  test("constant_MIN") {
    val test: ZoneOffset = ZoneOffset.MIN
    doTestOffset(test, -18, 0, 0)
  }

  test("constant_MAX") {
    val test: ZoneOffset = ZoneOffset.MAX
    doTestOffset(test, 18, 0, 0)
  }

  test("factory_string_UTC") {
    val values: Array[String] = Array[String]("Z",
                                              "+0",
                                              "+00",
                                              "+0000",
                                              "+00:00",
                                              "+000000",
                                              "+00:00:00",
                                              "-00",
                                              "-0000",
                                              "-00:00",
                                              "-000000",
                                              "-00:00:00"
    )

    var i: Int = 0
    while (i < values.length) {
      val test: ZoneOffset = ZoneOffset.of(values(i))
      assertSame(test, ZoneOffset.UTC)
      i += 1
    }
  }

  test("factory_string_invalid") {
    val values: Array[String] = Array[String](
      "",
      "A",
      "B",
      "C",
      "D",
      "E",
      "F",
      "G",
      "H",
      "I",
      "J",
      "K",
      "L",
      "M",
      "N",
      "O",
      "P",
      "Q",
      "R",
      "S",
      "T",
      "U",
      "V",
      "W",
      "X",
      "Y",
      "ZZ",
      "0",
      "+0:00",
      "+00:0",
      "+0:0",
      "+000",
      "+00000",
      "+0:00:00",
      "+00:0:00",
      "+00:00:0",
      "+0:0:0",
      "+0:0:00",
      "+00:0:0",
      "+0:00:0",
      "1",
      "+01_00",
      "+01;00",
      "+01@00",
      "+01:AA",
      "+19",
      "+19:00",
      "+18:01",
      "+18:00:01",
      "+1801",
      "+180001",
      "-0:00",
      "-00:0",
      "-0:0",
      "-000",
      "-00000",
      "-0:00:00",
      "-00:0:00",
      "-00:00:0",
      "-0:0:0",
      "-0:0:00",
      "-00:0:0",
      "-0:00:0",
      "-19",
      "-19:00",
      "-18:01",
      "-18:00:01",
      "-1801",
      "-180001",
      "-01_00",
      "-01;00",
      "-01@00",
      "-01:AA",
      "@01:00"
    )

    var i: Int = 0
    while (i < values.length) {
      try {
        ZoneOffset.of(values(i))
        fail("Should have failed:" + values(i))
      } catch {
        case _: DateTimeException =>
      }
      i += 1
    }
  }

  test("factory_string_null") {
    assertThrows[NullPointerException] {
      ZoneOffset.of(null.asInstanceOf[String])
    }
  }

  test("factory_string_singleDigitHours") {
    var i: Int = -9
    while (i <= 9) {
      val str: String      = (if (i < 0) "-" else "+") + Math.abs(i)
      val test: ZoneOffset = ZoneOffset.of(str)
      doTestOffset(test, i, 0, 0)
      i += 1
    }
  }

  test("factory_string_hours") {
    var i: Int = -18
    while (i <= 18) {
      val str: String      = (if (i < 0) "-" else "+") + Integer.toString(Math.abs(i) + 100).substring(1)
      val test: ZoneOffset = ZoneOffset.of(str)
      doTestOffset(test, i, 0, 0)
      i += 1
    }
  }

  test("factory_string_hours_minutes_noColon") {
    var i: Int = -17
    while (i <= 17) {
      var j: Int = -59
      while (j <= 59) {
        if (i < 0 && j <= 0 || i > 0 && j >= 0 || i == 0) {
          val str: String      = (if (i < 0 || j < 0) "-" else "+") + Integer
            .toString(Math.abs(i) + 100)
            .substring(1) + Integer.toString(Math.abs(j) + 100).substring(1)
          val test: ZoneOffset = ZoneOffset.of(str)
          doTestOffset(test, i, j, 0)
        }
        j += 1
      }
      i += 1
    }

    val test1: ZoneOffset = ZoneOffset.of("-1800")
    doTestOffset(test1, -18, 0, 0)
    val test2: ZoneOffset = ZoneOffset.of("+1800")
    doTestOffset(test2, 18, 0, 0)
  }

  test("factory_string_hours_minutes_colon") {
    var i: Int = -17
    while (i <= 17) {
      var j: Int = -59
      while (j <= 59) {
        if (i < 0 && j <= 0 || i > 0 && j >= 0 || i == 0) {
          val str: String      = (if (i < 0 || j < 0) "-" else "+") + Integer
            .toString(Math.abs(i) + 100)
            .substring(1) + ":" + Integer.toString(Math.abs(j) + 100).substring(1)
          val test: ZoneOffset = ZoneOffset.of(str)
          doTestOffset(test, i, j, 0)
        }
        j += 1
      }
      i += 1
    }

    val test1: ZoneOffset = ZoneOffset.of("-18:00")
    doTestOffset(test1, -18, 0, 0)
    val test2: ZoneOffset = ZoneOffset.of("+18:00")
    doTestOffset(test2, 18, 0, 0)
  }

  test("factory_string_hours_minutes_seconds_noColon") {
    var i: Int = -17
    while (i <= 17) {
      var j: Int = -59
      while (j <= 59) {
        var k: Int = -59
        while (k <= 59) {
          if (
            i < 0 && j <= 0 && k <= 0 || i > 0 && j >= 0 && k >= 0 || i == 0 && (j < 0 && k <= 0 || j > 0 && k >= 0 || j == 0)
          ) {
            val str: String      = (if (i < 0 || j < 0 || k < 0) "-" else "+") + Integer
              .toString(Math.abs(i) + 100)
              .substring(1) + Integer.toString(Math.abs(j) + 100).substring(1) + Integer
              .toString(Math.abs(k) + 100)
              .substring(1)
            val test: ZoneOffset = ZoneOffset.of(str)
            doTestOffset(test, i, j, k)
          }
          k += 1
        }
        j += 1
      }
      i += 1
    }

    val test1: ZoneOffset = ZoneOffset.of("-180000")
    doTestOffset(test1, -18, 0, 0)
    val test2: ZoneOffset = ZoneOffset.of("+180000")
    doTestOffset(test2, 18, 0, 0)
  }

  test("factory_string_hours_minutes_seconds_colon") {
    var i: Int = -17
    while (i <= 17) {
      var j: Int = -59
      while (j <= 59) {
        var k: Int = -59
        while (k <= 59) {
          if (
            i < 0 && j <= 0 && k <= 0 || i > 0 && j >= 0 && k >= 0 || i == 0 && (j < 0 && k <= 0 || j > 0 && k >= 0 || j == 0)
          ) {
            val str: String      = (if (i < 0 || j < 0 || k < 0) "-" else "+") + Integer
              .toString(Math.abs(i) + 100)
              .substring(1) + ":" + Integer.toString(Math.abs(j) + 100).substring(1) + ":" + Integer
              .toString(Math.abs(k) + 100)
              .substring(1)
            val test: ZoneOffset = ZoneOffset.of(str)
            doTestOffset(test, i, j, k)
          }
          k += 1
        }
        j += 1
      }
      i += 1
    }

    val test1: ZoneOffset = ZoneOffset.of("-18:00:00")
    doTestOffset(test1, -18, 0, 0)
    val test2: ZoneOffset = ZoneOffset.of("+18:00:00")
    doTestOffset(test2, 18, 0, 0)
  }

  test("factory_int_hours") {
    var i: Int = -18
    while (i <= 18) {
      val test: ZoneOffset = ZoneOffset.ofHours(i)
      doTestOffset(test, i, 0, 0)
      i += 1
    }
  }

  test("factory_int_hours_tooBig") {
    assertThrows[DateTimeException] {
      ZoneOffset.ofHours(19)
    }
  }

  test("factory_int_hours_tooSmall") {
    assertThrows[DateTimeException] {
      ZoneOffset.ofHours(-19)
    }
  }

  test("factory_int_hours_minutes") {
    var i: Int = -17
    while (i <= 17) {
      var j: Int = -59
      while (j <= 59) {
        if (i < 0 && j <= 0 || i > 0 && j >= 0 || i == 0) {
          val test: ZoneOffset = ZoneOffset.ofHoursMinutes(i, j)
          doTestOffset(test, i, j, 0)
        }
        j += 1
      }
      i += 1
    }

    val test1: ZoneOffset = ZoneOffset.ofHoursMinutes(-18, 0)
    doTestOffset(test1, -18, 0, 0)
    val test2: ZoneOffset = ZoneOffset.ofHoursMinutes(18, 0)
    doTestOffset(test2, 18, 0, 0)
  }

  test("factory_int_hours_minutes_tooBig") {
    assertThrows[DateTimeException] {
      ZoneOffset.ofHoursMinutes(19, 0)
    }
  }

  test("factory_int_hours_minutes_tooSmall") {
    assertThrows[DateTimeException] {
      ZoneOffset.ofHoursMinutes(-19, 0)
    }
  }

  test("factory_int_hours_minutes_seconds") {
    var i: Int = -17
    while (i <= 17) {
      var j: Int = -59
      while (j <= 59) {
        var k: Int = -59
        while (k <= 59) {
          if (
            i < 0 && j <= 0 && k <= 0 || i > 0 && j >= 0 && k >= 0 || i == 0 && (j < 0 && k <= 0 || j > 0 && k >= 0 || j == 0)
          ) {
            val test: ZoneOffset = ZoneOffset.ofHoursMinutesSeconds(i, j, k)
            doTestOffset(test, i, j, k)
          }
          k += 1
        }
        j += 1
      }
      i += 1
    }

    val test1: ZoneOffset = ZoneOffset.ofHoursMinutesSeconds(-18, 0, 0)
    doTestOffset(test1, -18, 0, 0)
    val test2: ZoneOffset = ZoneOffset.ofHoursMinutesSeconds(18, 0, 0)
    doTestOffset(test2, 18, 0, 0)
  }

  test("factory_int_hours_minutes_seconds_plusHoursMinusMinutes") {
    assertThrows[DateTimeException] {
      ZoneOffset.ofHoursMinutesSeconds(1, -1, 0)
    }
  }

  test("factory_int_hours_minutes_seconds_plusHoursMinusSeconds") {
    assertThrows[DateTimeException] {
      ZoneOffset.ofHoursMinutesSeconds(1, 0, -1)
    }
  }

  test("factory_int_hours_minutes_seconds_minusHoursPlusMinutes") {
    assertThrows[DateTimeException] {
      ZoneOffset.ofHoursMinutesSeconds(-1, 1, 0)
    }
  }

  test("factory_int_hours_minutes_seconds_minusHoursPlusSeconds") {
    assertThrows[DateTimeException] {
      ZoneOffset.ofHoursMinutesSeconds(-1, 0, 1)
    }
  }

  test("factory_int_hours_minutes_seconds_zeroHoursMinusMinutesPlusSeconds") {
    assertThrows[DateTimeException] {
      ZoneOffset.ofHoursMinutesSeconds(0, -1, 1)
    }
  }

  test("factory_int_hours_minutes_seconds_zeroHoursPlusMinutesMinusSeconds") {
    assertThrows[DateTimeException] {
      ZoneOffset.ofHoursMinutesSeconds(0, 1, -1)
    }
  }

  test("factory_int_hours_minutes_seconds_minutesTooLarge") {
    assertThrows[DateTimeException] {
      ZoneOffset.ofHoursMinutesSeconds(0, 60, 0)
    }
  }

  test("factory_int_hours_minutes_seconds_minutesTooSmall") {
    assertThrows[DateTimeException] {
      ZoneOffset.ofHoursMinutesSeconds(0, -60, 0)
    }
  }

  test("factory_int_hours_minutes_seconds_secondsTooLarge") {
    assertThrows[DateTimeException] {
      ZoneOffset.ofHoursMinutesSeconds(0, 0, 60)
    }
  }

  test("factory_int_hours_minutes_seconds_secondsTooSmall") {
    assertThrows[DateTimeException] {
      ZoneOffset.ofHoursMinutesSeconds(0, 0, 60)
    }
  }

  test("factory_int_hours_minutes_seconds_hoursTooBig") {
    assertThrows[DateTimeException] {
      ZoneOffset.ofHoursMinutesSeconds(19, 0, 0)
    }
  }

  test("factory_int_hours_minutes_seconds_hoursTooSmall") {
    assertThrows[DateTimeException] {
      ZoneOffset.ofHoursMinutesSeconds(-19, 0, 0)
    }
  }

  test("factory_ofTotalSeconds") {
    assertEquals(ZoneOffset.ofTotalSeconds(60 * 60 + 1), ZoneOffset.ofHoursMinutesSeconds(1, 0, 1))
    assertEquals(ZoneOffset.ofTotalSeconds(18 * 60 * 60), ZoneOffset.ofHours(18))
    assertEquals(ZoneOffset.ofTotalSeconds(-18 * 60 * 60), ZoneOffset.ofHours(-18))
  }

  test("factory_ofTotalSeconds_tooLarge") {
    assertThrows[DateTimeException] {
      ZoneOffset.ofTotalSeconds(18 * 60 * 60 + 1)
    }
  }

  test("factory_ofTotalSeconds_tooSmall") {
    assertThrows[DateTimeException] {
      ZoneOffset.ofTotalSeconds(-18 * 60 * 60 - 1)
    }
  }

  test("factory_TemporalAccessor") {
    assertEquals(ZoneOffset.from(OffsetTime.of(LocalTime.of(12, 30), ZoneOffset.ofHours(6))),
                 ZoneOffset.ofHours(6)
    )
    assertEquals(
      ZoneOffset.from(
        ZonedDateTime.of(LocalDateTime.of(LocalDate.of(2007, 7, 15), LocalTime.of(17, 30)),
                         ZoneOffset.ofHours(2)
        )
      ),
      ZoneOffset.ofHours(2)
    )
  }

  test("factory_TemporalAccessor_invalid_noDerive") {
    assertThrows[DateTimeException] {
      ZoneOffset.from(LocalTime.of(12, 30))
    }
  }

  test("factory_TemporalAccessor_null") {
    assertThrows[Platform.NPE] {
      ZoneOffset.from(null.asInstanceOf[TemporalAccessor])
    }
  }

  test("getTotalSeconds") {
    val offset: ZoneOffset = ZoneOffset.ofTotalSeconds(60 * 60 + 1)
    assertEquals(offset.getTotalSeconds, 60 * 60 + 1)
  }

  test("getId") {
    var offset: ZoneOffset = ZoneOffset.ofHoursMinutesSeconds(1, 0, 0)
    assertEquals(offset.getId, "+01:00")
    offset = ZoneOffset.ofHoursMinutesSeconds(1, 2, 3)
    assertEquals(offset.getId, "+01:02:03")
    offset = ZoneOffset.UTC
    assertEquals(offset.getId, "Z")
  }

  test("getRules") {
    val offset: ZoneOffset = ZoneOffset.ofHoursMinutesSeconds(1, 2, 3)
    assertEquals(offset.getRules.isFixedOffset, true)
    assertEquals(offset.getRules.getOffset(null.asInstanceOf[Instant]), offset)
    assertEquals(offset.getRules.getDaylightSavings(null.asInstanceOf[Instant]), Duration.ZERO)
    assertEquals(offset.getRules.getStandardOffset(null.asInstanceOf[Instant]), offset)
    assertEquals(offset.getRules.nextTransition(null.asInstanceOf[Instant]), null)
    assertEquals(offset.getRules.previousTransition(null.asInstanceOf[Instant]), null)
    assertEquals(offset.getRules.isValidOffset(null.asInstanceOf[LocalDateTime], offset), true)
    assertEquals(offset.getRules.isValidOffset(null.asInstanceOf[LocalDateTime], ZoneOffset.UTC),
                 false
    )
    assertEquals(offset.getRules.isValidOffset(null.asInstanceOf[LocalDateTime], null), false)
    assertEquals(offset.getRules.getOffset(null.asInstanceOf[LocalDateTime]), offset)
    assertEquals(offset.getRules.getValidOffsets(null.asInstanceOf[LocalDateTime]),
                 Arrays.asList(offset)
    )
    assertEquals(offset.getRules.getTransition(null.asInstanceOf[LocalDateTime]), null)
    assertEquals(offset.getRules.getTransitions.size, 0)
    assertEquals(offset.getRules.getTransitionRules.size, 0)
  }

  test("get_TemporalField") {
    assertEquals(ZoneOffset.UTC.get(OFFSET_SECONDS), 0)
    assertEquals(ZoneOffset.ofHours(-2).get(OFFSET_SECONDS), -7200)
    assertEquals(ZoneOffset.ofHoursMinutesSeconds(0, 1, 5).get(OFFSET_SECONDS), 65)
  }

  test("getLong_TemporalField") {
    assertEquals(ZoneOffset.UTC.getLong(OFFSET_SECONDS), 0)
    assertEquals(ZoneOffset.ofHours(-2).getLong(OFFSET_SECONDS), -7200)
    assertEquals(ZoneOffset.ofHoursMinutesSeconds(0, 1, 5).getLong(OFFSET_SECONDS), 65)
  }

  test("query") {
    assertEquals(ZoneOffset.UTC.query(TemporalQueries.chronology), null)
    assertEquals(ZoneOffset.UTC.query(TemporalQueries.localDate), null)
    assertEquals(ZoneOffset.UTC.query(TemporalQueries.localTime), null)
    assertEquals(ZoneOffset.UTC.query(TemporalQueries.offset), ZoneOffset.UTC)
    assertEquals(ZoneOffset.UTC.query(TemporalQueries.precision), null)
    assertEquals(ZoneOffset.UTC.query(TemporalQueries.zone), ZoneOffset.UTC)
    assertEquals(ZoneOffset.UTC.query(TemporalQueries.zoneId), null)
  }

  test("query_null") {
    assertThrows[Platform.NPE] {
      ZoneOffset.UTC.query(null)
    }
  }

  test("compareTo") {
    val offset1: ZoneOffset = ZoneOffset.ofHoursMinutesSeconds(1, 2, 3)
    val offset2: ZoneOffset = ZoneOffset.ofHoursMinutesSeconds(2, 3, 4)
    assertTrue(offset1.compareTo(offset2) > 0)
    assertTrue(offset2.compareTo(offset1) < 0)
    assertTrue(offset1.compareTo(offset1) == 0)
    assertTrue(offset2.compareTo(offset2) == 0)
  }

  test("equals") {
    val offset1: ZoneOffset  = ZoneOffset.ofHoursMinutesSeconds(1, 2, 3)
    val offset2: ZoneOffset  = ZoneOffset.ofHoursMinutesSeconds(2, 3, 4)
    val offset2b: ZoneOffset = ZoneOffset.ofHoursMinutesSeconds(2, 3, 4)
    assertEquals(offset1 == offset2, false)
    assertEquals(offset2 == offset1, false)
    assertEquals(offset1 == offset1, true)
    assertEquals(offset2 == offset2, true)
    assertEquals(offset2 == offset2b, true)
    assertEquals(offset1.hashCode == offset1.hashCode, true)
    assertEquals(offset2.hashCode == offset2.hashCode, true)
    assertEquals(offset2.hashCode == offset2b.hashCode, true)
  }

  test("toString") {
    var offset: ZoneOffset = ZoneOffset.ofHoursMinutesSeconds(1, 0, 0)
    assertEquals(offset.toString, "+01:00")
    offset = ZoneOffset.ofHoursMinutesSeconds(1, 2, 3)
    assertEquals(offset.toString, "+01:02:03")
    offset = ZoneOffset.UTC
    assertEquals(offset.toString, "Z")
  }

  private def doTestOffset(offset: ZoneOffset, hours: Int, minutes: Int, seconds: Int): Unit = {
    assertEquals(offset.getTotalSeconds, hours * 60 * 60 + minutes * 60 + seconds)
    var id: String = null
    if (hours == 0 && minutes == 0 && seconds == 0)
      id = "Z"
    else {
      var str: String = if (hours < 0 || minutes < 0 || seconds < 0) "-" else "+"
      str += Integer.toString(Math.abs(hours) + 100).substring(1)
      str += ":"
      str += Integer.toString(Math.abs(minutes) + 100).substring(1)
      if (seconds != 0) {
        str += ":"
        str += Integer.toString(Math.abs(seconds) + 100).substring(1)
      }
      id = str
    }
    assertEquals(offset.getId, id)
    assertEquals(offset, ZoneOffset.ofHoursMinutesSeconds(hours, minutes, seconds))
    if (seconds == 0) {
      assertEquals(offset, ZoneOffset.ofHoursMinutes(hours, minutes))
      if (minutes == 0)
        assertEquals(offset, ZoneOffset.ofHours(hours))
    }
    assertEquals(ZoneOffset.of(id), offset)
    assertEquals(offset.toString, id)
  }
}
