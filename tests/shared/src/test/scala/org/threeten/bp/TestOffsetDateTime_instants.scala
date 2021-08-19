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

import org.scalatest.funsuite.AnyFunSuite

/** Test OffsetDateTime creation. */
object TestOffsetDateTime_instants {
  private val OFFSET_PONE: ZoneOffset = ZoneOffset.ofHours(1)
  private val OFFSET_MAX: ZoneOffset  = ZoneOffset.ofHours(18)
  private val OFFSET_MIN: ZoneOffset  = ZoneOffset.ofHours(-18)
}

class TestOffsetDateTime_instants extends AnyFunSuite with AssertionsHelper {
  test("factory_ofInstant_nullInstant") {
    assertThrows[NullPointerException] {
      OffsetDateTime.ofInstant(null.asInstanceOf[Instant], TestOffsetDateTime_instants.OFFSET_PONE)
    }
  }

  test("factory_ofInstant_nullOffset") {
    assertThrows[NullPointerException] {
      val instant: Instant = Instant.ofEpochSecond(0L)
      OffsetDateTime.ofInstant(instant, null.asInstanceOf[ZoneOffset])
    }
  }

  test("factory_ofInstant_allSecsInDay") {
    var i: Int = 0
    while (i < (24 * 60 * 60)) {
      {
        val instant: Instant     = Instant.ofEpochSecond(i)
        val test: OffsetDateTime =
          OffsetDateTime.ofInstant(instant, TestOffsetDateTime_instants.OFFSET_PONE)
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
    var expected: OffsetDateTime =
      OffsetDateTime.of(LocalDate.of(1970, 1, 1), LocalTime.of(0, 0, 0, 0), ZoneOffset.UTC)

    {
      var i: Long = 0
      while (i < 146097) {
        {
          val instant: Instant     = Instant.ofEpochSecond(i * 24L * 60L * 60L)
          val test: OffsetDateTime = OffsetDateTime.ofInstant(instant, ZoneOffset.UTC)
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

  test("factory_ofInstant_history") {
    doTest_factory_ofInstant_all(-2820, 2820)
  }

  test("factory_ofInstant_minYear") {
    doTest_factory_ofInstant_all(Year.MIN_VALUE, Year.MIN_VALUE + 420)
  }

  test("factory_ofInstant_tooLow") {
    assertThrows[DateTimeException] {
      val days_0000_to_1970: Long = (146097 * 5) - (30 * 365 + 7)
      val year: Int               = Year.MIN_VALUE - 1
      val days: Long              = (year * 365L + (year / 4 - year / 100 + year / 400)) - days_0000_to_1970
      val instant: Instant        = Instant.ofEpochSecond(days * 24L * 60L * 60L)
      OffsetDateTime.ofInstant(instant, ZoneOffset.UTC)
    }
  }

  test("factory_ofInstant_maxYear") {
    doTest_factory_ofInstant_all(Year.MAX_VALUE - 420, Year.MAX_VALUE)
  }

  test("factory_ofInstant_tooBig") {
    assertThrows[DateTimeException] {
      val days_0000_to_1970: Long = (146097 * 5) - (30 * 365 + 7)
      val year: Long              = Year.MAX_VALUE + 1L
      val days: Long              = (year * 365L + (year / 4 - year / 100 + year / 400)) - days_0000_to_1970
      val instant: Instant        = Instant.ofEpochSecond(days * 24L * 60L * 60L)
      OffsetDateTime.ofInstant(instant, ZoneOffset.UTC)
    }
  }

  test("factory_ofInstant_minWithMinOffset") {
    val days_0000_to_1970: Long = (146097 * 5) - (30 * 365 + 7)
    val year: Int               = Year.MIN_VALUE
    val days: Long              = (year * 365L + (year / 4 - year / 100 + year / 400)) - days_0000_to_1970
    val instant: Instant        = Instant.ofEpochSecond(
      days * 24L * 60L * 60L - TestOffsetDateTime_instants.OFFSET_MIN.getTotalSeconds
    )
    val test: OffsetDateTime    =
      OffsetDateTime.ofInstant(instant, TestOffsetDateTime_instants.OFFSET_MIN)
    assertEquals(test.getYear, Year.MIN_VALUE)
    assertEquals(test.getMonth.getValue, 1)
    assertEquals(test.getDayOfMonth, 1)
    assertEquals(test.getOffset, TestOffsetDateTime_instants.OFFSET_MIN)
    assertEquals(test.getHour, 0)
    assertEquals(test.getMinute, 0)
    assertEquals(test.getSecond, 0)
    assertEquals(test.getNano, 0)
  }

  test("factory_ofInstant_minWithMaxOffset") {
    val days_0000_to_1970: Long = (146097 * 5) - (30 * 365 + 7)
    val year: Int               = Year.MIN_VALUE
    val days: Long              = (year * 365L + (year / 4 - year / 100 + year / 400)) - days_0000_to_1970
    val instant: Instant        = Instant.ofEpochSecond(
      days * 24L * 60L * 60L - TestOffsetDateTime_instants.OFFSET_MAX.getTotalSeconds
    )
    val test: OffsetDateTime    =
      OffsetDateTime.ofInstant(instant, TestOffsetDateTime_instants.OFFSET_MAX)
    assertEquals(test.getYear, Year.MIN_VALUE)
    assertEquals(test.getMonth.getValue, 1)
    assertEquals(test.getDayOfMonth, 1)
    assertEquals(test.getOffset, TestOffsetDateTime_instants.OFFSET_MAX)
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
      (days + 1) * 24L * 60L * 60L - 1 - TestOffsetDateTime_instants.OFFSET_MIN.getTotalSeconds
    )
    val test: OffsetDateTime    =
      OffsetDateTime.ofInstant(instant, TestOffsetDateTime_instants.OFFSET_MIN)
    assertEquals(test.getYear, Year.MAX_VALUE)
    assertEquals(test.getMonth.getValue, 12)
    assertEquals(test.getDayOfMonth, 31)
    assertEquals(test.getOffset, TestOffsetDateTime_instants.OFFSET_MIN)
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
      (days + 1) * 24L * 60L * 60L - 1 - TestOffsetDateTime_instants.OFFSET_MAX.getTotalSeconds
    )
    val test: OffsetDateTime    =
      OffsetDateTime.ofInstant(instant, TestOffsetDateTime_instants.OFFSET_MAX)
    assertEquals(test.getYear, Year.MAX_VALUE)
    assertEquals(test.getMonth.getValue, 12)
    assertEquals(test.getDayOfMonth, 31)
    assertEquals(test.getOffset, TestOffsetDateTime_instants.OFFSET_MAX)
    assertEquals(test.getHour, 23)
    assertEquals(test.getMinute, 59)
    assertEquals(test.getSecond, 59)
    assertEquals(test.getNano, 0)
  }

  test("factory_ofInstant_maxInstantWithMaxOffset") {
    assertThrows[DateTimeException] {
      val instant: Instant = Instant.ofEpochSecond(Long.MaxValue)
      OffsetDateTime.ofInstant(instant, TestOffsetDateTime_instants.OFFSET_MAX)
    }
  }

  test("factory_ofInstant_maxInstantWithMinOffset") {
    assertThrows[DateTimeException] {
      val instant: Instant = Instant.ofEpochSecond(Long.MaxValue)
      OffsetDateTime.ofInstant(instant, TestOffsetDateTime_instants.OFFSET_MIN)
    }
  }

  private def doTest_factory_ofInstant_all(minYear: Long, maxYear: Long): Unit = {
    val days_0000_to_1970: Long  = (146097 * 5) - (30 * 365 + 7)
    val minOffset: Int           = if (minYear <= 0) 0 else 3
    val maxOffset: Int           = if (maxYear <= 0) 0 else 3
    val minDays: Long            =
      (minYear * 365L + ((minYear + minOffset) / 4L - (minYear + minOffset) / 100L + (minYear + minOffset) / 400L)) - days_0000_to_1970
    val maxDays: Long            =
      (maxYear * 365L + ((maxYear + maxOffset) / 4L - (maxYear + maxOffset) / 100L + (maxYear + maxOffset) / 400L)) + 365L - days_0000_to_1970
    val maxDate: LocalDate       = LocalDate.of(Year.MAX_VALUE, 12, 31)
    var expected: OffsetDateTime =
      OffsetDateTime.of(LocalDate.of(minYear.toInt, 1, 1), LocalTime.of(0, 0, 0, 0), ZoneOffset.UTC)

    {
      var i: Long = minDays
      while (i < maxDays) {
        {
          val instant: Instant = Instant.ofEpochSecond(i * 24L * 60L * 60L)
          try {
            val test: OffsetDateTime = OffsetDateTime.ofInstant(instant, ZoneOffset.UTC)
            assertEquals(test, expected)
            if ((expected.toLocalDate == maxDate) == false)
              expected = expected.plusDays(1)
          } catch {
            case ex: RuntimeException =>
              System.out.println("RuntimeException: " + i + " " + expected)
              throw ex
            case ex: Error =>
              System.out.println("Error: " + i + " " + expected)
              throw ex
          }
        }
        {
          i += 1
          i - 1
        }
      }
    }
  }

  test("toInstant_19700101") {
    val dt: OffsetDateTime =
      OffsetDateTime.of(LocalDate.of(1970, 1, 1), LocalTime.of(0, 0, 0, 0), ZoneOffset.UTC)
    val test: Instant      = dt.toInstant
    assertEquals(test.getEpochSecond, 0)
    assertEquals(test.getNano, 0)
  }

  test("toInstant_19700101_oneNano") {
    val dt: OffsetDateTime =
      OffsetDateTime.of(LocalDate.of(1970, 1, 1), LocalTime.of(0, 0, 0, 1), ZoneOffset.UTC)
    val test: Instant      = dt.toInstant
    assertEquals(test.getEpochSecond, 0)
    assertEquals(test.getNano, 1)
  }

  test("toInstant_19700101_minusOneNano") {
    val dt: OffsetDateTime = OffsetDateTime.of(LocalDate.of(1969, 12, 31),
                                               LocalTime.of(23, 59, 59, 999999999),
                                               ZoneOffset.UTC
    )
    val test: Instant      = dt.toInstant
    assertEquals(test.getEpochSecond, -1)
    assertEquals(test.getNano, 999999999)
  }

  test("toInstant_19700102") {
    val dt: OffsetDateTime =
      OffsetDateTime.of(LocalDate.of(1970, 1, 2), LocalTime.of(0, 0, 0, 0), ZoneOffset.UTC)
    val test: Instant      = dt.toInstant
    assertEquals(test.getEpochSecond, 24L * 60L * 60L)
    assertEquals(test.getNano, 0)
  }

  test("toInstant_19691231") {
    val dt: OffsetDateTime =
      OffsetDateTime.of(LocalDate.of(1969, 12, 31), LocalTime.of(0, 0, 0, 0), ZoneOffset.UTC)
    val test: Instant      = dt.toInstant
    assertEquals(test.getEpochSecond, -24L * 60L * 60L)
    assertEquals(test.getNano, 0)
  }

  test("toEpochSecond_19700101") {
    val dt: OffsetDateTime =
      OffsetDateTime.of(LocalDate.of(1970, 1, 1), LocalTime.of(0, 0, 0, 0), ZoneOffset.UTC)
    assertEquals(dt.toEpochSecond, 0)
  }

  test("toEpochSecond_19700101_oneNano") {
    val dt: OffsetDateTime =
      OffsetDateTime.of(LocalDate.of(1970, 1, 1), LocalTime.of(0, 0, 0, 1), ZoneOffset.UTC)
    assertEquals(dt.toEpochSecond, 0)
  }

  test("toEpochSecond_19700101_minusOneNano") {
    val dt: OffsetDateTime = OffsetDateTime.of(LocalDate.of(1969, 12, 31),
                                               LocalTime.of(23, 59, 59, 999999999),
                                               ZoneOffset.UTC
    )
    assertEquals(dt.toEpochSecond, -1)
  }

  test("toEpochSecond_19700102") {
    val dt: OffsetDateTime =
      OffsetDateTime.of(LocalDate.of(1970, 1, 2), LocalTime.of(0, 0, 0, 0), ZoneOffset.UTC)
    assertEquals(dt.toEpochSecond, 24L * 60L * 60L)
  }

  test("toEpochSecond_19691231") {
    val dt: OffsetDateTime =
      OffsetDateTime.of(LocalDate.of(1969, 12, 31), LocalTime.of(0, 0, 0, 0), ZoneOffset.UTC)
    assertEquals(dt.toEpochSecond, -24L * 60L * 60L)
  }
}
