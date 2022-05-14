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
package org.threeten.bp.format

import java.util.Locale
import org.threeten.bp.temporal.ChronoField.EPOCH_DAY
import org.threeten.bp.temporal.ChronoField.INSTANT_SECONDS
import org.threeten.bp.temporal.ChronoField.MICRO_OF_SECOND
import org.threeten.bp.temporal.ChronoField.MILLI_OF_SECOND
import org.threeten.bp.temporal.ChronoField.NANO_OF_SECOND
import org.threeten.bp.temporal.ChronoField.OFFSET_SECONDS
import org.threeten.bp.temporal.ChronoField.SECOND_OF_DAY
import org.threeten.bp.temporal.ChronoField.SECOND_OF_MINUTE
import org.scalatest.funsuite.AnyFunSuite
import org.threeten.bp.AssertionsHelper
import org.threeten.bp.DateTimeException
import org.threeten.bp.Instant
import org.threeten.bp.LocalDateTime
import org.threeten.bp.ZoneId
import org.threeten.bp.ZoneOffset
import org.threeten.bp.ZonedDateTime
import org.threeten.bp.temporal.TemporalAccessor

/** Test parsing of edge cases. */
object TestDateTimeParsing {
  private val PARIS: ZoneId                                    = ZoneId.of("Europe/Paris")
  private val OFFSET_0230: ZoneOffset                          = ZoneOffset.ofHoursMinutes(2, 30)
  private val LOCALFIELDS: DateTimeFormatter                   =
    new DateTimeFormatterBuilder.appendPattern("yyyy-MM-dd HH:mm:ss").toFormatter
  private val LOCALFIELDS_ZONEID: DateTimeFormatter            =
    new DateTimeFormatterBuilder.appendPattern("yyyy-MM-dd HH:mm:ss ").appendZoneId.toFormatter
  private val LOCALFIELDS_OFFSETID: DateTimeFormatter          =
    new DateTimeFormatterBuilder.appendPattern("yyyy-MM-dd HH:mm:ss ").appendOffsetId.toFormatter
  private val LOCALFIELDS_WITH_PARIS: DateTimeFormatter        = LOCALFIELDS.withZone(PARIS)
  private val LOCALFIELDS_WITH_0230: DateTimeFormatter         = LOCALFIELDS.withZone(OFFSET_0230)
  private val INSTANT: DateTimeFormatter                       = new DateTimeFormatterBuilder.appendInstant.toFormatter
  private val INSTANT_WITH_PARIS: DateTimeFormatter            = INSTANT.withZone(PARIS)
  private val INSTANT_WITH_0230: DateTimeFormatter             = INSTANT.withZone(OFFSET_0230)
  private val INSTANT_OFFSETID: DateTimeFormatter              =
    new DateTimeFormatterBuilder.appendInstant.appendLiteral(' ').appendOffsetId.toFormatter
  private val INSTANT_OFFSETSECONDS: DateTimeFormatter         =
    new DateTimeFormatterBuilder.appendInstant
      .appendLiteral(' ')
      .appendValue(OFFSET_SECONDS)
      .toFormatter
  private val INSTANTSECONDS: DateTimeFormatter                =
    new DateTimeFormatterBuilder.appendValue(INSTANT_SECONDS).toFormatter
  private val INSTANTSECONDS_WITH_PARIS: DateTimeFormatter     = INSTANTSECONDS.withZone(PARIS)
  private val INSTANTSECONDS_NOS: DateTimeFormatter            = new DateTimeFormatterBuilder
    .appendValue(INSTANT_SECONDS)
    .appendLiteral('.')
    .appendValue(NANO_OF_SECOND)
    .toFormatter
  private val INSTANTSECONDS_NOS_WITH_PARIS: DateTimeFormatter = INSTANTSECONDS_NOS.withZone(PARIS)
  private val INSTANTSECONDS_OFFSETSECONDS: DateTimeFormatter  = new DateTimeFormatterBuilder
    .appendValue(INSTANT_SECONDS)
    .appendLiteral(' ')
    .appendValue(OFFSET_SECONDS)
    .toFormatter
  private val INSTANT_OFFSETSECONDS_ZONE: DateTimeFormatter    =
    new DateTimeFormatterBuilder.appendInstant
      .appendLiteral(' ')
      .appendValue(OFFSET_SECONDS)
      .appendLiteral(' ')
      .appendZoneId
      .toFormatter;
}

class TestDateTimeParsing extends AnyFunSuite with GenTestPrinterParser with AssertionsHelper {
  val data_instantZones: List[(DateTimeFormatter, String, ZonedDateTime)] =
    List(
      (TestDateTimeParsing.LOCALFIELDS_ZONEID,
       "2014-06-30 01:02:03 Europe/Paris",
       ZonedDateTime.of(2014, 6, 30, 1, 2, 3, 0, TestDateTimeParsing.PARIS)
      ),
      (TestDateTimeParsing.LOCALFIELDS_ZONEID,
       "2014-06-30 01:02:03 +02:30",
       ZonedDateTime.of(2014, 6, 30, 1, 2, 3, 0, TestDateTimeParsing.OFFSET_0230)
      ),
      (TestDateTimeParsing.LOCALFIELDS_OFFSETID,
       "2014-06-30 01:02:03 +02:30",
       ZonedDateTime.of(2014, 6, 30, 1, 2, 3, 0, TestDateTimeParsing.OFFSET_0230)
      ),
      (TestDateTimeParsing.LOCALFIELDS_WITH_PARIS,
       "2014-06-30 01:02:03",
       ZonedDateTime.of(2014, 6, 30, 1, 2, 3, 0, TestDateTimeParsing.PARIS)
      ),
      (TestDateTimeParsing.LOCALFIELDS_WITH_0230,
       "2014-06-30 01:02:03",
       ZonedDateTime.of(2014, 6, 30, 1, 2, 3, 0, TestDateTimeParsing.OFFSET_0230)
      ),
      (TestDateTimeParsing.INSTANT_WITH_PARIS,
       "2014-06-30T01:02:03Z",
       ZonedDateTime
         .of(2014, 6, 30, 1, 2, 3, 0, ZoneOffset.UTC)
         .withZoneSameInstant(TestDateTimeParsing.PARIS)
      ),
      (TestDateTimeParsing.INSTANT_WITH_0230,
       "2014-06-30T01:02:03Z",
       ZonedDateTime
         .of(2014, 6, 30, 1, 2, 3, 0, ZoneOffset.UTC)
         .withZoneSameInstant(TestDateTimeParsing.OFFSET_0230)
      ),
      (TestDateTimeParsing.INSTANT_OFFSETID,
       "2014-06-30T01:02:03Z +02:30",
       ZonedDateTime
         .of(2014, 6, 30, 1, 2, 3, 0, ZoneOffset.UTC)
         .withZoneSameInstant(TestDateTimeParsing.OFFSET_0230)
      ),
      (TestDateTimeParsing.INSTANT_OFFSETSECONDS,
       "2014-06-30T01:02:03Z 9000",
       ZonedDateTime
         .of(2014, 6, 30, 1, 2, 3, 0, ZoneOffset.UTC)
         .withZoneSameInstant(TestDateTimeParsing.OFFSET_0230)
      ),
      (TestDateTimeParsing.INSTANTSECONDS_WITH_PARIS,
       "86402",
       Instant.ofEpochSecond(86402).atZone(TestDateTimeParsing.PARIS)
      ),
      (TestDateTimeParsing.INSTANTSECONDS_NOS_WITH_PARIS,
       "86402.123456789",
       Instant.ofEpochSecond(86402, 123456789).atZone(TestDateTimeParsing.PARIS)
      ),
      (TestDateTimeParsing.INSTANTSECONDS_OFFSETSECONDS,
       "86402 9000",
       Instant.ofEpochSecond(86402).atZone(TestDateTimeParsing.OFFSET_0230)
      ),
      (TestDateTimeParsing.INSTANT_OFFSETSECONDS_ZONE,
       "2016-10-30T00:30:00Z 7200 Europe/Paris",
       ZonedDateTime.ofStrict(LocalDateTime.of(2016, 10, 30, 2, 30),
                              ZoneOffset.ofHours(2),
                              TestDateTimeParsing.PARIS
       )
      ),
      (TestDateTimeParsing.INSTANT_OFFSETSECONDS_ZONE,
       "2016-10-30T01:30:00Z 3600 Europe/Paris",
       ZonedDateTime.ofStrict(LocalDateTime.of(2016, 10, 30, 2, 30),
                              ZoneOffset.ofHours(1),
                              TestDateTimeParsing.PARIS
       )
      )
    )

  test("test_parse_instantZones_ZDT") {
    data_instantZones.foreach {
      case (formatter, text, expected) =>
        val actual: TemporalAccessor = formatter.parse(text)
        assertEquals(ZonedDateTime.from(actual), expected)
      case _                           =>
        fail()
    }
  }

  test("test_parse_instantZones_LDT") {
    data_instantZones.foreach {
      case (formatter, text, expected) =>
        val actual: TemporalAccessor = formatter.parse(text)
        assertEquals(LocalDateTime.from(actual), expected.toLocalDateTime)
      case _                           =>
        fail()
    }
  }

  test("test_parse_instantZones_Instant") {
    data_instantZones.foreach {
      case (formatter, text, expected) =>
        val actual: TemporalAccessor = formatter.parse(text)
        assertEquals(Instant.from(actual), expected.toInstant)
      case _                           =>
        fail()
    }
  }

  test("test_parse_instantZones_supported") {
    data_instantZones.foreach {
      case (formatter, text, _) =>
        val actual: TemporalAccessor = formatter.parse(text)
        assertEquals(actual.isSupported(INSTANT_SECONDS), true)
        assertEquals(actual.isSupported(EPOCH_DAY), true)
        assertEquals(actual.isSupported(SECOND_OF_DAY), true)
        assertEquals(actual.isSupported(NANO_OF_SECOND), true)
        assertEquals(actual.isSupported(MICRO_OF_SECOND), true)
        assertEquals(actual.isSupported(MILLI_OF_SECOND), true)
      case _                    =>
        fail()
    }
  }

  val data_instantNoZone: List[(DateTimeFormatter, String, Instant)] =
    List(
      (TestDateTimeParsing.INSTANT,
       "2014-06-30T01:02:03Z",
       ZonedDateTime.of(2014, 6, 30, 1, 2, 3, 0, ZoneOffset.UTC).toInstant
      ),
      (TestDateTimeParsing.INSTANTSECONDS, "86402", Instant.ofEpochSecond(86402)),
      (TestDateTimeParsing.INSTANTSECONDS_NOS,
       "86402.123456789",
       Instant.ofEpochSecond(86402, 123456789)
      )
    )

  test("test_parse_instantNoZone_ZDT") {
    data_instantNoZone.foreach {
      case (formatter, text, _) =>
        assertThrows[DateTimeException] {
          val actual: TemporalAccessor = formatter.parse(text)
          ZonedDateTime.from(actual)
        }
      case _                    =>
        fail()
    }
  }

  test("test_parse_instantNoZone_LDT") {
    data_instantNoZone.foreach {
      case (formatter, text, _) =>
        assertThrows[DateTimeException] {
          val actual: TemporalAccessor = formatter.parse(text)
          LocalDateTime.from(actual)
        }
      case _                    =>
        fail()
    }
  }

  test("test_parse_instantNoZone_Instant") {
    data_instantNoZone.foreach {
      case (formatter, text, expected) =>
        val actual: TemporalAccessor = formatter.parse(text)
        assertEquals(Instant.from(actual), expected)
      case _                           =>
        fail()
    }
  }

  test("test_parse_instantNoZone_supported") {
    data_instantNoZone.foreach {
      case (formatter, text, _) =>
        val actual: TemporalAccessor = formatter.parse(text)
        assertEquals(actual.isSupported(INSTANT_SECONDS), true)
        assertEquals(actual.isSupported(EPOCH_DAY), false)
        assertEquals(actual.isSupported(SECOND_OF_DAY), false)
        assertEquals(actual.isSupported(NANO_OF_SECOND), true)
        assertEquals(actual.isSupported(MICRO_OF_SECOND), true)
        assertEquals(actual.isSupported(MILLI_OF_SECOND), true)
      case _                    =>
        fail()
    }
  }

  test("test_parse_fromField_InstantSeconds") {
    val fmt: DateTimeFormatter =
      new DateTimeFormatterBuilder.appendValue(INSTANT_SECONDS).toFormatter
    val acc: TemporalAccessor  = fmt.parse("86402")
    val expected: Instant      = Instant.ofEpochSecond(86402)
    assertEquals(acc.isSupported(INSTANT_SECONDS), true)
    assertEquals(acc.isSupported(NANO_OF_SECOND), true)
    assertEquals(acc.isSupported(MICRO_OF_SECOND), true)
    assertEquals(acc.isSupported(MILLI_OF_SECOND), true)
    assertEquals(acc.getLong(INSTANT_SECONDS), 86402L)
    assertEquals(acc.getLong(NANO_OF_SECOND), 0L)
    assertEquals(acc.getLong(MICRO_OF_SECOND), 0L)
    assertEquals(acc.getLong(MILLI_OF_SECOND), 0L)
    assertEquals(Instant.from(acc), expected)
  }

  test("test_parse_fromField_InstantSeconds_NanoOfSecond") {
    val fmt: DateTimeFormatter = new DateTimeFormatterBuilder
      .appendValue(INSTANT_SECONDS)
      .appendLiteral('.')
      .appendValue(NANO_OF_SECOND)
      .toFormatter
    val acc: TemporalAccessor  = fmt.parse("86402.123456789")
    val expected: Instant      = Instant.ofEpochSecond(86402, 123456789)
    assertEquals(acc.isSupported(INSTANT_SECONDS), true)
    assertEquals(acc.isSupported(NANO_OF_SECOND), true)
    assertEquals(acc.isSupported(MICRO_OF_SECOND), true)
    assertEquals(acc.isSupported(MILLI_OF_SECOND), true)
    assertEquals(acc.getLong(INSTANT_SECONDS), 86402L)
    assertEquals(acc.getLong(NANO_OF_SECOND), 123456789L)
    assertEquals(acc.getLong(MICRO_OF_SECOND), 123456L)
    assertEquals(acc.getLong(MILLI_OF_SECOND), 123L)
    assertEquals(Instant.from(acc), expected)
  }

  test("test_parse_fromField_SecondOfDay") {
    val fmt: DateTimeFormatter =
      new DateTimeFormatterBuilder.appendValue(SECOND_OF_DAY).toFormatter
    val acc: TemporalAccessor  = fmt.parse("864")
    assertEquals(acc.isSupported(SECOND_OF_DAY), true)
    assertEquals(acc.isSupported(NANO_OF_SECOND), true)
    assertEquals(acc.isSupported(MICRO_OF_SECOND), true)
    assertEquals(acc.isSupported(MILLI_OF_SECOND), true)
    assertEquals(acc.getLong(SECOND_OF_DAY), 864L)
    assertEquals(acc.getLong(NANO_OF_SECOND), 0L)
    assertEquals(acc.getLong(MICRO_OF_SECOND), 0L)
    assertEquals(acc.getLong(MILLI_OF_SECOND), 0L)
  }

  test("test_parse_fromField_SecondOfDay_NanoOfSecond") {
    val fmt: DateTimeFormatter = new DateTimeFormatterBuilder
      .appendValue(SECOND_OF_DAY)
      .appendLiteral('.')
      .appendValue(NANO_OF_SECOND)
      .toFormatter
    val acc: TemporalAccessor  = fmt.parse("864.123456789")
    assertEquals(acc.isSupported(SECOND_OF_DAY), true)
    assertEquals(acc.isSupported(NANO_OF_SECOND), true)
    assertEquals(acc.isSupported(MICRO_OF_SECOND), true)
    assertEquals(acc.isSupported(MILLI_OF_SECOND), true)
    assertEquals(acc.getLong(SECOND_OF_DAY), 864L)
    assertEquals(acc.getLong(NANO_OF_SECOND), 123456789L)
    assertEquals(acc.getLong(MICRO_OF_SECOND), 123456L)
    assertEquals(acc.getLong(MILLI_OF_SECOND), 123L)
  }

  test("test_parse_fromField_SecondOfMinute") {
    val fmt: DateTimeFormatter =
      new DateTimeFormatterBuilder.appendValue(SECOND_OF_MINUTE).toFormatter
    val acc: TemporalAccessor  = fmt.parse("32")
    assertEquals(acc.isSupported(SECOND_OF_MINUTE), true)
    assertEquals(acc.isSupported(NANO_OF_SECOND), true)
    assertEquals(acc.isSupported(MICRO_OF_SECOND), true)
    assertEquals(acc.isSupported(MILLI_OF_SECOND), true)
    assertEquals(acc.getLong(SECOND_OF_MINUTE), 32L)
    assertEquals(acc.getLong(NANO_OF_SECOND), 0L)
    assertEquals(acc.getLong(MICRO_OF_SECOND), 0L)
    assertEquals(acc.getLong(MILLI_OF_SECOND), 0L)
  }

  test("test_parse_fromField_SecondOfMinute_NanoOfSecond") {
    val fmt: DateTimeFormatter = new DateTimeFormatterBuilder
      .appendValue(SECOND_OF_MINUTE)
      .appendLiteral('.')
      .appendValue(NANO_OF_SECOND)
      .toFormatter
    val acc: TemporalAccessor  = fmt.parse("32.123456789")
    assertEquals(acc.isSupported(SECOND_OF_MINUTE), true)
    assertEquals(acc.isSupported(NANO_OF_SECOND), true)
    assertEquals(acc.isSupported(MICRO_OF_SECOND), true)
    assertEquals(acc.isSupported(MILLI_OF_SECOND), true)
    assertEquals(acc.getLong(SECOND_OF_MINUTE), 32L)
    assertEquals(acc.getLong(NANO_OF_SECOND), 123456789L)
    assertEquals(acc.getLong(MICRO_OF_SECOND), 123456L)
    assertEquals(acc.getLong(MILLI_OF_SECOND), 123L)
  }

  // Needs zone strings not available on scala-java-locales
  ignore("test_parse_tzdbGmtZone") {
    val dateString = "2015,7,21,0,0,0,GMT+02:00"
    val formatter  = DateTimeFormatter.ofPattern("yyyy,M,d,H,m,s,z", Locale.US)
    val parsed     = ZonedDateTime.parse(dateString, formatter)
    assertEquals(parsed, ZonedDateTime.of(2015, 7, 21, 0, 0, 0, 0, ZoneId.of("Etc/GMT-2")))
  }
}
