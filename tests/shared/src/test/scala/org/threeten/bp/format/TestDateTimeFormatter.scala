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

import org.scalatest.BeforeAndAfterEach
import org.scalatest.funsuite.AnyFunSuite
import org.threeten.bp.AssertionsHelper
import org.threeten.bp.temporal.ChronoField.DAY_OF_MONTH
import java.lang.StringBuilder
import java.io.IOException
import java.text.Format
import java.text.ParseException
import java.text.ParsePosition
import java.util.Locale
import org.threeten.bp.DateTimeException
import org.threeten.bp.LocalDate
import org.threeten.bp.LocalTime
import org.threeten.bp.YearMonth
import org.threeten.bp.ZoneId
import org.threeten.bp.ZonedDateTime
import org.threeten.bp.temporal.TemporalAccessor
import org.threeten.bp.temporal.TemporalQuery

/** Test DateTimeFormatter. */
object TestDateTimeFormatter {
  private val BASIC_FORMATTER: DateTimeFormatter = DateTimeFormatter.ofPattern("'ONE'd")
  private val DATE_FORMATTER: DateTimeFormatter  = DateTimeFormatter.ofPattern("'ONE'uuuu MM dd")
}

class TestDateTimeFormatter
    extends AnyFunSuite
    with GenTestPrinterParser
    with AssertionsHelper
    with BeforeAndAfterEach {
  private var fmt: DateTimeFormatter = null

  def toTemporalQuery[T](f: TemporalAccessor => T): TemporalQuery[T] =
    new TemporalQuery[T] {
      override def queryFrom(temporal: TemporalAccessor): T = f(temporal)
    }

  override def beforeEach(): Unit =
    fmt = new DateTimeFormatterBuilder
      .appendLiteral("ONE")
      .appendValue(DAY_OF_MONTH, 1, 2, SignStyle.NOT_NEGATIVE)
      .toFormatter

  test("test_withLocale") {
    val base: DateTimeFormatter =
      fmt.withLocale(Locale.ENGLISH).withDecimalStyle(DecimalStyle.STANDARD)
    val test: DateTimeFormatter = base.withLocale(Locale.GERMAN)
    assertEquals(test.getLocale, Locale.GERMAN)
  }

  test("test_withLocale_null") {
    assertThrows[NullPointerException] {
      val base: DateTimeFormatter =
        fmt.withLocale(Locale.ENGLISH).withDecimalStyle(DecimalStyle.STANDARD)
      base.withLocale(null.asInstanceOf[Locale])
    }
  }

  test("test_print_Calendrical") {
    val test: DateTimeFormatter =
      fmt.withLocale(Locale.ENGLISH).withDecimalStyle(DecimalStyle.STANDARD)
    val result: String          = test.format(LocalDate.of(2008, 6, 30))
    assertEquals(result, "ONE30")
  }

  test("test_print_Calendrical_noSuchField") {
    assertThrows[DateTimeException] {
      val test: DateTimeFormatter =
        fmt.withLocale(Locale.ENGLISH).withDecimalStyle(DecimalStyle.STANDARD)
      test.format(LocalTime.of(11, 30))
    }
  }

  test("test_print_Calendrical_null") {
    assertThrows[NullPointerException] {
      val test: DateTimeFormatter =
        fmt.withLocale(Locale.ENGLISH).withDecimalStyle(DecimalStyle.STANDARD)
      test.format(null.asInstanceOf[TemporalAccessor])
    }
  }

  test("test_print_CalendricalAppendable") {
    val test: DateTimeFormatter =
      fmt.withLocale(Locale.ENGLISH).withDecimalStyle(DecimalStyle.STANDARD)
    val buf: StringBuilder      = new StringBuilder
    test.formatTo(LocalDate.of(2008, 6, 30), buf)
    assertEquals(buf.toString, "ONE30")
  }

  test("test_print_CalendricalAppendable_noSuchField") {
    assertThrows[DateTimeException] {
      val test: DateTimeFormatter =
        fmt.withLocale(Locale.ENGLISH).withDecimalStyle(DecimalStyle.STANDARD)
      val buf: StringBuilder      = new StringBuilder
      test.formatTo(LocalTime.of(11, 30), buf)
    }
  }

  test("test_print_CalendricalAppendable_nullCalendrical") {
    assertThrows[NullPointerException] {
      val test: DateTimeFormatter =
        fmt.withLocale(Locale.ENGLISH).withDecimalStyle(DecimalStyle.STANDARD)
      val buf: StringBuilder      = new StringBuilder
      test.formatTo(null.asInstanceOf[TemporalAccessor], buf)
    }
  }

  test("test_print_CalendricalAppendable_nullAppendable") {
    assertThrows[NullPointerException] {
      val test: DateTimeFormatter =
        fmt.withLocale(Locale.ENGLISH).withDecimalStyle(DecimalStyle.STANDARD)
      test.formatTo(LocalDate.of(2008, 6, 30), null.asInstanceOf[Appendable])
    }
  }

  test("test_print_CalendricalAppendable_ioError") {
    assertThrows[IOException] {
      val test: DateTimeFormatter =
        fmt.withLocale(Locale.ENGLISH).withDecimalStyle(DecimalStyle.STANDARD)
      try test.formatTo(LocalDate.of(2008, 6, 30), new MockIOExceptionAppendable)
      catch {
        case ex: DateTimeException =>
          assertEquals(ex.getCause.isInstanceOf[IOException], true)
          throw ex.getCause
      }
    }
  }

  test("test_parse_Class_String") {
    val result: LocalDate =
      TestDateTimeFormatter.DATE_FORMATTER.parse("ONE2012 07 27", toTemporalQuery(LocalDate.from))
    assertEquals(result, LocalDate.of(2012, 7, 27))
  }

  test("test_parse_Class_CharSequence") {
    val result: LocalDate = TestDateTimeFormatter.DATE_FORMATTER
      .parse(new StringBuilder("ONE2012 07 27"), toTemporalQuery(LocalDate.from))
    assertEquals(result, LocalDate.of(2012, 7, 27))
  }

  test("test_parse_Class_String_parseError") {
    assertThrows[DateTimeParseException] {
      try
        TestDateTimeFormatter.DATE_FORMATTER.parse("ONE2012 07 XX", toTemporalQuery(LocalDate.from))
      catch {
        case ex: DateTimeParseException =>
          assertEquals(ex.getMessage.contains("could not be parsed"), true)
          assertEquals(ex.getMessage.contains("ONE2012 07 XX"), true)
          assertEquals(ex.getParsedString, "ONE2012 07 XX")
          assertEquals(ex.getErrorIndex, 11)
          throw ex
      }
    }
  }

  test("test_parse_Class_String_parseErrorLongText") {
    assertThrows[DateTimeParseException] {
      try
        TestDateTimeFormatter.DATE_FORMATTER.parse(
          "ONEXXX67890123456789012345678901234567890123456789012345678901234567890123456789",
          toTemporalQuery(LocalDate.from)
        )
      catch {
        case ex: DateTimeParseException =>
          assertEquals(ex.getMessage.contains("could not be parsed"), true)
          assertEquals(ex.getMessage.contains(
                         "ONEXXX6789012345678901234567890123456789012345678901234567890123..."
                       ),
                       true
          )
          assertEquals(
            ex.getParsedString,
            "ONEXXX67890123456789012345678901234567890123456789012345678901234567890123456789"
          )
          assertEquals(ex.getErrorIndex, 3)
          throw ex
      }
    }
  }

  test("test_parse_Class_String_parseIncomplete") {
    assertThrows[DateTimeParseException] {
      try
        TestDateTimeFormatter.DATE_FORMATTER.parse("ONE2012 07 27SomethingElse",
                                                   toTemporalQuery(LocalDate.from)
        )
      catch {
        case ex: DateTimeParseException =>
          assertEquals(ex.getMessage.contains("could not be parsed"), true)
          assertEquals(ex.getMessage.contains("ONE2012 07 27SomethingElse"), true)
          assertEquals(ex.getParsedString, "ONE2012 07 27SomethingElse")
          assertEquals(ex.getErrorIndex, 13)
          throw ex
      }
    }
  }

  test("test_parse_Class_String_nullText") {
    assertThrows[NullPointerException] {
      TestDateTimeFormatter.DATE_FORMATTER.parse(null.asInstanceOf[String],
                                                 toTemporalQuery(LocalDate.from)
      )
    }
  }

  test("test_parse_Class_String_nullRule") {
    assertThrows[NullPointerException] {
      val test: DateTimeFormatter =
        fmt.withLocale(Locale.ENGLISH).withDecimalStyle(DecimalStyle.STANDARD)
      test.parse("30", null.asInstanceOf[TemporalQuery[Any]])
    }
  }

  test("test_parseBest_firstOption") {
    val test: DateTimeFormatter  = DateTimeFormatter.ofPattern("uuuu-MM[-dd]")
    val result: TemporalAccessor =
      test.parseBest("2011-06-30", toTemporalQuery(LocalDate.from), toTemporalQuery(YearMonth.from))
    assertEquals(result, LocalDate.of(2011, 6, 30))
  }

  test("test_parseBest_secondOption") {
    val test: DateTimeFormatter  = DateTimeFormatter.ofPattern("uuuu-MM[-dd]")
    val result: TemporalAccessor =
      test.parseBest("2011-06", toTemporalQuery(LocalDate.from), toTemporalQuery(YearMonth.from))
    assertEquals(result, YearMonth.of(2011, 6))
  }

  test("test_parseBest_String_parseError") {
    assertThrows[DateTimeParseException] {
      val test: DateTimeFormatter = DateTimeFormatter.ofPattern("uuuu-MM[-dd]")
      try
        test.parseBest("2011-XX-30",
                       toTemporalQuery(LocalDate.from),
                       toTemporalQuery(YearMonth.from)
        )
      catch {
        case ex: DateTimeParseException =>
          assertEquals(ex.getMessage.contains("could not be parsed"), true)
          assertEquals(ex.getMessage.contains("XX"), true)
          assertEquals(ex.getParsedString, "2011-XX-30")
          assertEquals(ex.getErrorIndex, 5)
          throw ex
      }
    }
  }

  test("test_parseBest_String_parseErrorLongText") {
    assertThrows[DateTimeParseException] {
      val test: DateTimeFormatter =
        fmt.withLocale(Locale.ENGLISH).withDecimalStyle(DecimalStyle.STANDARD)
      try
        test.parseBest(
          "ONEXXX67890123456789012345678901234567890123456789012345678901234567890123456789",
          toTemporalQuery(LocalDate.from),
          toTemporalQuery(YearMonth.from)
        )
      catch {
        case ex: DateTimeParseException =>
          assertEquals(ex.getMessage.contains("could not be parsed"), true)
          assertEquals(ex.getMessage.contains(
                         "ONEXXX6789012345678901234567890123456789012345678901234567890123..."
                       ),
                       true
          )
          assertEquals(
            ex.getParsedString,
            "ONEXXX67890123456789012345678901234567890123456789012345678901234567890123456789"
          )
          assertEquals(ex.getErrorIndex, 3)
          throw ex
      }
    }
  }

  test("test_parseBest_String_parseIncomplete") {
    assertThrows[DateTimeParseException] {
      val test: DateTimeFormatter =
        fmt.withLocale(Locale.ENGLISH).withDecimalStyle(DecimalStyle.STANDARD)
      try
        test.parseBest("ONE30SomethingElse",
                       toTemporalQuery(YearMonth.from),
                       toTemporalQuery(LocalDate.from)
        )
      catch {
        case ex: DateTimeParseException =>
          assertEquals(ex.getMessage.contains("could not be parsed"), true)
          assertEquals(ex.getMessage.contains("ONE30SomethingElse"), true)
          assertEquals(ex.getParsedString, "ONE30SomethingElse")
          assertEquals(ex.getErrorIndex, 5)
          throw ex
      }
    }
  }

  test("test_parseBest_String_nullText") {
    assertThrows[NullPointerException] {
      val test: DateTimeFormatter =
        fmt.withLocale(Locale.ENGLISH).withDecimalStyle(DecimalStyle.STANDARD)
      test.parseBest(null.asInstanceOf[String],
                     toTemporalQuery(YearMonth.from),
                     toTemporalQuery(LocalDate.from)
      )
    }
  }

  test("test_parseBest_String_nullRules") {
    assertThrows[NullPointerException] {
      val test: DateTimeFormatter =
        fmt.withLocale(Locale.ENGLISH).withDecimalStyle(DecimalStyle.STANDARD)
      test.parseBest("30", null.asInstanceOf[Array[TemporalQuery[AnyRef]]]: _*)
    }
  }

  test("test_parseBest_String_zeroRules") {
    assertThrows[IllegalArgumentException] {
      val test: DateTimeFormatter =
        fmt.withLocale(Locale.ENGLISH).withDecimalStyle(DecimalStyle.STANDARD)
      test.parseBest("30", new Array[TemporalQuery[Any]](0): _*)
    }
  }

  test("test_parseBest_String_oneRule") {
    assertThrows[IllegalArgumentException] {
      val test: DateTimeFormatter =
        fmt.withLocale(Locale.ENGLISH).withDecimalStyle(DecimalStyle.STANDARD)
      test.parseBest("30", toTemporalQuery(LocalDate.from))
    }
  }

  test("test_parseToBuilder_StringParsePosition") {
    val test: DateTimeFormatter  =
      fmt.withLocale(Locale.ENGLISH).withDecimalStyle(DecimalStyle.STANDARD)
    val pos: ParsePosition       = new ParsePosition(0)
    val result: TemporalAccessor = test.parseUnresolved("ONE30XXX", pos)
    assertEquals(pos.getIndex, 5)
    assertEquals(pos.getErrorIndex, -1)
    assertEquals(result.getLong(DAY_OF_MONTH), 30L)
  }

  test("test_parseToBuilder_StringParsePosition_parseError") {
    val test: DateTimeFormatter  =
      fmt.withLocale(Locale.ENGLISH).withDecimalStyle(DecimalStyle.STANDARD)
    val pos: ParsePosition       = new ParsePosition(0)
    val result: TemporalAccessor = test.parseUnresolved("ONEXXX", pos)
    assertEquals(pos.getIndex, 0)
    assertEquals(pos.getErrorIndex, 3)
    assertEquals(result, null)
  }

  test("test_parseToBuilder_StringParsePosition_nullString") {
    assertThrows[NullPointerException] {
      val test: DateTimeFormatter =
        fmt.withLocale(Locale.ENGLISH).withDecimalStyle(DecimalStyle.STANDARD)
      val pos: ParsePosition      = new ParsePosition(0)
      test.parseUnresolved(null.asInstanceOf[String], pos)
    }
  }

  test("test_parseToBuilder_StringParsePosition_nullParsePosition") {
    assertThrows[NullPointerException] {
      val test: DateTimeFormatter =
        fmt.withLocale(Locale.ENGLISH).withDecimalStyle(DecimalStyle.STANDARD)
      test.parseUnresolved("ONE30", null.asInstanceOf[ParsePosition])
    }
  }

  test("test_parseToBuilder_StringParsePosition_invalidPosition") {
    assertThrows[IndexOutOfBoundsException] {
      val test: DateTimeFormatter =
        fmt.withLocale(Locale.ENGLISH).withDecimalStyle(DecimalStyle.STANDARD)
      val pos: ParsePosition      = new ParsePosition(6)
      test.parseUnresolved("ONE30", pos)
    }
  }

  test("test_toFormat_format") {
    val test: DateTimeFormatter =
      fmt.withLocale(Locale.ENGLISH).withDecimalStyle(DecimalStyle.STANDARD)
    val format: Format          = test.toFormat
    val result: String          = format.format(LocalDate.of(2008, 6, 30))
    assertEquals(result, "ONE30")
  }

  test("test_toFormat_format_null") {
    assertThrows[NullPointerException] {
      val test: DateTimeFormatter =
        fmt.withLocale(Locale.ENGLISH).withDecimalStyle(DecimalStyle.STANDARD)
      val format: Format          = test.toFormat
      format.format(null)
    }
  }

  test("test_toFormat_format_notCalendrical") {
    assertThrows[IllegalArgumentException] {
      val test: DateTimeFormatter =
        fmt.withLocale(Locale.ENGLISH).withDecimalStyle(DecimalStyle.STANDARD)
      val format: Format          = test.toFormat
      format.format("Not a Calendrical")
    }
  }

  test("test_toFormat_parseObject_String") {
    val test: DateTimeFormatter =
      fmt.withLocale(Locale.ENGLISH).withDecimalStyle(DecimalStyle.STANDARD)
    val format: Format          = test.toFormat
    val result: DateTimeBuilder = format.parseObject("ONE30").asInstanceOf[DateTimeBuilder]
    assertEquals(result.getLong(DAY_OF_MONTH), 30L)
  }

  test("test_toFormat_parseObject_String_parseError") {
    assertThrows[ParseException] {
      val test: DateTimeFormatter =
        fmt.withLocale(Locale.ENGLISH).withDecimalStyle(DecimalStyle.STANDARD)
      val format: Format          = test.toFormat
      try format.parseObject("ONEXXX")
      catch {
        case ex: ParseException =>
          assertEquals(ex.getMessage.contains("ONEXXX"), true)
          assertEquals(ex.getErrorOffset, 3)
          throw ex
      }
    }
  }

  test("test_toFormat_parseObject_String_parseErrorLongText") {
    assertThrows[ParseException] {
      val test: DateTimeFormatter =
        fmt.withLocale(Locale.ENGLISH).withDecimalStyle(DecimalStyle.STANDARD)
      val format: Format          = test.toFormat
      try
        format.parseObject(
          "ONEXXX67890123456789012345678901234567890123456789012345678901234567890123456789"
        )
      catch {
        case ex: DateTimeParseException =>
          assertEquals(ex.getMessage.contains(
                         "ONEXXX6789012345678901234567890123456789012345678901234567890123..."
                       ),
                       true
          )
          assertEquals(
            ex.getParsedString,
            "ONEXXX67890123456789012345678901234567890123456789012345678901234567890123456789"
          )
          assertEquals(ex.getErrorIndex, 3)
          throw ex
      }
    }
  }

  test("test_toFormat_parseObject_String_null") {
    assertThrows[NullPointerException] {
      val test: DateTimeFormatter =
        fmt.withLocale(Locale.ENGLISH).withDecimalStyle(DecimalStyle.STANDARD)
      val format: Format          = test.toFormat
      format.parseObject(null.asInstanceOf[String])
    }
  }

  test("test_toFormat_parseObject_StringParsePosition") {
    val test: DateTimeFormatter =
      fmt.withLocale(Locale.ENGLISH).withDecimalStyle(DecimalStyle.STANDARD)
    val format: Format          = test.toFormat
    val pos: ParsePosition      = new ParsePosition(0)
    val result: DateTimeBuilder = format.parseObject("ONE30XXX", pos).asInstanceOf[DateTimeBuilder]
    assertEquals(pos.getIndex, 5)
    assertEquals(pos.getErrorIndex, -1)
    assertEquals(result.getLong(DAY_OF_MONTH), 30L)
  }

  test("test_toFormat_parseObject_StringParsePosition_parseError") {
    val test: DateTimeFormatter  =
      fmt.withLocale(Locale.ENGLISH).withDecimalStyle(DecimalStyle.STANDARD)
    val format: Format           = test.toFormat
    val pos: ParsePosition       = new ParsePosition(0)
    val result: TemporalAccessor = format.parseObject("ONEXXX", pos).asInstanceOf[TemporalAccessor]
    assertEquals(pos.getIndex, 0)
    assertEquals(pos.getErrorIndex, 3)
    assertEquals(result, null)
  }

  test("test_toFormat_parseObject_StringParsePosition_nullString") {
    assertThrows[NullPointerException] {
      val test: DateTimeFormatter =
        fmt.withLocale(Locale.ENGLISH).withDecimalStyle(DecimalStyle.STANDARD)
      val format: Format          = test.toFormat
      val pos: ParsePosition      = new ParsePosition(0)
      format.parseObject(null.asInstanceOf[String], pos)
    }
  }

  test("test_toFormat_parseObject_StringParsePosition_nullParsePosition") {
    assertThrows[NullPointerException] {
      val test: DateTimeFormatter =
        fmt.withLocale(Locale.ENGLISH).withDecimalStyle(DecimalStyle.STANDARD)
      val format: Format          = test.toFormat
      format.parseObject("ONE30", null.asInstanceOf[ParsePosition])
    }
  }

  test("test_toFormat_parseObject_StringParsePosition_invalidPosition_tooBig") {
    val dtf: DateTimeFormatter =
      fmt.withLocale(Locale.ENGLISH).withDecimalStyle(DecimalStyle.STANDARD)
    val pos: ParsePosition     = new ParsePosition(6)
    val test: Format           = dtf.toFormat
    assertNull(test.parseObject("ONE30", pos))
    assertTrue(pos.getErrorIndex >= 0)
  }

  test("test_toFormat_parseObject_StringParsePosition_invalidPosition_tooSmall") {
    val dtf: DateTimeFormatter =
      fmt.withLocale(Locale.ENGLISH).withDecimalStyle(DecimalStyle.STANDARD)
    val pos: ParsePosition     = new ParsePosition(-1)
    val test: Format           = dtf.toFormat
    assertNull(test.parseObject("ONE30", pos))
    assertTrue(pos.getErrorIndex >= 0)
  }

  test("test_toFormat_Class_format") {
    val format: Format = TestDateTimeFormatter.BASIC_FORMATTER.toFormat
    val result: String = format.format(LocalDate.of(2008, 6, 30))
    assertEquals(result, "ONE30")
  }

  test("test_toFormat_Class_parseObject_String") {
    val format: Format    =
      TestDateTimeFormatter.DATE_FORMATTER.toFormat(toTemporalQuery(LocalDate.from))
    val result: LocalDate = format.parseObject("ONE2012 07 27").asInstanceOf[LocalDate]
    assertEquals(result, LocalDate.of(2012, 7, 27))
  }

  test("test_toFormat_parseObject_StringParsePosition_dateTimeError") {
    assertThrows[ParseException] {
      val format: Format =
        TestDateTimeFormatter.DATE_FORMATTER.toFormat(toTemporalQuery(LocalDate.from))
      format.parseObject("ONE2012 07 32")
    }
  }

  test("test_toFormat_Class") {
    assertThrows[NullPointerException] {
      TestDateTimeFormatter.BASIC_FORMATTER.toFormat(null)
    }
  }

  test("test_parse_allZones") {
    import scala.collection.JavaConverters._
    for (zoneStr <- ZoneId.getAvailableZoneIds.asScala) {
      val zone: ZoneId        = ZoneId.of(zoneStr)
      val base: ZonedDateTime = ZonedDateTime.of(2014, 12, 31, 12, 0, 0, 0, zone)
      val test: ZonedDateTime = ZonedDateTime.parse(base.toString)
      assertEquals(test, base)
    }
  }
}
