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

import org.scalatest.funsuite.AnyFunSuite
import org.threeten.bp.temporal.ChronoField.NANO_OF_SECOND
import org.threeten.bp.temporal.ChronoField.SECOND_OF_MINUTE
import org.threeten.bp.{ AssertionsHelper, DateTimeException, LocalTime }
import org.threeten.bp.temporal.MockFieldValue
import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.format.internal.{ TTBPDateTimeFormatterBuilder, TTBPDateTimeParseContext }

/** Test FractionPrinterParser. */
class TestFractionPrinterParser
    extends AnyFunSuite
    with GenTestPrinterParser
    with AssertionsHelper {
  test("test_print_emptyCalendrical") {
    assertThrows[DateTimeException] {
      val pp: TTBPDateTimeFormatterBuilder.FractionPrinterParser =
        new TTBPDateTimeFormatterBuilder.FractionPrinterParser(NANO_OF_SECOND, 0, 9, true)
      pp.print(printEmptyContext, buf)
    }
  }

  test("test_print_append") {
    printContext.setDateTime(LocalTime.of(12, 30, 40, 3))
    val pp: TTBPDateTimeFormatterBuilder.FractionPrinterParser =
      new TTBPDateTimeFormatterBuilder.FractionPrinterParser(NANO_OF_SECOND, 0, 9, true)
    buf.append("EXISTING")
    pp.print(printContext, buf)
    assertEquals(buf.toString, "EXISTING.000000003")
  }

  val provider_nanos: List[List[Any]] =
    List(
      List(0, 9, 0, ""),
      List(0, 9, 2, ".000000002"),
      List(0, 9, 20, ".00000002"),
      List(0, 9, 200, ".0000002"),
      List(0, 9, 2000, ".000002"),
      List(0, 9, 20000, ".00002"),
      List(0, 9, 200000, ".0002"),
      List(0, 9, 2000000, ".002"),
      List(0, 9, 20000000, ".02"),
      List(0, 9, 200000000, ".2"),
      List(0, 9, 1, ".000000001"),
      List(0, 9, 12, ".000000012"),
      List(0, 9, 123, ".000000123"),
      List(0, 9, 1234, ".000001234"),
      List(0, 9, 12345, ".000012345"),
      List(0, 9, 123456, ".000123456"),
      List(0, 9, 1234567, ".001234567"),
      List(0, 9, 12345678, ".012345678"),
      List(0, 9, 123456789, ".123456789"),
      List(1, 9, 0, ".0"),
      List(1, 9, 2, ".000000002"),
      List(1, 9, 20, ".00000002"),
      List(1, 9, 200, ".0000002"),
      List(1, 9, 2000, ".000002"),
      List(1, 9, 20000, ".00002"),
      List(1, 9, 200000, ".0002"),
      List(1, 9, 2000000, ".002"),
      List(1, 9, 20000000, ".02"),
      List(1, 9, 200000000, ".2"),
      List(2, 3, 0, ".00"),
      List(2, 3, 2, ".000"),
      List(2, 3, 20, ".000"),
      List(2, 3, 200, ".000"),
      List(2, 3, 2000, ".000"),
      List(2, 3, 20000, ".000"),
      List(2, 3, 200000, ".000"),
      List(2, 3, 2000000, ".002"),
      List(2, 3, 20000000, ".02"),
      List(2, 3, 200000000, ".20"),
      List(2, 3, 1, ".000"),
      List(2, 3, 12, ".000"),
      List(2, 3, 123, ".000"),
      List(2, 3, 1234, ".000"),
      List(2, 3, 12345, ".000"),
      List(2, 3, 123456, ".000"),
      List(2, 3, 1234567, ".001"),
      List(2, 3, 12345678, ".012"),
      List(2, 3, 123456789, ".123"),
      List(6, 6, 0, ".000000"),
      List(6, 6, 2, ".000000"),
      List(6, 6, 20, ".000000"),
      List(6, 6, 200, ".000000"),
      List(6, 6, 2000, ".000002"),
      List(6, 6, 20000, ".000020"),
      List(6, 6, 200000, ".000200"),
      List(6, 6, 2000000, ".002000"),
      List(6, 6, 20000000, ".020000"),
      List(6, 6, 200000000, ".200000"),
      List(6, 6, 1, ".000000"),
      List(6, 6, 12, ".000000"),
      List(6, 6, 123, ".000000"),
      List(6, 6, 1234, ".000001"),
      List(6, 6, 12345, ".000012"),
      List(6, 6, 123456, ".000123"),
      List(6, 6, 1234567, ".001234"),
      List(6, 6, 12345678, ".012345"),
      List(6, 6, 123456789, ".123456")
    )

  test("test_print_nanos") {
    provider_nanos.foreach {
      case (minWidth: Int) :: (maxWidth: Int) :: (value: Int) :: (result: String) :: Nil =>
        super.beforeEach()
        printContext.setDateTime(new MockFieldValue(NANO_OF_SECOND, value))
        val pp: TTBPDateTimeFormatterBuilder.FractionPrinterParser =
          new TTBPDateTimeFormatterBuilder.FractionPrinterParser(NANO_OF_SECOND,
                                                                 minWidth,
                                                                 maxWidth,
                                                                 true
          )
        pp.print(printContext, buf)
        if (result == null)
          fail("Expected exception")
        assertEquals(buf.toString, result)
      case _                                                                             => fail()
    }
  }

  test("test_print_nanos_noDecimalPoint") {
    provider_nanos.foreach {
      case (minWidth: Int) :: (maxWidth: Int) :: (value: Int) :: (result: String) :: Nil =>
        super.beforeEach()
        printContext.setDateTime(new MockFieldValue(NANO_OF_SECOND, value))
        val pp: TTBPDateTimeFormatterBuilder.FractionPrinterParser =
          new TTBPDateTimeFormatterBuilder.FractionPrinterParser(NANO_OF_SECOND,
                                                                 minWidth,
                                                                 maxWidth,
                                                                 false
          )
        pp.print(printContext, buf)
        if (result == null)
          fail("Expected exception")
        assertEquals(buf.toString, if (result.startsWith(".")) result.substring(1) else result)
      case _                                                                             => fail()
    }
  }

  val provider_seconds: List[List[Any]] =
    List(
      List(0, 9, 0, ""),
      List(0, 9, 3, ".05"),
      List(0, 9, 6, ".1"),
      List(0, 9, 9, ".15"),
      List(0, 9, 12, ".2"),
      List(0, 9, 15, ".25"),
      List(0, 9, 30, ".5"),
      List(0, 9, 45, ".75"),
      List(2, 2, 0, ".00"),
      List(2, 2, 3, ".05"),
      List(2, 2, 6, ".10"),
      List(2, 2, 9, ".15"),
      List(2, 2, 12, ".20"),
      List(2, 2, 15, ".25"),
      List(2, 2, 30, ".50"),
      List(2, 2, 45, ".75")
    )

  test("test_print_seconds") {
    provider_seconds.foreach {
      case (minWidth: Int) :: (maxWidth: Int) :: (value: Int) :: (result: String) :: Nil =>
        super.beforeEach()
        printContext.setDateTime(new MockFieldValue(SECOND_OF_MINUTE, value))
        val pp: TTBPDateTimeFormatterBuilder.FractionPrinterParser =
          new TTBPDateTimeFormatterBuilder.FractionPrinterParser(SECOND_OF_MINUTE,
                                                                 minWidth,
                                                                 maxWidth,
                                                                 true
          )
        pp.print(printContext, buf)
        if (result == null)
          fail("Expected exception")
        assertEquals(buf.toString, result)
      case _                                                                             => fail()
    }
  }

  test("test_print_seconds_noDecimalPoint") {
    provider_seconds.foreach {
      case (minWidth: Int) :: (maxWidth: Int) :: (value: Int) :: (result: String) :: Nil =>
        super.beforeEach()
        printContext.setDateTime(new MockFieldValue(SECOND_OF_MINUTE, value))
        val pp: TTBPDateTimeFormatterBuilder.FractionPrinterParser =
          new TTBPDateTimeFormatterBuilder.FractionPrinterParser(SECOND_OF_MINUTE,
                                                                 minWidth,
                                                                 maxWidth,
                                                                 false
          )
        pp.print(printContext, buf)
        if (result == null)
          fail("Expected exception")
        assertEquals(buf.toString, if (result.startsWith(".")) result.substring(1) else result)
      case _                                                                             => fail()
    }
  }

  test("test_reverseParse") {
    provider_nanos.foreach {
      case (minWidth: Int) :: (maxWidth: Int) :: (value: Int) :: (result: String) :: Nil =>
        super.beforeEach()
        val pp: TTBPDateTimeFormatterBuilder.FractionPrinterParser =
          new TTBPDateTimeFormatterBuilder.FractionPrinterParser(NANO_OF_SECOND,
                                                                 minWidth,
                                                                 maxWidth,
                                                                 true
          )
        val newPos: Int                                            = pp.parse(parseContext, result, 0)
        assertEquals(newPos, result.length)
        val expectedValue: Int                                     = fixParsedValue(maxWidth, value)
        assertParsed(parseContext,
                     NANO_OF_SECOND,
                     if (value == 0 && minWidth == 0) null else expectedValue.toLong
        )
      case _                                                                             => fail()
    }
  }

  test("test_reverseParse_noDecimalPoint") {
    provider_nanos.foreach {
      case (minWidth: Int) :: (maxWidth: Int) :: (value: Int) :: (result: String) :: Nil =>
        super.beforeEach()
        val pp: TTBPDateTimeFormatterBuilder.FractionPrinterParser =
          new TTBPDateTimeFormatterBuilder.FractionPrinterParser(NANO_OF_SECOND,
                                                                 minWidth,
                                                                 maxWidth,
                                                                 false
          )
        val newPos: Int                                            = pp.parse(parseContext, result, if (result.startsWith(".")) 1 else 0)
        assertEquals(newPos, result.length)
        val expectedValue: Int                                     = fixParsedValue(maxWidth, value)
        assertParsed(parseContext,
                     NANO_OF_SECOND,
                     if (value == 0 && minWidth == 0) null else expectedValue.toLong
        )
      case _                                                                             => fail()
    }
  }

  test("test_reverseParse_followedByNonDigit") {
    provider_nanos.foreach {
      case (minWidth: Int) :: (maxWidth: Int) :: (value: Int) :: (result: String) :: Nil =>
        super.beforeEach()
        val pp: TTBPDateTimeFormatterBuilder.FractionPrinterParser =
          new TTBPDateTimeFormatterBuilder.FractionPrinterParser(NANO_OF_SECOND,
                                                                 minWidth,
                                                                 maxWidth,
                                                                 true
          )
        val newPos: Int                                            = pp.parse(parseContext, result + " ", 0)
        assertEquals(newPos, result.length)
        val expectedValue: Int                                     = fixParsedValue(maxWidth, value)
        assertParsed(parseContext,
                     NANO_OF_SECOND,
                     if (value == 0 && minWidth == 0) null else expectedValue.toLong
        )
      case _                                                                             => fail()
    }
  }

  test("test_reverseParse_preceededByNonDigit") {
    provider_nanos.foreach {
      case (minWidth: Int) :: (maxWidth: Int) :: (value: Int) :: (result: String) :: Nil =>
        super.beforeEach()
        val pp: TTBPDateTimeFormatterBuilder.FractionPrinterParser =
          new TTBPDateTimeFormatterBuilder.FractionPrinterParser(NANO_OF_SECOND,
                                                                 minWidth,
                                                                 maxWidth,
                                                                 true
          )
        val newPos: Int                                            = pp.parse(parseContext, " " + result, 1)
        assertEquals(newPos, result.length + 1)
        val expectedValue: Int                                     = fixParsedValue(maxWidth, value)
        assertParsed(parseContext,
                     NANO_OF_SECOND,
                     if (value == 0 && minWidth == 0) null else expectedValue.toLong
        )
      case _                                                                             => fail()
    }
  }

  private def fixParsedValue(maxWidth: Int, value: Int): Int = {
    var _value = value
    if (maxWidth < 9) {
      val power: Int = Math.pow(10, 9 - maxWidth).toInt
      _value = _value / power * power
    }
    _value
  }

  test("test_reverseParse_seconds") {
    provider_seconds.foreach {
      case (minWidth: Int) :: (maxWidth: Int) :: (value: Int) :: (result: String) :: Nil =>
        super.beforeEach()
        val pp: TTBPDateTimeFormatterBuilder.FractionPrinterParser =
          new TTBPDateTimeFormatterBuilder.FractionPrinterParser(SECOND_OF_MINUTE,
                                                                 minWidth,
                                                                 maxWidth,
                                                                 true
          )
        val newPos: Int                                            = pp.parse(parseContext, result, 0)
        assertEquals(newPos, result.length)
        assertParsed(parseContext,
                     SECOND_OF_MINUTE,
                     if (value == 0 && minWidth == 0) null else value.toLong
        )
      case _                                                                             => fail()
    }
  }

  private def assertParsed(
    context: TTBPDateTimeParseContext,
    field:   TemporalField,
    value:   java.lang.Long
  ): Unit =
    if (value == null)
      assertEquals(context.getParsed(field), null)
    else
      assertEquals(context.getParsed(field), value)

  val provider_parseNothing: List[List[Any]] =
    List(
      List(new TTBPDateTimeFormatterBuilder.FractionPrinterParser(NANO_OF_SECOND, 3, 6, true),
           "",
           0,
           ~0
      ),
      List(new TTBPDateTimeFormatterBuilder.FractionPrinterParser(NANO_OF_SECOND, 3, 6, true),
           "A",
           0,
           ~0
      ),
      List(new TTBPDateTimeFormatterBuilder.FractionPrinterParser(NANO_OF_SECOND, 3, 6, true),
           ".",
           0,
           ~1
      ),
      List(new TTBPDateTimeFormatterBuilder.FractionPrinterParser(NANO_OF_SECOND, 3, 6, true),
           ".5",
           0,
           ~1
      ),
      List(new TTBPDateTimeFormatterBuilder.FractionPrinterParser(NANO_OF_SECOND, 3, 6, true),
           ".51",
           0,
           ~1
      ),
      List(new TTBPDateTimeFormatterBuilder.FractionPrinterParser(NANO_OF_SECOND, 3, 6, true),
           ".A23456",
           0,
           ~1
      ),
      List(new TTBPDateTimeFormatterBuilder.FractionPrinterParser(NANO_OF_SECOND, 3, 6, true),
           ".1A3456",
           0,
           ~1
      )
    )

  test("test_parse_nothing") {
    provider_parseNothing.foreach {
      case (pp: TTBPDateTimeFormatterBuilder.FractionPrinterParser) :: (text: String) :: (pos: Int) :: (expected: Int) :: Nil =>
        super.beforeEach()
        val newPos: Int = pp.parse(parseContext, text, pos)
        assertEquals(newPos, expected)
        assertEquals(parseContext.getParsed(NANO_OF_SECOND), null)
      case _                                                                                                                  => fail()
    }
  }

  test("test_toString") {
    val pp: TTBPDateTimeFormatterBuilder.FractionPrinterParser =
      new TTBPDateTimeFormatterBuilder.FractionPrinterParser(NANO_OF_SECOND, 3, 6, true)
    assertEquals(pp.toString, "Fraction(NanoOfSecond,3,6,DecimalPoint)")
  }

  test("test_toString_noDecimalPoint") {
    val pp: TTBPDateTimeFormatterBuilder.FractionPrinterParser =
      new TTBPDateTimeFormatterBuilder.FractionPrinterParser(NANO_OF_SECOND, 3, 6, false)
    assertEquals(pp.toString, "Fraction(NanoOfSecond,3,6)")
  }
}
