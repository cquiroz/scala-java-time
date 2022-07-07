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
import org.threeten.bp.AssertionsHelper
import org.threeten.bp.temporal.ChronoField.{ DAY_OF_MONTH, DAY_OF_WEEK }
import org.threeten.bp.temporal.{ TemporalField, TemporalQueries }
import org.threeten.bp.format.internal.{ TTBPDateTimeFormatterBuilder, TTBPDateTimeParseContext }

/** Test NumberPrinterParser. */
class TestNumberParser extends AnyFunSuite with GenTestPrinterParser with AssertionsHelper {
  val data_error: List[List[Any]] =
    List[List[Any]](
      List(
        new TTBPDateTimeFormatterBuilder.NumberPrinterParser(DAY_OF_MONTH, 1, 2, SignStyle.NEVER),
        "12",
        -1,
        classOf[IndexOutOfBoundsException]
      ),
      List(
        new TTBPDateTimeFormatterBuilder.NumberPrinterParser(DAY_OF_MONTH, 1, 2, SignStyle.NEVER),
        "12",
        3,
        classOf[IndexOutOfBoundsException]
      )
    )

  test("parse_error") {
    data_error.foreach {
      case (pp: TTBPDateTimeFormatterBuilder.NumberPrinterParser) :: (text: String) :: (pos: Int) :: (expected: Class[
            _
          ]) :: Nil                 =>
        try pp.parse(parseContext, text, pos)
        catch {
          case ex: RuntimeException =>
            assertTrue(expected.isInstance(ex))
            assertEquals(parseContext.toParsed.query(TemporalQueries.chronology), null)
            assertEquals(parseContext.toParsed.query(TemporalQueries.zoneId), null)
        }
      case _ =>
        fail()
    }
  }

  val provider_parseData: List[List[Any]] =
    List(
      List(1, 2, SignStyle.NEVER, 0, "12", 0, 2, 12L),
      List(1, 2, SignStyle.NEVER, 0, "Xxx12Xxx", 3, 5, 12L),
      List(1, 2, SignStyle.NEVER, 0, "99912999", 3, 5, 12L),
      List(2, 4, SignStyle.NEVER, 0, "12345", 0, 4, 1234L),
      List(2, 4, SignStyle.NEVER, 0, "12-45", 0, 2, 12L),
      List(2, 4, SignStyle.NEVER, 0, "123-5", 0, 3, 123L),
      List(1, 10, SignStyle.NORMAL, 0, "2147483647", 0, 10, Integer.MAX_VALUE.toLong),
      List(1, 10, SignStyle.NORMAL, 0, "-2147483648", 0, 11, Integer.MIN_VALUE.toLong),
      List(1, 10, SignStyle.NORMAL, 0, "2147483648", 0, 10, 2147483648L),
      List(1, 10, SignStyle.NORMAL, 0, "-2147483649", 0, 11, -2147483649L),
      List(1, 10, SignStyle.NORMAL, 0, "987659876598765", 0, 10, 9876598765L),
      List(1, 19, SignStyle.NORMAL, 0, "999999999999999999", 0, 18, 999999999999999999L),
      List(1, 19, SignStyle.NORMAL, 0, "-999999999999999999", 0, 19, -999999999999999999L),
      List(1, 19, SignStyle.NORMAL, 0, "1000000000000000000", 0, 19, 1000000000000000000L),
      List(1, 19, SignStyle.NORMAL, 0, "-1000000000000000000", 0, 20, -1000000000000000000L),
      List(1, 19, SignStyle.NORMAL, 0, "000000000000000000", 0, 18, 0L),
      List(1, 19, SignStyle.NORMAL, 0, "0000000000000000000", 0, 19, 0L),
      List(1, 19, SignStyle.NORMAL, 0, "9223372036854775807", 0, 19, Long.MaxValue),
      List(1, 19, SignStyle.NORMAL, 0, "-9223372036854775808", 0, 20, Long.MinValue),
      List(1, 19, SignStyle.NORMAL, 0, "9223372036854775808", 0, 18, 922337203685477580L),
      List(1, 19, SignStyle.NORMAL, 0, "-9223372036854775809", 0, 19, -922337203685477580L),
      List(1, 2, SignStyle.NEVER, 1, "A1", 0, ~0, 0L),
      List(1, 2, SignStyle.NEVER, 1, " 1", 0, ~0, 0L),
      List(1, 2, SignStyle.NEVER, 1, "  1", 1, ~1, 0L),
      List(2, 2, SignStyle.NEVER, 1, "1", 0, ~0, 0L),
      List(2, 2, SignStyle.NEVER, 1, "Xxx1", 0, ~0, 0L),
      List(2, 2, SignStyle.NEVER, 1, "1", 1, ~1, 0L),
      List(2, 2, SignStyle.NEVER, 1, "Xxx1", 4, ~4, 0L),
      List(2, 2, SignStyle.NEVER, 1, "1-2", 0, ~0, 0L),
      List(1, 19, SignStyle.NORMAL, 0, "-000000000000000000", 0, ~0, 0L),
      List(1, 19, SignStyle.NORMAL, 0, "-0000000000000000000", 0, ~0, 0L),
      List(1, 1, SignStyle.NEVER, 1, "12", 0, 1, 1L),
      List(1, 19, SignStyle.NEVER, 1, "12", 0, 1, 1L),
      List(1, 19, SignStyle.NEVER, 1, "12345", 0, 4, 1234L),
      List(1, 19, SignStyle.NEVER, 1, "12345678901", 0, 10, 1234567890L),
      List(1,
           19,
           SignStyle.NEVER,
           1,
           "123456789012345678901234567890",
           0,
           19,
           1234567890123456789L
      ),
      List(1, 19, SignStyle.NEVER, 1, "1", 0, 1, 1L),
      List(2, 2, SignStyle.NEVER, 1, "12", 0, 2, 12L),
      List(2, 19, SignStyle.NEVER, 1, "1", 0, ~0, 0L),
      List(1, 1, SignStyle.NEVER, 2, "123", 0, 1, 1L),
      List(1, 19, SignStyle.NEVER, 2, "123", 0, 1, 1L),
      List(1, 19, SignStyle.NEVER, 2, "12345", 0, 3, 123L),
      List(1, 19, SignStyle.NEVER, 2, "12345678901", 0, 9, 123456789L),
      List(1,
           19,
           SignStyle.NEVER,
           2,
           "123456789012345678901234567890",
           0,
           19,
           1234567890123456789L
      ),
      List(1, 19, SignStyle.NEVER, 2, "1", 0, 1, 1L),
      List(1, 19, SignStyle.NEVER, 2, "12", 0, 1, 1L),
      List(2, 2, SignStyle.NEVER, 2, "12", 0, 2, 12L),
      List(2, 19, SignStyle.NEVER, 2, "1", 0, ~0, 0L),
      List(2, 19, SignStyle.NEVER, 2, "1AAAAABBBBBCCCCC", 0, ~0, 0L)
    )

  test("parse_fresh") {
    provider_parseData.foreach {
      case (minWidth: Int) :: (maxWidth: Int) :: (signStyle: SignStyle) :: (subsequentWidth: Int) :: (text: String) :: (pos: Int) :: (expectedPos: Int) :: (expectedValue: Long) :: Nil =>
        super.beforeEach()
        var pp: TTBPDateTimeFormatterBuilder.NumberPrinterParser =
          new TTBPDateTimeFormatterBuilder.NumberPrinterParser(DAY_OF_MONTH,
                                                               minWidth,
                                                               maxWidth,
                                                               signStyle
          )
        if (subsequentWidth > 0)
          pp = pp.withSubsequentWidth(subsequentWidth)
        val newPos: Int                                          = pp.parse(parseContext, text, pos)
        assertEquals(newPos, expectedPos)
        if (expectedPos > 0)
          assertParsed(parseContext, DAY_OF_MONTH, Some(expectedValue))
        else {
          assertEquals(parseContext.toParsed.query(TemporalQueries.chronology), null)
          assertEquals(parseContext.toParsed.query(TemporalQueries.zoneId), null)
        }
      case _                                                                                                                                                                            =>
        fail()
    }
  }

  test("parse_textField") {
    provider_parseData.foreach {
      case (minWidth: Int) :: (maxWidth: Int) :: (signStyle: SignStyle) :: (subsequentWidth: Int) :: (text: String) :: (pos: Int) :: (expectedPos: Int) :: (expectedValue: Long) :: Nil =>
        super.beforeEach()
        var pp: TTBPDateTimeFormatterBuilder.NumberPrinterParser =
          new TTBPDateTimeFormatterBuilder.NumberPrinterParser(DAY_OF_WEEK,
                                                               minWidth,
                                                               maxWidth,
                                                               signStyle
          )
        if (subsequentWidth > 0)
          pp = pp.withSubsequentWidth(subsequentWidth)
        val newPos: Int                                          = pp.parse(parseContext, text, pos)
        assertEquals(newPos, expectedPos)
        if (expectedPos > 0)
          assertParsed(parseContext, DAY_OF_WEEK, Some(expectedValue))
      case _                                                                                                                                                                            =>
        fail()
    }
  }

  val provider_parseSignsStrict: List[List[Any]] =
    List(
      List("0", 1, 2, SignStyle.NEVER, 1, Some(0)),
      List("1", 1, 2, SignStyle.NEVER, 1, Some(1)),
      List("2", 1, 2, SignStyle.NEVER, 1, Some(2)),
      List("3", 1, 2, SignStyle.NEVER, 1, Some(3)),
      List("4", 1, 2, SignStyle.NEVER, 1, Some(4)),
      List("5", 1, 2, SignStyle.NEVER, 1, Some(5)),
      List("6", 1, 2, SignStyle.NEVER, 1, Some(6)),
      List("7", 1, 2, SignStyle.NEVER, 1, Some(7)),
      List("8", 1, 2, SignStyle.NEVER, 1, Some(8)),
      List("9", 1, 2, SignStyle.NEVER, 1, Some(9)),
      List("10", 1, 2, SignStyle.NEVER, 2, Some(10)),
      List("100", 1, 2, SignStyle.NEVER, 2, Some(10)),
      List("100", 1, 3, SignStyle.NEVER, 3, Some(100)),
      List("0", 1, 2, SignStyle.NEVER, 1, Some(0)),
      List("5", 1, 2, SignStyle.NEVER, 1, Some(5)),
      List("50", 1, 2, SignStyle.NEVER, 2, Some(50)),
      List("500", 1, 2, SignStyle.NEVER, 2, Some(50)),
      List("-0", 1, 2, SignStyle.NEVER, ~0, None),
      List("-5", 1, 2, SignStyle.NEVER, ~0, None),
      List("-50", 1, 2, SignStyle.NEVER, ~0, None),
      List("-500", 1, 2, SignStyle.NEVER, ~0, None),
      List("-AAA", 1, 2, SignStyle.NEVER, ~0, None),
      List("+0", 1, 2, SignStyle.NEVER, ~0, None),
      List("+5", 1, 2, SignStyle.NEVER, ~0, None),
      List("+50", 1, 2, SignStyle.NEVER, ~0, None),
      List("+500", 1, 2, SignStyle.NEVER, ~0, None),
      List("+AAA", 1, 2, SignStyle.NEVER, ~0, None),
      List("0", 1, 2, SignStyle.NOT_NEGATIVE, 1, Some(0)),
      List("5", 1, 2, SignStyle.NOT_NEGATIVE, 1, Some(5)),
      List("50", 1, 2, SignStyle.NOT_NEGATIVE, 2, Some(50)),
      List("500", 1, 2, SignStyle.NOT_NEGATIVE, 2, Some(50)),
      List("-0", 1, 2, SignStyle.NOT_NEGATIVE, ~0, None),
      List("-5", 1, 2, SignStyle.NOT_NEGATIVE, ~0, None),
      List("-50", 1, 2, SignStyle.NOT_NEGATIVE, ~0, None),
      List("-500", 1, 2, SignStyle.NOT_NEGATIVE, ~0, None),
      List("-AAA", 1, 2, SignStyle.NOT_NEGATIVE, ~0, None),
      List("+0", 1, 2, SignStyle.NOT_NEGATIVE, ~0, None),
      List("+5", 1, 2, SignStyle.NOT_NEGATIVE, ~0, None),
      List("+50", 1, 2, SignStyle.NOT_NEGATIVE, ~0, None),
      List("+500", 1, 2, SignStyle.NOT_NEGATIVE, ~0, None),
      List("+AAA", 1, 2, SignStyle.NOT_NEGATIVE, ~0, None),
      List("0", 1, 2, SignStyle.NORMAL, 1, Some(0)),
      List("5", 1, 2, SignStyle.NORMAL, 1, Some(5)),
      List("50", 1, 2, SignStyle.NORMAL, 2, Some(50)),
      List("500", 1, 2, SignStyle.NORMAL, 2, Some(50)),
      List("-0", 1, 2, SignStyle.NORMAL, ~0, None),
      List("-5", 1, 2, SignStyle.NORMAL, 2, Some(-5)),
      List("-50", 1, 2, SignStyle.NORMAL, 3, Some(-50)),
      List("-500", 1, 2, SignStyle.NORMAL, 3, Some(-50)),
      List("-AAA", 1, 2, SignStyle.NORMAL, ~1, None),
      List("+0", 1, 2, SignStyle.NORMAL, ~0, None),
      List("+5", 1, 2, SignStyle.NORMAL, ~0, None),
      List("+50", 1, 2, SignStyle.NORMAL, ~0, None),
      List("+500", 1, 2, SignStyle.NORMAL, ~0, None),
      List("+AAA", 1, 2, SignStyle.NORMAL, ~0, None),
      List("0", 1, 2, SignStyle.ALWAYS, ~0, None),
      List("5", 1, 2, SignStyle.ALWAYS, ~0, None),
      List("50", 1, 2, SignStyle.ALWAYS, ~0, None),
      List("500", 1, 2, SignStyle.ALWAYS, ~0, None),
      List("-0", 1, 2, SignStyle.ALWAYS, ~0, None),
      List("-5", 1, 2, SignStyle.ALWAYS, 2, Some(-5)),
      List("-50", 1, 2, SignStyle.ALWAYS, 3, Some(-50)),
      List("-500", 1, 2, SignStyle.ALWAYS, 3, Some(-50)),
      List("-AAA", 1, 2, SignStyle.ALWAYS, ~1, None),
      List("+0", 1, 2, SignStyle.ALWAYS, 2, Some(0)),
      List("+5", 1, 2, SignStyle.ALWAYS, 2, Some(5)),
      List("+50", 1, 2, SignStyle.ALWAYS, 3, Some(50)),
      List("+500", 1, 2, SignStyle.ALWAYS, 3, Some(50)),
      List("+AAA", 1, 2, SignStyle.ALWAYS, ~1, None),
      List("0", 1, 2, SignStyle.EXCEEDS_PAD, 1, Some(0)),
      List("5", 1, 2, SignStyle.EXCEEDS_PAD, 1, Some(5)),
      List("50", 1, 2, SignStyle.EXCEEDS_PAD, ~0, None),
      List("500", 1, 2, SignStyle.EXCEEDS_PAD, ~0, None),
      List("-0", 1, 2, SignStyle.EXCEEDS_PAD, ~0, None),
      List("-5", 1, 2, SignStyle.EXCEEDS_PAD, 2, Some(-5)),
      List("-50", 1, 2, SignStyle.EXCEEDS_PAD, 3, Some(-50)),
      List("-500", 1, 2, SignStyle.EXCEEDS_PAD, 3, Some(-50)),
      List("-AAA", 1, 2, SignStyle.EXCEEDS_PAD, ~1, None),
      List("+0", 1, 2, SignStyle.EXCEEDS_PAD, ~0, None),
      List("+5", 1, 2, SignStyle.EXCEEDS_PAD, ~0, None),
      List("+50", 1, 2, SignStyle.EXCEEDS_PAD, 3, Some(50)),
      List("+500", 1, 2, SignStyle.EXCEEDS_PAD, 3, Some(50)),
      List("+AAA", 1, 2, SignStyle.EXCEEDS_PAD, ~1, None)
    )

  test("parseSignsStrict") {
    provider_parseSignsStrict.foreach {
      case (input: String) :: (min: Int) :: (max: Int) :: (style: SignStyle) :: (parseLen: Int) :: (Some(
            parseVal: Int
          )) :: Nil =>
        super.beforeEach()
        val pp: TTBPDateTimeFormatterBuilder.NumberPrinterParser =
          new TTBPDateTimeFormatterBuilder.NumberPrinterParser(DAY_OF_MONTH, min, max, style)
        val newPos: Int                                          = pp.parse(parseContext, input, 0)
        assertEquals(newPos, parseLen)
        assertParsed(parseContext, DAY_OF_MONTH, Some(parseVal.toLong))
      case (input: String) :: (min: Int) :: (max: Int) :: (style: SignStyle) :: (parseLen: Int) :: None :: Nil =>
        super.beforeEach()
        val pp: TTBPDateTimeFormatterBuilder.NumberPrinterParser =
          new TTBPDateTimeFormatterBuilder.NumberPrinterParser(DAY_OF_MONTH, min, max, style)
        val newPos: Int                                          = pp.parse(parseContext, input, 0)
        assertEquals(newPos, parseLen)
        assertParsed(parseContext, DAY_OF_MONTH, None)
      case _                                                                                                   =>
        fail()
    }
  }

  val provider_parseSignsLenient: List[List[Any]] =
    List(
      List("0", 1, 2, SignStyle.NEVER, 1, Some(0)),
      List("5", 1, 2, SignStyle.NEVER, 1, Some(5)),
      List("50", 1, 2, SignStyle.NEVER, 2, Some(50)),
      List("500", 1, 2, SignStyle.NEVER, 3, Some(500)),
      List("-0", 1, 2, SignStyle.NEVER, 2, Some(0)),
      List("-5", 1, 2, SignStyle.NEVER, 2, Some(-5)),
      List("-50", 1, 2, SignStyle.NEVER, 3, Some(-50)),
      List("-500", 1, 2, SignStyle.NEVER, 4, Some(-500)),
      List("-AAA", 1, 2, SignStyle.NEVER, ~1, None),
      List("+0", 1, 2, SignStyle.NEVER, 2, Some(0)),
      List("+5", 1, 2, SignStyle.NEVER, 2, Some(5)),
      List("+50", 1, 2, SignStyle.NEVER, 3, Some(50)),
      List("+500", 1, 2, SignStyle.NEVER, 4, Some(500)),
      List("+AAA", 1, 2, SignStyle.NEVER, ~1, None),
      List("50", 2, 2, SignStyle.NEVER, 2, Some(50)),
      List("-50", 2, 2, SignStyle.NEVER, ~0, None),
      List("+50", 2, 2, SignStyle.NEVER, ~0, None),
      List("0", 1, 2, SignStyle.NOT_NEGATIVE, 1, Some(0)),
      List("5", 1, 2, SignStyle.NOT_NEGATIVE, 1, Some(5)),
      List("50", 1, 2, SignStyle.NOT_NEGATIVE, 2, Some(50)),
      List("500", 1, 2, SignStyle.NOT_NEGATIVE, 3, Some(500)),
      List("-0", 1, 2, SignStyle.NOT_NEGATIVE, 2, Some(0)),
      List("-5", 1, 2, SignStyle.NOT_NEGATIVE, 2, Some(-5)),
      List("-50", 1, 2, SignStyle.NOT_NEGATIVE, 3, Some(-50)),
      List("-500", 1, 2, SignStyle.NOT_NEGATIVE, 4, Some(-500)),
      List("-AAA", 1, 2, SignStyle.NOT_NEGATIVE, ~1, None),
      List("+0", 1, 2, SignStyle.NOT_NEGATIVE, 2, Some(0)),
      List("+5", 1, 2, SignStyle.NOT_NEGATIVE, 2, Some(5)),
      List("+50", 1, 2, SignStyle.NOT_NEGATIVE, 3, Some(50)),
      List("+500", 1, 2, SignStyle.NOT_NEGATIVE, 4, Some(500)),
      List("+AAA", 1, 2, SignStyle.NOT_NEGATIVE, ~1, None),
      List("50", 2, 2, SignStyle.NOT_NEGATIVE, 2, Some(50)),
      List("-50", 2, 2, SignStyle.NOT_NEGATIVE, ~0, None),
      List("+50", 2, 2, SignStyle.NOT_NEGATIVE, ~0, None),
      List("0", 1, 2, SignStyle.NORMAL, 1, Some(0)),
      List("5", 1, 2, SignStyle.NORMAL, 1, Some(5)),
      List("50", 1, 2, SignStyle.NORMAL, 2, Some(50)),
      List("500", 1, 2, SignStyle.NORMAL, 3, Some(500)),
      List("-0", 1, 2, SignStyle.NORMAL, 2, Some(0)),
      List("-5", 1, 2, SignStyle.NORMAL, 2, Some(-5)),
      List("-50", 1, 2, SignStyle.NORMAL, 3, Some(-50)),
      List("-500", 1, 2, SignStyle.NORMAL, 4, Some(-500)),
      List("-AAA", 1, 2, SignStyle.NORMAL, ~1, None),
      List("+0", 1, 2, SignStyle.NORMAL, 2, Some(0)),
      List("+5", 1, 2, SignStyle.NORMAL, 2, Some(5)),
      List("+50", 1, 2, SignStyle.NORMAL, 3, Some(50)),
      List("+500", 1, 2, SignStyle.NORMAL, 4, Some(500)),
      List("+AAA", 1, 2, SignStyle.NORMAL, ~1, None),
      List("50", 2, 2, SignStyle.NORMAL, 2, Some(50)),
      List("-50", 2, 2, SignStyle.NORMAL, 3, Some(-50)),
      List("+50", 2, 2, SignStyle.NORMAL, 3, Some(50)),
      List("0", 1, 2, SignStyle.ALWAYS, 1, Some(0)),
      List("5", 1, 2, SignStyle.ALWAYS, 1, Some(5)),
      List("50", 1, 2, SignStyle.ALWAYS, 2, Some(50)),
      List("500", 1, 2, SignStyle.ALWAYS, 3, Some(500)),
      List("-0", 1, 2, SignStyle.ALWAYS, 2, Some(0)),
      List("-5", 1, 2, SignStyle.ALWAYS, 2, Some(-5)),
      List("-50", 1, 2, SignStyle.ALWAYS, 3, Some(-50)),
      List("-500", 1, 2, SignStyle.ALWAYS, 4, Some(-500)),
      List("-AAA", 1, 2, SignStyle.ALWAYS, ~1, None),
      List("+0", 1, 2, SignStyle.ALWAYS, 2, Some(0)),
      List("+5", 1, 2, SignStyle.ALWAYS, 2, Some(5)),
      List("+50", 1, 2, SignStyle.ALWAYS, 3, Some(50)),
      List("+500", 1, 2, SignStyle.ALWAYS, 4, Some(500)),
      List("+AAA", 1, 2, SignStyle.ALWAYS, ~1, None),
      List("0", 1, 2, SignStyle.EXCEEDS_PAD, 1, Some(0)),
      List("5", 1, 2, SignStyle.EXCEEDS_PAD, 1, Some(5)),
      List("50", 1, 2, SignStyle.EXCEEDS_PAD, 2, Some(50)),
      List("500", 1, 2, SignStyle.EXCEEDS_PAD, 3, Some(500)),
      List("-0", 1, 2, SignStyle.EXCEEDS_PAD, 2, Some(0)),
      List("-5", 1, 2, SignStyle.EXCEEDS_PAD, 2, Some(-5)),
      List("-50", 1, 2, SignStyle.EXCEEDS_PAD, 3, Some(-50)),
      List("-500", 1, 2, SignStyle.EXCEEDS_PAD, 4, Some(-500)),
      List("-AAA", 1, 2, SignStyle.EXCEEDS_PAD, ~1, None),
      List("+0", 1, 2, SignStyle.EXCEEDS_PAD, 2, Some(0)),
      List("+5", 1, 2, SignStyle.EXCEEDS_PAD, 2, Some(5)),
      List("+50", 1, 2, SignStyle.EXCEEDS_PAD, 3, Some(50)),
      List("+500", 1, 2, SignStyle.EXCEEDS_PAD, 4, Some(500)),
      List("+AAA", 1, 2, SignStyle.EXCEEDS_PAD, ~1, None)
    )

  test("parseSignsLenient") {
    provider_parseSignsLenient.foreach {
      case (input: String) :: (min: Int) :: (max: Int) :: (style: SignStyle) :: (parseLen: Int) :: (Some(
            parseVal: Int
          )) :: Nil =>
        super.beforeEach()
        parseContext.setStrict(false)
        val pp: TTBPDateTimeFormatterBuilder.NumberPrinterParser =
          new TTBPDateTimeFormatterBuilder.NumberPrinterParser(DAY_OF_MONTH, min, max, style)
        val newPos: Int                                          = pp.parse(parseContext, input, 0)
        assertEquals(newPos, parseLen)
        assertParsed(parseContext, DAY_OF_MONTH, Some(parseVal.toLong))
      case (input: String) :: (min: Int) :: (max: Int) :: (style: SignStyle) :: (parseLen: Int) :: None :: Nil =>
        super.beforeEach()
        parseContext.setStrict(false)
        val pp: TTBPDateTimeFormatterBuilder.NumberPrinterParser =
          new TTBPDateTimeFormatterBuilder.NumberPrinterParser(DAY_OF_MONTH, min, max, style)
        val newPos: Int                                          = pp.parse(parseContext, input, 0)
        assertEquals(newPos, parseLen)
        assertParsed(parseContext, DAY_OF_MONTH, None)
      case _                                                                                                   =>
        fail()
    }
  }

  private def assertParsed(
    context: TTBPDateTimeParseContext,
    field:   TemporalField,
    value:   Option[Long]
  ): Unit =
    value.fold(assertEquals(context.getParsed(field), null)) {
      assertEquals(context.getParsed(field), _)
    }
}
