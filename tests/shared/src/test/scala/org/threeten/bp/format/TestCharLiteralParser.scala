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
import org.threeten.bp.format.internal.TTBPDateTimeFormatterBuilder._
import org.threeten.bp.format.internal.DateTimePrinterParser
import org.threeten.bp.temporal.TemporalQueries

/** Test CharLiteralPrinterParser. */
class TestCharLiteralParser extends AnyFunSuite with GenTestPrinterParser with AssertionsHelper {
  val data_success: List[List[Any]] =
    List(
      List(new CharLiteralPrinterParser('a'), true, "a", 0, 1),
      List(new CharLiteralPrinterParser('a'), true, "aOTHER", 0, 1),
      List(new CharLiteralPrinterParser('a'), true, "OTHERaOTHER", 5, 6),
      List(new CharLiteralPrinterParser('a'), true, "OTHERa", 5, 6),
      List(new CharLiteralPrinterParser('a'), true, "", 0, ~0),
      List(new CharLiteralPrinterParser('a'), true, "a", 1, ~1),
      List(new CharLiteralPrinterParser('a'), true, "A", 0, ~0),
      List(new CharLiteralPrinterParser('a'), true, "b", 0, ~0),
      List(new CharLiteralPrinterParser('a'), true, "OTHERbOTHER", 5, ~5),
      List(new CharLiteralPrinterParser('a'), true, "OTHERb", 5, ~5),
      List(new CharLiteralPrinterParser('a'), false, "a", 0, 1),
      List(new CharLiteralPrinterParser('a'), false, "A", 0, 1)
    )

  test("parse_success") {
    data_success.foreach {
      case (pp: DateTimePrinterParser) :: (caseSensitive: Boolean) :: (text: String) :: (pos: Int) :: (expectedPos: Int) :: Nil =>
        parseContext.setCaseSensitive(caseSensitive)
        val result: Int = pp.parse(parseContext, text, pos)
        assertEquals(result, expectedPos)
        assertEquals(parseContext.toParsed.query(TemporalQueries.chronology), null)
        assertEquals(parseContext.toParsed.query(TemporalQueries.zoneId), null)
      case _                                                                                                                    =>
        fail()
    }
  }

  val data_error: List[List[Any]] =
    List[List[Any]](
      List(new CharLiteralPrinterParser('a'), "a", -1, classOf[IndexOutOfBoundsException]),
      List(new CharLiteralPrinterParser('a'), "a", 2, classOf[IndexOutOfBoundsException])
    )

  test("parse_error") {
    data_error.foreach {
      case (pp: CharLiteralPrinterParser) :: (text: String) :: (pos: Int) :: (expected: Class[
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
}
