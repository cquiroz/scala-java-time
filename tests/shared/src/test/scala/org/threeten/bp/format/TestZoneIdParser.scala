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
import org.threeten.bp.Platform
import org.threeten.bp.ZoneId
import org.threeten.bp.ZoneOffset
import org.threeten.bp.format.internal.TTBPDateTimeFormatterBuilder
import org.threeten.bp.temporal.TemporalQueries
import org.threeten.bp.zone.ZoneRulesProvider

/** Test ZonePrinterParser. */
object TestZoneIdParser {
  private val AMERICA_DENVER: String   = "America/Denver"
  private val TIME_ZONE_DENVER: ZoneId = ZoneId.of(AMERICA_DENVER)
}

class TestZoneIdParser extends AnyFunSuite with GenTestPrinterParser with AssertionsHelper {
  val data_error: List[List[Any]] =
    List(
      List(new TTBPDateTimeFormatterBuilder.ZoneIdPrinterParser(TemporalQueries.zoneId, null),
           "hello",
           -1,
           classOf[Platform.DFE]
      ),
      List(new TTBPDateTimeFormatterBuilder.ZoneIdPrinterParser(TemporalQueries.zoneId, null),
           "hello",
           6,
           classOf[Platform.DFE]
      )
    )

  test("test_parse_error") {
    data_error.foreach {
      case (pp: TTBPDateTimeFormatterBuilder.ZoneIdPrinterParser) :: (text: String) :: (pos: Int) :: (_: Class[
            _
          ]) :: Nil =>
        try pp.parse(parseContext, text, pos)
        catch {
          case _: Throwable =>
            // assertTrue(expected.isInstance(ex))
            assertEquals(parseContext.toParsed.fieldValues.size, 0)
        }
      case _ =>
        fail()
    }
  }

  test("test_parse_exactMatch_Denver") {
    val pp: TTBPDateTimeFormatterBuilder.ZoneIdPrinterParser =
      new TTBPDateTimeFormatterBuilder.ZoneIdPrinterParser(TemporalQueries.zoneId, null)
    val result: Int                                          = pp.parse(parseContext, TestZoneIdParser.AMERICA_DENVER, 0)
    assertEquals(result, TestZoneIdParser.AMERICA_DENVER.length)
    assertParsed(TestZoneIdParser.TIME_ZONE_DENVER)
  }

  test("test_parse_startStringMatch_Denver") {
    val pp: TTBPDateTimeFormatterBuilder.ZoneIdPrinterParser =
      new TTBPDateTimeFormatterBuilder.ZoneIdPrinterParser(TemporalQueries.zoneId, null)
    val result: Int                                          = pp.parse(parseContext, TestZoneIdParser.AMERICA_DENVER + "OTHER", 0)
    assertEquals(result, TestZoneIdParser.AMERICA_DENVER.length)
    assertParsed(TestZoneIdParser.TIME_ZONE_DENVER)
  }

  test("test_parse_midStringMatch_Denver") {
    val pp: TTBPDateTimeFormatterBuilder.ZoneIdPrinterParser =
      new TTBPDateTimeFormatterBuilder.ZoneIdPrinterParser(TemporalQueries.zoneId, null)
    val result: Int                                          = pp.parse(parseContext, "OTHER" + TestZoneIdParser.AMERICA_DENVER + "OTHER", 5)
    assertEquals(result, 5 + TestZoneIdParser.AMERICA_DENVER.length)
    assertParsed(TestZoneIdParser.TIME_ZONE_DENVER)
  }

  test("test_parse_endStringMatch_Denver") {
    val pp: TTBPDateTimeFormatterBuilder.ZoneIdPrinterParser =
      new TTBPDateTimeFormatterBuilder.ZoneIdPrinterParser(TemporalQueries.zoneId, null)
    val result: Int                                          = pp.parse(parseContext, "OTHER" + TestZoneIdParser.AMERICA_DENVER, 5)
    assertEquals(result, 5 + TestZoneIdParser.AMERICA_DENVER.length)
    assertParsed(TestZoneIdParser.TIME_ZONE_DENVER)
  }

  test("test_parse_partialMatch") {
    val pp: TTBPDateTimeFormatterBuilder.ZoneIdPrinterParser =
      new TTBPDateTimeFormatterBuilder.ZoneIdPrinterParser(TemporalQueries.zoneId, null)
    val result: Int                                          = pp.parse(parseContext, "OTHERAmerica/Bogusville", 5)
    assertEquals(result, -6)
    assertParsed(null)
  }

  val populateTestData: List[(String, ZoneId)] = {
    import scala.collection.JavaConverters._
    val ids: java.util.Set[String] = ZoneRulesProvider.getAvailableZoneIds
    ids.asScala.map(id => (id, ZoneId.of(id))).toList
  }

  test("test_parse_exactMatch") {
    populateTestData.foreach {
      case (parse, expected) =>
        val pp: TTBPDateTimeFormatterBuilder.ZoneIdPrinterParser =
          new TTBPDateTimeFormatterBuilder.ZoneIdPrinterParser(TemporalQueries.zoneId, null)
        val result: Int                                          = pp.parse(parseContext, parse, 0)
        assertEquals(result, parse.length)
        assertParsed(expected)
      case _                 =>
        fail()
    }
  }

  test("test_parse_lowerCase") {
    val pp: TTBPDateTimeFormatterBuilder.ZoneIdPrinterParser =
      new TTBPDateTimeFormatterBuilder.ZoneIdPrinterParser(TemporalQueries.zoneId, null)
    parseContext.setCaseSensitive(false)
    val result: Int                                          = pp.parse(parseContext, "europe/london", 0)
    assertEquals(result, 13)
    assertParsed(ZoneId.of("Europe/London"))
  }

  test("test_parse_endStringMatch_utc") {
    val pp: TTBPDateTimeFormatterBuilder.ZoneIdPrinterParser =
      new TTBPDateTimeFormatterBuilder.ZoneIdPrinterParser(TemporalQueries.zoneId, null)
    val result: Int                                          = pp.parse(parseContext, "OTHERZ", 5)
    assertEquals(result, 6)
    assertParsed(ZoneOffset.UTC)
  }

  test("test_parse_endStringMatch_utc_plus1") {
    val pp: TTBPDateTimeFormatterBuilder.ZoneIdPrinterParser =
      new TTBPDateTimeFormatterBuilder.ZoneIdPrinterParser(TemporalQueries.zoneId, null)
    val result: Int                                          = pp.parse(parseContext, "OTHER+01:00", 5)
    assertEquals(result, 11)
    assertParsed(ZoneId.of("+01:00"))
  }

  test("test_parse_midStringMatch_utc") {
    val pp: TTBPDateTimeFormatterBuilder.ZoneIdPrinterParser =
      new TTBPDateTimeFormatterBuilder.ZoneIdPrinterParser(TemporalQueries.zoneId, null)
    val result: Int                                          = pp.parse(parseContext, "OTHERZOTHER", 5)
    assertEquals(result, 6)
    assertParsed(ZoneOffset.UTC)
  }

  test("test_parse_midStringMatch_utc_plus1") {
    val pp: TTBPDateTimeFormatterBuilder.ZoneIdPrinterParser =
      new TTBPDateTimeFormatterBuilder.ZoneIdPrinterParser(TemporalQueries.zoneId, null)
    val result: Int                                          = pp.parse(parseContext, "OTHER+01:00OTHER", 5)
    assertEquals(result, 11)
    assertParsed(ZoneId.of("+01:00"))
  }

  test("test_toString_id") {
    val pp: TTBPDateTimeFormatterBuilder.ZoneIdPrinterParser =
      new TTBPDateTimeFormatterBuilder.ZoneIdPrinterParser(TemporalQueries.zoneId, "ZoneId()")
    assertEquals(pp.toString, "ZoneId()")
  }

  private def assertParsed(expectedZone: ZoneId): Unit =
    assertEquals(parseContext.toParsed.zone, expectedZone)
}
