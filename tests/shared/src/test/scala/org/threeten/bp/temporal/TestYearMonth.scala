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
package org.threeten.bp.temporal

import org.scalatest.BeforeAndAfter
import org.threeten.bp.temporal.ChronoField.ERA
import org.threeten.bp.temporal.ChronoField.MONTH_OF_YEAR
import org.threeten.bp.temporal.ChronoField.PROLEPTIC_MONTH
import org.threeten.bp.temporal.ChronoField.YEAR
import org.threeten.bp.temporal.ChronoField.YEAR_OF_ERA
import org.threeten.bp._
import org.threeten.bp.chrono.IsoChronology
import org.threeten.bp.format.DateTimeFormatter
import org.threeten.bp.format.DateTimeParseException

/** Test YearMonth. */
class TestYearMonth extends GenDateTimeTest with BeforeAndAfter {
  private var TEST_2008_06: YearMonth = null

  before {
    TEST_2008_06 = YearMonth.of(2008, 6)
  }

  protected def samples: List[TemporalAccessor] =
    List(TEST_2008_06)

  protected def validFields: List[TemporalField] =
    List(MONTH_OF_YEAR, PROLEPTIC_MONTH, YEAR_OF_ERA, YEAR, ERA)

  protected def invalidFields: List[TemporalField] = {
    val list: List[TemporalField] = List(ChronoField.values: _*)
    (list :+ JulianFields.JULIAN_DAY :+ JulianFields.MODIFIED_JULIAN_DAY :+ JulianFields.RATA_DIE)
      .filterNot(validFields.contains)
  }

  ignore("test_immutable") {
    // throw new SkipException("private constructor shows up public due to companion object")
    // AbstractTest.assertImmutable(classOf[YearMonth])
  }

  /*
  @Test
  def test_serialization(): Unit = {
    AbstractTest.assertSerializable(TEST_2008_06)
  }

  @Test
  def test_serialization_format(): Unit = {
    AbstractTest.assertEqualsSerialisedForm(YearMonth.of(2012, 9))
  }
   */

  private def check(test: YearMonth, y: Int, m: Int): Unit = {
    assertEquals(test.getYear, y)
    assertEquals(test.getMonth.getValue, m)
  }

  test("now") {
    var expected: YearMonth = YearMonth.now(Clock.systemDefaultZone)
    var test: YearMonth     = YearMonth.now
    var i: Int              = 0
    while (i < 100 && expected != test) {
      expected = YearMonth.now(Clock.systemDefaultZone)
      test = YearMonth.now
      i += 1
    }
    assertEquals(test, expected)
  }

  test("now_ZoneId_nullZoneId") {
    assertThrows[NullPointerException] {
      YearMonth.now(null.asInstanceOf[ZoneId])
    }
  }

  test("now_ZoneId") {
    val zone: ZoneId        = ZoneId.of("UTC+01:02:03")
    var expected: YearMonth = YearMonth.now(Clock.system(zone))
    var test: YearMonth     = YearMonth.now(zone)

    var i: Int = 0
    while (i < 100 && expected != test) {
      expected = YearMonth.now(Clock.system(zone))
      test = YearMonth.now(zone)
      i += 1
    }
    assertEquals(test, expected)
  }

  test("now_Clock") {
    val instant: Instant = LocalDateTime.of(2010, 12, 31, 0, 0).toInstant(ZoneOffset.UTC)
    val clock: Clock     = Clock.fixed(instant, ZoneOffset.UTC)
    val test: YearMonth  = YearMonth.now(clock)
    assertEquals(test.getYear, 2010)
    assertEquals(test.getMonth, Month.DECEMBER)
  }

  test("now_Clock_nullClock") {
    assertThrows[NullPointerException] {
      YearMonth.now(null.asInstanceOf[Clock])
    }
  }

  test("factory_intsMonth") {
    val test: YearMonth = YearMonth.of(2008, Month.FEBRUARY)
    check(test, 2008, 2)
  }

  test("test_factory_intsMonth_yearTooLow") {
    assertThrows[DateTimeException] {
      YearMonth.of(Year.MIN_VALUE - 1, Month.JANUARY)
    }
  }

  test("test_factory_intsMonth_dayTooHigh") {
    assertThrows[DateTimeException] {
      YearMonth.of(Year.MAX_VALUE + 1, Month.JANUARY)
    }
  }

  test("factory_intsMonth_nullMonth") {
    assertThrows[NullPointerException] {
      YearMonth.of(2008, null)
    }
  }

  test("factory_ints") {
    val test: YearMonth = YearMonth.of(2008, 2)
    check(test, 2008, 2)
  }

  test("test_factory_ints_yearTooLow") {
    assertThrows[DateTimeException] {
      YearMonth.of(Year.MIN_VALUE - 1, 2)
    }
  }

  test("test_factory_ints_dayTooHigh") {
    assertThrows[DateTimeException] {
      YearMonth.of(Year.MAX_VALUE + 1, 2)
    }
  }

  test("test_factory_ints_monthTooLow") {
    assertThrows[DateTimeException] {
      YearMonth.of(2008, 0)
    }
  }

  test("test_factory_ints_monthTooHigh") {
    assertThrows[DateTimeException] {
      YearMonth.of(2008, 13)
    }
  }

  test("test_factory_CalendricalObject") {
    assertEquals(YearMonth.from(LocalDate.of(2007, 7, 15)), YearMonth.of(2007, 7))
  }

  test("test_factory_CalendricalObject_invalid_noDerive") {
    assertThrows[DateTimeException] {
      YearMonth.from(LocalTime.of(12, 30))
    }
  }

  test("test_factory_CalendricalObject_null") {
    assertThrows[NullPointerException] {
      YearMonth.from(null.asInstanceOf[TemporalAccessor])
    }
  }

  val provider_goodParseData: List[(String, YearMonth)] =
    List(
      ("0000-01", YearMonth.of(0, 1)),
      ("0000-12", YearMonth.of(0, 12)),
      ("9999-12", YearMonth.of(9999, 12)),
      ("2000-01", YearMonth.of(2000, 1)),
      ("2000-02", YearMonth.of(2000, 2)),
      ("2000-03", YearMonth.of(2000, 3)),
      ("2000-04", YearMonth.of(2000, 4)),
      ("2000-05", YearMonth.of(2000, 5)),
      ("2000-06", YearMonth.of(2000, 6)),
      ("2000-07", YearMonth.of(2000, 7)),
      ("2000-08", YearMonth.of(2000, 8)),
      ("2000-09", YearMonth.of(2000, 9)),
      ("2000-10", YearMonth.of(2000, 10)),
      ("2000-11", YearMonth.of(2000, 11)),
      ("2000-12", YearMonth.of(2000, 12)),
      ("+12345678-03", YearMonth.of(12345678, 3)),
      ("+123456-03", YearMonth.of(123456, 3)),
      ("0000-03", YearMonth.of(0, 3)),
      ("-1234-03", YearMonth.of(-1234, 3)),
      ("-12345678-03", YearMonth.of(-12345678, 3)),
      ("+" + Year.MAX_VALUE + "-03", YearMonth.of(Year.MAX_VALUE, 3)),
      (Year.MIN_VALUE + "-03", YearMonth.of(Year.MIN_VALUE, 3))
    )

  test("factory_parse_success") {
    provider_goodParseData.foreach { case (text, expected) =>
      val yearMonth: YearMonth = YearMonth.parse(text)
      assertEquals(yearMonth, expected)
    }
  }

  val provider_badParseData: List[(String, Int)] =
    List(
      ("", 0),
      ("-00", 1),
      ("--01-0", 1),
      ("A01-3", 0),
      ("200-01", 0),
      ("2009/12", 4),
      ("-0000-10", 0),
      ("-12345678901-10", 11),
      ("+1-10", 1),
      ("+12-10", 1),
      ("+123-10", 1),
      ("+1234-10", 0),
      ("12345-10", 0),
      ("+12345678901-10", 11)
    )

  test("factory_parse_fail") {
    provider_badParseData.foreach { case (text, pos) =>
      try {
        YearMonth.parse(text)
        fail(f"Parse should have failed for $text%s at position $pos%d")
      } catch {
        case ex: DateTimeParseException =>
          assertEquals(ex.getParsedString, text)
          assertEquals(ex.getErrorIndex, pos)
      }
    }
  }

  test("factory_parse_illegalValue_Month") {
    assertThrows[DateTimeParseException] {
      YearMonth.parse("2008-13")
    }
  }

  test("factory_parse_nullText") {
    assertThrows[NullPointerException] {
      YearMonth.parse(null)
    }
  }

  test("factory_parse_formatter") {
    val f: DateTimeFormatter = DateTimeFormatter.ofPattern("u M")
    val test: YearMonth      = YearMonth.parse("2010 12", f)
    assertEquals(test, YearMonth.of(2010, 12))
  }

  test("factory_parse_formatter_nullText") {
    assertThrows[NullPointerException] {
      val f: DateTimeFormatter = DateTimeFormatter.ofPattern("u M")
      YearMonth.parse(null.asInstanceOf[String], f)
    }
  }

  test("factory_parse_formatter_nullFormatter") {
    assertThrows[NullPointerException] {
      YearMonth.parse("ANY", null)
    }
  }

  test("test_get_TemporalField") {
    assertEquals(TEST_2008_06.get(YEAR), 2008)
    assertEquals(TEST_2008_06.get(MONTH_OF_YEAR), 6)
    assertEquals(TEST_2008_06.get(YEAR_OF_ERA), 2008)
    assertEquals(TEST_2008_06.get(ERA), 1)
  }

  test("test_get_TemporalField_tooBig") {
    assertThrows[DateTimeException] {
      TEST_2008_06.get(PROLEPTIC_MONTH)
    }
  }

  test("test_get_TemporalField_null") {
    assertThrows[Platform.NPE] {
      TEST_2008_06.get(null.asInstanceOf[TemporalField])
    }
  }

  test("test_get_TemporalField_invalidField") {
    assertThrows[DateTimeException] {
      TEST_2008_06.get(MockFieldNoValue.INSTANCE)
    }
  }

  test("test_get_TemporalField_timeField") {
    assertThrows[DateTimeException] {
      TEST_2008_06.get(ChronoField.AMPM_OF_DAY)
    }
  }

  test("test_getLong_TemporalField") {
    assertEquals(TEST_2008_06.getLong(YEAR), 2008)
    assertEquals(TEST_2008_06.getLong(MONTH_OF_YEAR), 6)
    assertEquals(TEST_2008_06.getLong(YEAR_OF_ERA), 2008)
    assertEquals(TEST_2008_06.getLong(ERA), 1)
    assertEquals(TEST_2008_06.getLong(PROLEPTIC_MONTH), 2008 * 12 + 6 - 1)
  }

  test("test_getLong_TemporalField_null") {
    assertThrows[Platform.NPE] {
      TEST_2008_06.getLong(null.asInstanceOf[TemporalField])
    }
  }

  test("test_getLong_TemporalField_invalidField") {
    assertThrows[DateTimeException] {
      TEST_2008_06.getLong(MockFieldNoValue.INSTANCE)
    }
  }

  test("test_getLong_TemporalField_timeField") {
    assertThrows[DateTimeException] {
      TEST_2008_06.getLong(ChronoField.AMPM_OF_DAY)
    }
  }

  val provider_sampleDates: List[(Int, Int)] =
    List((2008, 1), (2008, 2), (-1, 3), (0, 12))

  test("test_hashCode") {
    provider_sampleDates.foreach { case (y, m) =>
      val a: YearMonth = YearMonth.of(y, m)
      assertEquals(a.hashCode, a.hashCode)
      val b: YearMonth = YearMonth.of(y, m)
      assertEquals(a.hashCode, b.hashCode)
    }
  }

  test("test_hashCode_unique") {
    val uniques: java.util.Set[Integer] = new java.util.HashSet[Integer](201 * 12)
    var i: Int                          = 1900
    while (i <= 2100) {
      var j: Int = 1
      while (j <= 12) {
        assertTrue(uniques.add(YearMonth.of(i, j).hashCode))
        j += 1
      }
      i += 1
    }
  }

  test("test_with_Year") {
    val test: YearMonth = YearMonth.of(2008, 6)
    assertEquals(test.`with`(Year.of(2000)), YearMonth.of(2000, 6))
  }

  test("test_with_Year_noChange_equal") {
    val test: YearMonth = YearMonth.of(2008, 6)
    assertEquals(test.`with`(Year.of(2008)), test)
  }

  test("test_with_Year_null") {
    assertThrows[Platform.NPE] {
      val test: YearMonth = YearMonth.of(2008, 6)
      test.`with`(null.asInstanceOf[Year])
    }
  }

  test("test_with_Month") {
    val test: YearMonth = YearMonth.of(2008, 6)
    assertEquals(test.`with`(Month.JANUARY), YearMonth.of(2008, 1))
  }

  test("test_with_Month_noChange_equal") {
    val test: YearMonth = YearMonth.of(2008, 6)
    assertEquals(test.`with`(Month.JUNE), test)
  }

  test("test_with_Month_null") {
    assertThrows[Platform.NPE] {
      val test: YearMonth = YearMonth.of(2008, 6)
      test.`with`(null.asInstanceOf[Month])
    }
  }

  test("test_withYear") {
    val test: YearMonth = YearMonth.of(2008, 6)
    assertEquals(test.withYear(1999), YearMonth.of(1999, 6))
  }

  test("test_withYear_int_noChange_equal") {
    val test: YearMonth = YearMonth.of(2008, 6)
    assertEquals(test.withYear(2008), test)
  }

  test("test_withYear_tooLow") {
    assertThrows[DateTimeException] {
      val test: YearMonth = YearMonth.of(2008, 6)
      test.withYear(Year.MIN_VALUE - 1)
    }
  }

  test("test_withYear_tooHigh") {
    assertThrows[DateTimeException] {
      val test: YearMonth = YearMonth.of(2008, 6)
      test.withYear(Year.MAX_VALUE + 1)
    }
  }

  test("test_withMonth") {
    val test: YearMonth = YearMonth.of(2008, 6)
    assertEquals(test.withMonth(1), YearMonth.of(2008, 1))
  }

  test("test_withMonth_int_noChange_equal") {
    val test: YearMonth = YearMonth.of(2008, 6)
    assertEquals(test.withMonth(6), test)
  }

  test("test_withMonth_tooLow") {
    assertThrows[DateTimeException] {
      val test: YearMonth = YearMonth.of(2008, 6)
      test.withMonth(0)
    }
  }

  test("test_withMonth_tooHigh") {
    assertThrows[DateTimeException] {
      val test: YearMonth = YearMonth.of(2008, 6)
      test.withMonth(13)
    }
  }

  test("test_plusYears_long") {
    val test: YearMonth = YearMonth.of(2008, 6)
    assertEquals(test.plusYears(1), YearMonth.of(2009, 6))
  }

  test("test_plusYears_long_noChange_equal") {
    val test: YearMonth = YearMonth.of(2008, 6)
    assertEquals(test.plusYears(0), test)
  }

  test("test_plusYears_long_negative") {
    val test: YearMonth = YearMonth.of(2008, 6)
    assertEquals(test.plusYears(-1), YearMonth.of(2007, 6))
  }

  test("test_plusYears_long_big") {
    val test: YearMonth = YearMonth.of(-40, 6)
    assertEquals(test.plusYears(20L + Year.MAX_VALUE),
                 YearMonth.of((-40L + 20L + Year.MAX_VALUE).asInstanceOf[Int], 6)
    )
  }

  test("test_plusYears_long_invalidTooLarge") {
    assertThrows[DateTimeException] {
      val test: YearMonth = YearMonth.of(Year.MAX_VALUE, 6)
      test.plusYears(1)
    }
  }

  test("test_plusYears_long_invalidTooLargeMaxAddMax") {
    assertThrows[DateTimeException] {
      val test: YearMonth = YearMonth.of(Year.MAX_VALUE, 12)
      test.plusYears(Long.MaxValue)
    }
  }

  test("test_plusYears_long_invalidTooLargeMaxAddMin") {
    assertThrows[DateTimeException] {
      val test: YearMonth = YearMonth.of(Year.MAX_VALUE, 12)
      test.plusYears(Long.MinValue)
    }
  }

  test("test_plusYears_long_invalidTooSmall") {
    assertThrows[DateTimeException] {
      val test: YearMonth = YearMonth.of(Year.MIN_VALUE, 6)
      test.plusYears(-1)
    }
  }

  test("test_plusMonths_long") {
    val test: YearMonth = YearMonth.of(2008, 6)
    assertEquals(test.plusMonths(1), YearMonth.of(2008, 7))
  }

  test("test_plusMonths_long_noChange_equal") {
    val test: YearMonth = YearMonth.of(2008, 6)
    assertEquals(test.plusMonths(0), test)
  }

  test("test_plusMonths_long_overYears") {
    val test: YearMonth = YearMonth.of(2008, 6)
    assertEquals(test.plusMonths(7), YearMonth.of(2009, 1))
  }

  test("test_plusMonths_long_negative") {
    val test: YearMonth = YearMonth.of(2008, 6)
    assertEquals(test.plusMonths(-1), YearMonth.of(2008, 5))
  }

  test("test_plusMonths_long_negativeOverYear") {
    val test: YearMonth = YearMonth.of(2008, 6)
    assertEquals(test.plusMonths(-6), YearMonth.of(2007, 12))
  }

  test("test_plusMonths_long_big") {
    val test: YearMonth = YearMonth.of(-40, 6)
    val months: Long    = 20L + Integer.MAX_VALUE
    assertEquals(test.plusMonths(months),
                 YearMonth.of((-40L + months / 12).toInt, 6 + (months % 12).toInt)
    )
  }

  test("test_plusMonths_long_invalidTooLarge") {
    assertThrows[DateTimeException] {
      val test: YearMonth = YearMonth.of(Year.MAX_VALUE, 12)
      test.plusMonths(1)
    }
  }

  test("test_plusMonths_long_invalidTooLargeMaxAddMax") {
    assertThrows[DateTimeException] {
      val test: YearMonth = YearMonth.of(Year.MAX_VALUE, 12)
      test.plusMonths(Long.MaxValue)
    }
  }

  test("test_plusMonths_long_invalidTooLargeMaxAddMin") {
    assertThrows[DateTimeException] {
      val test: YearMonth = YearMonth.of(Year.MAX_VALUE, 12)
      test.plusMonths(Long.MinValue)
    }
  }

  test("test_plusMonths_long_invalidTooSmall") {
    assertThrows[DateTimeException] {
      val test: YearMonth = YearMonth.of(Year.MIN_VALUE, 1)
      test.plusMonths(-1)
    }
  }

  test("test_minusYears_long") {
    val test: YearMonth = YearMonth.of(2008, 6)
    assertEquals(test.minusYears(1), YearMonth.of(2007, 6))
  }

  test("test_minusYears_long_noChange_equal") {
    val test: YearMonth = YearMonth.of(2008, 6)
    assertEquals(test.minusYears(0), test)
  }

  test("test_minusYears_long_negative") {
    val test: YearMonth = YearMonth.of(2008, 6)
    assertEquals(test.minusYears(-1), YearMonth.of(2009, 6))
  }

  test("test_minusYears_long_big") {
    val test: YearMonth = YearMonth.of(40, 6)
    assertEquals(test.minusYears(20L + Year.MAX_VALUE),
                 YearMonth.of((40L - 20L - Year.MAX_VALUE).asInstanceOf[Int], 6)
    )
  }

  test("test_minusYears_long_invalidTooLarge") {
    assertThrows[DateTimeException] {
      val test: YearMonth = YearMonth.of(Year.MAX_VALUE, 6)
      test.minusYears(-1)
    }
  }

  test("test_minusYears_long_invalidTooLargeMaxSubtractMax") {
    assertThrows[DateTimeException] {
      val test: YearMonth = YearMonth.of(Year.MIN_VALUE, 12)
      test.minusYears(Long.MaxValue)
    }
  }

  test("test_minusYears_long_invalidTooLargeMaxSubtractMin") {
    assertThrows[DateTimeException] {
      val test: YearMonth = YearMonth.of(Year.MIN_VALUE, 12)
      test.minusYears(Long.MinValue)
    }
  }

  test("test_minusYears_long_invalidTooSmall") {
    assertThrows[DateTimeException] {
      val test: YearMonth = YearMonth.of(Year.MIN_VALUE, 6)
      test.minusYears(1)
    }
  }

  test("test_minusMonths_long") {
    val test: YearMonth = YearMonth.of(2008, 6)
    assertEquals(test.minusMonths(1), YearMonth.of(2008, 5))
  }

  test("test_minusMonths_long_noChange_equal") {
    val test: YearMonth = YearMonth.of(2008, 6)
    assertEquals(test.minusMonths(0), test)
  }

  test("test_minusMonths_long_overYears") {
    val test: YearMonth = YearMonth.of(2008, 6)
    assertEquals(test.minusMonths(6), YearMonth.of(2007, 12))
  }

  test("test_minusMonths_long_negative") {
    val test: YearMonth = YearMonth.of(2008, 6)
    assertEquals(test.minusMonths(-1), YearMonth.of(2008, 7))
  }

  test("test_minusMonths_long_negativeOverYear") {
    val test: YearMonth = YearMonth.of(2008, 6)
    assertEquals(test.minusMonths(-7), YearMonth.of(2009, 1))
  }

  test("test_minusMonths_long_big") {
    val test: YearMonth = YearMonth.of(40, 6)
    val months: Long    = 20L + Integer.MAX_VALUE
    assertEquals(test.minusMonths(months),
                 YearMonth.of((40L - months / 12).toInt, 6 - (months % 12).toInt)
    )
  }

  test("test_minusMonths_long_invalidTooLarge") {
    assertThrows[DateTimeException] {
      val test: YearMonth = YearMonth.of(Year.MAX_VALUE, 12)
      test.minusMonths(-1)
    }
  }

  test("test_minusMonths_long_invalidTooLargeMaxSubtractMax") {
    assertThrows[DateTimeException] {
      val test: YearMonth = YearMonth.of(Year.MAX_VALUE, 12)
      test.minusMonths(Long.MaxValue)
    }
  }

  test("test_minusMonths_long_invalidTooLargeMaxSubtractMin") {
    assertThrows[DateTimeException] {
      val test: YearMonth = YearMonth.of(Year.MAX_VALUE, 12)
      test.minusMonths(Long.MinValue)
    }
  }

  test("test_minusMonths_long_invalidTooSmall") {
    assertThrows[DateTimeException] {
      val test: YearMonth = YearMonth.of(Year.MIN_VALUE, 1)
      test.minusMonths(1)
    }
  }

  test("test_adjustDate") {
    val test: YearMonth = YearMonth.of(2008, 6)
    val date: LocalDate = LocalDate.of(2007, 1, 1)
    assertEquals(test.adjustInto(date), LocalDate.of(2008, 6, 1))
  }

  test("test_adjustDate_preserveDoM") {
    val test: YearMonth = YearMonth.of(2011, 3)
    val date: LocalDate = LocalDate.of(2008, 2, 29)
    assertEquals(test.adjustInto(date), LocalDate.of(2011, 3, 29))
  }

  test("test_adjustDate_resolve") {
    val test: YearMonth = YearMonth.of(2007, 2)
    val date: LocalDate = LocalDate.of(2008, 3, 31)
    assertEquals(test.adjustInto(date), LocalDate.of(2007, 2, 28))
  }

  test("test_adjustDate_equal") {
    val test: YearMonth = YearMonth.of(2008, 6)
    val date: LocalDate = LocalDate.of(2008, 6, 30)
    assertEquals(test.adjustInto(date), date)
  }

  test("test_adjustDate_null") {
    assertThrows[NullPointerException] {
      TEST_2008_06.adjustInto(null.asInstanceOf[LocalDate])
    }
  }

  test("test_isLeapYear") {
    assertEquals(YearMonth.of(2007, 6).isLeapYear, false)
    assertEquals(YearMonth.of(2008, 6).isLeapYear, true)
  }

  test("test_lengthOfMonth_june") {
    val test: YearMonth = YearMonth.of(2007, 6)
    assertEquals(test.lengthOfMonth, 30)
  }

  test("test_lengthOfMonth_febNonLeap") {
    val test: YearMonth = YearMonth.of(2007, 2)
    assertEquals(test.lengthOfMonth, 28)
  }

  test("test_lengthOfMonth_febLeap") {
    val test: YearMonth = YearMonth.of(2008, 2)
    assertEquals(test.lengthOfMonth, 29)
  }

  test("test_lengthOfYear") {
    assertEquals(YearMonth.of(2007, 6).lengthOfYear, 365)
    assertEquals(YearMonth.of(2008, 6).lengthOfYear, 366)
  }

  test("test_isValidDay_int_june") {
    val test: YearMonth = YearMonth.of(2007, 6)
    assertEquals(test.isValidDay(1), true)
    assertEquals(test.isValidDay(30), true)
    assertEquals(test.isValidDay(-1), false)
    assertEquals(test.isValidDay(0), false)
    assertEquals(test.isValidDay(31), false)
    assertEquals(test.isValidDay(32), false)
  }

  test("test_isValidDay_int_febNonLeap") {
    val test: YearMonth = YearMonth.of(2007, 2)
    assertEquals(test.isValidDay(1), true)
    assertEquals(test.isValidDay(28), true)
    assertEquals(test.isValidDay(-1), false)
    assertEquals(test.isValidDay(0), false)
    assertEquals(test.isValidDay(29), false)
    assertEquals(test.isValidDay(32), false)
  }

  test("test_isValidDay_int_febLeap") {
    val test: YearMonth = YearMonth.of(2008, 2)
    assertEquals(test.isValidDay(1), true)
    assertEquals(test.isValidDay(29), true)
    assertEquals(test.isValidDay(-1), false)
    assertEquals(test.isValidDay(0), false)
    assertEquals(test.isValidDay(30), false)
    assertEquals(test.isValidDay(32), false)
  }

  test("test_atDay_int") {
    val test: YearMonth = YearMonth.of(2008, 6)
    assertEquals(test.atDay(30), LocalDate.of(2008, 6, 30))
  }

  test("test_atDay_int_invalidDay") {
    assertThrows[DateTimeException] {
      val test: YearMonth = YearMonth.of(2008, 6)
      test.atDay(31)
    }
  }

  test("test_query") {
    assertEquals(TEST_2008_06.query(TemporalQueries.chronology), IsoChronology.INSTANCE)
    assertEquals(TEST_2008_06.query(TemporalQueries.localDate), null)
    assertEquals(TEST_2008_06.query(TemporalQueries.localTime), null)
    assertEquals(TEST_2008_06.query(TemporalQueries.offset), null)
    assertEquals(TEST_2008_06.query(TemporalQueries.precision), ChronoUnit.MONTHS)
    assertEquals(TEST_2008_06.query(TemporalQueries.zone), null)
    assertEquals(TEST_2008_06.query(TemporalQueries.zoneId), null)
  }

  test("test_query_null") {
    assertThrows[Platform.NPE] {
      TEST_2008_06.query(null)
    }
  }

  test("test_comparisons") {
    doTest_comparisons_YearMonth(
      YearMonth.of(-1, 1),
      YearMonth.of(0, 1),
      YearMonth.of(0, 12),
      YearMonth.of(1, 1),
      YearMonth.of(1, 2),
      YearMonth.of(1, 12),
      YearMonth.of(2008, 1),
      YearMonth.of(2008, 6),
      YearMonth.of(2008, 12)
    )
  }

  private def doTest_comparisons_YearMonth(localDates: YearMonth*): Unit = {
    var i: Int = 0
    while (i < localDates.length) {
      val a: YearMonth = localDates(i)
      var j: Int       = 0
      while (j < localDates.length) {
        val b: YearMonth = localDates(j)
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
          assertEquals(a.compareTo(b), 0)
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
      TEST_2008_06.compareTo(null)
    }
  }

  test("test_isBefore_ObjectNull") {
    assertThrows[Platform.NPE] {
      TEST_2008_06.isBefore(null)
    }
  }

  test("test_isAfter_ObjectNull") {
    assertThrows[Platform.NPE] {
      TEST_2008_06.isAfter(null)
    }
  }

  test("test_equals") {
    val a: YearMonth = YearMonth.of(2008, 6)
    val b: YearMonth = YearMonth.of(2008, 6)
    val c: YearMonth = YearMonth.of(2007, 6)
    val d: YearMonth = YearMonth.of(2008, 5)
    assertEquals(a == a, true)
    assertEquals(a == b, true)
    assertEquals(a == c, false)
    assertEquals(a == d, false)
    assertEquals(b == a, true)
    assertEquals(b == b, true)
    assertEquals(b == c, false)
    assertEquals(b == d, false)
    assertEquals(c == a, false)
    assertEquals(c == b, false)
    assertEquals(c == c, true)
    assertEquals(c == d, false)
    assertEquals(d == a, false)
    assertEquals(d == b, false)
    assertEquals(d == c, false)
    assertEquals(d == d, true)
  }

  test("test_equals_itself_true") {
    assertEquals(TEST_2008_06 == TEST_2008_06, true)
  }

  test("test_equals_string_false") {
    assertEquals(TEST_2008_06 == "2007-07-15", false)
  }

  test("test_equals_null_false") {
    assertEquals(TEST_2008_06 == null, false)
  }

  val provider_sampleToString: List[(Int, Int, String)] =
    List((2008, 1, "2008-01"),
         (2008, 12, "2008-12"),
         (7, 5, "0007-05"),
         (0, 5, "0000-05"),
         (-1, 1, "-0001-01")
    )

  test("test_toString") {
    provider_sampleToString.foreach { case (y, m, expected) =>
      val test: YearMonth = YearMonth.of(y, m)
      val str: String     = test.toString
      assertEquals(str, expected)
    }
  }

  test("test_format_formatter") {
    val f: DateTimeFormatter = DateTimeFormatter.ofPattern("y M")
    val t: String            = YearMonth.of(2010, 12).format(f)
    assertEquals(t, "2010 12")
  }

  test("test_format_formatter_null") {
    assertThrows[NullPointerException] {
      YearMonth.of(2010, 12).format(null)
    }
  }
}
