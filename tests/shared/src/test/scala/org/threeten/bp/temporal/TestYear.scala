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

import org.threeten.bp.Platform
import org.threeten.bp.temporal.ChronoField.ERA
import org.threeten.bp.temporal.ChronoField.YEAR
import org.threeten.bp.temporal.ChronoField.YEAR_OF_ERA
import org.threeten.bp._
import org.threeten.bp.chrono.IsoChronology
import org.threeten.bp.format.DateTimeFormatter
import org.threeten.bp.format.DateTimeParseException

/** Test Year. */
object TestYear {
  private val TEST_2008: Year = Year.of(2008)
}

class TestYear extends GenDateTimeTest {
  protected def samples: List[TemporalAccessor] =
    List(TestYear.TEST_2008)

  protected def validFields: List[TemporalField] =
    List(YEAR_OF_ERA, YEAR, ERA)

  protected def invalidFields: List[TemporalField] = {
    val list: List[TemporalField] = List(ChronoField.values: _*)
    (list :+ JulianFields.JULIAN_DAY :+ JulianFields.MODIFIED_JULIAN_DAY :+ JulianFields.RATA_DIE)
      .filterNot(validFields.contains)
  }

  ignore("test_immutable") {
    // throw new SkipException("private constructor shows up public due to companion object")
    // AbstractTest.assertImmutable(classOf[Year])
  }

  /*
  test("test_serialization") {
    AbstractTest.assertSerializable(Year.of(-1))
  }

  test("test_serialization_format") {
    AbstractTest.assertEqualsSerialisedForm(Year.of(2012))
  }*/

  test("now") {
    var expected: Year = Year.now(Clock.systemDefaultZone)
    var test: Year     = Year.now
    var i: Int         = 0
    while (i < 100 && expected != test) {
      expected = Year.now(Clock.systemDefaultZone)
      test = Year.now
      i += 1
    }
    assertEquals(test, expected)
  }

  test("now_ZoneId_nullZoneId") {
    assertThrows[NullPointerException] {
      Year.now(null.asInstanceOf[ZoneId])
    }
  }

  test("now_ZoneId") {
    val zone: ZoneId   = ZoneId.of("UTC+01:02:03")
    var expected: Year = Year.now(Clock.system(zone))
    var test: Year     = Year.now(zone)
    var i: Int         = 0
    while (i < 100 && expected == test) {
      expected = Year.now(Clock.system(zone))
      test = Year.now(zone)
      i += 1
    }
    assertEquals(test, expected)
  }

  test("now_Clock") {
    val instant: Instant = LocalDateTime.of(2010, 12, 31, 0, 0).toInstant(ZoneOffset.UTC)
    val clock: Clock     = Clock.fixed(instant, ZoneOffset.UTC)
    val test: Year       = Year.now(clock)
    assertEquals(test.getValue, 2010)
  }

  test("now_Clock_nullClock") {
    assertThrows[NullPointerException] {
      Year.now(null.asInstanceOf[Clock])
    }
  }

  test("test_factory_int_singleton") {
    var i: Int = -4
    while (i <= 2104) {
      val test: Year = Year.of(i)
      assertEquals(test.getValue, i)
      assertEquals(Year.of(i), test)
      i += 1
    }
  }

  test("test_factory_int_tooLow") {
    assertThrows[DateTimeException] {
      Year.of(Year.MIN_VALUE - 1)
    }
  }

  test("test_factory_int_tooHigh") {
    assertThrows[DateTimeException] {
      Year.of(Year.MAX_VALUE + 1)
    }
  }

  test("test_factory_CalendricalObject") {
    assertEquals(Year.from(LocalDate.of(2007, 7, 15)), Year.of(2007))
  }

  test("test_factory_CalendricalObject_invalid_noDerive") {
    assertThrows[DateTimeException] {
      Year.from(LocalTime.of(12, 30))
    }
  }

  test("test_factory_CalendricalObject_null") {
    assertThrows[NullPointerException] {
      Year.from(null.asInstanceOf[TemporalAccessor])
    }
  }

  val provider_goodParseData: List[(String, Year)] =
    List(
      ("0000", Year.of(0)),
      ("9999", Year.of(9999)),
      ("2000", Year.of(2000)),
      ("+12345678", Year.of(12345678)),
      ("+123456", Year.of(123456)),
      ("-1234", Year.of(-1234)),
      ("-12345678", Year.of(-12345678)),
      ("+" + Year.MAX_VALUE, Year.of(Year.MAX_VALUE)),
      ("" + Year.MIN_VALUE, Year.of(Year.MIN_VALUE))
    )

  test("factory_parse_success") {
    provider_goodParseData.foreach { case (text, expected) =>
      val year: Year = Year.parse(text)
      assertEquals(year, expected)
    }
  }

  val provider_badParseData: List[(String, Int)] =
    List(
      ("", 0),
      ("-00", 1),
      ("--01-0", 1),
      ("A01", 0),
      ("200", 0),
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
        Year.parse(text)
        fail(f"Parse should have failed for $text%s at position $pos%d")
      } catch {
        case ex: DateTimeParseException =>
          assertEquals(ex.getParsedString, text)
          assertEquals(ex.getErrorIndex, pos)
      }
    }
  }

  test("factory_parse_nullText") {
    assertThrows[NullPointerException] {
      Year.parse(null)
    }
  }

  test("factory_parse_formatter") {
    val f: DateTimeFormatter = DateTimeFormatter.ofPattern("u")
    val test: Year           = Year.parse("2010", f)
    assertEquals(test, Year.of(2010))
  }

  test("factory_parse_formatter_nullText") {
    assertThrows[NullPointerException] {
      val f: DateTimeFormatter = DateTimeFormatter.ofPattern("u")
      Year.parse(null.asInstanceOf[String], f)
    }
  }

  test("factory_parse_formatter_nullFormatter") {
    assertThrows[NullPointerException] {
      Year.parse("ANY", null)
    }
  }

  test("test_get_DateTimeField") {
    assertEquals(TestYear.TEST_2008.getLong(ChronoField.YEAR), 2008)
    assertEquals(TestYear.TEST_2008.getLong(ChronoField.YEAR_OF_ERA), 2008)
    assertEquals(TestYear.TEST_2008.getLong(ChronoField.ERA), 1)
  }

  test("test_get_DateTimeField_null") {
    assertThrows[Platform.NPE] {
      TestYear.TEST_2008.getLong(null.asInstanceOf[TemporalField])
    }
  }

  test("test_get_DateTimeField_invalidField") {
    assertThrows[DateTimeException] {
      TestYear.TEST_2008.getLong(MockFieldNoValue.INSTANCE)
    }
  }

  test("test_get_DateTimeField_timeField") {
    assertThrows[DateTimeException] {
      TestYear.TEST_2008.getLong(ChronoField.AMPM_OF_DAY)
    }
  }

  test("test_isLeap") {
    assertEquals(Year.of(1999).isLeap, false)
    assertEquals(Year.of(2000).isLeap, true)
    assertEquals(Year.of(2001).isLeap, false)
    assertEquals(Year.of(2007).isLeap, false)
    assertEquals(Year.of(2008).isLeap, true)
    assertEquals(Year.of(2009).isLeap, false)
    assertEquals(Year.of(2010).isLeap, false)
    assertEquals(Year.of(2011).isLeap, false)
    assertEquals(Year.of(2012).isLeap, true)
    assertEquals(Year.of(2095).isLeap, false)
    assertEquals(Year.of(2096).isLeap, true)
    assertEquals(Year.of(2097).isLeap, false)
    assertEquals(Year.of(2098).isLeap, false)
    assertEquals(Year.of(2099).isLeap, false)
    assertEquals(Year.of(2100).isLeap, false)
    assertEquals(Year.of(2101).isLeap, false)
    assertEquals(Year.of(2102).isLeap, false)
    assertEquals(Year.of(2103).isLeap, false)
    assertEquals(Year.of(2104).isLeap, true)
    assertEquals(Year.of(2105).isLeap, false)
    assertEquals(Year.of(-500).isLeap, false)
    assertEquals(Year.of(-400).isLeap, true)
    assertEquals(Year.of(-300).isLeap, false)
    assertEquals(Year.of(-200).isLeap, false)
    assertEquals(Year.of(-100).isLeap, false)
    assertEquals(Year.of(0).isLeap, true)
    assertEquals(Year.of(100).isLeap, false)
    assertEquals(Year.of(200).isLeap, false)
    assertEquals(Year.of(300).isLeap, false)
    assertEquals(Year.of(400).isLeap, true)
    assertEquals(Year.of(500).isLeap, false)
  }

  test("test_plusYears") {
    assertEquals(Year.of(2007).plusYears(-1), Year.of(2006))
    assertEquals(Year.of(2007).plusYears(0), Year.of(2007))
    assertEquals(Year.of(2007).plusYears(1), Year.of(2008))
    assertEquals(Year.of(2007).plusYears(2), Year.of(2009))
    assertEquals(Year.of(Year.MAX_VALUE - 1).plusYears(1), Year.of(Year.MAX_VALUE))
    assertEquals(Year.of(Year.MAX_VALUE).plusYears(0), Year.of(Year.MAX_VALUE))
    assertEquals(Year.of(Year.MIN_VALUE + 1).plusYears(-1), Year.of(Year.MIN_VALUE))
    assertEquals(Year.of(Year.MIN_VALUE).plusYears(0), Year.of(Year.MIN_VALUE))
  }

  test("test_plusYear_zero_equals") {
    val base: Year = Year.of(2007)
    assertEquals(base.plusYears(0), base)
  }

  test("test_plusYears_big") {
    val years: Long = 20L + Year.MAX_VALUE
    assertEquals(Year.of(-40).plusYears(years), Year.of((-40L + years).toInt))
  }

  test("test_plusYears_max") {
    assertThrows[DateTimeException] {
      Year.of(Year.MAX_VALUE).plusYears(1)
    }
  }

  test("test_plusYears_maxLots") {
    assertThrows[DateTimeException] {
      Year.of(Year.MAX_VALUE).plusYears(1000)
    }
  }

  test("test_plusYears_min") {
    assertThrows[DateTimeException] {
      Year.of(Year.MIN_VALUE).plusYears(-1)
    }
  }

  test("test_plusYears_minLots") {
    assertThrows[DateTimeException] {
      Year.of(Year.MIN_VALUE).plusYears(-1000)
    }
  }

  test("test_minusYears") {
    assertEquals(Year.of(2007).minusYears(-1), Year.of(2008))
    assertEquals(Year.of(2007).minusYears(0), Year.of(2007))
    assertEquals(Year.of(2007).minusYears(1), Year.of(2006))
    assertEquals(Year.of(2007).minusYears(2), Year.of(2005))
    assertEquals(Year.of(Year.MAX_VALUE - 1).minusYears(-1), Year.of(Year.MAX_VALUE))
    assertEquals(Year.of(Year.MAX_VALUE).minusYears(0), Year.of(Year.MAX_VALUE))
    assertEquals(Year.of(Year.MIN_VALUE + 1).minusYears(1), Year.of(Year.MIN_VALUE))
    assertEquals(Year.of(Year.MIN_VALUE).minusYears(0), Year.of(Year.MIN_VALUE))
  }

  test("test_minusYear_zero_equals") {
    val base: Year = Year.of(2007)
    assertEquals(base.minusYears(0), base)
  }

  test("test_minusYears_big") {
    val years: Long = 20L + Year.MAX_VALUE
    assertEquals(Year.of(40).minusYears(years), Year.of((40L - years).toInt))
  }

  test("test_minusYears_max") {
    assertThrows[DateTimeException] {
      Year.of(Year.MAX_VALUE).minusYears(-1)
    }
  }

  test("test_minusYears_maxLots") {
    assertThrows[DateTimeException] {
      Year.of(Year.MAX_VALUE).minusYears(-1000)
    }
  }

  test("test_minusYears_min") {
    assertThrows[DateTimeException] {
      Year.of(Year.MIN_VALUE).minusYears(1)
    }
  }

  test("test_minusYears_minLots") {
    assertThrows[DateTimeException] {
      Year.of(Year.MIN_VALUE).minusYears(1000)
    }
  }

  test("test_adjustDate") {
    val base: LocalDate = LocalDate.of(2007, 2, 12)
    var i: Int          = -4
    while (i <= 2104) {
      val result: Temporal = Year.of(i).adjustInto(base)
      assertEquals(result, LocalDate.of(i, 2, 12))
      i += 1
    }
  }

  test("test_adjustDate_resolve") {
    val test: Year = Year.of(2011)
    assertEquals(test.adjustInto(LocalDate.of(2012, 2, 29)), LocalDate.of(2011, 2, 28))
  }

  test("test_adjustDate_nullLocalDate") {
    assertThrows[NullPointerException] {
      val test: Year = Year.of(1)
      test.adjustInto(null.asInstanceOf[LocalDate])
    }
  }

  test("test_length") {
    assertEquals(Year.of(1999).length, 365)
    assertEquals(Year.of(2000).length, 366)
    assertEquals(Year.of(2001).length, 365)
    assertEquals(Year.of(2007).length, 365)
    assertEquals(Year.of(2008).length, 366)
    assertEquals(Year.of(2009).length, 365)
    assertEquals(Year.of(2010).length, 365)
    assertEquals(Year.of(2011).length, 365)
    assertEquals(Year.of(2012).length, 366)
    assertEquals(Year.of(2095).length, 365)
    assertEquals(Year.of(2096).length, 366)
    assertEquals(Year.of(2097).length, 365)
    assertEquals(Year.of(2098).length, 365)
    assertEquals(Year.of(2099).length, 365)
    assertEquals(Year.of(2100).length, 365)
    assertEquals(Year.of(2101).length, 365)
    assertEquals(Year.of(2102).length, 365)
    assertEquals(Year.of(2103).length, 365)
    assertEquals(Year.of(2104).length, 366)
    assertEquals(Year.of(2105).length, 365)
    assertEquals(Year.of(-500).length, 365)
    assertEquals(Year.of(-400).length, 366)
    assertEquals(Year.of(-300).length, 365)
    assertEquals(Year.of(-200).length, 365)
    assertEquals(Year.of(-100).length, 365)
    assertEquals(Year.of(0).length, 366)
    assertEquals(Year.of(100).length, 365)
    assertEquals(Year.of(200).length, 365)
    assertEquals(Year.of(300).length, 365)
    assertEquals(Year.of(400).length, 366)
    assertEquals(Year.of(500).length, 365)
  }

  test("test_isValidMonthDay_june") {
    val test: Year         = Year.of(2007)
    val monthDay: MonthDay = MonthDay.of(6, 30)
    assertEquals(test.isValidMonthDay(monthDay), true)
  }

  test("test_isValidMonthDay_febNonLeap") {
    val test: Year         = Year.of(2007)
    val monthDay: MonthDay = MonthDay.of(2, 29)
    assertEquals(test.isValidMonthDay(monthDay), false)
  }

  test("test_isValidMonthDay_febLeap") {
    val test: Year         = Year.of(2008)
    val monthDay: MonthDay = MonthDay.of(2, 29)
    assertEquals(test.isValidMonthDay(monthDay), true)
  }

  test("test_isValidMonthDay_null") {
    val test: Year = Year.of(2008)
    assertEquals(test.isValidMonthDay(null), false)
  }

  test("test_atMonth") {
    val test: Year = Year.of(2008)
    assertEquals(test.atMonth(Month.JUNE), YearMonth.of(2008, 6))
  }

  test("test_atMonth_nullMonth") {
    assertThrows[NullPointerException] {
      val test: Year = Year.of(2008)
      test.atMonth(null.asInstanceOf[Month])
    }
  }

  test("test_atMonth_int") {
    val test: Year = Year.of(2008)
    assertEquals(test.atMonth(6), YearMonth.of(2008, 6))
  }

  test("test_atMonth_int_invalidMonth") {
    assertThrows[DateTimeException] {
      val test: Year = Year.of(2008)
      test.atMonth(13)
    }
  }

  val data_atMonthDay: List[(Year, MonthDay, LocalDate)] =
    List(
      (Year.of(2008), MonthDay.of(6, 30), LocalDate.of(2008, 6, 30)),
      (Year.of(2008), MonthDay.of(2, 29), LocalDate.of(2008, 2, 29)),
      (Year.of(2009), MonthDay.of(2, 29), LocalDate.of(2009, 2, 28))
    )

  test("test_atMonthDay") {
    data_atMonthDay.foreach { case (year, monthDay, expected) =>
      assertEquals(year.atMonthDay(monthDay), expected)
    }
  }

  test("test_atMonthDay_nullMonthDay") {
    assertThrows[Platform.NPE] {
      val test: Year = Year.of(2008)
      test.atMonthDay(null.asInstanceOf[MonthDay])
    }
  }

  test("test_atDay_notLeapYear") {
    val test: Year          = Year.of(2007)
    var expected: LocalDate = LocalDate.of(2007, 1, 1)
    var i: Int              = 1
    while (i <= 365) {
      assertEquals(test.atDay(i), expected)
      expected = expected.plusDays(1)
      i += 1
    }
  }

  test("test_atDay_notLeapYear_day366") {
    assertThrows[DateTimeException] {
      val test: Year = Year.of(2007)
      test.atDay(366)
    }
  }

  test("test_atDay_leapYear") {
    val test: Year          = Year.of(2008)
    var expected: LocalDate = LocalDate.of(2008, 1, 1)
    var i: Int              = 1
    while (i <= 366) {
      assertEquals(test.atDay(i), expected)
      expected = expected.plusDays(1)
      i += 1
    }
  }

  test("test_atDay_day0") {
    assertThrows[DateTimeException] {
      val test: Year = Year.of(2007)
      test.atDay(0)
    }
  }

  test("test_atDay_day367") {
    assertThrows[DateTimeException] {
      val test: Year = Year.of(2007)
      test.atDay(367)
    }
  }

  test("test_query") {
    assertEquals(TestYear.TEST_2008.query(TemporalQueries.chronology), IsoChronology.INSTANCE)
    assertEquals(TestYear.TEST_2008.query(TemporalQueries.localDate), null)
    assertEquals(TestYear.TEST_2008.query(TemporalQueries.localTime), null)
    assertEquals(TestYear.TEST_2008.query(TemporalQueries.offset), null)
    assertEquals(TestYear.TEST_2008.query(TemporalQueries.precision), ChronoUnit.YEARS)
    assertEquals(TestYear.TEST_2008.query(TemporalQueries.zone), null)
    assertEquals(TestYear.TEST_2008.query(TemporalQueries.zoneId), null)
  }

  test("test_query_null") {
    assertThrows[Platform.NPE] {
      TestYear.TEST_2008.query(null)
    }
  }

  test("test_compareTo") {
    var i: Int = -4
    while (i <= 2104) {
      val a: Year = Year.of(i)
      var j: Int  = -4
      while (j <= 2104) {
        val b: Year = Year.of(j)
        if (i < j) {
          assertEquals(a.compareTo(b) < 0, true)
          assertEquals(b.compareTo(a) > 0, true)
          assertEquals(a.isAfter(b), false)
          assertEquals(a.isBefore(b), true)
          assertEquals(b.isAfter(a), true)
          assertEquals(b.isBefore(a), false)
        } else if (i > j) {
          assertEquals(a.compareTo(b) > 0, true)
          assertEquals(b.compareTo(a) < 0, true)
          assertEquals(a.isAfter(b), true)
          assertEquals(a.isBefore(b), false)
          assertEquals(b.isAfter(a), false)
          assertEquals(b.isBefore(a), true)
        } else {
          assertEquals(a.compareTo(b), 0)
          assertEquals(b.compareTo(a), 0)
          assertEquals(a.isAfter(b), false)
          assertEquals(a.isBefore(b), false)
          assertEquals(b.isAfter(a), false)
          assertEquals(b.isBefore(a), false)
        }
        j += 1
      }
      i += 1
    }
  }

  test("test_compareTo_nullYear") {
    assertThrows[Platform.NPE] {
      val doy: Year  = null
      val test: Year = Year.of(1)
      test.compareTo(doy)
    }
  }

  test("test_equals") {
    var i: Int = -4
    while (i <= 2104) {
      val a: Year = Year.of(i)
      var j: Int  = -4
      while (j <= 2104) {
        val b: Year = Year.of(j)
        assertEquals(a == b, i == j)
        assertEquals(a.hashCode == b.hashCode, i == j)
        j += 1
      }
      i += 1
    }
  }

  test("test_equals_same") {
    val test: Year = Year.of(2011)
    assertEquals(test == test, true)
  }

  test("test_equals_nullYear") {
    val doy: Year  = null
    val test: Year = Year.of(1)
    assertEquals(test == doy, false)
  }

  test("test_equals_incorrectType") {
    val test: Year = Year.of(1)
    assertEquals(test == "Incorrect type", false)
  }

  test("test_toString") {
    var i: Int = -4
    while (i <= 2104) {
      val a: Year = Year.of(i)
      assertEquals(a.toString, "" + i)
      i += 1
    }
  }

  test("test_format_formatter") {
    val f: DateTimeFormatter = DateTimeFormatter.ofPattern("y")
    val t: String            = Year.of(2010).format(f)
    assertEquals(t, "2010")
  }

  test("format_formatter_null") {
    assertThrows[NullPointerException] {
      Year.of(2010).format(null)
    }
  }
}
