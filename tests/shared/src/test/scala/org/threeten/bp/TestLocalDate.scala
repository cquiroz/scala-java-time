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
import org.scalatest.BeforeAndAfter
import org.threeten.bp.chrono.IsoChronology
import org.threeten.bp.format.{ DateTimeFormatter, DateTimeParseException }
import org.threeten.bp.temporal.ChronoField._
import org.threeten.bp.temporal.ChronoUnit._
import org.threeten.bp.temporal._

/** Test LocalDate. */
object TestLocalDate {
  private val OFFSET_PONE: ZoneOffset = ZoneOffset.ofHours(1)
  private val ZONE_PARIS: ZoneId      = ZoneId.of("Europe/Paris")
  private val ZONE_GAZA: ZoneId       = ZoneId.of("Asia/Gaza")
}

class TestLocalDate
    extends AnyFunSuite
    with GenDateTimeTest
    with AssertionsHelper
    with BeforeAndAfter {
  private var TEST_2007_07_15: LocalDate = null
  private var MAX_VALID_EPOCHDAYS: Long  = 0L
  private var MIN_VALID_EPOCHDAYS: Long  = 0L
  private var MAX_DATE: LocalDate        = null
  private var MIN_DATE: LocalDate        = null
  private var MAX_INSTANT: Instant       = null
  private var MIN_INSTANT: Instant       = null

  before {
    TEST_2007_07_15 = LocalDate.of(2007, 7, 15)
    val max: LocalDate = LocalDate.MAX
    val min: LocalDate = LocalDate.MIN
    MAX_VALID_EPOCHDAYS = max.toEpochDay
    MIN_VALID_EPOCHDAYS = min.toEpochDay
    MAX_DATE = max
    MIN_DATE = min
    MAX_INSTANT = max.atStartOfDay(ZoneOffset.UTC).toInstant
    MIN_INSTANT = min.atStartOfDay(ZoneOffset.UTC).toInstant
  }

  protected def samples: List[TemporalAccessor] =
    List(TEST_2007_07_15, LocalDate.MAX, LocalDate.MIN)

  protected def validFields: List[TemporalField] =
    List(
      DAY_OF_WEEK,
      ALIGNED_DAY_OF_WEEK_IN_MONTH,
      ALIGNED_DAY_OF_WEEK_IN_YEAR,
      DAY_OF_MONTH,
      DAY_OF_YEAR,
      EPOCH_DAY,
      ALIGNED_WEEK_OF_MONTH,
      ALIGNED_WEEK_OF_YEAR,
      MONTH_OF_YEAR,
      PROLEPTIC_MONTH,
      YEAR_OF_ERA,
      YEAR,
      ERA,
      JulianFields.JULIAN_DAY,
      JulianFields.MODIFIED_JULIAN_DAY,
      JulianFields.RATA_DIE
    )

  protected def invalidFields: List[TemporalField] =
    List(ChronoField.values: _*).filterNot(validFields.contains)

  private def check(test_2008_02_29: LocalDate, y: Int, m: Int, d: Int): Unit = {
    assertEquals(test_2008_02_29.getYear, y)
    assertEquals(test_2008_02_29.getMonth.getValue, m)
    assertEquals(test_2008_02_29.getDayOfMonth, d)
  }

  test("now") {
    var expected: LocalDate = LocalDate.now(Clock.systemDefaultZone)
    var test: LocalDate     = LocalDate.now

    {
      var i: Int = 0
      while (i < 100) {
        {
          if (expected != test) {
            expected = LocalDate.now(Clock.systemDefaultZone)
            test = LocalDate.now
          }
        }
        {
          i += 1
          i - 1
        }
      }
    }
    assertEquals(test, expected)
  }

  test("now_ZoneId_nullZoneId") {
    assertThrows[NullPointerException] {
      LocalDate.now(null.asInstanceOf[ZoneId])
    }
  }

  test("now_ZoneId") {
    val zone: ZoneId        = ZoneId.of("UTC+01:02:03")
    var expected: LocalDate = LocalDate.now(Clock.system(zone))
    var test: LocalDate     = LocalDate.now(zone)

    {
      var i: Int = 0
      while (i < 100) {
        {
          if (expected != test) {
            expected = LocalDate.now(Clock.system(zone))
            test = LocalDate.now(zone)
          }
        }
        {
          i += 1
          i - 1
        }
      }
    }
    assertEquals(test, expected)
  }

  test("now_Clock_nullClock") {
    assertThrows[NullPointerException] {
      LocalDate.now(null.asInstanceOf[Clock])
    }
  }

  test("now_Clock_allSecsInDay_utc") {
    var i: Int = 0
    while (i < (2 * 24 * 60 * 60)) {
      {
        val instant: Instant = Instant.ofEpochSecond(i)
        val clock: Clock     = Clock.fixed(instant, ZoneOffset.UTC)
        val test: LocalDate  = LocalDate.now(clock)
        assertEquals(test.getYear, 1970)
        assertEquals(test.getMonth, Month.JANUARY)
        assertEquals(test.getDayOfMonth, if (i < 24 * 60 * 60) 1 else 2)
      }
      {
        i += 1
        i - 1
      }
    }
  }

  test("now_Clock_allSecsInDay_offset") {
    var i: Int = 0
    while (i < (2 * 24 * 60 * 60)) {
      {
        val instant: Instant = Instant.ofEpochSecond(i)
        val clock: Clock     =
          Clock.fixed(instant.minusSeconds(TestLocalDate.OFFSET_PONE.getTotalSeconds),
                      TestLocalDate.OFFSET_PONE
          )
        val test: LocalDate  = LocalDate.now(clock)
        assertEquals(test.getYear, 1970)
        assertEquals(test.getMonth, Month.JANUARY)
        assertEquals(test.getDayOfMonth, if (i < 24 * 60 * 60) 1 else 2)
      }
      {
        i += 1
        i - 1
      }
    }
  }

  test("now_Clock_allSecsInDay_beforeEpoch") {
    var i: Int = -1
    while (i >= -(2 * 24 * 60 * 60)) {
      {
        val instant: Instant = Instant.ofEpochSecond(i)
        val clock: Clock     = Clock.fixed(instant, ZoneOffset.UTC)
        val test: LocalDate  = LocalDate.now(clock)
        assertEquals(test.getYear, 1969)
        assertEquals(test.getMonth, Month.DECEMBER)
        assertEquals(test.getDayOfMonth, if (i >= -24 * 60 * 60) 31 else 30)
      }
      {
        i -= 1
        i + 1
      }
    }
  }

  test("now_Clock_maxYear") {
    val clock: Clock    = Clock.fixed(MAX_INSTANT, ZoneOffset.UTC)
    val test: LocalDate = LocalDate.now(clock)
    assertEquals(test, MAX_DATE)
  }

  test("now_Clock_tooBig") {
    assertThrows[DateTimeException] {
      val clock: Clock = Clock.fixed(MAX_INSTANT.plusSeconds(24 * 60 * 60), ZoneOffset.UTC)
      LocalDate.now(clock)
    }
  }

  test("now_Clock_minYear") {
    val clock: Clock    = Clock.fixed(MIN_INSTANT, ZoneOffset.UTC)
    val test: LocalDate = LocalDate.now(clock)
    assertEquals(test, MIN_DATE)
  }

  test("now_Clock_tooLow") {
    assertThrows[DateTimeException] {
      val clock: Clock = Clock.fixed(MIN_INSTANT.minusNanos(1), ZoneOffset.UTC)
      LocalDate.now(clock)
    }
  }

  test("factory_of_intsMonth") {
    assertEquals(TEST_2007_07_15, LocalDate.of(2007, Month.JULY, 15))
  }

  test("factory_of_intsMonth_29febNonLeap") {
    assertThrows[DateTimeException] {
      LocalDate.of(2007, Month.FEBRUARY, 29)
    }
  }

  test("factory_of_intsMonth_31apr") {
    assertThrows[DateTimeException] {
      LocalDate.of(2007, Month.APRIL, 31)
    }
  }

  test("factory_of_intsMonth_dayTooLow") {
    assertThrows[DateTimeException] {
      LocalDate.of(2007, Month.JANUARY, 0)
    }
  }

  test("factory_of_intsMonth_dayTooHigh") {
    assertThrows[DateTimeException] {
      LocalDate.of(2007, Month.JANUARY, 32)
    }
  }

  test("factory_of_intsMonth_nullMonth") {
    assertThrows[NullPointerException] {
      LocalDate.of(2007, null, 30)
    }
  }

  test("factory_of_intsMonth_yearTooLow") {
    assertThrows[DateTimeException] {
      LocalDate.of(Integer.MIN_VALUE, Month.JANUARY, 1)
    }
  }

  test("factory_of_ints") {
    check(TEST_2007_07_15, 2007, 7, 15)
  }

  test("factory_of_ints_29febNonLeap") {
    assertThrows[DateTimeException] {
      LocalDate.of(2007, 2, 29)
    }
  }

  test("factory_of_ints_31apr") {
    assertThrows[DateTimeException] {
      LocalDate.of(2007, 4, 31)
    }
  }

  test("factory_of_ints_dayTooLow") {
    assertThrows[DateTimeException] {
      LocalDate.of(2007, 1, 0)
    }
  }

  test("factory_of_ints_dayTooHigh") {
    assertThrows[DateTimeException] {
      LocalDate.of(2007, 1, 32)
    }
  }

  test("factory_of_ints_monthTooLow") {
    assertThrows[DateTimeException] {
      LocalDate.of(2007, 0, 1)
    }
  }

  test("factory_of_ints_monthTooHigh") {
    assertThrows[DateTimeException] {
      LocalDate.of(2007, 13, 1)
    }
  }

  test("factory_of_ints_yearTooLow") {
    assertThrows[DateTimeException] {
      LocalDate.of(Integer.MIN_VALUE, 1, 1)
    }
  }

  test("factory_ofYearDay_ints_nonLeap") {
    var date: LocalDate = LocalDate.of(2007, 1, 1)

    {
      var i: Int = 1
      while (i <= 365) {
        {
          assertEquals(LocalDate.ofYearDay(2007, i), date)
          date = next(date)
        }
        {
          i += 1
          i - 1
        }
      }
    }
  }

  test("factory_ofYearDay_ints_leap") {
    var date: LocalDate = LocalDate.of(2008, 1, 1)

    {
      var i: Int = 1
      while (i <= 366) {
        {
          assertEquals(LocalDate.ofYearDay(2008, i), date)
          date = next(date)
        }
        {
          i += 1
          i - 1
        }
      }
    }
  }

  test("factory_ofYearDay_ints_366nonLeap") {
    assertThrows[DateTimeException] {
      LocalDate.ofYearDay(2007, 366)
    }
  }

  test("factory_ofYearDay_ints_dayTooLow") {
    assertThrows[DateTimeException] {
      LocalDate.ofYearDay(2007, 0)
    }
  }

  test("factory_ofYearDay_ints_dayTooHigh") {
    assertThrows[DateTimeException] {
      LocalDate.ofYearDay(2007, 367)
    }
  }

  test("factory_ofYearDay_ints_yearTooLow") {
    assertThrows[DateTimeException] {
      LocalDate.ofYearDay(Integer.MIN_VALUE, 1)
    }
  }

  private def next(date: LocalDate): LocalDate = {
    var _date              = date
    val newDayOfMonth: Int = _date.getDayOfMonth + 1
    if (newDayOfMonth <= _date.getMonth.length(isIsoLeap(_date.getYear)))
      return _date.withDayOfMonth(newDayOfMonth)
    _date = _date.withDayOfMonth(1)
    if (_date.getMonth eq Month.DECEMBER)
      _date = _date.withYear(_date.getYear + 1)
    _date.`with`(_date.getMonth.plus(1))
  }

  private def previous(date: LocalDate): LocalDate = {
    var _date              = date
    val newDayOfMonth: Int = _date.getDayOfMonth - 1
    if (newDayOfMonth > 0)
      return _date.withDayOfMonth(newDayOfMonth)
    _date = _date.`with`(_date.getMonth.minus(1))
    if (_date.getMonth eq Month.DECEMBER)
      _date = _date.withYear(_date.getYear - 1)
    _date.withDayOfMonth(_date.getMonth.length(isIsoLeap(_date.getYear)))
  }

  test("factory_ofEpochDay") {
    val date_0000_01_01: Long = -678941 - 40587
    assertEquals(LocalDate.ofEpochDay(0), LocalDate.of(1970, 1, 1))
    assertEquals(LocalDate.ofEpochDay(date_0000_01_01), LocalDate.of(0, 1, 1))
    assertEquals(LocalDate.ofEpochDay(date_0000_01_01 - 1), LocalDate.of(-1, 12, 31))
    assertEquals(LocalDate.ofEpochDay(MAX_VALID_EPOCHDAYS), LocalDate.of(Year.MAX_VALUE, 12, 31))
    assertEquals(LocalDate.ofEpochDay(MIN_VALID_EPOCHDAYS), LocalDate.of(Year.MIN_VALUE, 1, 1))
    var test: LocalDate       = LocalDate.of(0, 1, 1)

    {
      var i: Long = date_0000_01_01
      while (i < 700000) {
        {
          assertEquals(LocalDate.ofEpochDay(i), test)
          test = next(test)
        }
        {
          i += 1
          i - 1
        }
      }
    }
    test = LocalDate.of(0, 1, 1)

    {
      var i: Long = date_0000_01_01
      while (i > -2000000) {
        {
          assertEquals(LocalDate.ofEpochDay(i), test)
          test = previous(test)
        }
        {
          i -= 1
          i + 1
        }
      }
    }
  }

  test("factory_ofEpochDay_aboveMax") {
    assertThrows[DateTimeException] {
      LocalDate.ofEpochDay(MAX_VALID_EPOCHDAYS + 1)
    }
  }

  test("factory_ofEpochDay_belowMin") {
    assertThrows[DateTimeException] {
      LocalDate.ofEpochDay(MIN_VALID_EPOCHDAYS - 1)
    }
  }

  test("factory_CalendricalObject") {
    assertEquals(LocalDate.from(LocalDate.of(2007, 7, 15)), LocalDate.of(2007, 7, 15))
    assertEquals(LocalDate.from(LocalDateTime.of(2007, 7, 15, 12, 30)), LocalDate.of(2007, 7, 15))
  }

  test("factory_CalendricalObject_invalid_noDerive") {
    assertThrows[DateTimeException] {
      LocalDate.from(LocalTime.of(12, 30))
    }
  }

  test("test_factory_CalendricalObject_null") {
    assertThrows[Platform.NPE] {
      LocalDate.from(null.asInstanceOf[TemporalAccessor])
    }
  }

  test("factory_parse_validText") {
    provider_sampleToString.foreach {
      case (y: Int) :: (m: Int) :: (d: Int) :: (parsable: String) :: Nil =>
        val t: LocalDate = LocalDate.parse(parsable)
        assertNotNull(t, parsable)
        assertEquals(t.getYear, y, parsable)
        assertEquals(t.getMonth.getValue, m, parsable)
        assertEquals(t.getDayOfMonth, d, parsable)
      case _                                                             =>
        fail()
    }
  }

  def provider_sampleBadParse: List[String] =
    List(
      "2008/07/05",
      "10000-01-01",
      "2008-1-1",
      "2008--01",
      "ABCD-02-01",
      "2008-AB-01",
      "2008-02-AB",
      "-0000-02-01",
      "2008-02-01Z",
      "2008-02-01+01:00",
      "2008-02-01+01:00[Europe/Paris]"
    )

  test("factory_parse_invalidText") {
    provider_sampleBadParse.foreach { unparsable =>
      assertThrows[DateTimeParseException] {
        LocalDate.parse(unparsable)
      }
    }
  }

  test("factory_parse_illegalValue") {
    assertThrows[DateTimeParseException] {
      LocalDate.parse("2008-06-32")
    }
  }

  test("factory_parse_invalidValue") {
    assertThrows[DateTimeParseException] {
      LocalDate.parse("2008-06-31")
    }
  }

  test("factory_parse_nullText") {
    assertThrows[NullPointerException] {
      LocalDate.parse(null.asInstanceOf[String])
    }
  }

  test("factory_parse_formatter") {
    val f: DateTimeFormatter = DateTimeFormatter.ofPattern("u M d")
    val test: LocalDate      = LocalDate.parse("2010 12 3", f)
    assertEquals(test, LocalDate.of(2010, 12, 3))
  }

  test("factory_parse_formatter_nullText") {
    assertThrows[NullPointerException] {
      val f: DateTimeFormatter = DateTimeFormatter.ofPattern("u M d")
      LocalDate.parse(null.asInstanceOf[String], f)
    }
  }

  test("ry_parse_formatter_nullFormatter") {
    assertThrows[NullPointerException] {
      LocalDate.parse("ANY", null)
    }
  }

  test("get_TemporalField") {
    val test: LocalDate = LocalDate.of(2008, 6, 30)
    assertEquals(test.get(YEAR), 2008)
    assertEquals(test.get(MONTH_OF_YEAR), 6)
    assertEquals(test.get(DAY_OF_MONTH), 30)
    assertEquals(test.get(DAY_OF_WEEK), 1)
    assertEquals(test.get(DAY_OF_YEAR), 182)
    assertEquals(test.get(YEAR_OF_ERA), 2008)
    assertEquals(test.get(ERA), 1)
  }

  test("get_TemporalField_tooBig") {
    assertThrows[DateTimeException] {
      TEST_2007_07_15.get(EPOCH_DAY)
    }
  }

  test("get_TemporalField_null") {
    assertThrows[Platform.NPE] {
      TEST_2007_07_15.get(null.asInstanceOf[TemporalField])
    }
  }

  test("get_TemporalField_invalidField") {
    assertThrows[DateTimeException] {
      TEST_2007_07_15.get(MockFieldNoValue.INSTANCE)
    }
  }

  test("get_TemporalField_timeField") {
    assertThrows[DateTimeException] {
      TEST_2007_07_15.get(ChronoField.AMPM_OF_DAY)
    }
  }

  test("getLong_TemporalField") {
    val test: LocalDate = LocalDate.of(2008, 6, 30)
    assertEquals(test.getLong(YEAR), 2008)
    assertEquals(test.getLong(MONTH_OF_YEAR), 6)
    assertEquals(test.getLong(DAY_OF_MONTH), 30)
    assertEquals(test.getLong(DAY_OF_WEEK), 1)
    assertEquals(test.getLong(DAY_OF_YEAR), 182)
    assertEquals(test.getLong(YEAR_OF_ERA), 2008)
    assertEquals(test.getLong(ERA), 1)
    assertEquals(test.getLong(PROLEPTIC_MONTH), 2008 * 12 + 6 - 1)
  }

  test("getLong_TemporalField_null") {
    assertThrows[Platform.NPE] {
      TEST_2007_07_15.getLong(null.asInstanceOf[TemporalField])
    }
  }

  test("getLong_TemporalField_invalidField") {
    assertThrows[DateTimeException] {
      TEST_2007_07_15.getLong(MockFieldNoValue.INSTANCE)
    }
  }

  test("getLong_TemporalField_timeField") {
    assertThrows[DateTimeException] {
      TEST_2007_07_15.getLong(ChronoField.AMPM_OF_DAY)
    }
  }

  test("query") {
    assertEquals(TEST_2007_07_15.query(TemporalQueries.chronology), IsoChronology.INSTANCE)
    assertEquals(TEST_2007_07_15.query(TemporalQueries.localDate), TEST_2007_07_15)
    assertEquals(TEST_2007_07_15.query(TemporalQueries.localTime), null)
    assertEquals(TEST_2007_07_15.query(TemporalQueries.offset), null)
    assertEquals(TEST_2007_07_15.query(TemporalQueries.precision), ChronoUnit.DAYS)
    assertEquals(TEST_2007_07_15.query(TemporalQueries.zone), null)
    assertEquals(TEST_2007_07_15.query(TemporalQueries.zoneId), null)
  }

  test("query_null") {
    assertThrows[Platform.NPE] {
      TEST_2007_07_15.query(null)
    }
  }

  def provider_sampleDates: List[List[Int]] =
    List(List(2008, 7, 5),
         List(2007, 7, 5),
         List(2006, 7, 5),
         List(2005, 7, 5),
         List(2004, 1, 1),
         List(-1, 1, 2)
    )

  test("get") {
    provider_sampleDates.foreach {
      case (y: Int) :: (m: Int) :: (d: Int) :: Nil =>
        val a: LocalDate = LocalDate.of(y, m, d)
        assertEquals(a.getYear, y)
        assertEquals(a.getMonth, Month.of(m))
        assertEquals(a.getDayOfMonth, d)
      case _                                       =>
        fail()
    }
  }

  test("getDOY") {
    provider_sampleDates.foreach {
      case (y: Int) :: (m: Int) :: (d: Int) :: Nil =>
        val a: LocalDate = LocalDate.of(y, m, d)
        var total: Int   = 0

        {
          var i: Int = 1
          while (i < m) {
            {
              total += Month.of(i).length(isIsoLeap(y))
            }
            {
              i += 1
              i - 1
            }
          }
        }
        val doy: Int = total + d
        assertEquals(a.getDayOfYear, doy)
      case _                                       =>
        fail()
    }
  }

  test("getDayOfWeek") {
    var dow: DayOfWeek = DayOfWeek.MONDAY
    for (month <- Month.values) {
      val length: Int = month.length(false)

      {
        var i: Int = 1
        while (i <= length) {
          {
            val d: LocalDate = LocalDate.of(2007, month, i)
            assertSame(d.getDayOfWeek, dow)
            dow = dow.plus(1)
          }
          {
            i += 1
            i - 1
          }
        }
      }
    }
  }

  test("isLeapYear") {
    assertEquals(LocalDate.of(1999, 1, 1).isLeapYear, false)
    assertEquals(LocalDate.of(2000, 1, 1).isLeapYear, true)
    assertEquals(LocalDate.of(2001, 1, 1).isLeapYear, false)
    assertEquals(LocalDate.of(2002, 1, 1).isLeapYear, false)
    assertEquals(LocalDate.of(2003, 1, 1).isLeapYear, false)
    assertEquals(LocalDate.of(2004, 1, 1).isLeapYear, true)
    assertEquals(LocalDate.of(2005, 1, 1).isLeapYear, false)
    assertEquals(LocalDate.of(1500, 1, 1).isLeapYear, false)
    assertEquals(LocalDate.of(1600, 1, 1).isLeapYear, true)
    assertEquals(LocalDate.of(1700, 1, 1).isLeapYear, false)
    assertEquals(LocalDate.of(1800, 1, 1).isLeapYear, false)
    assertEquals(LocalDate.of(1900, 1, 1).isLeapYear, false)
  }

  test("lengthOfMonth_notLeapYear") {
    assertEquals(LocalDate.of(2007, 1, 1).lengthOfMonth, 31)
    assertEquals(LocalDate.of(2007, 2, 1).lengthOfMonth, 28)
    assertEquals(LocalDate.of(2007, 3, 1).lengthOfMonth, 31)
    assertEquals(LocalDate.of(2007, 4, 1).lengthOfMonth, 30)
    assertEquals(LocalDate.of(2007, 5, 1).lengthOfMonth, 31)
    assertEquals(LocalDate.of(2007, 6, 1).lengthOfMonth, 30)
    assertEquals(LocalDate.of(2007, 7, 1).lengthOfMonth, 31)
    assertEquals(LocalDate.of(2007, 8, 1).lengthOfMonth, 31)
    assertEquals(LocalDate.of(2007, 9, 1).lengthOfMonth, 30)
    assertEquals(LocalDate.of(2007, 10, 1).lengthOfMonth, 31)
    assertEquals(LocalDate.of(2007, 11, 1).lengthOfMonth, 30)
    assertEquals(LocalDate.of(2007, 12, 1).lengthOfMonth, 31)
  }

  test("lengthOfMonth_leapYear") {
    assertEquals(LocalDate.of(2008, 1, 1).lengthOfMonth, 31)
    assertEquals(LocalDate.of(2008, 2, 1).lengthOfMonth, 29)
    assertEquals(LocalDate.of(2008, 3, 1).lengthOfMonth, 31)
    assertEquals(LocalDate.of(2008, 4, 1).lengthOfMonth, 30)
    assertEquals(LocalDate.of(2008, 5, 1).lengthOfMonth, 31)
    assertEquals(LocalDate.of(2008, 6, 1).lengthOfMonth, 30)
    assertEquals(LocalDate.of(2008, 7, 1).lengthOfMonth, 31)
    assertEquals(LocalDate.of(2008, 8, 1).lengthOfMonth, 31)
    assertEquals(LocalDate.of(2008, 9, 1).lengthOfMonth, 30)
    assertEquals(LocalDate.of(2008, 10, 1).lengthOfMonth, 31)
    assertEquals(LocalDate.of(2008, 11, 1).lengthOfMonth, 30)
    assertEquals(LocalDate.of(2008, 12, 1).lengthOfMonth, 31)
  }

  test("lengthOfYear") {
    assertEquals(LocalDate.of(2007, 1, 1).lengthOfYear, 365)
    assertEquals(LocalDate.of(2008, 1, 1).lengthOfYear, 366)
  }

  test("with_adjustment") {
    val sample: LocalDate          = LocalDate.of(2012, 3, 4)
    val adjuster: TemporalAdjuster = new TemporalAdjuster() {
      def adjustInto(dateTime: Temporal): Temporal =
        sample
    }
    assertEquals(TEST_2007_07_15.`with`(adjuster), sample)
  }

  test("with_adjustment_null") {
    assertThrows[Platform.NPE] {
      TEST_2007_07_15.`with`(null.asInstanceOf[TemporalAdjuster])
    }
  }

  test("with_DateTimeField_long_normal") {
    val t: LocalDate = TEST_2007_07_15.`with`(YEAR, 2008)
    assertEquals(t, LocalDate.of(2008, 7, 15))
  }

  test("with_DateTimeField_long_null") {
    assertThrows[Platform.NPE] {
      TEST_2007_07_15.`with`(null.asInstanceOf[TemporalField], 1)
    }
  }

  test("with_DateTimeField_long_invalidField") {
    assertThrows[DateTimeException] {
      TEST_2007_07_15.`with`(MockFieldNoValue.INSTANCE, 1)
    }
  }

  test("with_DateTimeField_long_timeField") {
    assertThrows[DateTimeException] {
      TEST_2007_07_15.`with`(ChronoField.AMPM_OF_DAY, 1)
    }
  }

  test("with_DateTimeField_long_invalidValue") {
    assertThrows[DateTimeException] {
      TEST_2007_07_15.`with`(ChronoField.DAY_OF_WEEK, -1)
    }
  }

  test("withYear_int_normal") {
    val t: LocalDate = TEST_2007_07_15.withYear(2008)
    assertEquals(t, LocalDate.of(2008, 7, 15))
  }

  test("withYear_int_invalid") {
    assertThrows[DateTimeException] {
      TEST_2007_07_15.withYear(Year.MIN_VALUE - 1)
    }
  }

  test("withYear_int_adjustDay") {
    val t: LocalDate        = LocalDate.of(2008, 2, 29).withYear(2007)
    val expected: LocalDate = LocalDate.of(2007, 2, 28)
    assertEquals(t, expected)
  }

  test("withMonth_int_normal") {
    val t: LocalDate = TEST_2007_07_15.withMonth(1)
    assertEquals(t, LocalDate.of(2007, 1, 15))
  }

  test("withMonth_int_invalid") {
    assertThrows[DateTimeException] {
      TEST_2007_07_15.withMonth(13)
    }
  }

  test("withMonth_int_adjustDay") {
    val t: LocalDate        = LocalDate.of(2007, 12, 31).withMonth(11)
    val expected: LocalDate = LocalDate.of(2007, 11, 30)
    assertEquals(t, expected)
  }

  test("withDayOfMonth_normal") {
    val t: LocalDate = TEST_2007_07_15.withDayOfMonth(1)
    assertEquals(t, LocalDate.of(2007, 7, 1))
  }

  test("withDayOfMonth_illegal") {
    assertThrows[DateTimeException] {
      TEST_2007_07_15.withDayOfMonth(32)
    }
  }

  test("withDayOfMonth_invalid") {
    assertThrows[DateTimeException] {
      LocalDate.of(2007, 11, 30).withDayOfMonth(31)
    }
  }

  test("withDayOfYear_normal") {
    val t: LocalDate = TEST_2007_07_15.withDayOfYear(33)
    assertEquals(t, LocalDate.of(2007, 2, 2))
  }

  test("withDayOfYear_illegal") {
    assertThrows[DateTimeException] {
      TEST_2007_07_15.withDayOfYear(367)
    }
  }

  test("withDayOfYear_invalid") {
    assertThrows[DateTimeException] {
      TEST_2007_07_15.withDayOfYear(366)
    }
  }

  test("plus_Period_positiveMonths") {
    val period: MockSimplePeriod = MockSimplePeriod.of(7, ChronoUnit.MONTHS)
    val t: LocalDate             = TEST_2007_07_15.plus(period)
    assertEquals(t, LocalDate.of(2008, 2, 15))
  }

  test("plus_Period_negativeDays") {
    val period: MockSimplePeriod = MockSimplePeriod.of(-25, ChronoUnit.DAYS)
    val t: LocalDate             = TEST_2007_07_15.plus(period)
    assertEquals(t, LocalDate.of(2007, 6, 20))
  }

  test("plus_Period_timeNotAllowed") {
    assertThrows[DateTimeException] {
      val period: MockSimplePeriod = MockSimplePeriod.of(7, ChronoUnit.HOURS)
      TEST_2007_07_15.plus(period)
    }
  }

  test("plus_Period_null") {
    assertThrows[Platform.NPE] {
      TEST_2007_07_15.plus(null.asInstanceOf[MockSimplePeriod])
    }
  }

  test("plus_Period_invalidTooLarge") {
    assertThrows[DateTimeException] {
      val period: MockSimplePeriod = MockSimplePeriod.of(1, ChronoUnit.YEARS)
      LocalDate.of(Year.MAX_VALUE, 1, 1).plus(period)
    }
  }

  test("plus_Period_invalidTooSmall") {
    assertThrows[DateTimeException] {
      val period: MockSimplePeriod = MockSimplePeriod.of(-1, ChronoUnit.YEARS)
      LocalDate.of(Year.MIN_VALUE, 1, 1).plus(period)
    }
  }

  test("plus_longPeriodUnit_positiveMonths") {
    val t: LocalDate = TEST_2007_07_15.plus(7, ChronoUnit.MONTHS)
    assertEquals(t, LocalDate.of(2008, 2, 15))
  }

  test("plus_longPeriodUnit_negativeDays") {
    val t: LocalDate = TEST_2007_07_15.plus(-25, ChronoUnit.DAYS)
    assertEquals(t, LocalDate.of(2007, 6, 20))
  }

  test("plus_longPeriodUnit_timeNotAllowed") {
    assertThrows[DateTimeException] {
      TEST_2007_07_15.plus(7, ChronoUnit.HOURS)
    }
  }

  test("plus_longPeriodUnit_null") {
    assertThrows[Platform.NPE] {
      TEST_2007_07_15.plus(1, null.asInstanceOf[TemporalUnit])
    }
  }

  test("plus_longPeriodUnit_invalidTooLarge") {
    assertThrows[DateTimeException] {
      LocalDate.of(Year.MAX_VALUE, 1, 1).plus(1, ChronoUnit.YEARS)
    }
  }

  test("plus_longPeriodUnit_invalidTooSmall") {
    assertThrows[DateTimeException] {
      LocalDate.of(Year.MIN_VALUE, 1, 1).plus(-1, ChronoUnit.YEARS)
    }
  }

  test("plusYears_long_normal") {
    val t: LocalDate = TEST_2007_07_15.plusYears(1)
    assertEquals(t, LocalDate.of(2008, 7, 15))
  }

  test("plusYears_long_negative") {
    val t: LocalDate = TEST_2007_07_15.plusYears(-1)
    assertEquals(t, LocalDate.of(2006, 7, 15))
  }

  test("plusYears_long_adjustDay") {
    val t: LocalDate        = LocalDate.of(2008, 2, 29).plusYears(1)
    val expected: LocalDate = LocalDate.of(2009, 2, 28)
    assertEquals(t, expected)
  }

  test("plusYears_long_big") {
    val years: Long     = 20L + Year.MAX_VALUE
    val test: LocalDate = LocalDate.of(-40, 6, 1).plusYears(years)
    assertEquals(test, LocalDate.of((-40L + years).toInt, 6, 1))
  }

  test("plusYears_long_invalidTooLarge") {
    assertThrows[DateTimeException] {
      val test: LocalDate = LocalDate.of(Year.MAX_VALUE, 6, 1)
      test.plusYears(1)
    }
  }

  test("plusYears_long_invalidTooLargeMaxAddMax") {
    assertThrows[DateTimeException] {
      val test: LocalDate = LocalDate.of(Year.MAX_VALUE, 12, 1)
      test.plusYears(Long.MaxValue)
    }
  }

  test("plusYears_long_invalidTooLargeMaxAddMin") {
    assertThrows[DateTimeException] {
      val test: LocalDate = LocalDate.of(Year.MAX_VALUE, 12, 1)
      test.plusYears(Long.MinValue)
    }
  }

  test("plusYears_long_invalidTooSmall_validInt") {
    assertThrows[DateTimeException] {
      LocalDate.of(Year.MIN_VALUE, 1, 1).plusYears(-1)
    }
  }

  test("plusYears_long_invalidTooSmall_invalidInt") {
    assertThrows[DateTimeException] {
      LocalDate.of(Year.MIN_VALUE, 1, 1).plusYears(-10)
    }
  }

  test("plusMonths_long_normal") {
    val t: LocalDate = TEST_2007_07_15.plusMonths(1)
    assertEquals(t, LocalDate.of(2007, 8, 15))
  }

  test("plusMonths_long_overYears") {
    val t: LocalDate = TEST_2007_07_15.plusMonths(25)
    assertEquals(t, LocalDate.of(2009, 8, 15))
  }

  test("plusMonths_long_negative") {
    val t: LocalDate = TEST_2007_07_15.plusMonths(-1)
    assertEquals(t, LocalDate.of(2007, 6, 15))
  }

  test("plusMonths_long_negativeAcrossYear") {
    val t: LocalDate = TEST_2007_07_15.plusMonths(-7)
    assertEquals(t, LocalDate.of(2006, 12, 15))
  }

  test("plusMonths_long_negativeOverYears") {
    val t: LocalDate = TEST_2007_07_15.plusMonths(-31)
    assertEquals(t, LocalDate.of(2004, 12, 15))
  }

  test("plusMonths_long_adjustDayFromLeapYear") {
    val t: LocalDate        = LocalDate.of(2008, 2, 29).plusMonths(12)
    val expected: LocalDate = LocalDate.of(2009, 2, 28)
    assertEquals(t, expected)
  }

  test("plusMonths_long_adjustDayFromMonthLength") {
    val t: LocalDate        = LocalDate.of(2007, 3, 31).plusMonths(1)
    val expected: LocalDate = LocalDate.of(2007, 4, 30)
    assertEquals(t, expected)
  }

  test("plusMonths_long_big") {
    val months: Long    = 20L + Integer.MAX_VALUE
    val test: LocalDate = LocalDate.of(-40, 6, 1).plusMonths(months)
    assertEquals(test, LocalDate.of((-40L + months / 12).toInt, 6 + (months % 12).toInt, 1))
  }

  test("plusMonths_long_invalidTooLarge") {
    assertThrows[DateTimeException] {
      LocalDate.of(Year.MAX_VALUE, 12, 1).plusMonths(1)
    }
  }

  test("plusMonths_long_invalidTooLargeMaxAddMax") {
    assertThrows[DateTimeException] {
      val test: LocalDate = LocalDate.of(Year.MAX_VALUE, 12, 1)
      test.plusMonths(Long.MaxValue)
    }
  }

  test("plusMonths_long_invalidTooLargeMaxAddMin") {
    assertThrows[DateTimeException] {
      val test: LocalDate = LocalDate.of(Year.MAX_VALUE, 12, 1)
      test.plusMonths(Long.MinValue)
    }
  }

  test("plusMonths_long_invalidTooSmall") {
    assertThrows[DateTimeException] {
      LocalDate.of(Year.MIN_VALUE, 1, 1).plusMonths(-1)
    }
  }

  test("plusWeeks_normal") {
    val t: LocalDate = TEST_2007_07_15.plusWeeks(1)
    assertEquals(t, LocalDate.of(2007, 7, 22))
  }

  test("plusWeeks_overMonths") {
    val t: LocalDate = TEST_2007_07_15.plusWeeks(9)
    assertEquals(t, LocalDate.of(2007, 9, 16))
  }

  test("plusWeeks_overYears") {
    val t: LocalDate = LocalDate.of(2006, 7, 16).plusWeeks(52)
    assertEquals(t, TEST_2007_07_15)
  }

  test("plusWeeks_overLeapYears") {
    val t: LocalDate = TEST_2007_07_15.plusYears(-1).plusWeeks(104)
    assertEquals(t, LocalDate.of(2008, 7, 12))
  }

  test("plusWeeks_negative") {
    val t: LocalDate = TEST_2007_07_15.plusWeeks(-1)
    assertEquals(t, LocalDate.of(2007, 7, 8))
  }

  test("plusWeeks_negativeAcrossYear") {
    val t: LocalDate = TEST_2007_07_15.plusWeeks(-28)
    assertEquals(t, LocalDate.of(2006, 12, 31))
  }

  test("plusWeeks_negativeOverYears") {
    val t: LocalDate = TEST_2007_07_15.plusWeeks(-104)
    assertEquals(t, LocalDate.of(2005, 7, 17))
  }

  test("plusWeeks_maximum") {
    val t: LocalDate        = LocalDate.of(Year.MAX_VALUE, 12, 24).plusWeeks(1)
    val expected: LocalDate = LocalDate.of(Year.MAX_VALUE, 12, 31)
    assertEquals(t, expected)
  }

  test("plusWeeks_minimum") {
    val t: LocalDate        = LocalDate.of(Year.MIN_VALUE, 1, 8).plusWeeks(-1)
    val expected: LocalDate = LocalDate.of(Year.MIN_VALUE, 1, 1)
    assertEquals(t, expected)
  }

  test("plusWeeks_invalidTooLarge") {
    assertThrows[DateTimeException] {
      LocalDate.of(Year.MAX_VALUE, 12, 25).plusWeeks(1)
    }
  }

  test("plusWeeks_invalidTooSmall") {
    assertThrows[DateTimeException] {
      LocalDate.of(Year.MIN_VALUE, 1, 7).plusWeeks(-1)
    }
  }

  test("plusWeeks_invalidMaxMinusMax") {
    assertThrows[ArithmeticException] {
      LocalDate.of(Year.MAX_VALUE, 12, 25).plusWeeks(Long.MaxValue)
    }
  }

  test("plusWeeks_invalidMaxMinusMin") {
    assertThrows[ArithmeticException] {
      LocalDate.of(Year.MAX_VALUE, 12, 25).plusWeeks(Long.MinValue)
    }
  }

  test("plusDays_normal") {
    val t: LocalDate = TEST_2007_07_15.plusDays(1)
    assertEquals(t, LocalDate.of(2007, 7, 16))
  }

  test("plusDays_overMonths") {
    val t: LocalDate = TEST_2007_07_15.plusDays(62)
    assertEquals(t, LocalDate.of(2007, 9, 15))
  }

  test("plusDays_overYears") {
    val t: LocalDate = LocalDate.of(2006, 7, 14).plusDays(366)
    assertEquals(t, TEST_2007_07_15)
  }

  test("plusDays_overLeapYears") {
    val t: LocalDate = TEST_2007_07_15.plusYears(-1).plusDays(365 + 366)
    assertEquals(t, LocalDate.of(2008, 7, 15))
  }

  test("plusDays_negative") {
    val t: LocalDate = TEST_2007_07_15.plusDays(-1)
    assertEquals(t, LocalDate.of(2007, 7, 14))
  }

  test("plusDays_negativeAcrossYear") {
    val t: LocalDate = TEST_2007_07_15.plusDays(-196)
    assertEquals(t, LocalDate.of(2006, 12, 31))
  }

  test("plusDays_negativeOverYears") {
    val t: LocalDate = TEST_2007_07_15.plusDays(-730)
    assertEquals(t, LocalDate.of(2005, 7, 15))
  }

  test("plusDays_maximum") {
    val t: LocalDate        = LocalDate.of(Year.MAX_VALUE, 12, 30).plusDays(1)
    val expected: LocalDate = LocalDate.of(Year.MAX_VALUE, 12, 31)
    assertEquals(t, expected)
  }

  test("plusDays_minimum") {
    val t: LocalDate        = LocalDate.of(Year.MIN_VALUE, 1, 2).plusDays(-1)
    val expected: LocalDate = LocalDate.of(Year.MIN_VALUE, 1, 1)
    assertEquals(t, expected)
  }

  test("plusDays_invalidTooLarge") {
    assertThrows[DateTimeException] {
      LocalDate.of(Year.MAX_VALUE, 12, 31).plusDays(1)
    }
  }

  test("plusDays_invalidTooSmall") {
    assertThrows[DateTimeException] {
      LocalDate.of(Year.MIN_VALUE, 1, 1).plusDays(-1)
    }
  }

  test("plusDays_overflowTooLarge") {
    assertThrows[ArithmeticException] {
      LocalDate.of(Year.MAX_VALUE, 12, 31).plusDays(Long.MaxValue)
    }
  }

  test("plusDays_overflowTooSmall") {
    assertThrows[ArithmeticException] {
      LocalDate.of(Year.MIN_VALUE, 1, 1).plusDays(Long.MinValue)
    }
  }

  test("minus_Period_positiveMonths") {
    val period: MockSimplePeriod = MockSimplePeriod.of(7, ChronoUnit.MONTHS)
    val t: LocalDate             = TEST_2007_07_15.minus(period)
    assertEquals(t, LocalDate.of(2006, 12, 15))
  }

  test("minus_Period_negativeDays") {
    val period: MockSimplePeriod = MockSimplePeriod.of(-25, ChronoUnit.DAYS)
    val t: LocalDate             = TEST_2007_07_15.minus(period)
    assertEquals(t, LocalDate.of(2007, 8, 9))
  }

  test("minus_Period_timeNotAllowed") {
    assertThrows[DateTimeException] {
      val period: MockSimplePeriod = MockSimplePeriod.of(7, ChronoUnit.HOURS)
      TEST_2007_07_15.minus(period)
    }
  }

  test("minus_Period_null") {
    assertThrows[Platform.NPE] {
      TEST_2007_07_15.minus(null.asInstanceOf[MockSimplePeriod])
    }
  }

  test("minus_Period_invalidTooLarge") {
    assertThrows[DateTimeException] {
      val period: MockSimplePeriod = MockSimplePeriod.of(-1, ChronoUnit.YEARS)
      LocalDate.of(Year.MAX_VALUE, 1, 1).minus(period)
    }
  }

  test("minus_Period_invalidTooSmall") {
    assertThrows[DateTimeException] {
      val period: MockSimplePeriod = MockSimplePeriod.of(1, ChronoUnit.YEARS)
      LocalDate.of(Year.MIN_VALUE, 1, 1).minus(period)
    }
  }

  test("minus_longPeriodUnit_positiveMonths") {
    val t: LocalDate = TEST_2007_07_15.minus(7, ChronoUnit.MONTHS)
    assertEquals(t, LocalDate.of(2006, 12, 15))
  }

  test("minus_longPeriodUnit_negativeDays") {
    val t: LocalDate = TEST_2007_07_15.minus(-25, ChronoUnit.DAYS)
    assertEquals(t, LocalDate.of(2007, 8, 9))
  }

  test("minus_longPeriodUnit_timeNotAllowed") {
    assertThrows[DateTimeException] {
      TEST_2007_07_15.minus(7, ChronoUnit.HOURS)
    }
  }

  test("minus_longPeriodUnit_null") {
    assertThrows[Platform.NPE] {
      TEST_2007_07_15.minus(1, null.asInstanceOf[TemporalUnit])
    }
  }

  test("minus_longPeriodUnit_invalidTooLarge") {
    assertThrows[DateTimeException] {
      LocalDate.of(Year.MAX_VALUE, 1, 1).minus(-1, ChronoUnit.YEARS)
    }
  }

  test("minus_longPeriodUnit_invalidTooSmall") {
    assertThrows[DateTimeException] {
      LocalDate.of(Year.MIN_VALUE, 1, 1).minus(1, ChronoUnit.YEARS)
    }
  }

  test("minusYears_long_normal") {
    val t: LocalDate = TEST_2007_07_15.minusYears(1)
    assertEquals(t, LocalDate.of(2006, 7, 15))
  }

  test("minusYears_long_negative") {
    val t: LocalDate = TEST_2007_07_15.minusYears(-1)
    assertEquals(t, LocalDate.of(2008, 7, 15))
  }

  test("minusYears_long_adjustDay") {
    val t: LocalDate        = LocalDate.of(2008, 2, 29).minusYears(1)
    val expected: LocalDate = LocalDate.of(2007, 2, 28)
    assertEquals(t, expected)
  }

  test("minusYears_long_big") {
    val years: Long     = 20L + Year.MAX_VALUE
    val test: LocalDate = LocalDate.of(40, 6, 1).minusYears(years)
    assertEquals(test, LocalDate.of((40L - years).toInt, 6, 1))
  }

  test("minusYears_long_invalidTooLarge") {
    assertThrows[DateTimeException] {
      val test: LocalDate = LocalDate.of(Year.MAX_VALUE, 6, 1)
      test.minusYears(-1)
    }
  }

  test("minusYears_long_invalidTooLargeMaxAddMax") {
    assertThrows[DateTimeException] {
      val test: LocalDate = LocalDate.of(Year.MAX_VALUE, 12, 1)
      test.minusYears(Long.MaxValue)
    }
  }

  test("minusYears_long_invalidTooLargeMaxAddMin") {
    assertThrows[DateTimeException] {
      val test: LocalDate = LocalDate.of(Year.MAX_VALUE, 12, 1)
      test.minusYears(Long.MinValue)
    }
  }

  test("minusYears_long_invalidTooSmall") {
    assertThrows[DateTimeException] {
      LocalDate.of(Year.MIN_VALUE, 1, 1).minusYears(1)
    }
  }

  test("minusMonths_long_normal") {
    val t: LocalDate = TEST_2007_07_15.minusMonths(1)
    assertEquals(t, LocalDate.of(2007, 6, 15))
  }

  test("minusMonths_long_overYears") {
    val t: LocalDate = TEST_2007_07_15.minusMonths(25)
    assertEquals(t, LocalDate.of(2005, 6, 15))
  }

  test("minusMonths_long_negative") {
    val t: LocalDate = TEST_2007_07_15.minusMonths(-1)
    assertEquals(t, LocalDate.of(2007, 8, 15))
  }

  test("minusMonths_long_negativeAcrossYear") {
    val t: LocalDate = TEST_2007_07_15.minusMonths(-7)
    assertEquals(t, LocalDate.of(2008, 2, 15))
  }

  test("minusMonths_long_negativeOverYears") {
    val t: LocalDate = TEST_2007_07_15.minusMonths(-31)
    assertEquals(t, LocalDate.of(2010, 2, 15))
  }

  test("minusMonths_long_adjustDayFromLeapYear") {
    val t: LocalDate        = LocalDate.of(2008, 2, 29).minusMonths(12)
    val expected: LocalDate = LocalDate.of(2007, 2, 28)
    assertEquals(t, expected)
  }

  test("minusMonths_long_adjustDayFromMonthLength") {
    val t: LocalDate        = LocalDate.of(2007, 3, 31).minusMonths(1)
    val expected: LocalDate = LocalDate.of(2007, 2, 28)
    assertEquals(t, expected)
  }

  test("minusMonths_long_big") {
    val months: Long    = 20L + Integer.MAX_VALUE
    val test: LocalDate = LocalDate.of(40, 6, 1).minusMonths(months)
    assertEquals(test, LocalDate.of((40L - months / 12).toInt, 6 - (months % 12).toInt, 1))
  }

  test("minusMonths_long_invalidTooLarge") {
    assertThrows[DateTimeException] {
      LocalDate.of(Year.MAX_VALUE, 12, 1).minusMonths(-1)
    }
  }

  test("minusMonths_long_invalidTooLargeMaxAddMax") {
    assertThrows[DateTimeException] {
      val test: LocalDate = LocalDate.of(Year.MAX_VALUE, 12, 1)
      test.minusMonths(Long.MaxValue)
    }
  }

  test("minusMonths_long_invalidTooLargeMaxAddMin") {
    assertThrows[DateTimeException] {
      val test: LocalDate = LocalDate.of(Year.MAX_VALUE, 12, 1)
      test.minusMonths(Long.MinValue)
    }
  }

  test("minusMonths_long_invalidTooSmall") {
    assertThrows[DateTimeException] {
      LocalDate.of(Year.MIN_VALUE, 1, 1).minusMonths(1)
    }
  }

  test("minusWeeks_normal") {
    val t: LocalDate = TEST_2007_07_15.minusWeeks(1)
    assertEquals(t, LocalDate.of(2007, 7, 8))
  }

  test("minusWeeks_overMonths") {
    val t: LocalDate = TEST_2007_07_15.minusWeeks(9)
    assertEquals(t, LocalDate.of(2007, 5, 13))
  }

  test("minusWeeks_overYears") {
    val t: LocalDate = LocalDate.of(2008, 7, 13).minusWeeks(52)
    assertEquals(t, TEST_2007_07_15)
  }

  test("minusWeeks_overLeapYears") {
    val t: LocalDate = TEST_2007_07_15.minusYears(-1).minusWeeks(104)
    assertEquals(t, LocalDate.of(2006, 7, 18))
  }

  test("minusWeeks_negative") {
    val t: LocalDate = TEST_2007_07_15.minusWeeks(-1)
    assertEquals(t, LocalDate.of(2007, 7, 22))
  }

  test("minusWeeks_negativeAcrossYear") {
    val t: LocalDate = TEST_2007_07_15.minusWeeks(-28)
    assertEquals(t, LocalDate.of(2008, 1, 27))
  }

  test("minusWeeks_negativeOverYears") {
    val t: LocalDate = TEST_2007_07_15.minusWeeks(-104)
    assertEquals(t, LocalDate.of(2009, 7, 12))
  }

  test("minusWeeks_maximum") {
    val t: LocalDate        = LocalDate.of(Year.MAX_VALUE, 12, 24).minusWeeks(-1)
    val expected: LocalDate = LocalDate.of(Year.MAX_VALUE, 12, 31)
    assertEquals(t, expected)
  }

  test("minusWeeks_minimum") {
    val t: LocalDate        = LocalDate.of(Year.MIN_VALUE, 1, 8).minusWeeks(1)
    val expected: LocalDate = LocalDate.of(Year.MIN_VALUE, 1, 1)
    assertEquals(t, expected)
  }

  test("minusWeeks_invalidTooLarge") {
    assertThrows[DateTimeException] {
      LocalDate.of(Year.MAX_VALUE, 12, 25).minusWeeks(-1)
    }
  }

  test("minusWeeks_invalidTooSmall") {
    assertThrows[DateTimeException] {
      LocalDate.of(Year.MIN_VALUE, 1, 7).minusWeeks(1)
    }
  }

  test("minusWeeks_invalidMaxMinusMax") {
    assertThrows[ArithmeticException] {
      LocalDate.of(Year.MAX_VALUE, 12, 25).minusWeeks(Long.MaxValue)
    }
  }

  test("minusWeeks_invalidMaxMinusMin") {
    assertThrows[ArithmeticException] {
      LocalDate.of(Year.MAX_VALUE, 12, 25).minusWeeks(Long.MinValue)
    }
  }

  test("minusDays_normal") {
    val t: LocalDate = TEST_2007_07_15.minusDays(1)
    assertEquals(t, LocalDate.of(2007, 7, 14))
  }

  test("minusDays_overMonths") {
    val t: LocalDate = TEST_2007_07_15.minusDays(62)
    assertEquals(t, LocalDate.of(2007, 5, 14))
  }

  test("minusDays_overYears") {
    val t: LocalDate = LocalDate.of(2008, 7, 16).minusDays(367)
    assertEquals(t, TEST_2007_07_15)
  }

  test("minusDays_overLeapYears") {
    val t: LocalDate = TEST_2007_07_15.plusYears(2).minusDays(365 + 366)
    assertEquals(t, TEST_2007_07_15)
  }

  test("minusDays_negative") {
    val t: LocalDate = TEST_2007_07_15.minusDays(-1)
    assertEquals(t, LocalDate.of(2007, 7, 16))
  }

  test("minusDays_negativeAcrossYear") {
    val t: LocalDate = TEST_2007_07_15.minusDays(-169)
    assertEquals(t, LocalDate.of(2007, 12, 31))
  }

  test("minusDays_negativeOverYears") {
    val t: LocalDate = TEST_2007_07_15.minusDays(-731)
    assertEquals(t, LocalDate.of(2009, 7, 15))
  }

  test("minusDays_maximum") {
    val t: LocalDate        = LocalDate.of(Year.MAX_VALUE, 12, 30).minusDays(-1)
    val expected: LocalDate = LocalDate.of(Year.MAX_VALUE, 12, 31)
    assertEquals(t, expected)
  }

  test("minusDays_minimum") {
    val t: LocalDate        = LocalDate.of(Year.MIN_VALUE, 1, 2).minusDays(1)
    val expected: LocalDate = LocalDate.of(Year.MIN_VALUE, 1, 1)
    assertEquals(t, expected)
  }

  test("minusDays_invalidTooLarge") {
    assertThrows[DateTimeException] {
      LocalDate.of(Year.MAX_VALUE, 12, 31).minusDays(-1)
    }
  }

  test("minusDays_invalidTooSmall") {
    assertThrows[DateTimeException] {
      LocalDate.of(Year.MIN_VALUE, 1, 1).minusDays(1)
    }
  }

  test("minusDays_overflowTooLarge") {
    assertThrows[ArithmeticException] {
      LocalDate.of(Year.MAX_VALUE, 12, 31).minusDays(Long.MinValue)
    }
  }

  test("minusDays_overflowTooSmall") {
    assertThrows[ArithmeticException] {
      LocalDate.of(Year.MIN_VALUE, 1, 1).minusDays(Long.MaxValue)
    }
  }

  def provider_until: List[List[Any]] =
    List(
      List("2012-06-30", "2012-06-30", DAYS, 0),
      List("2012-06-30", "2012-06-30", WEEKS, 0),
      List("2012-06-30", "2012-06-30", MONTHS, 0),
      List("2012-06-30", "2012-06-30", YEARS, 0),
      List("2012-06-30", "2012-06-30", DECADES, 0),
      List("2012-06-30", "2012-06-30", CENTURIES, 0),
      List("2012-06-30", "2012-06-30", MILLENNIA, 0),
      List("2012-06-30", "2012-07-01", DAYS, 1),
      List("2012-06-30", "2012-07-01", WEEKS, 0),
      List("2012-06-30", "2012-07-01", MONTHS, 0),
      List("2012-06-30", "2012-07-01", YEARS, 0),
      List("2012-06-30", "2012-07-01", DECADES, 0),
      List("2012-06-30", "2012-07-01", CENTURIES, 0),
      List("2012-06-30", "2012-07-01", MILLENNIA, 0),
      List("2012-06-30", "2012-07-07", DAYS, 7),
      List("2012-06-30", "2012-07-07", WEEKS, 1),
      List("2012-06-30", "2012-07-07", MONTHS, 0),
      List("2012-06-30", "2012-07-07", YEARS, 0),
      List("2012-06-30", "2012-07-07", DECADES, 0),
      List("2012-06-30", "2012-07-07", CENTURIES, 0),
      List("2012-06-30", "2012-07-07", MILLENNIA, 0),
      List("2012-06-30", "2012-07-29", MONTHS, 0),
      List("2012-06-30", "2012-07-30", MONTHS, 1),
      List("2012-06-30", "2012-07-31", MONTHS, 1)
    )

  test("until") {
    provider_until.foreach {
      case (startStr: String) :: (endStr: String) :: (unit: TemporalUnit) :: (expected: Int) :: Nil =>
        val start: LocalDate = LocalDate.parse(startStr)
        val end: LocalDate   = LocalDate.parse(endStr)
        assertEquals(start.until(end, unit), expected)
        assertEquals(end.until(start, unit), -expected)
      case _                                                                                        =>
        fail()
    }
  }

  test("atTime_LocalTime") {
    val t: LocalDate = LocalDate.of(2008, 6, 30)
    assertEquals(t.atTime(LocalTime.of(11, 30)), LocalDateTime.of(2008, 6, 30, 11, 30))
  }

  test("atTime_LocalTime_null") {
    assertThrows[NullPointerException] {
      val t: LocalDate = LocalDate.of(2008, 6, 30)
      t.atTime(null.asInstanceOf[LocalTime])
    }
  }

  test("atTime_int_int") {
    val t: LocalDate = LocalDate.of(2008, 6, 30)
    assertEquals(t.atTime(11, 30), LocalDateTime.of(2008, 6, 30, 11, 30))
  }

  test("atTime_int_int_hourTooSmall") {
    assertThrows[DateTimeException] {
      val t: LocalDate = LocalDate.of(2008, 6, 30)
      t.atTime(-1, 30)
    }
  }

  test("atTime_int_int_hourTooBig") {
    assertThrows[DateTimeException] {
      val t: LocalDate = LocalDate.of(2008, 6, 30)
      t.atTime(24, 30)
    }
  }

  test("atTime_int_int_minuteTooSmall") {
    assertThrows[DateTimeException] {
      val t: LocalDate = LocalDate.of(2008, 6, 30)
      t.atTime(11, -1)
    }
  }

  test("atTime_int_int_minuteTooBig") {
    assertThrows[DateTimeException] {
      val t: LocalDate = LocalDate.of(2008, 6, 30)
      t.atTime(11, 60)
    }
  }

  test("atTime_int_int_int") {
    val t: LocalDate = LocalDate.of(2008, 6, 30)
    assertEquals(t.atTime(11, 30, 40), LocalDateTime.of(2008, 6, 30, 11, 30, 40))
  }

  test("atTime_int_int_int_hourTooSmall") {
    assertThrows[DateTimeException] {
      val t: LocalDate = LocalDate.of(2008, 6, 30)
      t.atTime(-1, 30, 40)
    }
  }

  test("atTime_int_int_int_hourTooBig") {
    assertThrows[DateTimeException] {
      val t: LocalDate = LocalDate.of(2008, 6, 30)
      t.atTime(24, 30, 40)
    }
  }

  test("atTime_int_int_int_minuteTooSmall") {
    assertThrows[DateTimeException] {
      val t: LocalDate = LocalDate.of(2008, 6, 30)
      t.atTime(11, -1, 40)
    }
  }

  test("atTime_int_int_int_minuteTooBig") {
    assertThrows[DateTimeException] {
      val t: LocalDate = LocalDate.of(2008, 6, 30)
      t.atTime(11, 60, 40)
    }
  }

  test("atTime_int_int_int_secondTooSmall") {
    assertThrows[DateTimeException] {
      val t: LocalDate = LocalDate.of(2008, 6, 30)
      t.atTime(11, 30, -1)
    }
  }

  test("atTime_int_int_int_secondTooBig") {
    assertThrows[DateTimeException] {
      val t: LocalDate = LocalDate.of(2008, 6, 30)
      t.atTime(11, 30, 60)
    }
  }

  test("atTime_int_int_int_int") {
    val t: LocalDate = LocalDate.of(2008, 6, 30)
    assertEquals(t.atTime(11, 30, 40, 50), LocalDateTime.of(2008, 6, 30, 11, 30, 40, 50))
  }

  test("atTime_int_int_int_int_hourTooSmall") {
    assertThrows[DateTimeException] {
      val t: LocalDate = LocalDate.of(2008, 6, 30)
      t.atTime(-1, 30, 40, 50)
    }
  }

  test("atTime_int_int_int_int_hourTooBig") {
    assertThrows[DateTimeException] {
      val t: LocalDate = LocalDate.of(2008, 6, 30)
      t.atTime(24, 30, 40, 50)
    }
  }

  test("atTime_int_int_int_int_minuteTooSmall") {
    assertThrows[DateTimeException] {
      val t: LocalDate = LocalDate.of(2008, 6, 30)
      t.atTime(11, -1, 40, 50)
    }
  }

  test("atTime_int_int_int_int_minuteTooBig") {
    assertThrows[DateTimeException] {
      val t: LocalDate = LocalDate.of(2008, 6, 30)
      t.atTime(11, 60, 40, 50)
    }
  }

  test("atTime_int_int_int_int_secondTooSmall") {
    assertThrows[DateTimeException] {
      val t: LocalDate = LocalDate.of(2008, 6, 30)
      t.atTime(11, 30, -1, 50)
    }
  }

  test("atTime_int_int_int_int_secondTooBig") {
    assertThrows[DateTimeException] {
      val t: LocalDate = LocalDate.of(2008, 6, 30)
      t.atTime(11, 30, 60, 50)
    }
  }

  test("atTime_int_int_int_int_nanoTooSmall") {
    assertThrows[DateTimeException] {
      val t: LocalDate = LocalDate.of(2008, 6, 30)
      t.atTime(11, 30, 40, -1)
    }
  }

  test("atTime_int_int_int_int_nanoTooBig") {
    assertThrows[DateTimeException] {
      val t: LocalDate = LocalDate.of(2008, 6, 30)
      t.atTime(11, 30, 40, 1000000000)
    }
  }

  test("atStartOfDay") {
    val t: LocalDate = LocalDate.of(2008, 6, 30)
    assertEquals(t.atStartOfDay(TestLocalDate.ZONE_PARIS),
                 ZonedDateTime.of(LocalDateTime.of(2008, 6, 30, 0, 0), TestLocalDate.ZONE_PARIS)
    )
  }

  test("atStartOfDay_dstGap") {
    val t: LocalDate = LocalDate.of(2007, 4, 1)
    assertEquals(t.atStartOfDay(TestLocalDate.ZONE_GAZA),
                 ZonedDateTime.of(LocalDateTime.of(2007, 4, 1, 1, 0), TestLocalDate.ZONE_GAZA)
    )
  }

  test("atStartOfDay_nullTimeZone") {
    assertThrows[NullPointerException] {
      val t: LocalDate = LocalDate.of(2008, 6, 30)
      t.atStartOfDay(null.asInstanceOf[ZoneId])
    }
  }

  test("toEpochDay") {
    val date_0000_01_01: Long = -678941 - 40587
    var test: LocalDate       = LocalDate.of(0, 1, 1)

    {
      var i: Long = date_0000_01_01
      while (i < 700000) {
        {
          assertEquals(test.toEpochDay, i)
          test = next(test)
        }
        {
          i += 1
          i - 1
        }
      }
    }
    test = LocalDate.of(0, 1, 1)

    {
      var i: Long = date_0000_01_01
      while (i > -2000000) {
        {
          assertEquals(test.toEpochDay, i)
          test = previous(test)
        }
        {
          i -= 1
          i + 1
        }
      }
    }
    assertEquals(LocalDate.of(1858, 11, 17).toEpochDay, -40587)
    assertEquals(LocalDate.of(1, 1, 1).toEpochDay, -678575 - 40587)
    assertEquals(LocalDate.of(1995, 9, 27).toEpochDay, 49987 - 40587)
    assertEquals(LocalDate.of(1970, 1, 1).toEpochDay, 0)
    assertEquals(LocalDate.of(-1, 12, 31).toEpochDay, -678942 - 40587)
  }

  test("comparisons") {
    doTest_comparisons_LocalDate(
      LocalDate.of(Year.MIN_VALUE, 1, 1),
      LocalDate.of(Year.MIN_VALUE, 12, 31),
      LocalDate.of(-1, 1, 1),
      LocalDate.of(-1, 12, 31),
      LocalDate.of(0, 1, 1),
      LocalDate.of(0, 12, 31),
      LocalDate.of(1, 1, 1),
      LocalDate.of(1, 12, 31),
      LocalDate.of(2006, 1, 1),
      LocalDate.of(2006, 12, 31),
      LocalDate.of(2007, 1, 1),
      LocalDate.of(2007, 12, 31),
      LocalDate.of(2008, 1, 1),
      LocalDate.of(2008, 2, 29),
      LocalDate.of(2008, 12, 31),
      LocalDate.of(Year.MAX_VALUE, 1, 1),
      LocalDate.of(Year.MAX_VALUE, 12, 31)
    )
  }

  def doTest_comparisons_LocalDate(localDates: LocalDate*): Unit = {
    var i: Int = 0
    while (i < localDates.length) {
      {
        val a: LocalDate = localDates(i)

        {
          var j: Int = 0
          while (j < localDates.length) {
            {
              val b: LocalDate = localDates(j)
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
                assertEquals(a.compareTo(b), 0, a + " <=> " + b)
                assertEquals(a.isBefore(b), false, a + " <=> " + b)
                assertEquals(a.isAfter(b), false, a + " <=> " + b)
                assertEquals(a == b, true, a + " <=> " + b)
              }
            }
            {
              j += 1
              j - 1
            }
          }
        }
      }
      {
        i += 1
        i - 1
      }
    }
  }

  test("compareTo_ObjectNull") {
    assertThrows[Platform.NPE] {
      TEST_2007_07_15.compareTo(null)
    }
  }

  test("isBefore") {
    assertTrue(TEST_2007_07_15.isBefore(LocalDate.of(2007, 7, 16)))
    assertFalse(TEST_2007_07_15.isBefore(LocalDate.of(2007, 7, 14)))
    assertFalse(TEST_2007_07_15.isBefore(TEST_2007_07_15))
  }

  test("isBefore_ObjectNull") {
    assertThrows[Platform.NPE] {
      TEST_2007_07_15.isBefore(null)
    }
  }

  test("isAfter_ObjectNull") {
    assertThrows[Platform.NPE] {
      TEST_2007_07_15.isAfter(null)
    }
  }

  test("isAfter") {
    assertTrue(TEST_2007_07_15.isAfter(LocalDate.of(2007, 7, 14)))
    assertFalse(TEST_2007_07_15.isAfter(LocalDate.of(2007, 7, 16)))
    assertFalse(TEST_2007_07_15.isAfter(TEST_2007_07_15))
  }

  test("equals_true") {
    provider_sampleDates.foreach {
      case (y: Int) :: (m: Int) :: (d: Int) :: Nil =>
        val a: LocalDate = LocalDate.of(y, m, d)
        val b: LocalDate = LocalDate.of(y, m, d)
        assertEquals(a == b, true)
      case _                                       =>
        fail()
    }
  }

  test("equals_false_year_differs") {
    provider_sampleDates.foreach {
      case (y: Int) :: (m: Int) :: (d: Int) :: Nil =>
        val a: LocalDate = LocalDate.of(y, m, d)
        val b: LocalDate = LocalDate.of(y + 1, m, d)
        assertEquals(a == b, false)
      case _                                       =>
        fail()
    }
  }

  test("equals_false_month_differs") {
    provider_sampleDates.foreach {
      case (y: Int) :: (m: Int) :: (d: Int) :: Nil =>
        val a: LocalDate = LocalDate.of(y, m, d)
        val b: LocalDate = LocalDate.of(y, m + 1, d)
        assertEquals(a == b, false)
      case _                                       =>
        fail()
    }
  }

  test("equals_false_day_differs") {
    provider_sampleDates.foreach {
      case (y: Int) :: (m: Int) :: (d: Int) :: Nil =>
        val a: LocalDate = LocalDate.of(y, m, d)
        val b: LocalDate = LocalDate.of(y, m, d + 1)
        assertEquals(a == b, false)
      case _                                       =>
        fail()
    }
  }

  test("equals_itself_true") {
    assertEquals(TEST_2007_07_15, TEST_2007_07_15)
  }

  test("equals_string_false") {
    assertNotEquals(TEST_2007_07_15, "2007-07-15")
  }

  test("equals_null_false") {
    assertEquals(TEST_2007_07_15 == null, false)
  }

  test("hashCode") {
    provider_sampleDates.foreach {
      case (y: Int) :: (m: Int) :: (d: Int) :: Nil =>
        val a: LocalDate = LocalDate.of(y, m, d)
        assertEquals(a.hashCode, a.hashCode)
        val b: LocalDate = LocalDate.of(y, m, d)
        assertEquals(a.hashCode, b.hashCode)
      case _                                       =>
        fail()
    }
  }

  def provider_sampleToString: List[List[Any]] =
    List(
      List(2008, 7, 5, "2008-07-05"),
      List(2007, 12, 31, "2007-12-31"),
      List(999, 12, 31, "0999-12-31"),
      List(-1, 1, 2, "-0001-01-02"),
      List(9999, 12, 31, "9999-12-31"),
      List(-9999, 12, 31, "-9999-12-31"),
      List(10000, 1, 1, "+10000-01-01"),
      List(-10000, 1, 1, "-10000-01-01"),
      List(12345678, 1, 1, "+12345678-01-01"),
      List(-12345678, 1, 1, "-12345678-01-01")
    )

  test("toString") {
    provider_sampleToString.foreach {
      case (y: Int) :: (m: Int) :: (d: Int) :: (expected: String) :: Nil =>
        val t: LocalDate = LocalDate.of(y, m, d)
        val str: String  = t.toString
        assertEquals(str, expected)
      case _                                                             =>
        fail()
    }
  }

  test("format_formatter") {
    val f: DateTimeFormatter = DateTimeFormatter.ofPattern("y M d")
    val t: String            = LocalDate.of(2010, 12, 3).format(f)
    assertEquals(t, "2010 12 3")
  }

  test("format_formatter_null") {
    assertThrows[NullPointerException] {
      LocalDate.of(2010, 12, 3).format(null)
    }
  }
}
