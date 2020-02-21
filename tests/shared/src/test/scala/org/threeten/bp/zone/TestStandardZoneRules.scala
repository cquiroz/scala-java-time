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
package org.threeten.bp.zone

import org.threeten.bp._
import org.threeten.bp.zone.ZoneOffsetTransitionRule.TimeDefinition
import org.scalatest.funsuite.AnyFunSuite

/** Test ZoneRules. */
object TestStandardZoneRules {
  val OFFSET_ZERO: ZoneOffset = ZoneOffset.ofHours(0)
  val OFFSET_PONE: ZoneOffset = ZoneOffset.ofHours(1)
  val OFFSET_PTWO: ZoneOffset = ZoneOffset.ofHours(2)
  val LATEST_TZDB: String = "2009b"
  val OVERLAP: Int = 2
  val GAP: Int = 0
}

class TestStandardZoneRules extends AnyFunSuite with AssertionsHelper {

  private def etcGmt: ZoneRules = {
    ZoneId.of("Etc/GMT").getRules
  }

  test("EtcGmt_nextTransition") {
    assertNull(etcGmt.nextTransition(Instant.EPOCH))
  }

  test("EtcGmt_previousTransition") {
    assertNull(etcGmt.previousTransition(Instant.EPOCH))
  }

  private def europeLondon: ZoneRules = {
    ZoneId.of("Europe/London").getRules
  }

  test("London") {
    val test: ZoneRules = europeLondon
    assertEquals(test.isFixedOffset, false)
  }

  test("London_preTimeZones") {
    val test: ZoneRules = europeLondon
    val old: ZonedDateTime = createZDT(1800, 1, 1, ZoneOffset.UTC)
    val instant: Instant = old.toInstant
    val offset: ZoneOffset = ZoneOffset.ofHoursMinutesSeconds(0, -1, -15)
    assertEquals(test.getOffset(instant), offset)
    checkOffset(test, old.toLocalDateTime, offset, 1)
    assertEquals(test.getStandardOffset(instant), offset)
    assertEquals(test.getDaylightSavings(instant), Duration.ZERO)
    assertEquals(test.isDaylightSavings(instant), false)
  }

  test("London_getOffset") {
    val test: ZoneRules = europeLondon
    assertEquals(test.getOffset(createInstant(2008, 1, 1, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(test.getOffset(createInstant(2008, 2, 1, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(test.getOffset(createInstant(2008, 3, 1, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(test.getOffset(createInstant(2008, 4, 1, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 5, 1, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 6, 1, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 7, 1, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 8, 1, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 9, 1, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 10, 1, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 11, 1, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(test.getOffset(createInstant(2008, 12, 1, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_ZERO)
  }

  test("London_getOffset_toDST") {
    val test: ZoneRules = europeLondon
    assertEquals(test.getOffset(createInstant(2008, 3, 24, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(test.getOffset(createInstant(2008, 3, 25, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(test.getOffset(createInstant(2008, 3, 26, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(test.getOffset(createInstant(2008, 3, 27, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(test.getOffset(createInstant(2008, 3, 28, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(test.getOffset(createInstant(2008, 3, 29, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(test.getOffset(createInstant(2008, 3, 30, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(test.getOffset(createInstant(2008, 3, 31, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 3, 30, 0, 59, 59, 999999999, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(test.getOffset(createInstant(2008, 3, 30, 1, 0, 0, 0, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
  }

  test("London_getOffset_fromDST") {
    val test: ZoneRules = europeLondon
    assertEquals(test.getOffset(createInstant(2008, 10, 24, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 10, 25, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 10, 26, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 10, 27, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(test.getOffset(createInstant(2008, 10, 28, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(test.getOffset(createInstant(2008, 10, 29, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(test.getOffset(createInstant(2008, 10, 30, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(test.getOffset(createInstant(2008, 10, 31, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(test.getOffset(createInstant(2008, 10, 26, 0, 59, 59, 999999999, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 10, 26, 1, 0, 0, 0, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_ZERO)
  }

  test("London_getOffsetInfo") {
    val test: ZoneRules = europeLondon
    checkOffset(test, createLDT(2008, 1, 1), TestStandardZoneRules.OFFSET_ZERO, 1)
    checkOffset(test, createLDT(2008, 2, 1), TestStandardZoneRules.OFFSET_ZERO, 1)
    checkOffset(test, createLDT(2008, 3, 1), TestStandardZoneRules.OFFSET_ZERO, 1)
    checkOffset(test, createLDT(2008, 4, 1), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, createLDT(2008, 5, 1), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, createLDT(2008, 6, 1), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, createLDT(2008, 7, 1), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, createLDT(2008, 8, 1), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, createLDT(2008, 9, 1), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, createLDT(2008, 10, 1), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, createLDT(2008, 11, 1), TestStandardZoneRules.OFFSET_ZERO, 1)
    checkOffset(test, createLDT(2008, 12, 1), TestStandardZoneRules.OFFSET_ZERO, 1)
  }

  test("London_getOffsetInfo_toDST") {
    val test: ZoneRules = europeLondon
    checkOffset(test, createLDT(2008, 3, 24), TestStandardZoneRules.OFFSET_ZERO, 1)
    checkOffset(test, createLDT(2008, 3, 25), TestStandardZoneRules.OFFSET_ZERO, 1)
    checkOffset(test, createLDT(2008, 3, 26), TestStandardZoneRules.OFFSET_ZERO, 1)
    checkOffset(test, createLDT(2008, 3, 27), TestStandardZoneRules.OFFSET_ZERO, 1)
    checkOffset(test, createLDT(2008, 3, 28), TestStandardZoneRules.OFFSET_ZERO, 1)
    checkOffset(test, createLDT(2008, 3, 29), TestStandardZoneRules.OFFSET_ZERO, 1)
    checkOffset(test, createLDT(2008, 3, 30), TestStandardZoneRules.OFFSET_ZERO, 1)
    checkOffset(test, createLDT(2008, 3, 31), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, LocalDateTime.of(2008, 3, 30, 0, 59, 59, 999999999), TestStandardZoneRules.OFFSET_ZERO, 1)
    checkOffset(test, LocalDateTime.of(2008, 3, 30, 2, 0, 0, 0), TestStandardZoneRules.OFFSET_PONE, 1)
  }

  test("London_getOffsetInfo_fromDST") {
    val test: ZoneRules = europeLondon
    checkOffset(test, createLDT(2008, 10, 24), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, createLDT(2008, 10, 25), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, createLDT(2008, 10, 26), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, createLDT(2008, 10, 27), TestStandardZoneRules.OFFSET_ZERO, 1)
    checkOffset(test, createLDT(2008, 10, 28), TestStandardZoneRules.OFFSET_ZERO, 1)
    checkOffset(test, createLDT(2008, 10, 29), TestStandardZoneRules.OFFSET_ZERO, 1)
    checkOffset(test, createLDT(2008, 10, 30), TestStandardZoneRules.OFFSET_ZERO, 1)
    checkOffset(test, createLDT(2008, 10, 31), TestStandardZoneRules.OFFSET_ZERO, 1)
    checkOffset(test, LocalDateTime.of(2008, 10, 26, 0, 59, 59, 999999999), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, LocalDateTime.of(2008, 10, 26, 2, 0, 0, 0), TestStandardZoneRules.OFFSET_ZERO, 1)
  }

  test("London_getOffsetInfo_gap") {
    val test: ZoneRules = europeLondon
    val dateTime: LocalDateTime = LocalDateTime.of(2008, 3, 30, 1, 0, 0, 0)
    val trans: ZoneOffsetTransition = checkOffset(test, dateTime, TestStandardZoneRules.OFFSET_ZERO, TestStandardZoneRules.GAP)
    assertEquals(trans.isGap, true)
    assertEquals(trans.isOverlap, false)
    assertEquals(trans.getOffsetBefore, TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(trans.getOffsetAfter, TestStandardZoneRules.OFFSET_PONE)
    assertEquals(trans.getInstant, createInstant(2008, 3, 30, 1, 0, ZoneOffset.UTC))
    assertEquals(trans.getDateTimeBefore, LocalDateTime.of(2008, 3, 30, 1, 0))
    assertEquals(trans.getDateTimeAfter, LocalDateTime.of(2008, 3, 30, 2, 0))
    assertEquals(trans.isValidOffset(TestStandardZoneRules.OFFSET_ZERO), false)
    assertEquals(trans.isValidOffset(TestStandardZoneRules.OFFSET_PONE), false)
    assertEquals(trans.isValidOffset(TestStandardZoneRules.OFFSET_PTWO), false)
    assertEquals(trans.toString, "Transition[Gap at 2008-03-30T01:00Z to +01:00]")
    assertFalse(trans == null)
    assertNotEquals(trans, TestStandardZoneRules.OFFSET_ZERO)
    assertTrue(trans == trans)
    val otherTrans: ZoneOffsetTransition = test.getTransition(dateTime)
    assertTrue(trans == otherTrans)
    assertEquals(trans.hashCode, otherTrans.hashCode)
  }

  test("London_getOffsetInfo_overlap") {
    val test: ZoneRules = europeLondon
    val dateTime: LocalDateTime = LocalDateTime.of(2008, 10, 26, 1, 0, 0, 0)
    val trans: ZoneOffsetTransition = checkOffset(test, dateTime, TestStandardZoneRules.OFFSET_PONE, TestStandardZoneRules.OVERLAP)
    assertEquals(trans.isGap, false)
    assertEquals(trans.isOverlap, true)
    assertEquals(trans.getOffsetBefore, TestStandardZoneRules.OFFSET_PONE)
    assertEquals(trans.getOffsetAfter, TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(trans.getInstant, createInstant(2008, 10, 26, 1, 0, ZoneOffset.UTC))
    assertEquals(trans.getDateTimeBefore, LocalDateTime.of(2008, 10, 26, 2, 0))
    assertEquals(trans.getDateTimeAfter, LocalDateTime.of(2008, 10, 26, 1, 0))
    assertEquals(trans.isValidOffset(ZoneOffset.ofHours(-1)), false)
    assertEquals(trans.isValidOffset(TestStandardZoneRules.OFFSET_ZERO), true)
    assertEquals(trans.isValidOffset(TestStandardZoneRules.OFFSET_PONE), true)
    assertEquals(trans.isValidOffset(TestStandardZoneRules.OFFSET_PTWO), false)
    assertEquals(trans.toString, "Transition[Overlap at 2008-10-26T02:00+01:00 to Z]")
    assertFalse(trans == null)
    assertNotEquals(trans, TestStandardZoneRules.OFFSET_PONE)
    assertTrue(trans == trans)
    val otherTrans: ZoneOffsetTransition = test.getTransition(dateTime)
    assertTrue(trans == otherTrans)
    assertEquals(trans.hashCode, otherTrans.hashCode)
  }

  test("London_getStandardOffset") {
    val test: ZoneRules = europeLondon
    var zdt: ZonedDateTime = createZDT(1840, 1, 1, ZoneOffset.UTC)
    while (zdt.getYear < 2010) {
      val instant: Instant = zdt.toInstant
      if (zdt.getYear < 1848) {
        assertEquals(test.getStandardOffset(instant), ZoneOffset.ofHoursMinutesSeconds(0, -1, -15))
      }
      else if (zdt.getYear >= 1969 && zdt.getYear < 1972) {
        assertEquals(test.getStandardOffset(instant), TestStandardZoneRules.OFFSET_PONE)
      }
      else {
        assertEquals(test.getStandardOffset(instant), TestStandardZoneRules.OFFSET_ZERO)
      }
      zdt = zdt.plusMonths(6)
    }
  }

  test("London_getTransitions") {
    val test: ZoneRules = europeLondon
    val trans: java.util.List[ZoneOffsetTransition] = test.getTransitions
    val first: ZoneOffsetTransition = trans.get(0)
    assertEquals(first.getDateTimeBefore, LocalDateTime.of(1847, 12, 1, 0, 0))
    assertEquals(first.getOffsetBefore, ZoneOffset.ofHoursMinutesSeconds(0, -1, -15))
    assertEquals(first.getOffsetAfter, TestStandardZoneRules.OFFSET_ZERO)
    val spring1916: ZoneOffsetTransition = trans.get(1)
    assertEquals(spring1916.getDateTimeBefore, LocalDateTime.of(1916, 5, 21, 2, 0))
    assertEquals(spring1916.getOffsetBefore, TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(spring1916.getOffsetAfter, TestStandardZoneRules.OFFSET_PONE)
    val autumn1916: ZoneOffsetTransition = trans.get(2)
    assertEquals(autumn1916.getDateTimeBefore, LocalDateTime.of(1916, 10, 1, 3, 0))
    assertEquals(autumn1916.getOffsetBefore, TestStandardZoneRules.OFFSET_PONE)
    assertEquals(autumn1916.getOffsetAfter, TestStandardZoneRules.OFFSET_ZERO)
    var zot: ZoneOffsetTransition = null
    val it: java.util.Iterator[ZoneOffsetTransition] = trans.iterator
    scala.util.control.Breaks.breakable {
      while (it.hasNext) {
        zot = it.next
        if (zot.getDateTimeBefore.getYear == 1990) {
          scala.util.control.Breaks.break()
        }
      }
    }
    assertEquals(zot.getDateTimeBefore, LocalDateTime.of(1990, 3, 25, 1, 0))
    assertEquals(zot.getOffsetBefore, TestStandardZoneRules.OFFSET_ZERO)
    zot = it.next
    assertEquals(zot.getDateTimeBefore, LocalDateTime.of(1990, 10, 28, 2, 0))
    assertEquals(zot.getOffsetBefore, TestStandardZoneRules.OFFSET_PONE)
    zot = it.next
    assertEquals(zot.getDateTimeBefore, LocalDateTime.of(1991, 3, 31, 1, 0))
    assertEquals(zot.getOffsetBefore, TestStandardZoneRules.OFFSET_ZERO)
    zot = it.next
    assertEquals(zot.getDateTimeBefore, LocalDateTime.of(1991, 10, 27, 2, 0))
    assertEquals(zot.getOffsetBefore, TestStandardZoneRules.OFFSET_PONE)
    zot = it.next
    assertEquals(zot.getDateTimeBefore, LocalDateTime.of(1992, 3, 29, 1, 0))
    assertEquals(zot.getOffsetBefore, TestStandardZoneRules.OFFSET_ZERO)
    zot = it.next
    assertEquals(zot.getDateTimeBefore, LocalDateTime.of(1992, 10, 25, 2, 0))
    assertEquals(zot.getOffsetBefore, TestStandardZoneRules.OFFSET_PONE)
    zot = it.next
    assertEquals(zot.getDateTimeBefore, LocalDateTime.of(1993, 3, 28, 1, 0))
    assertEquals(zot.getOffsetBefore, TestStandardZoneRules.OFFSET_ZERO)
    zot = it.next
    assertEquals(zot.getDateTimeBefore, LocalDateTime.of(1993, 10, 24, 2, 0))
    assertEquals(zot.getOffsetBefore, TestStandardZoneRules.OFFSET_PONE)
    zot = it.next
    assertEquals(zot.getDateTimeBefore, LocalDateTime.of(1994, 3, 27, 1, 0))
    assertEquals(zot.getOffsetBefore, TestStandardZoneRules.OFFSET_ZERO)
    zot = it.next
    assertEquals(zot.getDateTimeBefore, LocalDateTime.of(1994, 10, 23, 2, 0))
    assertEquals(zot.getOffsetBefore, TestStandardZoneRules.OFFSET_PONE)
    zot = it.next
    assertEquals(zot.getDateTimeBefore, LocalDateTime.of(1995, 3, 26, 1, 0))
    assertEquals(zot.getOffsetBefore, TestStandardZoneRules.OFFSET_ZERO)
    zot = it.next
    assertEquals(zot.getDateTimeBefore, LocalDateTime.of(1995, 10, 22, 2, 0))
    assertEquals(zot.getOffsetBefore, TestStandardZoneRules.OFFSET_PONE)
    zot = it.next
    assertEquals(zot.getDateTimeBefore, LocalDateTime.of(1996, 3, 31, 1, 0))
    assertEquals(zot.getOffsetBefore, TestStandardZoneRules.OFFSET_ZERO)
    zot = it.next
    assertEquals(zot.getDateTimeBefore, LocalDateTime.of(1996, 10, 27, 2, 0))
    assertEquals(zot.getOffsetBefore, TestStandardZoneRules.OFFSET_PONE)
    zot = it.next
    assertEquals(zot.getDateTimeBefore, LocalDateTime.of(1997, 3, 30, 1, 0))
    assertEquals(zot.getOffsetBefore, TestStandardZoneRules.OFFSET_ZERO)
    zot = it.next
    assertEquals(zot.getDateTimeBefore, LocalDateTime.of(1997, 10, 26, 2, 0))
    assertEquals(zot.getOffsetBefore, TestStandardZoneRules.OFFSET_PONE)
    assertEquals(it.hasNext, false)
  }

  test("London_getTransitionRules") {
    val test: ZoneRules = europeLondon
    val rules: java.util.List[ZoneOffsetTransitionRule] = test.getTransitionRules
    assertEquals(rules.size, 2)
    val in: ZoneOffsetTransitionRule = rules.get(0)
    assertEquals(in.getMonth, Month.MARCH)
    assertEquals(in.getDayOfMonthIndicator, 25)
    assertEquals(in.getDayOfWeek, DayOfWeek.SUNDAY)
    assertEquals(in.getLocalTime, LocalTime.of(1, 0))
    assertEquals(in.getTimeDefinition, TimeDefinition.UTC)
    assertEquals(in.getStandardOffset, TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(in.getOffsetBefore, TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(in.getOffsetAfter, TestStandardZoneRules.OFFSET_PONE)
    val out: ZoneOffsetTransitionRule = rules.get(1)
    assertEquals(out.getMonth, Month.OCTOBER)
    assertEquals(out.getDayOfMonthIndicator, 25)
    assertEquals(out.getDayOfWeek, DayOfWeek.SUNDAY)
    assertEquals(out.getLocalTime, LocalTime.of(1, 0))
    assertEquals(out.getTimeDefinition, TimeDefinition.UTC)
    assertEquals(out.getStandardOffset, TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(out.getOffsetBefore, TestStandardZoneRules.OFFSET_PONE)
    assertEquals(out.getOffsetAfter, TestStandardZoneRules.OFFSET_ZERO)
  }

  test("London_nextTransition_historic") {
    val test: ZoneRules = europeLondon
    val trans: java.util.List[ZoneOffsetTransition] = test.getTransitions
    val first: ZoneOffsetTransition = trans.get(0)
    assertEquals(test.nextTransition(first.getInstant.minusNanos(1)), first)
    var i: Int = 0
    while (i < trans.size - 1) {
      val cur: ZoneOffsetTransition = trans.get(i)
      val next: ZoneOffsetTransition = trans.get(i + 1)
      assertEquals(test.nextTransition(cur.getInstant), next)
      assertEquals(test.nextTransition(next.getInstant.minusNanos(1)), next)
      i += 1
    }
  }

  test("London_nextTransition_rulesBased") {
    val test: ZoneRules = europeLondon
    val rules: java.util.List[ZoneOffsetTransitionRule] = test.getTransitionRules
    val trans: java.util.List[ZoneOffsetTransition] = test.getTransitions
    val last: ZoneOffsetTransition = trans.get(trans.size - 1)
    assertEquals(test.nextTransition(last.getInstant), rules.get(0).createTransition(1998))
    var year: Int = 1998
    while (year < 2010) {
      val a: ZoneOffsetTransition = rules.get(0).createTransition(year)
      val b: ZoneOffsetTransition = rules.get(1).createTransition(year)
      val c: ZoneOffsetTransition = rules.get(0).createTransition(year + 1)
      assertEquals(test.nextTransition(a.getInstant), b)
      assertEquals(test.nextTransition(b.getInstant.minusNanos(1)), b)
      assertEquals(test.nextTransition(b.getInstant), c)
      assertEquals(test.nextTransition(c.getInstant.minusNanos(1)), c)
      year += 1
    }
  }

  test("London_nextTransition_lastYear") {
    val test: ZoneRules = europeLondon
    val rules: java.util.List[ZoneOffsetTransitionRule] = test.getTransitionRules
    val zot: ZoneOffsetTransition = rules.get(1).createTransition(Year.MAX_VALUE)
    assertEquals(test.nextTransition(zot.getInstant), null)
  }

  test("London_previousTransition_historic") {
    val test: ZoneRules = europeLondon
    val trans: java.util.List[ZoneOffsetTransition] = test.getTransitions
    val first: ZoneOffsetTransition = trans.get(0)
    assertEquals(test.previousTransition(first.getInstant), null)
    assertEquals(test.previousTransition(first.getInstant.minusNanos(1)), null)
    var i: Int = 0
    while (i < trans.size - 1) {
      val prev: ZoneOffsetTransition = trans.get(i)
      val cur: ZoneOffsetTransition = trans.get(i + 1)
      assertEquals(test.previousTransition(cur.getInstant), prev)
      assertEquals(test.previousTransition(prev.getInstant.plusSeconds(1)), prev)
      assertEquals(test.previousTransition(prev.getInstant.plusNanos(1)), prev)
      i += 1
    }
  }

  test("London_previousTransition_rulesBased") {
    val test: ZoneRules = europeLondon
    val rules: java.util.List[ZoneOffsetTransitionRule] = test.getTransitionRules
    val trans: java.util.List[ZoneOffsetTransition] = test.getTransitions
    val last: ZoneOffsetTransition = trans.get(trans.size - 1)
    assertEquals(test.previousTransition(last.getInstant.plusSeconds(1)), last)
    assertEquals(test.previousTransition(last.getInstant.plusNanos(1)), last)
    var odt: ZonedDateTime = ZonedDateTime.ofInstant(last.getInstant, last.getOffsetAfter)
    odt = odt.withDayOfYear(1).plusYears(1).`with`(LocalTime.MIDNIGHT)
    assertEquals(test.previousTransition(odt.toInstant), last)
    var year: Int = 1998
    while (year < 2010) {
      val a: ZoneOffsetTransition = rules.get(0).createTransition(year)
      val b: ZoneOffsetTransition = rules.get(1).createTransition(year)
      val c: ZoneOffsetTransition = rules.get(0).createTransition(year + 1)
      assertEquals(test.previousTransition(c.getInstant), b)
      assertEquals(test.previousTransition(b.getInstant.plusSeconds(1)), b)
      assertEquals(test.previousTransition(b.getInstant.plusNanos(1)), b)
      assertEquals(test.previousTransition(b.getInstant), a)
      assertEquals(test.previousTransition(a.getInstant.plusSeconds(1)), a)
      assertEquals(test.previousTransition(a.getInstant.plusNanos(1)), a)
      year += 1
    }
  }

  private def europeDublin: ZoneRules =
    ZoneId.of("Europe/Dublin").getRules

  test("Dublin") {
    val test: ZoneRules = europeDublin
    assertEquals(test.isFixedOffset, false)
  }
  test("Dublin_getOffset") {
    val test: ZoneRules = europeDublin
    assertEquals(test.getOffset(createInstant(2008, 1, 1, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(test.getOffset(createInstant(2008, 2, 1, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(test.getOffset(createInstant(2008, 3, 1, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(test.getOffset(createInstant(2008, 4, 1, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 5, 1, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 6, 1, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 7, 1, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 8, 1, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 9, 1, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 10, 1, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 11, 1, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(test.getOffset(createInstant(2008, 12, 1, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_ZERO)
  }

  test("Dublin_getOffset_toDST") {
    val test: ZoneRules = europeDublin
    assertEquals(test.getOffset(createInstant(2008, 3, 24, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(test.getOffset(createInstant(2008, 3, 25, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(test.getOffset(createInstant(2008, 3, 26, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(test.getOffset(createInstant(2008, 3, 27, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(test.getOffset(createInstant(2008, 3, 28, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(test.getOffset(createInstant(2008, 3, 29, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(test.getOffset(createInstant(2008, 3, 30, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(test.getOffset(createInstant(2008, 3, 31, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    // cutover at 01:00Z
    assertEquals(test.getOffset(createInstant(2008, 3, 30, 0, 59, 59, 999999999, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(test.getOffset(createInstant(2008, 3, 30, 1, 0, 0, 0, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
  }

  test("Dublin_getOffset_fromDST") {
    val test: ZoneRules = europeDublin
    assertEquals(test.getOffset(createInstant(2008, 10, 24, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 10, 25, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 10, 26, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 10, 27, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(test.getOffset(createInstant(2008, 10, 28, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(test.getOffset(createInstant(2008, 10, 29, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(test.getOffset(createInstant(2008, 10, 30, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(test.getOffset(createInstant(2008, 10, 31, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_ZERO)
    // cutover at 01:00Z
    assertEquals(test.getOffset(createInstant(2008, 10, 26, 0, 59, 59, 999999999, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 10, 26, 1, 0, 0, 0, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_ZERO)
  }

  test("Dublin_getOffsetInfo") {
    val test: ZoneRules = europeDublin
    checkOffset(test, createLDT(2008, 1, 1), TestStandardZoneRules.OFFSET_ZERO, 1)
    checkOffset(test, createLDT(2008, 2, 1), TestStandardZoneRules.OFFSET_ZERO, 1)
    checkOffset(test, createLDT(2008, 3, 1), TestStandardZoneRules.OFFSET_ZERO, 1)
    checkOffset(test, createLDT(2008, 4, 1), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, createLDT(2008, 5, 1), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, createLDT(2008, 6, 1), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, createLDT(2008, 7, 1), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, createLDT(2008, 8, 1), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, createLDT(2008, 9, 1), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, createLDT(2008, 10, 1), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, createLDT(2008, 11, 1), TestStandardZoneRules.OFFSET_ZERO, 1)
    checkOffset(test, createLDT(2008, 12, 1), TestStandardZoneRules.OFFSET_ZERO, 1)
  }

  test("Dublin_getOffsetInfo_toDST") {
    val test: ZoneRules = europeDublin
    checkOffset(test, createLDT(2008, 3, 24), TestStandardZoneRules.OFFSET_ZERO, 1)
    checkOffset(test, createLDT(2008, 3, 25), TestStandardZoneRules.OFFSET_ZERO, 1)
    checkOffset(test, createLDT(2008, 3, 26), TestStandardZoneRules.OFFSET_ZERO, 1)
    checkOffset(test, createLDT(2008, 3, 27), TestStandardZoneRules.OFFSET_ZERO, 1)
    checkOffset(test, createLDT(2008, 3, 28), TestStandardZoneRules.OFFSET_ZERO, 1)
    checkOffset(test, createLDT(2008, 3, 29), TestStandardZoneRules.OFFSET_ZERO, 1)
    checkOffset(test, createLDT(2008, 3, 30), TestStandardZoneRules.OFFSET_ZERO, 1)
    checkOffset(test, createLDT(2008, 3, 31), TestStandardZoneRules.OFFSET_PONE, 1)
    // cutover at 01:00Z
    checkOffset(test, LocalDateTime.of(2008, 3, 30, 0, 59, 59, 999999999), TestStandardZoneRules.OFFSET_ZERO, 1)
    checkOffset(test, LocalDateTime.of(2008, 3, 30, 2, 0, 0, 0), TestStandardZoneRules.OFFSET_PONE, 1)
  }

  test("Dublin_getOffsetInfo_fromDST") {
    val test: ZoneRules = europeDublin
    checkOffset(test, createLDT(2008, 10, 24), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, createLDT(2008, 10, 25), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, createLDT(2008, 10, 26), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, createLDT(2008, 10, 27), TestStandardZoneRules.OFFSET_ZERO, 1)
    checkOffset(test, createLDT(2008, 10, 28), TestStandardZoneRules.OFFSET_ZERO, 1)
    checkOffset(test, createLDT(2008, 10, 29), TestStandardZoneRules.OFFSET_ZERO, 1)
    checkOffset(test, createLDT(2008, 10, 30), TestStandardZoneRules.OFFSET_ZERO, 1)
    checkOffset(test, createLDT(2008, 10, 31), TestStandardZoneRules.OFFSET_ZERO, 1)
    // cutover at 01:00Z
    checkOffset(test, LocalDateTime.of(2008, 10, 26, 0, 59, 59, 999999999), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, LocalDateTime.of(2008, 10, 26, 2, 0, 0, 0), TestStandardZoneRules.OFFSET_ZERO, 1)
  }

  test("Dublin_getOffsetInfo_gap") {
    val test: ZoneRules = europeDublin
    val dateTime = LocalDateTime.of(2008, 3, 30, 1, 0, 0, 0)
    val trans = checkOffset(test, dateTime, TestStandardZoneRules.OFFSET_ZERO, TestStandardZoneRules.GAP)
    assertEquals(trans.isGap, true)
    assertEquals(trans.isOverlap, false)
    assertEquals(trans.getOffsetBefore, TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(trans.getOffsetAfter, TestStandardZoneRules.OFFSET_PONE)
    assertEquals(trans.getInstant, createInstant(2008, 3, 30, 1, 0, ZoneOffset.UTC))
    assertEquals(trans.getDateTimeBefore, LocalDateTime.of(2008, 3, 30, 1, 0))
    assertEquals(trans.getDateTimeAfter, LocalDateTime.of(2008, 3, 30, 2, 0))
    assertEquals(trans.isValidOffset(TestStandardZoneRules.OFFSET_ZERO), false)
    assertEquals(trans.isValidOffset(TestStandardZoneRules.OFFSET_PONE), false)
    assertEquals(trans.isValidOffset(TestStandardZoneRules.OFFSET_PTWO), false)
    assertEquals(trans.toString, "Transition[Gap at 2008-03-30T01:00Z to +01:00]")
  }

  test("Dublin_getOffsetInfo_overlap") {
    val test: ZoneRules = europeDublin
    val dateTime = LocalDateTime.of(2008, 10, 26, 1, 0, 0, 0)
    val trans = checkOffset(test, dateTime, TestStandardZoneRules.OFFSET_PONE, TestStandardZoneRules.OVERLAP)
    assertEquals(trans.isGap, false)
    assertEquals(trans.isOverlap, true)
    assertEquals(trans.getOffsetBefore, TestStandardZoneRules.OFFSET_PONE)
    assertEquals(trans.getOffsetAfter, TestStandardZoneRules.OFFSET_ZERO)
    assertEquals(trans.getInstant, createInstant(2008, 10, 26, 1, 0, ZoneOffset.UTC))
    assertEquals(trans.getDateTimeBefore, LocalDateTime.of(2008, 10, 26, 2, 0))
    assertEquals(trans.getDateTimeAfter, LocalDateTime.of(2008, 10, 26, 1, 0))
    assertEquals(trans.isValidOffset(ZoneOffset.ofHours(-1)), false)
    assertEquals(trans.isValidOffset(TestStandardZoneRules.OFFSET_ZERO), true)
    assertEquals(trans.isValidOffset(TestStandardZoneRules.OFFSET_PONE), true)
    assertEquals(trans.isValidOffset(TestStandardZoneRules.OFFSET_PTWO), false)
    assertEquals(trans.toString, "Transition[Overlap at 2008-10-26T02:00+01:00 to Z]")
  }

  test("Dublin_getStandardOffset") {
    val test: ZoneRules = europeDublin
    var zdt: ZonedDateTime = createZDT(1840, 1, 1, ZoneOffset.UTC)
    while (zdt.getYear < 2010) {
      val instant: Instant = zdt.toInstant
      if (zdt.getYear < 1881) {
          assertEquals(test.getStandardOffset(instant), ZoneOffset.ofHoursMinutes(0, -25))
      } else if (zdt.getYear >= 1881 && zdt.getYear < 1917) {
          assertEquals(test.getStandardOffset(instant), ZoneOffset.ofHoursMinutesSeconds(0, -25, -21))
      } else if (zdt.getYear >= 1917 && zdt.getYear < 1969) {
          assertEquals(test.getStandardOffset(instant), TestStandardZoneRules.OFFSET_ZERO, zdt.toString())
      } else {
          // assertEquals(test.getStandardOffset(instant), TestStandardZoneRules.OFFSET_PONE)  // negative DST
      }
      zdt = zdt.plusMonths(6)
    }
  }

  test("Dublin_dst") {
    val test = europeDublin
    assertEquals(test.isDaylightSavings(createZDT(1960, 1, 1, ZoneOffset.UTC).toInstant), false)
    assertEquals(test.getDaylightSavings(createZDT(1960, 1, 1, ZoneOffset.UTC).toInstant), Duration.ofHours(0))
    assertEquals(test.isDaylightSavings(createZDT(1960, 7, 1, ZoneOffset.UTC).toInstant), true)
    assertEquals(test.getDaylightSavings(createZDT(1960, 7, 1, ZoneOffset.UTC).toInstant), Duration.ofHours(1))
    // negative DST causes isDaylightSavings() to reverse
    // assertEquals(test.isDaylightSavings(createZDT(2016, 1, 1, ZoneOffset.UTC).toInstant), true)
    // assertEquals(test.getDaylightSavings(createZDT(2016, 1, 1, ZoneOffset.UTC).toInstant), Duration.ofHours(-1))
    // assertEquals(test.isDaylightSavings(createZDT(2016, 7, 1, ZoneOffset.UTC).toInstant), false)
    // assertEquals(test.getDaylightSavings(createZDT(2016, 7, 1, ZoneOffset.UTC).toInstant), Duration.ofHours(0))

    // TZDB data is messed up, comment out tests until better fix available
    // val formatter1 = new DateTimeFormatterBuilder().appendZoneText(TextStyle.FULL).toFormatter()
    // assertEquals(formatter1.format(createZDT(2016, 1, 1, ZoneId.of("Europe/Dublin"))), "Greenwich Mean Time")
    // assertEquals(formatter1.format(createZDT(2016, 7, 1, ZoneId.of("Europe/Dublin"))), "Irish Standard Time")
    //
    // val formatter2 = new DateTimeFormatterBuilder().appendZoneText(TextStyle.SHORT).toFormatter()
    // assertEquals(formatter2.format(createZDT(2016, 1, 1, ZoneId.of("Europe/Dublin"))), "GMT")
    // assertEquals(formatter2.format(createZDT(2016, 7, 1, ZoneId.of("Europe/Dublin"))), "IST")
  }

  private def europeParis: ZoneRules = {
    ZoneId.of("Europe/Paris").getRules
  }

  test("Paris") {
    val test: ZoneRules = europeParis
    assertEquals(test.isFixedOffset, false)
  }

  test("Paris_preTimeZones") {
    val test: ZoneRules = europeParis
    val old: ZonedDateTime = createZDT(1800, 1, 1, ZoneOffset.UTC)
    val instant: Instant = old.toInstant
    val offset: ZoneOffset = ZoneOffset.ofHoursMinutesSeconds(0, 9, 21)
    assertEquals(test.getOffset(instant), offset)
    checkOffset(test, old.toLocalDateTime, offset, 1)
    assertEquals(test.getStandardOffset(instant), offset)
    assertEquals(test.getDaylightSavings(instant), Duration.ZERO)
    assertEquals(test.isDaylightSavings(instant), false)
  }

  test("Paris_getOffset") {
    val test: ZoneRules = europeParis
    assertEquals(test.getOffset(createInstant(2008, 1, 1, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 2, 1, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 3, 1, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 4, 1, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PTWO)
    assertEquals(test.getOffset(createInstant(2008, 5, 1, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PTWO)
    assertEquals(test.getOffset(createInstant(2008, 6, 1, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PTWO)
    assertEquals(test.getOffset(createInstant(2008, 7, 1, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PTWO)
    assertEquals(test.getOffset(createInstant(2008, 8, 1, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PTWO)
    assertEquals(test.getOffset(createInstant(2008, 9, 1, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PTWO)
    assertEquals(test.getOffset(createInstant(2008, 10, 1, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PTWO)
    assertEquals(test.getOffset(createInstant(2008, 11, 1, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 12, 1, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
  }

  test("Paris_getOffset_toDST") {
    val test: ZoneRules = europeParis
    assertEquals(test.getOffset(createInstant(2008, 3, 24, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 3, 25, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 3, 26, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 3, 27, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 3, 28, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 3, 29, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 3, 30, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 3, 31, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PTWO)
    assertEquals(test.getOffset(createInstant(2008, 3, 30, 0, 59, 59, 999999999, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 3, 30, 1, 0, 0, 0, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PTWO)
  }

  test("Paris_getOffset_fromDST") {
    val test: ZoneRules = europeParis
    assertEquals(test.getOffset(createInstant(2008, 10, 24, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PTWO)
    assertEquals(test.getOffset(createInstant(2008, 10, 25, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PTWO)
    assertEquals(test.getOffset(createInstant(2008, 10, 26, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PTWO)
    assertEquals(test.getOffset(createInstant(2008, 10, 27, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 10, 28, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 10, 29, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 10, 30, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 10, 31, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
    assertEquals(test.getOffset(createInstant(2008, 10, 26, 0, 59, 59, 999999999, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PTWO)
    assertEquals(test.getOffset(createInstant(2008, 10, 26, 1, 0, 0, 0, ZoneOffset.UTC)), TestStandardZoneRules.OFFSET_PONE)
  }

  test("Paris_getOffsetInfo") {
    val test: ZoneRules = europeParis
    checkOffset(test, createLDT(2008, 1, 1), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, createLDT(2008, 2, 1), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, createLDT(2008, 3, 1), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, createLDT(2008, 4, 1), TestStandardZoneRules.OFFSET_PTWO, 1)
    checkOffset(test, createLDT(2008, 5, 1), TestStandardZoneRules.OFFSET_PTWO, 1)
    checkOffset(test, createLDT(2008, 6, 1), TestStandardZoneRules.OFFSET_PTWO, 1)
    checkOffset(test, createLDT(2008, 7, 1), TestStandardZoneRules.OFFSET_PTWO, 1)
    checkOffset(test, createLDT(2008, 8, 1), TestStandardZoneRules.OFFSET_PTWO, 1)
    checkOffset(test, createLDT(2008, 9, 1), TestStandardZoneRules.OFFSET_PTWO, 1)
    checkOffset(test, createLDT(2008, 10, 1), TestStandardZoneRules.OFFSET_PTWO, 1)
    checkOffset(test, createLDT(2008, 11, 1), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, createLDT(2008, 12, 1), TestStandardZoneRules.OFFSET_PONE, 1)
  }

  test("Paris_getOffsetInfo_toDST") {
    val test: ZoneRules = europeParis
    checkOffset(test, createLDT(2008, 3, 24), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, createLDT(2008, 3, 25), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, createLDT(2008, 3, 26), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, createLDT(2008, 3, 27), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, createLDT(2008, 3, 28), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, createLDT(2008, 3, 29), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, createLDT(2008, 3, 30), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, createLDT(2008, 3, 31), TestStandardZoneRules.OFFSET_PTWO, 1)
    checkOffset(test, LocalDateTime.of(2008, 3, 30, 1, 59, 59, 999999999), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, LocalDateTime.of(2008, 3, 30, 3, 0, 0, 0), TestStandardZoneRules.OFFSET_PTWO, 1)
  }

  test("Paris_getOffsetInfo_fromDST") {
    val test: ZoneRules = europeParis
    checkOffset(test, createLDT(2008, 10, 24), TestStandardZoneRules.OFFSET_PTWO, 1)
    checkOffset(test, createLDT(2008, 10, 25), TestStandardZoneRules.OFFSET_PTWO, 1)
    checkOffset(test, createLDT(2008, 10, 26), TestStandardZoneRules.OFFSET_PTWO, 1)
    checkOffset(test, createLDT(2008, 10, 27), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, createLDT(2008, 10, 28), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, createLDT(2008, 10, 29), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, createLDT(2008, 10, 30), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, createLDT(2008, 10, 31), TestStandardZoneRules.OFFSET_PONE, 1)
    checkOffset(test, LocalDateTime.of(2008, 10, 26, 1, 59, 59, 999999999), TestStandardZoneRules.OFFSET_PTWO, 1)
    checkOffset(test, LocalDateTime.of(2008, 10, 26, 3, 0, 0, 0), TestStandardZoneRules.OFFSET_PONE, 1)
  }

  test("Paris_getOffsetInfo_gap") {
    val test: ZoneRules = europeParis
    val dateTime: LocalDateTime = LocalDateTime.of(2008, 3, 30, 2, 0, 0, 0)
    val trans: ZoneOffsetTransition = checkOffset(test, dateTime, TestStandardZoneRules.OFFSET_PONE, TestStandardZoneRules.GAP)
    assertEquals(trans.isGap, true)
    assertEquals(trans.isOverlap, false)
    assertEquals(trans.getOffsetBefore, TestStandardZoneRules.OFFSET_PONE)
    assertEquals(trans.getOffsetAfter, TestStandardZoneRules.OFFSET_PTWO)
    assertEquals(trans.getInstant, createInstant(2008, 3, 30, 1, 0, ZoneOffset.UTC))
    assertEquals(trans.isValidOffset(TestStandardZoneRules.OFFSET_ZERO), false)
    assertEquals(trans.isValidOffset(TestStandardZoneRules.OFFSET_PONE), false)
    assertEquals(trans.isValidOffset(TestStandardZoneRules.OFFSET_PTWO), false)
    assertEquals(trans.toString, "Transition[Gap at 2008-03-30T02:00+01:00 to +02:00]")
    assertFalse(trans == null)
    assertNotEquals(trans, TestStandardZoneRules.OFFSET_PONE)
    assertTrue(trans == trans)
    val otherTrans: ZoneOffsetTransition = test.getTransition(dateTime)
    assertTrue(trans == otherTrans)
    assertEquals(trans.hashCode, otherTrans.hashCode)
  }

  test("Paris_getOffsetInfo_overlap") {
    val test: ZoneRules = europeParis
    val dateTime: LocalDateTime = LocalDateTime.of(2008, 10, 26, 2, 0, 0, 0)
    val trans: ZoneOffsetTransition = checkOffset(test, dateTime, TestStandardZoneRules.OFFSET_PTWO, TestStandardZoneRules.OVERLAP)
    assertEquals(trans.isGap, false)
    assertEquals(trans.isOverlap, true)
    assertEquals(trans.getOffsetBefore, TestStandardZoneRules.OFFSET_PTWO)
    assertEquals(trans.getOffsetAfter, TestStandardZoneRules.OFFSET_PONE)
    assertEquals(trans.getInstant, createInstant(2008, 10, 26, 1, 0, ZoneOffset.UTC))
    assertEquals(trans.isValidOffset(TestStandardZoneRules.OFFSET_ZERO), false)
    assertEquals(trans.isValidOffset(TestStandardZoneRules.OFFSET_PONE), true)
    assertEquals(trans.isValidOffset(TestStandardZoneRules.OFFSET_PTWO), true)
    assertEquals(trans.isValidOffset(ZoneOffset.ofHours(3)), false)
    assertEquals(trans.toString, "Transition[Overlap at 2008-10-26T03:00+02:00 to +01:00]")
    assertFalse(trans == null)
    assertNotEquals(trans, TestStandardZoneRules.OFFSET_PTWO)
    assertTrue(trans == trans)
    val otherTrans: ZoneOffsetTransition = test.getTransition(dateTime)
    assertTrue(trans == otherTrans)
    assertEquals(trans.hashCode, otherTrans.hashCode)
  }

  test("Paris_getStandardOffset") {
    val test: ZoneRules = europeParis
    var zdt: ZonedDateTime = createZDT(1840, 1, 1, ZoneOffset.UTC)
    while (zdt.getYear < 2010) {
      val instant: Instant = zdt.toInstant
      if (zdt.toLocalDate.isBefore(LocalDate.of(1911, 3, 11))) {
        assertEquals(test.getStandardOffset(instant), ZoneOffset.ofHoursMinutesSeconds(0, 9, 21))
      }
      else if (zdt.toLocalDate.isBefore(LocalDate.of(1940, 6, 14))) {
        assertEquals(test.getStandardOffset(instant), TestStandardZoneRules.OFFSET_ZERO)
      }
      else if (zdt.toLocalDate.isBefore(LocalDate.of(1944, 8, 25))) {
        assertEquals(test.getStandardOffset(instant), TestStandardZoneRules.OFFSET_PONE)
      }
      else if (zdt.toLocalDate.isBefore(LocalDate.of(1945, 9, 16))) {
        assertEquals(test.getStandardOffset(instant), TestStandardZoneRules.OFFSET_ZERO)
      }
      else {
        assertEquals(test.getStandardOffset(instant), TestStandardZoneRules.OFFSET_PONE)
      }
      zdt = zdt.plusMonths(6)
    }
  }

  private def americaNewYork: ZoneRules = {
    ZoneId.of("America/New_York").getRules
  }

  test("NewYork") {
    val test: ZoneRules = americaNewYork
    assertEquals(test.isFixedOffset, false)
  }

  test("NewYork_preTimeZones") {
    val test: ZoneRules = americaNewYork
    val old: ZonedDateTime = createZDT(1800, 1, 1, ZoneOffset.UTC)
    val instant: Instant = old.toInstant
    val offset: ZoneOffset = ZoneOffset.of("-04:56:02")
    assertEquals(test.getOffset(instant), offset)
    checkOffset(test, old.toLocalDateTime, offset, 1)
    assertEquals(test.getStandardOffset(instant), offset)
    assertEquals(test.getDaylightSavings(instant), Duration.ZERO)
    assertEquals(test.isDaylightSavings(instant), false)
  }

  test("NewYork_getOffset") {
    val test: ZoneRules = americaNewYork
    val offset: ZoneOffset = ZoneOffset.ofHours(-5)
    assertEquals(test.getOffset(createInstant(2008, 1, 1, offset)), ZoneOffset.ofHours(-5))
    assertEquals(test.getOffset(createInstant(2008, 2, 1, offset)), ZoneOffset.ofHours(-5))
    assertEquals(test.getOffset(createInstant(2008, 3, 1, offset)), ZoneOffset.ofHours(-5))
    assertEquals(test.getOffset(createInstant(2008, 4, 1, offset)), ZoneOffset.ofHours(-4))
    assertEquals(test.getOffset(createInstant(2008, 5, 1, offset)), ZoneOffset.ofHours(-4))
    assertEquals(test.getOffset(createInstant(2008, 6, 1, offset)), ZoneOffset.ofHours(-4))
    assertEquals(test.getOffset(createInstant(2008, 7, 1, offset)), ZoneOffset.ofHours(-4))
    assertEquals(test.getOffset(createInstant(2008, 8, 1, offset)), ZoneOffset.ofHours(-4))
    assertEquals(test.getOffset(createInstant(2008, 9, 1, offset)), ZoneOffset.ofHours(-4))
    assertEquals(test.getOffset(createInstant(2008, 10, 1, offset)), ZoneOffset.ofHours(-4))
    assertEquals(test.getOffset(createInstant(2008, 11, 1, offset)), ZoneOffset.ofHours(-4))
    assertEquals(test.getOffset(createInstant(2008, 12, 1, offset)), ZoneOffset.ofHours(-5))
    assertEquals(test.getOffset(createInstant(2008, 1, 28, offset)), ZoneOffset.ofHours(-5))
    assertEquals(test.getOffset(createInstant(2008, 2, 28, offset)), ZoneOffset.ofHours(-5))
    assertEquals(test.getOffset(createInstant(2008, 3, 28, offset)), ZoneOffset.ofHours(-4))
    assertEquals(test.getOffset(createInstant(2008, 4, 28, offset)), ZoneOffset.ofHours(-4))
    assertEquals(test.getOffset(createInstant(2008, 5, 28, offset)), ZoneOffset.ofHours(-4))
    assertEquals(test.getOffset(createInstant(2008, 6, 28, offset)), ZoneOffset.ofHours(-4))
    assertEquals(test.getOffset(createInstant(2008, 7, 28, offset)), ZoneOffset.ofHours(-4))
    assertEquals(test.getOffset(createInstant(2008, 8, 28, offset)), ZoneOffset.ofHours(-4))
    assertEquals(test.getOffset(createInstant(2008, 9, 28, offset)), ZoneOffset.ofHours(-4))
    assertEquals(test.getOffset(createInstant(2008, 10, 28, offset)), ZoneOffset.ofHours(-4))
    assertEquals(test.getOffset(createInstant(2008, 11, 28, offset)), ZoneOffset.ofHours(-5))
    assertEquals(test.getOffset(createInstant(2008, 12, 28, offset)), ZoneOffset.ofHours(-5))
  }

  test("NewYork_getOffset_toDST") {
    val test: ZoneRules = americaNewYork
    val offset: ZoneOffset = ZoneOffset.ofHours(-5)
    assertEquals(test.getOffset(createInstant(2008, 3, 8, offset)), ZoneOffset.ofHours(-5))
    assertEquals(test.getOffset(createInstant(2008, 3, 9, offset)), ZoneOffset.ofHours(-5))
    assertEquals(test.getOffset(createInstant(2008, 3, 10, offset)), ZoneOffset.ofHours(-4))
    assertEquals(test.getOffset(createInstant(2008, 3, 11, offset)), ZoneOffset.ofHours(-4))
    assertEquals(test.getOffset(createInstant(2008, 3, 12, offset)), ZoneOffset.ofHours(-4))
    assertEquals(test.getOffset(createInstant(2008, 3, 13, offset)), ZoneOffset.ofHours(-4))
    assertEquals(test.getOffset(createInstant(2008, 3, 14, offset)), ZoneOffset.ofHours(-4))
    assertEquals(test.getOffset(createInstant(2008, 3, 9, 1, 59, 59, 999999999, offset)), ZoneOffset.ofHours(-5))
    assertEquals(test.getOffset(createInstant(2008, 3, 9, 2, 0, 0, 0, offset)), ZoneOffset.ofHours(-4))
  }

  test("NewYork_getOffset_fromDST") {
    val test: ZoneRules = americaNewYork
    val offset: ZoneOffset = ZoneOffset.ofHours(-4)
    assertEquals(test.getOffset(createInstant(2008, 11, 1, offset)), ZoneOffset.ofHours(-4))
    assertEquals(test.getOffset(createInstant(2008, 11, 2, offset)), ZoneOffset.ofHours(-4))
    assertEquals(test.getOffset(createInstant(2008, 11, 3, offset)), ZoneOffset.ofHours(-5))
    assertEquals(test.getOffset(createInstant(2008, 11, 4, offset)), ZoneOffset.ofHours(-5))
    assertEquals(test.getOffset(createInstant(2008, 11, 5, offset)), ZoneOffset.ofHours(-5))
    assertEquals(test.getOffset(createInstant(2008, 11, 6, offset)), ZoneOffset.ofHours(-5))
    assertEquals(test.getOffset(createInstant(2008, 11, 7, offset)), ZoneOffset.ofHours(-5))
    assertEquals(test.getOffset(createInstant(2008, 11, 2, 1, 59, 59, 999999999, offset)), ZoneOffset.ofHours(-4))
    assertEquals(test.getOffset(createInstant(2008, 11, 2, 2, 0, 0, 0, offset)), ZoneOffset.ofHours(-5))
  }

  test("NewYork_getOffsetInfo") {
    val test: ZoneRules = americaNewYork
    checkOffset(test, createLDT(2008, 1, 1), ZoneOffset.ofHours(-5), 1)
    checkOffset(test, createLDT(2008, 2, 1), ZoneOffset.ofHours(-5), 1)
    checkOffset(test, createLDT(2008, 3, 1), ZoneOffset.ofHours(-5), 1)
    checkOffset(test, createLDT(2008, 4, 1), ZoneOffset.ofHours(-4), 1)
    checkOffset(test, createLDT(2008, 5, 1), ZoneOffset.ofHours(-4), 1)
    checkOffset(test, createLDT(2008, 6, 1), ZoneOffset.ofHours(-4), 1)
    checkOffset(test, createLDT(2008, 7, 1), ZoneOffset.ofHours(-4), 1)
    checkOffset(test, createLDT(2008, 8, 1), ZoneOffset.ofHours(-4), 1)
    checkOffset(test, createLDT(2008, 9, 1), ZoneOffset.ofHours(-4), 1)
    checkOffset(test, createLDT(2008, 10, 1), ZoneOffset.ofHours(-4), 1)
    checkOffset(test, createLDT(2008, 11, 1), ZoneOffset.ofHours(-4), 1)
    checkOffset(test, createLDT(2008, 12, 1), ZoneOffset.ofHours(-5), 1)
    checkOffset(test, createLDT(2008, 1, 28), ZoneOffset.ofHours(-5), 1)
    checkOffset(test, createLDT(2008, 2, 28), ZoneOffset.ofHours(-5), 1)
    checkOffset(test, createLDT(2008, 3, 28), ZoneOffset.ofHours(-4), 1)
    checkOffset(test, createLDT(2008, 4, 28), ZoneOffset.ofHours(-4), 1)
    checkOffset(test, createLDT(2008, 5, 28), ZoneOffset.ofHours(-4), 1)
    checkOffset(test, createLDT(2008, 6, 28), ZoneOffset.ofHours(-4), 1)
    checkOffset(test, createLDT(2008, 7, 28), ZoneOffset.ofHours(-4), 1)
    checkOffset(test, createLDT(2008, 8, 28), ZoneOffset.ofHours(-4), 1)
    checkOffset(test, createLDT(2008, 9, 28), ZoneOffset.ofHours(-4), 1)
    checkOffset(test, createLDT(2008, 10, 28), ZoneOffset.ofHours(-4), 1)
    checkOffset(test, createLDT(2008, 11, 28), ZoneOffset.ofHours(-5), 1)
    checkOffset(test, createLDT(2008, 12, 28), ZoneOffset.ofHours(-5), 1)
  }

  test("NewYork_getOffsetInfo_toDST") {
    val test: ZoneRules = americaNewYork
    checkOffset(test, createLDT(2008, 3, 8), ZoneOffset.ofHours(-5), 1)
    checkOffset(test, createLDT(2008, 3, 9), ZoneOffset.ofHours(-5), 1)
    checkOffset(test, createLDT(2008, 3, 10), ZoneOffset.ofHours(-4), 1)
    checkOffset(test, createLDT(2008, 3, 11), ZoneOffset.ofHours(-4), 1)
    checkOffset(test, createLDT(2008, 3, 12), ZoneOffset.ofHours(-4), 1)
    checkOffset(test, createLDT(2008, 3, 13), ZoneOffset.ofHours(-4), 1)
    checkOffset(test, createLDT(2008, 3, 14), ZoneOffset.ofHours(-4), 1)
    checkOffset(test, LocalDateTime.of(2008, 3, 9, 1, 59, 59, 999999999), ZoneOffset.ofHours(-5), 1)
    checkOffset(test, LocalDateTime.of(2008, 3, 9, 3, 0, 0, 0), ZoneOffset.ofHours(-4), 1)
  }

  test("NewYork_getOffsetInfo_fromDST") {
    val test: ZoneRules = americaNewYork
    checkOffset(test, createLDT(2008, 11, 1), ZoneOffset.ofHours(-4), 1)
    checkOffset(test, createLDT(2008, 11, 2), ZoneOffset.ofHours(-4), 1)
    checkOffset(test, createLDT(2008, 11, 3), ZoneOffset.ofHours(-5), 1)
    checkOffset(test, createLDT(2008, 11, 4), ZoneOffset.ofHours(-5), 1)
    checkOffset(test, createLDT(2008, 11, 5), ZoneOffset.ofHours(-5), 1)
    checkOffset(test, createLDT(2008, 11, 6), ZoneOffset.ofHours(-5), 1)
    checkOffset(test, createLDT(2008, 11, 7), ZoneOffset.ofHours(-5), 1)
    checkOffset(test, LocalDateTime.of(2008, 11, 2, 0, 59, 59, 999999999), ZoneOffset.ofHours(-4), 1)
    checkOffset(test, LocalDateTime.of(2008, 11, 2, 2, 0, 0, 0), ZoneOffset.ofHours(-5), 1)
  }

  test("NewYork_getOffsetInfo_gap") {
    val test: ZoneRules = americaNewYork
    val dateTime: LocalDateTime = LocalDateTime.of(2008, 3, 9, 2, 0, 0, 0)
    val trans: ZoneOffsetTransition = checkOffset(test, dateTime, ZoneOffset.ofHours(-5), TestStandardZoneRules.GAP)
    assertEquals(trans.isGap, true)
    assertEquals(trans.isOverlap, false)
    assertEquals(trans.getOffsetBefore, ZoneOffset.ofHours(-5))
    assertEquals(trans.getOffsetAfter, ZoneOffset.ofHours(-4))
    assertEquals(trans.getInstant, createInstant(2008, 3, 9, 2, 0, ZoneOffset.ofHours(-5)))
    assertEquals(trans.isValidOffset(TestStandardZoneRules.OFFSET_PTWO), false)
    assertEquals(trans.isValidOffset(ZoneOffset.ofHours(-5)), false)
    assertEquals(trans.isValidOffset(ZoneOffset.ofHours(-4)), false)
    assertEquals(trans.toString, "Transition[Gap at 2008-03-09T02:00-05:00 to -04:00]")
    assertFalse(trans == null)
    assertNotEquals(trans, ZoneOffset.ofHours(-5))
    assertTrue(trans == trans)
    val otherTrans: ZoneOffsetTransition = test.getTransition(dateTime)
    assertTrue(trans == otherTrans)
    assertEquals(trans.hashCode, otherTrans.hashCode)
  }

  test("NewYork_getOffsetInfo_overlap") {
    val test: ZoneRules = americaNewYork
    val dateTime: LocalDateTime = LocalDateTime.of(2008, 11, 2, 1, 0, 0, 0)
    val trans: ZoneOffsetTransition = checkOffset(test, dateTime, ZoneOffset.ofHours(-4), TestStandardZoneRules.OVERLAP)
    assertEquals(trans.isGap, false)
    assertEquals(trans.isOverlap, true)
    assertEquals(trans.getOffsetBefore, ZoneOffset.ofHours(-4))
    assertEquals(trans.getOffsetAfter, ZoneOffset.ofHours(-5))
    assertEquals(trans.getInstant, createInstant(2008, 11, 2, 2, 0, ZoneOffset.ofHours(-4)))
    assertEquals(trans.isValidOffset(ZoneOffset.ofHours(-1)), false)
    assertEquals(trans.isValidOffset(ZoneOffset.ofHours(-5)), true)
    assertEquals(trans.isValidOffset(ZoneOffset.ofHours(-4)), true)
    assertEquals(trans.isValidOffset(TestStandardZoneRules.OFFSET_PTWO), false)
    assertEquals(trans.toString, "Transition[Overlap at 2008-11-02T02:00-04:00 to -05:00]")
    assertFalse(trans == null)
    assertNotEquals(trans, ZoneOffset.ofHours(-4))
    assertTrue(trans == trans)
    val otherTrans: ZoneOffsetTransition = test.getTransition(dateTime)
    assertTrue(trans == otherTrans)
    assertEquals(trans.hashCode, otherTrans.hashCode)
  }

  test("NewYork_getStandardOffset") {
    val test: ZoneRules = americaNewYork
    var dateTime: ZonedDateTime = createZDT(1860, 1, 1, ZoneOffset.UTC)
    while (dateTime.getYear < 2010) {
      val instant: Instant = dateTime.toInstant
      if (dateTime.toLocalDate.isBefore(LocalDate.of(1883, 11, 18))) {
        assertEquals(test.getStandardOffset(instant), ZoneOffset.of("-04:56:02"))
      }
      else {
        assertEquals(test.getStandardOffset(instant), ZoneOffset.ofHours(-5))
      }
      dateTime = dateTime.plusMonths(6)
    }
  }

  private def asiaKathmandu: ZoneRules = {
    ZoneId.of("Asia/Kathmandu").getRules
  }

  test("Kathmandu_nextTransition_historic") {
    val test: ZoneRules = asiaKathmandu
    val trans: java.util.List[ZoneOffsetTransition] = test.getTransitions
    val first: ZoneOffsetTransition = trans.get(0)
    assertEquals(test.nextTransition(first.getInstant.minusNanos(1)), first)
    var i: Int = 0
    while (i < trans.size - 1) {
      val cur: ZoneOffsetTransition = trans.get(i)
      val next: ZoneOffsetTransition = trans.get(i + 1)
      assertEquals(test.nextTransition(cur.getInstant), next)
      assertEquals(test.nextTransition(next.getInstant.minusNanos(1)), next)
      i += 1
    }
  }

  test("Kathmandu_nextTransition_noRules") {
    val test: ZoneRules = asiaKathmandu
    val trans: java.util.List[ZoneOffsetTransition] = test.getTransitions
    val last: ZoneOffsetTransition = trans.get(trans.size - 1)
    assertEquals(test.nextTransition(last.getInstant), null)
  }

  test("getTransitions_immutable") {
    assertThrows[UnsupportedOperationException] {
      val test: ZoneRules = europeParis
      test.getTransitions.clear()
    }
  }

  test("getTransitionRules_immutable") {
    assertThrows[UnsupportedOperationException] {
      val test: ZoneRules = europeParis
      test.getTransitionRules.clear()
    }
  }

  test("equals") {
    val test1: ZoneRules = europeLondon
    val test2: ZoneRules = europeParis
    val test2b: ZoneRules = europeParis
    assertEquals(test1 == test2, false)
    assertEquals(test2 == test1, false)
    assertEquals(test1 == test1, true)
    assertEquals(test2 == test2, true)
    assertEquals(test2 == test2b, true)
    assertEquals(test1.hashCode == test1.hashCode, true)
    assertEquals(test2.hashCode == test2.hashCode, true)
    assertEquals(test2.hashCode == test2b.hashCode, true)
  }

  test("equals_null") {
    assertEquals(europeLondon == null, false)
  }

  test("equals_notZoneRules") {
    assertNotEquals(europeLondon, "Europe/London")
  }

  test("toString") {
    assertEquals(europeLondon.toString.contains("ZoneRules"), true)
  }

  private def createInstant(year: Int, month: Int, day: Int, offset: ZoneOffset): Instant = {
    LocalDateTime.of(year, month, day, 0, 0).toInstant(offset)
  }

  private def createInstant(year: Int, month: Int, day: Int, hour: Int, min: Int, offset: ZoneOffset): Instant = {
    LocalDateTime.of(year, month, day, hour, min).toInstant(offset)
  }

  private def createInstant(year: Int, month: Int, day: Int, hour: Int, min: Int, sec: Int, nano: Int, offset: ZoneOffset): Instant = {
    LocalDateTime.of(year, month, day, hour, min, sec, nano).toInstant(offset)
  }

  private def createZDT(year: Int, month: Int, day: Int, zone: ZoneId): ZonedDateTime = {
    LocalDateTime.of(year, month, day, 0, 0).atZone(zone)
  }

  private def createLDT(year: Int, month: Int, day: Int): LocalDateTime = {
    LocalDateTime.of(year, month, day, 0, 0)
  }

  private def checkOffset(rules: ZoneRules, dateTime: LocalDateTime, offset: ZoneOffset, `type`: Int): ZoneOffsetTransition = {
    val validOffsets: java.util.List[ZoneOffset] = rules.getValidOffsets(dateTime)
    assertEquals(validOffsets.size, `type`)
    assertEquals(rules.getOffset(dateTime), offset)
    if (`type` == 1) {
      assertEquals(validOffsets.get(0), offset)
      null
    }
    else {
      val zot: ZoneOffsetTransition = rules.getTransition(dateTime)
      assertNotNull(zot)
      assertEquals(zot.isOverlap, `type` == 2)
      assertEquals(zot.isGap, `type` == 0)
      assertEquals(zot.isValidOffset(offset), `type` == 2)
      zot
    }
  }
}
