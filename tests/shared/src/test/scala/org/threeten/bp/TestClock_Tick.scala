/*
 * Copyright (c) 2007-present Stephen Colebourne & Michael Nascimento Santos
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

/** Test tick clock. */
object TestClock_Tick {
  val MOSCOW: ZoneId     = ZoneId.of("Europe/Moscow")
  val PARIS: ZoneId      = ZoneId.of("Europe/Paris")
  val AMOUNT: Duration   = Duration.ofSeconds(2)
  val ZDT: ZonedDateTime =
    LocalDateTime.of(2008, 6, 30, 11, 30, 10, 500).atZone(ZoneOffset.ofHours(2))
  val INSTANT: Instant   = ZDT.toInstant
}

class TestClock_Tick extends AnyFunSuite with AssertionsHelper {
  test("tick_ClockDuration_250millis") {
    var i: Int = 0
    while (i < 1000) {
      {
        val test: Clock = Clock.tick(
          Clock.fixed(TestClock_Tick.ZDT.withNano(i * 1000000).toInstant, TestClock_Tick.PARIS),
          Duration.ofMillis(250)
        )
        assertEquals(test.instant, TestClock_Tick.ZDT.withNano((i / 250) * 250000000).toInstant)
        assertEquals(test.getZone, TestClock_Tick.PARIS)
      }
      {
        i += 1
        i - 1
      }
    }
  }

  test("tick_ClockDuration_250micros") {
    var i: Int = 0
    while (i < 1000) {
      {
        val test: Clock = Clock.tick(
          Clock.fixed(TestClock_Tick.ZDT.withNano(i * 1000).toInstant, TestClock_Tick.PARIS),
          Duration.ofNanos(250000)
        )
        assertEquals(test.instant, TestClock_Tick.ZDT.withNano((i / 250) * 250000).toInstant)
        assertEquals(test.getZone, TestClock_Tick.PARIS)
      }
      {
        i += 1
        i - 1
      }
    }
  }

  test("tick_ClockDuration_20nanos") {
    var i: Int = 0
    while (i < 1000) {
      {
        val test: Clock =
          Clock.tick(Clock.fixed(TestClock_Tick.ZDT.withNano(i).toInstant, TestClock_Tick.PARIS),
                     Duration.ofNanos(20)
          )
        assertEquals(test.instant, TestClock_Tick.ZDT.withNano((i / 20) * 20).toInstant)
        assertEquals(test.getZone, TestClock_Tick.PARIS)
      }
      {
        i += 1
        i - 1
      }
    }
  }

  test("tick_ClockDuration_zeroDuration") {
    val underlying: Clock = Clock.system(TestClock_Tick.PARIS)
    val test: Clock       = Clock.tick(underlying, Duration.ZERO)
    assertSame(test, underlying)
  }

  test("tick_ClockDuration_1nsDuration") {
    val underlying: Clock = Clock.system(TestClock_Tick.PARIS)
    val test: Clock       = Clock.tick(underlying, Duration.ofNanos(1))
    assertSame(test, underlying)
  }

  test("tick_ClockDuration_maxDuration") {
    assertThrows[ArithmeticException] {
      Clock.tick(Clock.systemUTC, Duration.ofSeconds(Long.MaxValue))
    }
  }

  test("tick_ClockDuration_subMilliNotDivisible_123ns") {
    assertThrows[IllegalArgumentException] {
      Clock.tick(Clock.systemUTC, Duration.ofSeconds(0, 123))
    }
  }

  test("tick_ClockDuration_subMilliNotDivisible_999ns") {
    assertThrows[IllegalArgumentException] {
      Clock.tick(Clock.systemUTC, Duration.ofSeconds(0, 999))
    }
  }

  test("tick_ClockDuration_subMilliNotDivisible_999999999ns") {
    assertThrows[IllegalArgumentException] {
      Clock.tick(Clock.systemUTC, Duration.ofSeconds(0, 999999999))
    }
  }

  test("tick_ClockDuration_negative1ns") {
    assertThrows[IllegalArgumentException] {
      Clock.tick(Clock.systemUTC, Duration.ofSeconds(0, -1))
    }
  }

  test("tick_ClockDuration_negative1s") {
    assertThrows[IllegalArgumentException] {
      Clock.tick(Clock.systemUTC, Duration.ofSeconds(-1))
    }
  }

  test("tick_ClockDuration_nullClock") {
    assertThrows[NullPointerException] {
      Clock.tick(null, Duration.ZERO)
    }
  }

  test("tick_ClockDuration_nullDuration") {
    assertThrows[NullPointerException] {
      Clock.tick(Clock.systemUTC, null)
    }
  }

  test("tickSeconds_ZoneId") {
    pending
    val test: Clock = Clock.tickSeconds(TestClock_Tick.PARIS)
    assertEquals(test.getZone, TestClock_Tick.PARIS)
    assertEquals(test.instant.getNano, 0)
    // Sleep is not available in scala.js
    //Thread.sleep(100)
    assertEquals(test.instant.getNano, 0)
  }

  test("tickSeconds_ZoneId_nullZoneId") {
    assertThrows[NullPointerException] {
      Clock.tickSeconds(null)
    }
  }

  test("tickMinutes_ZoneId") {
    val test: Clock      = Clock.tickMinutes(TestClock_Tick.PARIS)
    assertEquals(test.getZone, TestClock_Tick.PARIS)
    val instant: Instant = test.instant
    assertEquals(instant.getEpochSecond % 60, 0)
    assertEquals(instant.getNano, 0)
  }

  test("tickMinutes_ZoneId_nullZoneId") {
    assertThrows[NullPointerException] {
      Clock.tickMinutes(null)
    }
  }

  test("withZone") {
    val test: Clock    = Clock.tick(Clock.system(TestClock_Tick.PARIS), Duration.ofMillis(500))
    val changed: Clock = test.withZone(TestClock_Tick.MOSCOW)
    assertEquals(test.getZone, TestClock_Tick.PARIS)
    assertEquals(changed.getZone, TestClock_Tick.MOSCOW)
  }

  test("withZone_same") {
    val test: Clock    = Clock.tick(Clock.system(TestClock_Tick.PARIS), Duration.ofMillis(500))
    val changed: Clock = test.withZone(TestClock_Tick.PARIS)
    assertSame(test, changed)
  }

  test("withZone_null") {
    assertThrows[NullPointerException] {
      Clock.tick(Clock.system(TestClock_Tick.PARIS), Duration.ofMillis(500)).withZone(null)
    }
  }

  test("_equals") {
    val a: Clock = Clock.tick(Clock.system(TestClock_Tick.PARIS), Duration.ofMillis(500))
    val b: Clock = Clock.tick(Clock.system(TestClock_Tick.PARIS), Duration.ofMillis(500))
    assertEquals(a == a, true)
    assertEquals(a == b, true)
    assertEquals(b == a, true)
    assertEquals(b == b, true)
    val c: Clock = Clock.tick(Clock.system(TestClock_Tick.MOSCOW), Duration.ofMillis(500))
    assertEquals(a == c, false)
    val d: Clock = Clock.tick(Clock.system(TestClock_Tick.PARIS), Duration.ofMillis(499))
    assertEquals(a == d, false)
    assertEquals(a == null, false)
    assertNotEquals(a, "other type")
    assertEquals(a == Clock.systemUTC, false)
  }

  test("hashCode") {
    val a: Clock = Clock.tick(Clock.system(TestClock_Tick.PARIS), Duration.ofMillis(500))
    val b: Clock = Clock.tick(Clock.system(TestClock_Tick.PARIS), Duration.ofMillis(500))
    assertEquals(a.hashCode, a.hashCode)
    assertEquals(a.hashCode, b.hashCode)
    val c: Clock = Clock.tick(Clock.system(TestClock_Tick.MOSCOW), Duration.ofMillis(500))
    assertEquals(a.hashCode == c.hashCode, false)
    val d: Clock = Clock.tick(Clock.system(TestClock_Tick.PARIS), Duration.ofMillis(499))
    assertEquals(a.hashCode == d.hashCode, false)
  }

  test("toString") {
    val test: Clock = Clock.tick(Clock.systemUTC, Duration.ofMillis(500))
    assertEquals(test.toString, "TickClock[SystemClock[Z],PT0.5S]")
  }
}
