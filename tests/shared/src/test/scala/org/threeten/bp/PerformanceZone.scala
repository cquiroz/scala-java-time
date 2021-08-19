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

import java.text.NumberFormat
import java.util.{ Calendar, Date, GregorianCalendar, TimeZone }
import org.threeten.bp.zone.ZoneRules

/**
 * Test Performance.
 */
object PerformanceZone {

  /** The year to test. */
  private val YEAR: Int = 1980

  /** Size. */
  private val NF: NumberFormat = {
    val nf = NumberFormat.getIntegerInstance
    nf.setGroupingUsed(true)
    nf
  }

  /** Size. */
  private val SIZE: Int = 200000

  /**
   * Main.
   * @param args
   *   the arguments
   */
  def main(args: Array[String]): Unit = {
    val time: LocalTime = LocalTime.of(12, 30, 20)
    System.out.println(time)

    {
      var i: Int = 0
      while (i < 6) {
        {
          jsrLocalGetOffset()
          jsrInstantGetOffset()
          jsrRulesLocalGetOffset()
          jsrRulesInstantGetOffset()
          jdkLocalGetOffset()
          jdkInstantGetOffset()
          System.out.println()
        }
        {
          i += 1
          i - 1
        }
      }
    }
  }

  private def jsrLocalGetOffset(): Unit = {
    val dt: LocalDateTime       = LocalDateTime.of(YEAR, 6, 1, 12, 0)
    val tz: ZoneId              = ZoneId.of("Europe/London")
    val list: Array[ZoneOffset] = new Array[ZoneOffset](SIZE)
    val start: Long             = System.nanoTime

    {
      var i: Int = 0
      while (i < SIZE) {
        {
          list(i) = tz.getRules.getOffset(dt)
        }
        {
          i += 1
          i - 1
        }
      }
    }
    val end: Long = System.nanoTime
    System.out.println("JSR-Loc: Setup:  " + NF.format(end - start) + " ns" + list(0))
  }

  private def jsrInstantGetOffset(): Unit = {
    val instant: Instant        = LocalDateTime.of(YEAR, 6, 1, 12, 0).toInstant(ZoneOffset.ofHours(1))
    val tz: ZoneId              = ZoneId.of("Europe/London")
    val list: Array[ZoneOffset] = new Array[ZoneOffset](SIZE)
    val start: Long             = System.nanoTime

    {
      var i: Int = 0
      while (i < SIZE) {
        {
          list(i) = tz.getRules.getOffset(instant)
        }
        {
          i += 1
          i - 1
        }
      }
    }
    val end: Long = System.nanoTime
    System.out.println("JSR-Ins: Setup:  " + NF.format(end - start) + " ns" + list(0))
  }

  private def jsrRulesLocalGetOffset(): Unit = {
    val dt: LocalDateTime       = LocalDateTime.of(YEAR, 6, 1, 12, 0)
    val tz: ZoneRules           = ZoneId.of("Europe/London").getRules
    val list: Array[ZoneOffset] = new Array[ZoneOffset](SIZE)
    val start: Long             = System.nanoTime

    {
      var i: Int = 0
      while (i < SIZE) {
        {
          list(i) = tz.getOffset(dt)
        }
        {
          i += 1
          i - 1
        }
      }
    }
    val end: Long = System.nanoTime
    System.out.println("JSR-LoR: Setup:  " + NF.format(end - start) + " ns" + list(0))
  }

  private def jsrRulesInstantGetOffset(): Unit = {
    val instant: Instant        = LocalDateTime.of(YEAR, 6, 1, 12, 0).toInstant(ZoneOffset.ofHours(1))
    val tz: ZoneRules           = ZoneId.of("Europe/London").getRules
    val list: Array[ZoneOffset] = new Array[ZoneOffset](SIZE)
    val start: Long             = System.nanoTime

    {
      var i: Int = 0
      while (i < SIZE) {
        {
          list(i) = tz.getOffset(instant)
        }
        {
          i += 1
          i - 1
        }
      }
    }
    val end: Long = System.nanoTime
    System.out.println("JSR-InR: Setup:  " + NF.format(end - start) + " ns" + list(0))
  }

  private def jdkLocalGetOffset(): Unit = {
    val tz: TimeZone     = java.util.TimeZone.getTimeZone("Europe/London")
    val list: Array[Int] = new Array[Int](SIZE)
    val start: Long      = System.nanoTime

    {
      var i: Int = 0
      while (i < SIZE) {
        {
          list(i) = tz.getOffset(GregorianCalendar.AD, YEAR, 0, 11, Calendar.SUNDAY, 0)
        }
        {
          i += 1
          i - 1
        }
      }
    }
    val end: Long = System.nanoTime
    System.out.println("GCalLoc: Setup:  " + NF.format(end - start) + " ns" + list(0))
  }

  private def jdkInstantGetOffset(): Unit = {
    val tz: TimeZone          = java.util.TimeZone.getTimeZone("Europe/London")
    val dt: GregorianCalendar = new GregorianCalendar(tz)
    dt.setGregorianChange(new Date(Long.MinValue))
    dt.set(YEAR, 5, 1, 12, 0)
    val list: Array[Int]      = new Array[Int](SIZE)
    val start: Long           = System.nanoTime

    {
      var i: Int = 0
      while (i < SIZE) {
        {
          list(i) = tz.getOffset(dt.getTimeInMillis)
        }
        {
          i += 1
          i - 1
        }
      }
    }
    val end: Long = System.nanoTime
    System.out.println("GCalIns: Setup:  " + NF.format(end - start) + " ns" + list(0))
  }
}
