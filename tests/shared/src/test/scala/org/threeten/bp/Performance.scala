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

import org.threeten.bp.temporal.ChronoField.DAY_OF_MONTH
import org.threeten.bp.temporal.ChronoField.HOUR_OF_DAY
import org.threeten.bp.temporal.ChronoField.MINUTE_OF_HOUR
import org.threeten.bp.temporal.ChronoField.MONTH_OF_YEAR
import org.threeten.bp.temporal.ChronoField.NANO_OF_SECOND
import org.threeten.bp.temporal.ChronoField.SECOND_OF_MINUTE
import org.threeten.bp.temporal.ChronoField.YEAR
import java.math.BigDecimal
import java.text.NumberFormat
import java.text.SimpleDateFormat
import java.util.Arrays
import java.util.Calendar
import java.util.Collections
import java.util.Date
import java.util.GregorianCalendar
import java.util.Locale
import java.util.Random
import org.threeten.bp.format.DateTimeFormatter
import scala.collection.JavaConverters._

/** Test Performance. */
object Performance {

  /** Size. */
  private val NF: NumberFormat = {
    val nf = NumberFormat.getIntegerInstance
    nf.setGroupingUsed(true)
    nf
  }

  /** Size. */
  private val SIZE: Int = 100000

  /** Results. */
  private val RESULTS: java.util.Map[String, Array[Long]] =
    new java.util.TreeMap[String, Array[Long]]

  /** Count. */
  private var loop: Int = 0

  /**
   * Main.
   *
   * @param args
   *   the arguments
   */
  def main(args: Array[String]): Unit = {
    {
      loop = 0
      while (loop < 5) {
        {
          System.out.println("-------------------------------------")
          process
        }
        {
          loop += 1
          loop - 1
        }
      }
    }
    System.out.println()
    for (name <- RESULTS.keySet.asScala)
      System.out.println(name + " " + Arrays.toString(RESULTS.get(name)))
    System.out.println()
    for (name <- RESULTS.keySet.asScala) {
      val r: Array[Long]      = RESULTS.get(name)
      val percent: BigDecimal = BigDecimal.valueOf(r(6), 1)
      var max: String         = "           " + NF.format(r(0))
      max = max.substring(max.length - 12)
      var min: String         = "           " + NF.format(r(5))
      min = min.substring(min.length - 12)
      System.out.println(name + "\t" + max + "\t" + min + "\t-" + percent + "%")
    }
  }

  def process(): Unit = {
    val time: LocalTime                          = LocalTime.of(12, 30, 20)
    System.out.println(time)
    val ldt: java.util.List[LocalDateTime]       = setupDateTime
    queryListDateTime(ldt)
    formatListDateTime(ldt)
    sortListDateTime(ldt)
    val zdt: java.util.List[ZonedDateTime]       = setupZonedDateTime
    queryListZonedDateTime(zdt)
    formatListZonedDateTime(zdt)
    sortListZonedDateTime(zdt)
    val instants: java.util.List[Instant]        = setupInstant
    queryListInstant(instants)
    formatListInstant(instants)
    sortListInstant(instants)
    val judates: java.util.List[Date]            = setupDate
    queryListDate(judates)
    formatListDate(judates)
    sortListDate(judates)
    val ld: java.util.List[LocalDate]            = setupLocalDate
    queryListLocalDate(ld)
    formatListLocalDate(ld)
    sortListLocalDate(ld)
    val lt: java.util.List[LocalTime]            = setupTime
    queryListTime(lt)
    formatListTime(lt)
    sortListTime(lt)
    val gcals: java.util.List[GregorianCalendar] = setupGCal
    queryListGCal(gcals)
    formatListGCal(gcals)
    sortListGCal(gcals)
    deriveTime(lt)
    deriveDateTime(ldt)
  }

  private def setupDateTime: java.util.List[LocalDateTime] = {
    val random: Random                      = new Random(47658758756875687L)
    val list: java.util.List[LocalDateTime] = new java.util.ArrayList[LocalDateTime](SIZE)
    val start: Long                         = System.nanoTime
    var i: Int                              = 0
    while (i < SIZE) {
      val t: LocalDateTime = LocalDateTime.of(random.nextInt(10000),
                                              random.nextInt(12) + 1,
                                              random.nextInt(28) + 1,
                                              random.nextInt(24),
                                              random.nextInt(60),
                                              random.nextInt(60)
      )
      list.add(t)
      i += 1
    }
    val end: Long                           = System.nanoTime
    System.out.println("LocalDT:   Setup:  " + NF.format(end - start) + " ns")
    result("LocalDT-I", end - start)
    list
  }

  private def sortListDateTime(list: java.util.List[LocalDateTime]): Unit = {
    val start: Long = System.nanoTime
    Collections.sort(list)
    val end: Long   = System.nanoTime
    System.out.println("LocalDT:   Sort:   " + NF.format(end - start) + " ns " + list.get(0))
    result("LocalDT-S", end - start)
  }

  private def queryListDateTime(list: java.util.List[LocalDateTime]): Unit = {
    var total: Long = 0
    val start: Long = System.nanoTime
    for (dt <- list.asScala) {
      total += dt.getYear
      total += dt.getMonth.getValue
      total += dt.getDayOfMonth
      total += dt.getHour
      total += dt.getMinute
      total += dt.getSecond
    }
    val end: Long = System.nanoTime
    System.out.println("LocalDT:   Query:  " + NF.format(end - start) + " ns" + " " + total)
    result("LocalDT-Q", end - start)
  }

  private def formatListDateTime(list: java.util.List[LocalDateTime]): Unit = {
    val buf: StringBuilder        = new StringBuilder
    val format: DateTimeFormatter = DateTimeFormatter.ISO_DATE.withLocale(Locale.ENGLISH)
    val start: Long               = System.nanoTime
    for (dt <- list.asScala) {
      buf.setLength(0)
      buf.append(format.format(dt))
    }
    val end: Long = System.nanoTime
    System.out.println("LocalDT:   Format: " + NF.format(end - start) + " ns" + " " + buf)
    result("LocalDT-P", end - start)
  }

  private def deriveDateTime(list: java.util.List[LocalDateTime]): Unit = {
    var total: Long = 0
    val start: Long = System.nanoTime
    for (dt <- list.asScala) {
      total += dt.get(YEAR)
      total += dt.get(MONTH_OF_YEAR)
      total += dt.get(DAY_OF_MONTH)
      total += dt.get(HOUR_OF_DAY)
      total += dt.get(MINUTE_OF_HOUR)
    }
    val end: Long = System.nanoTime
    System.out.println("LocalDT:   Derive: " + NF.format(end - start) + " ns" + " " + total)
    result("LocalDT-V", end - start)
  }

  private def setupLocalDate: java.util.List[LocalDate] = {
    val random: Random                  = new Random(47658758756875687L)
    val list: java.util.List[LocalDate] = new java.util.ArrayList[LocalDate](SIZE)
    val start: Long                     = System.nanoTime
    var i: Int                          = 0
    while (i < SIZE) {
      val t: LocalDate =
        LocalDate.of(random.nextInt(10000), random.nextInt(12) + 1, random.nextInt(28) + 1)
      list.add(t)
      i += 1
    }
    val end: Long                       = System.nanoTime
    System.out.println("LocalD:    Setup:  " + NF.format(end - start) + " ns")
    result("LocalD-I", end - start)
    list
  }

  private def sortListLocalDate(list: java.util.List[LocalDate]): Unit = {
    val start: Long = System.nanoTime
    Collections.sort(list)
    val end: Long   = System.nanoTime
    System.out.println("LocalD:    Sort:   " + NF.format(end - start) + " ns " + list.get(0))
    result("LocalD-S", end - start)
  }

  private def queryListLocalDate(list: java.util.List[LocalDate]): Unit = {
    var total: Long = 0
    val start: Long = System.nanoTime
    for (dt <- list.asScala) {
      total += dt.getYear
      total += dt.getMonth.getValue
      total += dt.getDayOfMonth
    }
    val end: Long = System.nanoTime
    System.out.println("LocalD:    Query:  " + NF.format(end - start) + " ns" + " " + total)
    result("LocalD-Q", end - start)
  }

  private def formatListLocalDate(list: java.util.List[LocalDate]): Unit = {
    val buf: StringBuilder        = new StringBuilder
    val format: DateTimeFormatter = DateTimeFormatter.ISO_DATE.withLocale(Locale.ENGLISH)
    val start: Long               = System.nanoTime
    for (dt <- list.asScala) {
      buf.setLength(0)
      buf.append(format.format(dt))
    }
    val end: Long = System.nanoTime
    System.out.println("LocalD:    Format: " + NF.format(end - start) + " ns" + " " + buf)
    result("LocalD-P", end - start)
  }

  private def setupTime: java.util.List[LocalTime] = {
    val random: Random                  = new Random(47658758756875687L)
    val list: java.util.List[LocalTime] = new java.util.ArrayList[LocalTime](SIZE)
    val start: Long                     = System.nanoTime
    var i: Int                          = 0
    while (i < SIZE) {
      val t: LocalTime = LocalTime.of(random.nextInt(24),
                                      random.nextInt(60),
                                      random.nextInt(60),
                                      random.nextInt(1000000000)
      )
      list.add(t)
      i += 1
    }
    val end: Long                       = System.nanoTime
    System.out.println("LocalT:    Setup:  " + NF.format(end - start) + " ns")
    result("LocalT-I", end - start)
    list
  }

  private def sortListTime(list: java.util.List[LocalTime]): Unit = {
    val start: Long = System.nanoTime
    Collections.sort(list)
    val end: Long   = System.nanoTime
    System.out.println("LocalT:    Sort:   " + NF.format(end - start) + " ns " + list.get(0))
    result("LocalT-S", end - start)
  }

  private def queryListTime(list: java.util.List[LocalTime]): Unit = {
    var total: Long = 0
    val start: Long = System.nanoTime
    for (dt <- list.asScala) {
      total += dt.getHour
      total += dt.getMinute
      total += dt.getSecond
      total += dt.getNano
    }
    val end: Long = System.nanoTime
    System.out.println("LocalT:    Query:  " + NF.format(end - start) + " ns" + " " + total)
    result("LocalT-Q", end - start)
  }

  private def formatListTime(list: java.util.List[LocalTime]): Unit = {
    val buf: StringBuilder        = new StringBuilder
    val format: DateTimeFormatter = DateTimeFormatter.ISO_TIME.withLocale(Locale.ENGLISH)
    val start: Long               = System.nanoTime
    for (dt <- list.asScala) {
      buf.setLength(0)
      buf.append(format.format(dt))
    }
    val end: Long = System.nanoTime
    System.out.println("LocalT:    Format: " + NF.format(end - start) + " ns" + " " + buf)
    result("LocalT-P", end - start)
  }

  private def deriveTime(list: java.util.List[LocalTime]): Unit = {
    var total: Long = 0
    val start: Long = System.nanoTime
    for (dt <- list.asScala) {
      total += dt.get(HOUR_OF_DAY)
      total += dt.get(MINUTE_OF_HOUR)
      total += dt.get(SECOND_OF_MINUTE)
      total += dt.get(NANO_OF_SECOND)
    }
    val end: Long = System.nanoTime
    System.out.println("LocalT:    Derive: " + NF.format(end - start) + " ns" + " " + total)
    result("LocalT-V", end - start)
  }

  private def setupZonedDateTime: java.util.List[ZonedDateTime] = {
    val tz: ZoneId                          = ZoneId.of("Europe/London")
    val random: Random                      = new Random(47658758756875687L)
    val list: java.util.List[ZonedDateTime] = new java.util.ArrayList[ZonedDateTime](SIZE)
    val start: Long                         = System.nanoTime
    var i: Int                              = 0
    while (i < SIZE) {
      val t: ZonedDateTime = LocalDateTime
        .of(2008,
            random.nextInt(12) + 1,
            random.nextInt(28) + 1,
            random.nextInt(24),
            random.nextInt(60),
            random.nextInt(60),
            0
        )
        .atZone(tz)
      list.add(t)
      i += 1
    }
    val end: Long                           = System.nanoTime
    System.out.println("ZonedDT:   Setup:  " + NF.format(end - start) + " ns")
    result("ZonedDT-I", end - start)
    list
  }

  private def sortListZonedDateTime(list: java.util.List[ZonedDateTime]): Unit = {
    val start: Long = System.nanoTime
    Collections.sort(list)
    val end: Long   = System.nanoTime
    System.out.println("ZonedDT:   Sort:   " + NF.format(end - start) + " ns")
    result("ZonedDT-S", end - start)
  }

  private def queryListZonedDateTime(list: java.util.List[ZonedDateTime]): Unit = {
    var total: Long = 0
    val start: Long = System.nanoTime
    for (dt <- list.asScala) {
      total += dt.getYear
      total += dt.getMonth.getValue
      total += dt.getDayOfMonth
      total += dt.getHour
      total += dt.getMinute
      total += dt.getSecond
    }
    val end: Long = System.nanoTime
    System.out.println("ZonedDT:   Query:  " + NF.format(end - start) + " ns" + " " + total)
    result("ZonedDT-Q", end - start)
  }

  private def formatListZonedDateTime(list: java.util.List[ZonedDateTime]): Unit = {
    val buf: StringBuilder        = new StringBuilder
    val format: DateTimeFormatter = DateTimeFormatter.ISO_DATE.withLocale(Locale.ENGLISH)
    val start: Long               = System.nanoTime
    for (dt <- list.asScala) {
      buf.setLength(0)
      buf.append(format.format(dt))
    }
    val end: Long = System.nanoTime
    System.out.println("ZonedDT:   Format: " + NF.format(end - start) + " ns" + " " + buf)
    result("ZonedDT-P", end - start)
  }

  private def setupInstant: java.util.List[Instant] = {
    val random: Random                = new Random(47658758756875687L)
    val list: java.util.List[Instant] = new java.util.ArrayList[Instant](SIZE)
    val start: Long                   = System.nanoTime
    var i: Int                        = 0
    while (i < SIZE) {
      val t: Instant = Instant.ofEpochMilli(random.nextLong)
      list.add(t)
      i += 1
    }
    val end: Long                     = System.nanoTime
    System.out.println("Instant:   Setup:  " + NF.format(end - start) + " ns")
    result("Instant-I", end - start)
    list
  }

  private def sortListInstant(list: java.util.List[Instant]): Unit = {
    val start: Long = System.nanoTime
    Collections.sort(list)
    val end: Long   = System.nanoTime
    System.out.println("Instant:   Sort:   " + NF.format(end - start) + " ns")
    result("Instant-S", end - start)
  }

  private def queryListInstant(list: java.util.List[Instant]): Unit = {
    var total: Long = 0
    val start: Long = System.nanoTime
    for (dt <- list.asScala) {
      total += dt.getEpochSecond
      total += dt.getNano
    }
    val end: Long = System.nanoTime
    System.out.println("Instant:   Query:  " + NF.format(end - start) + " ns" + " " + total)
    result("Instant-Q", end - start)
  }

  private def formatListInstant(list: java.util.List[Instant]): Unit = {
    val buf: StringBuilder = new StringBuilder
    val start: Long        = System.nanoTime
    for (dt <- list.asScala) {
      buf.setLength(0)
      buf.append(dt.toString)
    }
    val end: Long = System.nanoTime
    System.out.println("Instant:   Format: " + NF.format(end - start) + " ns" + " " + buf)
    result("Instant-P", end - start)
  }

  private def setupDate: java.util.List[Date] = {
    val random: Random             = new Random(47658758756875687L)
    val list: java.util.List[Date] = new java.util.ArrayList[Date](SIZE)
    val start: Long                = System.nanoTime
    var i: Int                     = 0
    while (i < SIZE) {
      val t: Date = new Date(random.nextLong)
      list.add(t)
      i += 1
    }
    val end: Long                  = System.nanoTime
    System.out.println("Date:      Setup:  " + NF.format(end - start) + " ns")
    result("JUDate-I", end - start)
    list
  }

  private def sortListDate(list: java.util.List[Date]): Unit = {
    val start: Long = System.nanoTime
    Collections.sort(list)
    val end: Long   = System.nanoTime
    System.out.println("Date:      Sort:   " + NF.format(end - start) + " ns " + list.get(0))
    result("JUDate-S", end - start)
  }

  private def queryListDate(list: java.util.List[Date]): Unit = {
    var total: Long = 0
    val start: Long = System.nanoTime
    for (dt <- list.asScala)
      total += dt.getTime
    val end: Long = System.nanoTime
    System.out.println("Date:      Query:  " + NF.format(end - start) + " ns" + " " + total)
    result("JUDate-Q", end - start)
  }

  private def formatListDate(list: java.util.List[Date]): Unit = {
    val buf: StringBuilder = new StringBuilder
    val start: Long        = System.nanoTime
    for (dt <- list.asScala) {
      buf.setLength(0)
      buf.append(dt.toString)
    }
    val end: Long = System.nanoTime
    System.out.println("Date:      Format: " + NF.format(end - start) + " ns" + " " + buf)
    result("JUDate-P", end - start)
  }

  private def setupGCal: java.util.List[GregorianCalendar] = {
    val tz: java.util.TimeZone                  = java.util.TimeZone.getTimeZone("Europe/London")
    val random: Random                          = new Random(47658758756875687L)
    val list: java.util.List[GregorianCalendar] = new java.util.ArrayList[GregorianCalendar](SIZE)
    val start: Long                             = System.nanoTime
    var i: Int                                  = 0
    while (i < SIZE) {
      val t: GregorianCalendar = new GregorianCalendar(tz)
      t.setGregorianChange(new Date(Long.MinValue))
      t.set(random.nextInt(10000),
            random.nextInt(12),
            random.nextInt(28) + 1,
            random.nextInt(24),
            random.nextInt(60),
            random.nextInt(60)
      )
      list.add(t)
      i += 1
    }
    val end: Long                               = System.nanoTime
    System.out.println("GCalendar: Setup:  " + NF.format(end - start) + " ns")
    result("GregCal-I", end - start)
    list
  }

  private def sortListGCal(list: java.util.List[GregorianCalendar]): Unit = {
    val start: Long = System.nanoTime
    Collections.sort(list)
    val end: Long   = System.nanoTime
    System.out.println("GCalendar: Sort:   " + NF.format(end - start) + " ns")
    result("GregCal-S", end - start)
  }

  private def queryListGCal(list: java.util.List[GregorianCalendar]): Unit = {
    var total: Long = 0
    val start: Long = System.nanoTime
    for (gcal <- list.asScala) {
      total += gcal.get(Calendar.YEAR)
      total += gcal.get(Calendar.MONTH + 1)
      total += gcal.get(Calendar.DAY_OF_MONTH)
      total += gcal.get(Calendar.HOUR_OF_DAY)
      total += gcal.get(Calendar.MINUTE)
      total += gcal.get(Calendar.SECOND)
      total += gcal.get(Calendar.SECOND)
    }
    val end: Long = System.nanoTime
    System.out.println("GCalendar: Query:  " + NF.format(end - start) + " ns" + " " + total)
    result("GregCal-Q", end - start)
  }

  private def formatListGCal(list: java.util.List[GregorianCalendar]): Unit = {
    val buf: StringBuilder       = new StringBuilder
    val format: SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd", Locale.ENGLISH)
    val start: Long              = System.nanoTime
    for (gcal <- list.asScala) {
      buf.setLength(0)
      buf.append(format.format(gcal.getTime))
    }
    val end: Long = System.nanoTime
    System.out.println("GCalendar: Format: " + NF.format(end - start) + " ns" + " " + buf)
    result("GregCal-P", end - start)
  }

  private def result(name: String, result: Long): Unit = {
    var values: Array[Long] = RESULTS.get(name)
    if (values == null) {
      values = new Array[Long](7)
      RESULTS.put(name, values)
    }
    values(loop) = result
    if (loop == 4) {
      values(5) = Math.min(values(0),
                           Math.min(values(1), Math.min(values(2), Math.min(values(3), values(4))))
      )
      values(6) = ((values(0) - values(5)) * 1000) / values(0)
    }
  }
}
