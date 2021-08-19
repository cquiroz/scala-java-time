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
import org.threeten.bp.temporal.ChronoField.DAY_OF_WEEK
import org.threeten.bp.temporal.ChronoField.EPOCH_DAY
import java.io.PrintStream
import org.threeten.bp.chrono.Chronology
import org.threeten.bp.chrono.ChronoLocalDate
import org.threeten.bp.chrono.HijrahChronology
import org.threeten.bp.chrono.IsoChronology
import org.threeten.bp.chrono.JapaneseChronology
import org.threeten.bp.chrono.MinguoChronology
import org.threeten.bp.chrono.ThaiBuddhistChronology
import org.threeten.bp.temporal.ChronoField
import org.threeten.bp.temporal.ChronoUnit
import org.threeten.bp.temporal.JulianFields

/** Usability class for package. */
object UsabilityChrono {
  def main(args: Array[String]): Unit = {
    System.out.println("------")
    newPackagePluggable()
    System.out.println("------")
    epochDays()
    System.out.println("------")
    printMinguoCal()
    System.out.println("------")
    example1()
  }

  private def newPackagePluggable(): Unit = {
    val chrono: Chronology    = MinguoChronology.INSTANCE
    var date: ChronoLocalDate = chrono.dateNow
    System.out.printf("now: %s%n", date)
    date = date.`with`(DAY_OF_MONTH, 1)
    System.out.printf("first of month: %s%n", date)
    val month: Int            = date.get(ChronoField.MONTH_OF_YEAR)
    date = date.`with`(DAY_OF_WEEK, 1)
    System.out.printf("start of first week: %s%n", date)
    while (date.get(ChronoField.MONTH_OF_YEAR) <= month) {
      var row: String = ""
      var i: Int      = 0
      while (i < 7) {
        row += date.get(ChronoField.DAY_OF_MONTH) + " "
        date = date.plus(1, ChronoUnit.DAYS)
        i += 1
      }
      System.out.println(row)
    }
  }

  private def epochDays(): Unit = {
    output(LocalDate.now)
    output(LocalDate.of(1945, 11, 12))
    output(LocalDate.of(-4713, 11, 24))
    output(LocalDate.of(1858, 11, 17))
    output(LocalDate.of(1970, 1, 1))
    output(LocalDate.of(1, 1, 1))
  }

  protected def output(date: LocalDate): Unit = {
    System.out.println(date)
    System.out.println("EPOCH_DAY " + date.getLong(EPOCH_DAY))
    System.out.println("JDN " + date.getLong(JulianFields.JULIAN_DAY))
    System.out.println("MJD " + date.getLong(JulianFields.MODIFIED_JULIAN_DAY))
    System.out.println("RD  " + date.getLong(JulianFields.RATA_DIE))
    System.out.println()
  }

  /**
   * Example code.
   */
  private[bp] def example1(): Unit = {
    System.out.printf("Available Calendars%n")
    val now1: ChronoLocalDate              = MinguoChronology.INSTANCE.dateNow
    val day: Int                           = now1.get(ChronoField.DAY_OF_MONTH)
    val dow: Int                           = now1.get(ChronoField.DAY_OF_WEEK)
    val month: Int                         = now1.get(ChronoField.MONTH_OF_YEAR)
    val year: Int                          = now1.get(ChronoField.YEAR)
    System.out.printf(
      "  Today is %s %s %d-%s-%d%n",
      now1.getChronology.getId,
      DayOfWeek.of(dow),
      year.asInstanceOf[AnyRef],
      month.asInstanceOf[AnyRef],
      day.asInstanceOf[AnyRef]
    )
    val first: ChronoLocalDate             =
      now1.`with`(ChronoField.DAY_OF_MONTH, 1).`with`(ChronoField.MONTH_OF_YEAR, 1)
    val last: ChronoLocalDate              = first.plus(1, ChronoUnit.YEARS).minus(1, ChronoUnit.DAYS)
    System.out.printf("  1st of year: %s; end of year: %s%n", first, last)
    val before: LocalDate                  = LocalDate.of(-500, 1, 1)
    val chronos: java.util.Set[Chronology] = Chronology.getAvailableChronologies
    import scala.collection.JavaConverters._
    for (chrono <- chronos.asScala) {
      val date: ChronoLocalDate  = chrono.dateNow
      val date2: ChronoLocalDate = chrono.date(before)
      System.out.printf("   %20s: %22s, %22s%n", chrono.getId, date, date2)
    }
  }

  /**
   * Prints a Minguo calendar for the current month.
   */
  private def printMinguoCal(): Unit = {
    val chronoName: String     = "Minguo"
    val chrono: Chronology     = Chronology.of(chronoName)
    val today: ChronoLocalDate = chrono.dateNow
    printMonthCal(today, System.out)
  }

  /**
   * Print a month calendar with complete week rows.
   * @param date
   *   A date in some calendar
   * @param out
   *   a PrintStream
   */
  private def printMonthCal(date: ChronoLocalDate, out: PrintStream): Unit = {
    val lengthOfMonth: Int     = date.lengthOfMonth.toInt
    var end: ChronoLocalDate   = date.`with`(ChronoField.DAY_OF_MONTH, lengthOfMonth)
    end = end.plus(7 - end.get(ChronoField.DAY_OF_WEEK), ChronoUnit.DAYS)
    var start: ChronoLocalDate = date.`with`(ChronoField.DAY_OF_MONTH, 1)
    start = start.minus(start.get(ChronoField.DAY_OF_WEEK), ChronoUnit.DAYS)
    out.printf(
      "%9s Month %2d, %4d%n",
      date.getChronology.getId,
      date.get(ChronoField.MONTH_OF_YEAR).asInstanceOf[AnyRef],
      date.get(ChronoField.YEAR).asInstanceOf[AnyRef]
    )
    val colText: Array[String] = Array("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    printMonthRow(colText, " ", out)
    val cell: Array[String]    = new Array[String](7)
    while (start.compareTo(end) <= 0) {
      val ndx: Int = start.get(ChronoField.DAY_OF_WEEK) - 1
      cell(ndx) = Integer.toString(start.get(ChronoField.DAY_OF_MONTH))
      if (ndx == 6)
        printMonthRow(cell, "|", out)
      start = start.plus(1, ChronoUnit.DAYS)
    }
  }

  private def printMonthRow(cells: Array[String], delim: String, out: PrintStream): Unit = {
    var i: Int = 0
    while (i < cells.length) {
      out.printf("%s%3s ", delim, cells(i))
      i += 1
    }
    out.println(delim)
  }

  {
    var c: Chronology = JapaneseChronology.INSTANCE
    c = MinguoChronology.INSTANCE
    c = ThaiBuddhistChronology.INSTANCE
    c = JapaneseChronology.INSTANCE
    c = MinguoChronology.INSTANCE
    c = HijrahChronology.INSTANCE
    c = IsoChronology.INSTANCE
    c.toString
  }
}
