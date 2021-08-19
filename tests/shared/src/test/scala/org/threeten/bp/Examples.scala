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

import org.threeten.bp.Month.DECEMBER
import org.threeten.bp.temporal.ChronoField.DAY_OF_MONTH
import org.threeten.bp.temporal.ChronoField.DAY_OF_YEAR
import org.threeten.bp.temporal.ChronoField.MONTH_OF_YEAR
import org.threeten.bp.temporal.ChronoField.YEAR
import org.threeten.bp.temporal.TemporalAdjusters
import java.util.Locale
import org.threeten.bp.format.DateTimeFormatter
import org.threeten.bp.format.DateTimeFormatterBuilder
import org.threeten.bp.format.SignStyle

/**
 * Examples for this project.
 */
object Examples {

  /**
   * Main method.
   * @param args
   *   no arguments needed
   */
  def main(args: Array[String]): Unit = {
    val clock: Clock              = Clock.systemDefaultZone
    val zdt: ZonedDateTime        = ZonedDateTime.now(clock)
    println("Current date-time: " + zdt)
    val zdtNewYork: ZonedDateTime = ZonedDateTime.now(Clock.system(ZoneId.of("America/New_York")))
    println("Current date-time in New York: " + zdtNewYork)
    val zdtParis: ZonedDateTime   = ZonedDateTime.now(Clock.system(ZoneId.of("Europe/Paris")))
    println("Current date-time in Paris: " + zdtParis)
    val ldt: LocalDateTime        = LocalDateTime.now(clock)
    println("Current local date-time: " + ldt)
    val year: Year                = Year.now(clock)
    println("Year: " + year.getValue)
    val today: LocalDate          = LocalDate.now(clock)
    println("Today: " + today)
    println("Current day-of-year: " + today.get(DAY_OF_YEAR))
    val time: LocalTime           = LocalTime.now(clock)
    println("Current time of day: " + time)
    val later: LocalDate          = LocalDate.now(clock).plusMonths(2).plusDays(3)
    println("Two months three days after today: " + later)
    val dec: LocalDate            = LocalDate.now(clock).`with`(DECEMBER)
    println("Change to same day in December: " + dec)
    val lastDayOfMonth: LocalDate = LocalDate.now(clock).`with`(TemporalAdjusters.lastDayOfMonth)
    println("Last day of month: " + lastDayOfMonth)
    val dt: LocalDateTime         = LocalDateTime.of(2008, 3, 30, 1, 30)
    println("Local date-time in Spring DST gap: " + dt)
    val resolved: ZonedDateTime   = ZonedDateTime.of(dt, ZoneId.of("Europe/London"))
    println("...resolved to valid date-time in Europe/London: " + resolved)
    val formattedRFC: String      = DateTimeFormatter.RFC_1123_DATE_TIME.format(resolved)
    println("...printed as RFC1123: " + formattedRFC)
    val f: DateTimeFormatter      = new DateTimeFormatterBuilder()
      .appendValue(YEAR, 4, 10, SignStyle.ALWAYS)
      .appendLiteral(' ')
      .appendText(MONTH_OF_YEAR)
      .appendLiteral('(')
      .appendValue(MONTH_OF_YEAR)
      .appendLiteral(')')
      .appendLiteral(' ')
      .appendValue(DAY_OF_MONTH, 2)
      .toFormatter(Locale.ENGLISH)
    val formatted: String         = f.format(resolved)
    println("...printed using complex format: " + formatted)
    val bday: MonthDay            = MonthDay.of(DECEMBER, 3)
    println("Brazillian birthday (no year): " + bday)
  }
}
