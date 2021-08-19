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
package org.threeten.bp.format.internal

import java.util.{ Locale, Objects }

import org.threeten.bp.DateTimeException
import org.threeten.bp.Instant
import org.threeten.bp.ZoneId
import org.threeten.bp.ZoneOffset
import org.threeten.bp.chrono.ChronoLocalDate
import org.threeten.bp.chrono.Chronology
import org.threeten.bp.chrono.IsoChronology
import org.threeten.bp.temporal.ChronoField
import org.threeten.bp.temporal.TemporalAccessor
import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.temporal.TemporalQueries
import org.threeten.bp.temporal.TemporalQuery
import org.threeten.bp.temporal.ValueRange
import org.threeten.bp.format.DateTimeFormatter
import org.threeten.bp.format.DecimalStyle

object TTBPDateTimePrintContext {
  def adjust(temporal: TemporalAccessor, formatter: DateTimeFormatter): TemporalAccessor = {
    var overrideChrono: Chronology = formatter.getChronology
    var overrideZone: ZoneId       = formatter.getZone
    if (overrideChrono == null && overrideZone == null)
      temporal
    else {
      val temporalChrono: Chronology     = temporal.query(TemporalQueries.chronology)
      val temporalZone: ZoneId           = temporal.query(TemporalQueries.zoneId)
      if (Objects.equals(temporalChrono, overrideChrono))
        overrideChrono = null
      if (Objects.equals(temporalZone, overrideZone))
        overrideZone = null
      if (overrideChrono == null && overrideZone == null)
        return temporal
      val effectiveChrono: Chronology    =
        if (overrideChrono != null) overrideChrono else temporalChrono
      val effectiveZone: ZoneId          = if (overrideZone != null) overrideZone else temporalZone
      if (overrideZone != null) {
        if (temporal.isSupported(ChronoField.INSTANT_SECONDS)) {
          val chrono: Chronology =
            if (effectiveChrono != null) effectiveChrono else IsoChronology.INSTANCE
          return chrono.zonedDateTime(Instant.from(temporal), overrideZone)
        }
        val normalizedOffset: ZoneId   = overrideZone.normalized
        val temporalOffset: ZoneOffset = temporal.query(TemporalQueries.offset)
        if (
          normalizedOffset
            .isInstanceOf[
              ZoneOffset
            ] && temporalOffset != null && !(normalizedOffset == temporalOffset)
        )
          throw new DateTimeException(
            s"Invalid override zone for temporal: $overrideZone $temporal"
          )
      }
      val effectiveDate: ChronoLocalDate =
        if (overrideChrono != null)
          if (temporal.isSupported(ChronoField.EPOCH_DAY))
            effectiveChrono.date(temporal)
          else {
            if (!((overrideChrono eq IsoChronology.INSTANCE) && temporalChrono == null))
              for (f <- ChronoField.values)
                if (f.isDateBased && temporal.isSupported(f))
                  throw new DateTimeException(
                    s"Invalid override chronology for temporal: $overrideChrono $temporal"
                  )
            null
          }
        else
          null
      new TemporalAccessor() {
        def isSupported(field: TemporalField): Boolean =
          if (effectiveDate != null && field.isDateBased)
            effectiveDate.isSupported(field)
          else
            temporal.isSupported(field)

        override def range(field: TemporalField): ValueRange =
          if (effectiveDate != null && field.isDateBased)
            effectiveDate.range(field)
          else
            temporal.range(field)

        def getLong(field: TemporalField): Long =
          if (effectiveDate != null && field.isDateBased)
            effectiveDate.getLong(field)
          else
            temporal.getLong(field)

        override def query[R](query: TemporalQuery[R]): R =
          if (query eq TemporalQueries.chronology)
            effectiveChrono.asInstanceOf[R]
          else if (query eq TemporalQueries.zoneId)
            effectiveZone.asInstanceOf[R]
          else if (query eq TemporalQueries.precision)
            temporal.query(query)
          else
            query.queryFrom(this)
      }
    }
  }
}

/**
 * Context object used during date and time printing.
 *
 * This class provides a single wrapper to items used in the print.
 *
 * <h3>Specification for implementors</h3> This class is a mutable context intended for use from a
 * single thread. Usage of the class is thread-safe within standard printing as the framework
 * creates a new instance of the class for each print and printing is single-threaded.
 */
final class TTBPDateTimePrintContext(
  private var temporal: TemporalAccessor,
  private var locale:   Locale,
  private var symbols:  DecimalStyle
) {

  /** Whether the current formatter is optional. */
  private var optional: Int = 0

  /**
   * Creates a new instance of the context.
   *
   * @param temporal
   *   the temporal object being output, not null
   * @param formatter
   *   the formatter controlling the print, not null
   */
  def this(temporal: TemporalAccessor, formatter: DateTimeFormatter) = {
    this(TTBPDateTimePrintContext.adjust(temporal, formatter),
         formatter.getLocale,
         formatter.getDecimalStyle
    )
  }

  /**
   * Gets the temporal object being output.
   *
   * @return
   *   the temporal object, not null
   */
  def getTemporal: TemporalAccessor = temporal

  /**
   * Gets the locale.
   *
   * This locale is used to control localization in the print output except where localization is
   * controlled by the symbols.
   *
   * @return
   *   the locale, not null
   */
  def getLocale: Locale = locale

  /**
   * Gets the formatting symbols.
   *
   * The symbols control the localization of numeric output.
   *
   * @return
   *   the formatting symbols, not null
   */
  def getSymbols: DecimalStyle = symbols

  /** Starts the printing of an optional segment of the input. */
  def startOptional(): Unit = this.optional += 1

  /** Ends the printing of an optional segment of the input. */
  def endOptional(): Unit = this.optional -= 1

  /**
   * Gets a value using a query.
   *
   * @param query
   *   the query to use, not null
   * @return
   *   the result, null if not found and optional is true
   * @throws DateTimeException
   *   if the type is not available and the section is not optional
   */
  def getValue[R >: Null](query: TemporalQuery[R]): R = {
    val result: R = temporal.query(query)
    if (result == null && optional == 0)
      throw new DateTimeException(s"Unable to extract value: ${temporal.getClass}")
    else
      result
  }

  /**
   * Gets the value of the specified field.
   *
   * This will return the value for the specified field.
   *
   * @param field
   *   the field to find, not null
   * @return
   *   the value, null if not found and optional is true
   * @throws DateTimeException
   *   if the field is not available and the section is not optional
   */
  def getValue(field: TemporalField): java.lang.Long =
    try temporal.getLong(field)
    catch {
      case ex: DateTimeException => if (optional > 0) null else throw ex
    }

  /**
   * Returns a string version of the context for debugging.
   *
   * @return
   *   a string representation of the context, not null
   */
  override def toString: String = temporal.toString

  /**
   * Sets the date-time being output.
   *
   * @param temporal
   *   the date-time object, not null
   */
  def setDateTime(temporal: TemporalAccessor): Unit = {
    Objects.requireNonNull(temporal, "temporal")
    this.temporal = temporal
  }

  /**
   * Sets the locale.
   *
   * This locale is used to control localization in the print output except where localization is
   * controlled by the symbols.
   *
   * @param locale
   *   the locale, not null
   */
  def setLocale(locale: Locale): Unit = {
    Objects.requireNonNull(locale, "locale")
    this.locale = locale
  }
}
