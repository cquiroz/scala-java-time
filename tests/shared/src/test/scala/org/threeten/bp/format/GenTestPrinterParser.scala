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
package org.threeten.bp.format

import java.util.Locale
import java.lang.StringBuilder

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.BeforeAndAfterEach
import org.threeten.bp.LocalDateTime
import org.threeten.bp.ZoneId
import org.threeten.bp.ZonedDateTime
import org.threeten.bp.DateTimeException
import org.threeten.bp.chrono.IsoChronology
import org.threeten.bp.temporal.{ TemporalAccessor, TemporalField, TemporalQuery, ValueRange }
import org.threeten.bp.format.internal.TTBPDateTimeParseContext
import org.threeten.bp.format.internal.TTBPDateTimePrintContext

/** Abstract PrinterParser test. */
object GenTestPrinterParser {
  private val EMPTY: TemporalAccessor = new TemporalAccessor() {
    def isSupported(field:       TemporalField): Boolean    = true
    def getLong(field:           TemporalField): Long       = throw new DateTimeException("Mock")
    override def get(field: TemporalField): Int             =
      range(field).checkValidIntValue(getLong(field), field)
    override def query[R](query: TemporalQuery[R]): R       = query.queryFrom(this)
    override def range(field:    TemporalField): ValueRange = field.range
  }
}

trait GenTestPrinterParser extends BeforeAndAfterEach { this: AnyFunSuite =>
  protected var printEmptyContext: TTBPDateTimePrintContext = null
  protected var printContext: TTBPDateTimePrintContext      = null
  protected var parseContext: TTBPDateTimeParseContext      = null
  protected var buf: StringBuilder                          = null

  override def beforeEach() = {
    printEmptyContext = new TTBPDateTimePrintContext(GenTestPrinterParser.EMPTY,
                                                     Locale.ENGLISH,
                                                     DecimalStyle.STANDARD
    )
    val zdt: ZonedDateTime =
      LocalDateTime.of(2011, 6, 30, 12, 30, 40, 0).atZone(ZoneId.of("Europe/Paris"))
    printContext = new TTBPDateTimePrintContext(zdt, Locale.ENGLISH, DecimalStyle.STANDARD)
    parseContext =
      new TTBPDateTimeParseContext(Locale.ENGLISH, DecimalStyle.STANDARD, IsoChronology.INSTANCE)
    buf = new StringBuilder
  }
}
