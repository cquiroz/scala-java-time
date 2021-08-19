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

import java.util.Locale
import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.format.TextStyle

private[format] object TTBPDateTimeTextProvider {
  lazy val Provider: TTBPDateTimeTextProvider = new TTBPSimpleDateTimeTextProvider()
}

/**
 * The Service Provider Interface (SPI) to be implemented by classes providing the textual form of a
 * date-time field.
 *
 * <h3>Specification for implementors</h3> This interface is a service provider that can be called
 * by multiple threads. Implementations must be thread-safe. Implementations should cache the
 * textual information. <p> This class has been made pubilc primarily for the benefit of Android.
 */
abstract class TTBPDateTimeTextProvider {

  /**
   * Gets the text for the specified field, locale and style for the purpose of printing.
   *
   * The text associated with the value is returned. The null return value should be used if there
   * is no applicable text, or if the text would be a numeric representation of the value.
   *
   * @param field
   *   the field to get text for, not null
   * @param value
   *   the field value to get text for, not null
   * @param style
   *   the style to get text for, not null
   * @param locale
   *   the locale to get text for, not null
   * @return
   *   the text for the field value, null if no text found
   */
  def getText(field: TemporalField, value: Long, style: TextStyle, locale: Locale): String

  /**
   * Gets an iterator of text to field for the specified field, locale and style for the purpose of
   * parsing.
   *
   * The iterator must be returned in order from the longest text to the shortest.
   *
   * The null return value should be used if there is no applicable parsable text, or if the text
   * would be a numeric representation of the value. Text can only be parsed if all the values for
   * that field-style-locale combination are unique.
   *
   * @param field
   *   the field to get text for, not null
   * @param style
   *   the style to get text for, null for all parsable text
   * @param locale
   *   the locale to get text for, not null
   * @return
   *   the iterator of text to field pairs, in order from longest text to shortest text, null if the
   *   field or style is not parsable
   */
  def getTextIterator(
    field:  TemporalField,
    style:  TextStyle,
    locale: Locale
  ): Iterator[(String, Long)]
}
