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

import org.threeten.bp.temporal.ChronoField.AMPM_OF_DAY
import org.threeten.bp.temporal.ChronoField.DAY_OF_WEEK
import org.threeten.bp.temporal.ChronoField.ERA
import org.threeten.bp.temporal.ChronoField.MONTH_OF_YEAR
import java.text.DateFormatSymbols
import java.util.Calendar
import java.util.GregorianCalendar
import java.util.Locale
import java.util.HashMap
import java.util.{ Map => JMap }

import org.threeten.bp.temporal.IsoFields
import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.format.TextStyle

object TTBPSimpleDateTimeTextProvider {

  private def createLocaleStore(
    valueTextMap: Map[TextStyle, Map[Long, String]]
  ): TTBPSimpleDateTimeTextProvider.LocaleStore = {
    val tmp1 =
      (valueTextMap + (TextStyle.FULL_STANDALONE -> valueTextMap
        .get(TextStyle.FULL)
        .orNull)) + (TextStyle.SHORT_STANDALONE  -> valueTextMap.get(TextStyle.SHORT).orNull)
    val tmp2 =
      if (
        valueTextMap.contains(TextStyle.NARROW) && !valueTextMap
          .contains(TextStyle.NARROW_STANDALONE)
      )
        tmp1 + (TextStyle.NARROW_STANDALONE -> valueTextMap.get(TextStyle.NARROW).orNull)
      else
        tmp1
    new TTBPSimpleDateTimeTextProvider.LocaleStore(tmp2)
  }

  /**
   * Stores the text for a single locale.
   *
   * Some fields have a textual representation, such as day-of-week or month-of-year. These textual
   * representations can be captured in this class for printing and parsing.
   *
   * This class is immutable and thread-safe.
   *
   * @constructor
   *
   * @param valueTextMap
   *   the map of values to text to store, assigned and not altered, not null
   */
  private[format] final case class LocaleStore private[format] (
    private val valueTextMap: Map[TextStyle, Map[Long, String]]
  ) {

    /** Parsable data. */
    private val parsable: (List[(String, Long)], Map[TextStyle, List[(String, Long)]]) = {
      val u = valueTextMap.foldLeft(
        (List.empty[(String, Long)], Map.empty[TextStyle, List[(String, Long)]])
      ) { case ((all, map), (style, entries)) =>
        val reverse =
          entries.toList.sortBy(_._1).foldLeft((true, Map.empty[String, (String, Long)])) {
            case (a @ (false, _), _)   => a
            case ((true, acc), (k, v)) =>
              val continue = !acc.contains(v)
              (continue, acc + (v -> (v -> k)))
          }
        val list    = reverse._2.values.toList.sortBy(x => -x._1.length)
        (all ::: list, map + (style -> list))
      }
      (u._1.sortBy(x => -x._1.length), u._2)
    }

    /**
     * Gets the text for the specified field value, locale and style for the purpose of printing.
     *
     * @param value
     *   the value to get text for, not null
     * @param style
     *   the style to get text for, not null
     * @return
     *   the text for the field value, null if no text found
     */
    private[format] def getText(value: Long, style: TextStyle): String =
      valueTextMap.get(style).flatMap(_.get(value)).orNull

    /**
     * Gets an iterator of text to field for the specified style for the purpose of parsing.
     *
     * The iterator must be returned in order from the longest text to the shortest.
     *
     * @param style
     *   the style to get text for, null for all parsable text
     * @return
     *   the iterator of text to field pairs, in order from longest text to shortest text, null if
     *   the style is not parsable
     */
    private[format] def getTextIterator(style: TextStyle): Iterator[(String, Long)] =
      parsable._2.getOrElse(style, parsable._1).iterator
  }
}

/**
 * The Service Provider Implementation to obtain date-time text for a field.
 *
 * This implementation is based on extraction of data from a {@link DateFormatSymbols} .
 *
 * <h3>Specification for implementors</h3> This class is immutable and thread-safe.
 */
final class TTBPSimpleDateTimeTextProvider extends TTBPDateTimeTextProvider {

  /** Cache. */
  private val cache: JMap[(TemporalField, Locale), AnyRef] =
    new HashMap[(TemporalField, Locale), AnyRef]()

  override def getText(
    field:  TemporalField,
    value:  Long,
    style:  TextStyle,
    locale: Locale
  ): String =
    findStore(field, locale) match {
      case store: TTBPSimpleDateTimeTextProvider.LocaleStore => store.getText(value, style)
      case _                                                 => null
    }

  override def getTextIterator(
    field:  TemporalField,
    style:  TextStyle,
    locale: Locale
  ): Iterator[(String, Long)] =
    findStore(field, locale) match {
      case store: TTBPSimpleDateTimeTextProvider.LocaleStore => store.getTextIterator(style)
      case _                                                 => null
    }

  private def findStore(field: TemporalField, locale: Locale): AnyRef = {
    val key           = (field, locale)
    var store: AnyRef = cache.get(key)
    if (store == null) {
      store = createStore(field, locale)
      cache.put(key, store)
      store = cache.get(key)
    }
    store
  }

  private def createStore(field: TemporalField, locale: Locale): AnyRef = {
    if (field eq MONTH_OF_YEAR) {
      val oldSymbols: DateFormatSymbols               = DateFormatSymbols.getInstance(locale)
      val f1: Long                                    = 1L
      val f2: Long                                    = 2L
      val f3: Long                                    = 3L
      val f4: Long                                    = 4L
      val f5: Long                                    = 5L
      val f6: Long                                    = 6L
      val f7: Long                                    = 7L
      val f8: Long                                    = 8L
      val f9: Long                                    = 9L
      val f10: Long                                   = 10L
      val f11: Long                                   = 11L
      val f12: Long                                   = 12L
      var array: Array[String]                        = oldSymbols.getMonths
      val mapF: Map[Long, String]                     = Map[Long, String](
        f1  -> array(Calendar.JANUARY),
        f2  -> array(Calendar.FEBRUARY),
        f3  -> array(Calendar.MARCH),
        f4  -> array(Calendar.APRIL),
        f5  -> array(Calendar.MAY),
        f6  -> array(Calendar.JUNE),
        f7  -> array(Calendar.JULY),
        f8  -> array(Calendar.AUGUST),
        f9  -> array(Calendar.SEPTEMBER),
        f10 -> array(Calendar.OCTOBER),
        f11 -> array(Calendar.NOVEMBER),
        f12 -> array(Calendar.DECEMBER)
      )
      val mapN                                        = Map[Long, String](
        f1  -> array(Calendar.JANUARY).substring(0, 1),
        f2  -> array(Calendar.FEBRUARY).substring(0, 1),
        f3  -> array(Calendar.MARCH).substring(0, 1),
        f4  -> array(Calendar.APRIL).substring(0, 1),
        f5  -> array(Calendar.MAY).substring(0, 1),
        f6  -> array(Calendar.JUNE).substring(0, 1),
        f7  -> array(Calendar.JULY).substring(0, 1),
        f8  -> array(Calendar.AUGUST).substring(0, 1),
        f9  -> array(Calendar.SEPTEMBER).substring(0, 1),
        f10 -> array(Calendar.OCTOBER).substring(0, 1),
        f11 -> array(Calendar.NOVEMBER).substring(0, 1),
        f12 -> array(Calendar.DECEMBER).substring(0, 1)
      )
      array = oldSymbols.getShortMonths
      val mapS                                        = Map[Long, String](
        f1  -> array(Calendar.JANUARY),
        f2  -> array(Calendar.FEBRUARY),
        f3  -> array(Calendar.MARCH),
        f4  -> array(Calendar.APRIL),
        f5  -> array(Calendar.MAY),
        f6  -> array(Calendar.JUNE),
        f7  -> array(Calendar.JULY),
        f8  -> array(Calendar.AUGUST),
        f9  -> array(Calendar.SEPTEMBER),
        f10 -> array(Calendar.OCTOBER),
        f11 -> array(Calendar.NOVEMBER),
        f12 -> array(Calendar.DECEMBER)
      )
      val styleMap: Map[TextStyle, Map[Long, String]] = Map[TextStyle, Map[Long, String]](
        TextStyle.FULL   -> mapF,
        TextStyle.NARROW -> mapN,
        TextStyle.SHORT  -> mapS
      )
      return TTBPSimpleDateTimeTextProvider.createLocaleStore(styleMap)
    }

    if (field eq DAY_OF_WEEK) {
      val oldSymbols: DateFormatSymbols               = DateFormatSymbols.getInstance(locale)
      val f1: Long                                    = 1L
      val f2: Long                                    = 2L
      val f3: Long                                    = 3L
      val f4: Long                                    = 4L
      val f5: Long                                    = 5L
      val f6: Long                                    = 6L
      val f7: Long                                    = 7L
      var array: Array[String]                        = oldSymbols.getWeekdays
      val mapF: Map[Long, String]                     = Map[Long, String](
        f1 -> array(Calendar.MONDAY),
        f2 -> array(Calendar.TUESDAY),
        f3 -> array(Calendar.WEDNESDAY),
        f4 -> array(Calendar.THURSDAY),
        f5 -> array(Calendar.FRIDAY),
        f6 -> array(Calendar.SATURDAY),
        f7 -> array(Calendar.SUNDAY)
      )
      val mapN                                        = Map[Long, String](
        f1 -> array(Calendar.MONDAY).substring(0, 1),
        f2 -> array(Calendar.TUESDAY).substring(0, 1),
        f3 -> array(Calendar.WEDNESDAY).substring(0, 1),
        f4 -> array(Calendar.THURSDAY).substring(0, 1),
        f5 -> array(Calendar.FRIDAY).substring(0, 1),
        f6 -> array(Calendar.SATURDAY).substring(0, 1),
        f7 -> array(Calendar.SUNDAY).substring(0, 1)
      )
      array = oldSymbols.getShortWeekdays
      val mapS                                        = Map[Long, String](
        f1 -> array(Calendar.MONDAY),
        f2 -> array(Calendar.TUESDAY),
        f3 -> array(Calendar.WEDNESDAY),
        f4 -> array(Calendar.THURSDAY),
        f5 -> array(Calendar.FRIDAY),
        f6 -> array(Calendar.SATURDAY),
        f7 -> array(Calendar.SUNDAY)
      )
      val styleMap: Map[TextStyle, Map[Long, String]] = Map[TextStyle, Map[Long, String]](
        TextStyle.FULL   -> mapF,
        TextStyle.NARROW -> mapN,
        TextStyle.SHORT  -> mapS
      )
      return TTBPSimpleDateTimeTextProvider.createLocaleStore(styleMap)
    }

    if (field eq AMPM_OF_DAY) {
      val oldSymbols: DateFormatSymbols               = DateFormatSymbols.getInstance(locale)
      val array: Array[String]                        = oldSymbols.getAmPmStrings
      val map: Map[Long, String]                      =
        Map[Long, String](0L -> array(Calendar.AM), 1L -> array(Calendar.PM))
      val styleMap: Map[TextStyle, Map[Long, String]] =
        Map[TextStyle, Map[Long, String]](TextStyle.FULL -> map, TextStyle.SHORT -> map)
      return TTBPSimpleDateTimeTextProvider.createLocaleStore(styleMap)
    }
    if (field eq ERA) {
      val oldSymbols: DateFormatSymbols               = DateFormatSymbols.getInstance(locale)
      val array: Array[String]                        = oldSymbols.getEras
      val mapS: Map[Long, String]                     =
        Map[Long, String](0L -> array(GregorianCalendar.BC), 1L -> array(GregorianCalendar.AD))
      val mapF                                        =
        if (locale.getLanguage == Locale.ENGLISH.getLanguage)
          Map[Long, String](0L -> "Before Christ", 1L -> "Anno Domini")
        else
          mapS
      val mapN                                        = Map[Long, String](0L -> array(GregorianCalendar.BC).substring(0, 1),
                                   1L -> array(GregorianCalendar.AD).substring(0, 1)
      )
      val styleMap: Map[TextStyle, Map[Long, String]] = Map[TextStyle, Map[Long, String]](
        TextStyle.SHORT  -> mapS,
        TextStyle.FULL   -> mapF,
        TextStyle.NARROW -> mapN
      )
      return TTBPSimpleDateTimeTextProvider.createLocaleStore(styleMap)
    }
    if (field eq IsoFields.QUARTER_OF_YEAR) {
      val mapS: Map[Long, String]                     =
        Map[Long, String](1L -> "Q1", 2L -> "Q2", 3L -> "Q3", 4L -> "Q4")
      val mapF                                        = Map[Long, String](1L -> "1st quarter",
                                   2L -> "2nd quarter",
                                   3L -> "3rd quarter",
                                   4L -> "4th quarter"
      )
      val styleMap: Map[TextStyle, Map[Long, String]] =
        Map[TextStyle, Map[Long, String]](TextStyle.SHORT -> mapS, TextStyle.FULL -> mapF)
      return TTBPSimpleDateTimeTextProvider.createLocaleStore(styleMap)
    }
    ""
  }
}
