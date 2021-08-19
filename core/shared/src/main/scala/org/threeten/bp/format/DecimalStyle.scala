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

import java.text.DecimalFormatSymbols
import java.util.{ Locale, Objects }
import java.util.HashMap
import java.util.Map
import scala.collection.JavaConverters._

/**
 * Localized symbols used in date and time formatting.
 *
 * A significant part of dealing with dates and times is the localization. This class acts as a
 * central point for accessing the information.
 *
 * <h3>Specification for implementors</h3> This class is immutable and thread-safe.
 */
object DecimalStyle {

  /**
   * The standard set of non-localized symbols.
   *
   * This uses standard ASCII characters for zero, positive, negative and a dot for the decimal
   * point.
   */
  lazy val STANDARD: DecimalStyle = new DecimalStyle('0', '+', '-', '.')

  /** The cache of symbols instances. */
  private lazy val CACHE: Map[Locale, DecimalStyle] =
    new HashMap[Locale, DecimalStyle]()

  /**
   * Lists all the locales that are supported.
   *
   * The locale 'en_US' will always be present.
   *
   * @return
   *   an array of locales for which localization is supported
   */
  def getAvailableLocales: java.util.Set[Locale] =
    DecimalFormatSymbols.getAvailableLocales.toSet.asJava

  /**
   * Obtains symbols for the default locale.
   *
   * This method provides access to locale sensitive symbols.
   *
   * @return
   *   the info, not null
   */
  def ofDefaultLocale: DecimalStyle = of(Locale.getDefault)

  /**
   * Obtains symbols for the specified locale.
   *
   * This method provides access to locale sensitive symbols.
   *
   * @param locale
   *   the locale, not null
   * @return
   *   the info, not null
   */
  def of(locale: Locale): DecimalStyle = {
    Objects.requireNonNull(locale, "locale")
    // Size reduced
    if (CACHE.get(locale) == null)
      CACHE.put(locale, create(locale))
    CACHE.get(locale)
  }

  private def create(locale: Locale): DecimalStyle = {
    val oldSymbols: DecimalFormatSymbols = DecimalFormatSymbols.getInstance(locale)
    val zeroDigit: Char                  = oldSymbols.getZeroDigit
    val positiveSign: Char               = '+'
    val negativeSign: Char               = oldSymbols.getMinusSign
    val decimalSeparator: Char           = oldSymbols.getDecimalSeparator
    if (zeroDigit == '0' && negativeSign == '-' && decimalSeparator == '.') STANDARD
    else new DecimalStyle(zeroDigit, positiveSign, negativeSign, decimalSeparator)
  }
}

/**
 * @constructor
 *   Restricted constructor.
 *
 * @param zeroDigit
 *   the character to use for the digit of zero
 * @param positiveSign
 *   the character to use for the positive sign
 * @param negativeSign
 *   the character to use for the negative sign
 * @param decimalSeparator
 *   the character to use for the decimal point
 */
final class DecimalStyle private (
  val zeroDigit:        Char,
  val positiveSign:     Char,
  val negativeSign:     Char,
  val decimalSeparator: Char
) {

  /**
   * Gets the character that represents zero.
   *
   * The character used to represent digits may vary by culture. This method specifies the zero
   * character to use, which implies the characters for one to nine.
   *
   * @return
   *   the character for zero
   */
  def getZeroDigit: Char = zeroDigit

  /**
   * Returns a copy of the info with a new character that represents zero.
   *
   * The character used to represent digits may vary by culture. This method specifies the zero
   * character to use, which implies the characters for one to nine.
   *
   * @param zeroDigit
   *   the character for zero
   * @return
   *   a copy with a new character that represents zero, not null
   */
  def withZeroDigit(zeroDigit: Char): DecimalStyle =
    if (zeroDigit == this.zeroDigit) this
    else new DecimalStyle(zeroDigit, positiveSign, negativeSign, decimalSeparator)

  /**
   * Gets the character that represents the positive sign.
   *
   * The character used to represent a positive number may vary by culture. This method specifies
   * the character to use.
   *
   * @return
   *   the character for the positive sign
   */
  def getPositiveSign: Char = positiveSign

  /**
   * Returns a copy of the info with a new character that represents the positive sign.
   *
   * The character used to represent a positive number may vary by culture. This method specifies
   * the character to use.
   *
   * @param positiveSign
   *   the character for the positive sign
   * @return
   *   a copy with a new character that represents the positive sign, not null
   */
  def withPositiveSign(positiveSign: Char): DecimalStyle =
    if (positiveSign == this.positiveSign)
      this
    else
      new DecimalStyle(zeroDigit, positiveSign, negativeSign, decimalSeparator)

  /**
   * Gets the character that represents the negative sign.
   *
   * The character used to represent a negative number may vary by culture. This method specifies
   * the character to use.
   *
   * @return
   *   the character for the negative sign
   */
  def getNegativeSign: Char = negativeSign

  /**
   * Returns a copy of the info with a new character that represents the negative sign.
   *
   * The character used to represent a negative number may vary by culture. This method specifies
   * the character to use.
   *
   * @param negativeSign
   *   the character for the negative sign
   * @return
   *   a copy with a new character that represents the negative sign, not null
   */
  def withNegativeSign(negativeSign: Char): DecimalStyle =
    if (negativeSign == this.negativeSign)
      this
    else
      new DecimalStyle(zeroDigit, positiveSign, negativeSign, decimalSeparator)

  /**
   * Gets the character that represents the decimal point.
   *
   * The character used to represent a decimal point may vary by culture. This method specifies the
   * character to use.
   *
   * @return
   *   the character for the decimal point
   */
  def getDecimalSeparator: Char = decimalSeparator

  /**
   * Returns a copy of the info with a new character that represents the decimal point.
   *
   * The character used to represent a decimal point may vary by culture. This method specifies the
   * character to use.
   *
   * @param decimalSeparator
   *   the character for the decimal point
   * @return
   *   a copy with a new character that represents the decimal point, not null
   */
  def withDecimalSeparator(decimalSeparator: Char): DecimalStyle =
    if (decimalSeparator == this.decimalSeparator)
      this
    else
      new DecimalStyle(zeroDigit, positiveSign, negativeSign, decimalSeparator)

  /**
   * Checks whether the character is a digit, based on the currently set zero character.
   *
   * @param ch
   *   the character to check
   * @return
   *   the value, 0 to 9, of the character, or -1 if not a digit
   */
  private[format] def convertToDigit(ch: Char): Int = {
    val value: Int = ch - zeroDigit
    if (value >= 0 && value <= 9) value else -1
  }

  /**
   * Converts the input numeric text to the internationalized form using the zero character.
   *
   * @param numericText
   *   the text, consisting of digits 0 to 9, to convert, not null
   * @return
   *   the internationalized text, not null
   */
  private[format] def convertNumberToI18N(numericText: String): String = {
    // Size verified
    if (zeroDigit == '0')
      return numericText
    val diff: Int          = zeroDigit - '0'
    val array: Array[Char] = numericText.toCharArray

    var i: Int = 0
    while (i < array.length) {
      array(i) = (array(i) + diff).toChar
      i += 1
    }

    new String(array)
  }

  /**
   * Checks if these symbols equal another set of symbols.
   *
   * @param obj
   *   the object to check, null returns false
   * @return
   *   true if this is equal to the other date
   */
  override def equals(obj: Any): Boolean =
    obj match {
      case other: DecimalStyle =>
        (this eq other) || (zeroDigit == other.zeroDigit && positiveSign == other.positiveSign && negativeSign == other.negativeSign && decimalSeparator == other.decimalSeparator)
      case _                   =>
        false
    }

  /**
   * A hash code for these symbols.
   *
   * @return
   *   a suitable hash code
   */
  override def hashCode: Int = zeroDigit + positiveSign + negativeSign + decimalSeparator

  /**
   * Returns a string describing these symbols.
   *
   * @return
   *   a string description, not null
   */
  override def toString: String =
    s"DecimalStyle[$zeroDigit$positiveSign$negativeSign$decimalSeparator]"
}
