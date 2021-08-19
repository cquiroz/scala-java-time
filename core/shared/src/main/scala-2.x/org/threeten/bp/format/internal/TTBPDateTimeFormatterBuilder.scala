package org.threeten.bp.format.internal

import java.math.{ BigDecimal, BigInteger, RoundingMode }
import java.util._
import java.lang.StringBuilder
import java.lang.Long

import org.threeten.bp._
import org.threeten.bp.chrono.{ ChronoLocalDate, Chronology }
import org.threeten.bp.format.DateTimeFormatterBuilder.LENGTH_SORT
import org.threeten.bp.format._
import org.threeten.bp.format.internal.TTBPDateTimeFormatterBuilder.ZoneIdPrinterParser.SubstringTree
import org.threeten.bp.temporal._
import org.threeten.bp.zone.ZoneRulesProvider

import scala.annotation.tailrec

object TTBPDateTimeFormatterBuilder {

  /** Composite printer and parser. */
  private[format] final class CompositePrinterParser private[format] (
    private val printerParsers: Array[DateTimePrinterParser],
    private val optional:       Boolean
  ) extends DateTimePrinterParser {

    private[format] def this(printerParsers: List[DateTimePrinterParser], optional: Boolean) =
      this(printerParsers.toArray(new Array[DateTimePrinterParser](printerParsers.size)), optional)

    /**
     * Returns a copy of this printer-parser with the optional flag changed.
     *
     * @param optional
     *   the optional flag to set in the copy
     * @return
     *   the new printer-parser, not null
     */
    def withOptional(optional: Boolean): CompositePrinterParser =
      if (optional == this.optional)
        this
      else
        new CompositePrinterParser(printerParsers, optional)

    def print(context: TTBPDateTimePrintContext, buf: StringBuilder): Boolean = {
      val length: Int = buf.length
      if (optional)
        context.startOptional()
      try for (pp <- printerParsers)
        if (!pp.print(context, buf)) {
          buf.setLength(length)
          return true
        } finally if (optional)
        context.endOptional()
      true
    }

    def parse(context: TTBPDateTimeParseContext, text: CharSequence, position: Int): Int = {
      val _position = position

      if (optional) {
        context.startOptional()
        var pos: Int = _position
        for (pp <- printerParsers) {
          pos = pp.parse(context, text, pos)
          if (pos < 0) {
            context.endOptional(false)
            return _position
          }
        }
        context.endOptional(true)
        pos
      } else
        printerParsers.foldLeft(_position) {
          case (pos, _) if pos < 0 =>
            pos
          case (pos, pp)           =>
            pp.parse(context, text, pos)
        }
    }

    override def toString: String = {
      // Doing this with mkString produces a bigger output
      // Option(printerParsers).map(_.mkString(if (optional) "[" else "(", "", if (optional) "]" else ")")).getOrElse("")
      val buf: StringBuilder = new StringBuilder
      if (printerParsers != null) {
        buf.append(if (optional) "[" else "(")
        for (pp <- printerParsers)
          buf.append(pp)
        buf.append(if (optional) "]" else ")")
      }
      buf.toString
    }
  }

  /**
   * Pads the output to a fixed width.
   *
   * @constructor
   *
   * @param printerParser
   *   the printer, not null
   * @param padWidth
   *   the width to pad to, 1 or greater
   * @param padChar
   *   the pad character
   */
  private[format] final class PadPrinterParserDecorator private[format] (
    private val printerParser: DateTimePrinterParser,
    private val padWidth:      Int,
    private val padChar:       Char
  ) extends DateTimePrinterParser {

    def print(context: TTBPDateTimePrintContext, buf: StringBuilder): Boolean = {
      val preLen: Int = buf.length
      if (!printerParser.print(context, buf))
        return false
      val len: Int    = buf.length - preLen
      if (len > padWidth)
        throw new DateTimeException(
          s"Cannot print as output of $len characters exceeds pad width of $padWidth"
        )
      var i: Int      = 0
      while (i < padWidth - len) {
        buf.insert(preLen, padChar)
        i += 1
      }
      true
    }

    def parse(context: TTBPDateTimeParseContext, text: CharSequence, position: Int): Int = {
      var _text                  = text
      val strict: Boolean        = context.isStrict
      val caseSensitive: Boolean = context.isCaseSensitive
      if (position > _text.length)
        throw new IndexOutOfBoundsException
      if (position == _text.length)
        return ~position
      var endPos: Int            = position + padWidth
      if (endPos > _text.length) {
        if (strict)
          return ~position
        endPos = _text.length
      }
      var pos: Int               = position
      while (
        pos < endPos && (if (caseSensitive) _text.charAt(pos) == padChar
                         else context.charEquals(_text.charAt(pos), padChar))
      )
        pos += 1
      _text = _text.subSequence(0, endPos)
      val resultPos: Int         = printerParser.parse(context, _text, pos)
      if (resultPos != endPos && strict)
        return ~(position + pos)
      resultPos
    }

    override def toString: String =
      s"Pad($printerParser,$padWidth${if (padChar == ' ') ")" else ",'" + padChar + "')"}"
  }

  /** Enumeration to apply simple parse settings. */
  private[format] object SettingsParser {
    lazy val SENSITIVE   = new SettingsParser("SENSITIVE", 0)
    lazy val INSENSITIVE = new SettingsParser("INSENSITIVE", 1)
    lazy val STRICT      = new SettingsParser("STRICT", 2)
    lazy val LENIENT     = new SettingsParser("LENIENT", 3)
  }

  private[format] final class SettingsParser private (name: String, ordinal: Int)
      extends Enum[SettingsParser](name, ordinal)
      with DateTimePrinterParser {
    def print(context: TTBPDateTimePrintContext, buf: StringBuilder): Boolean = true

    def parse(context: TTBPDateTimeParseContext, text: CharSequence, position: Int): Int = {
      ordinal match {
        case 0 => context.setCaseSensitive(true)
        case 1 => context.setCaseSensitive(false)
        case 2 => context.setStrict(true)
        case 3 => context.setStrict(false)
      }
      position
    }

    override def toString: String =
      ordinal match {
        case 0 => "ParseCaseSensitive(true)"
        case 1 => "ParseCaseSensitive(false)"
        case 2 => "ParseStrict(true)"
        case 3 => "ParseStrict(false)"
        case _ => throw new IllegalStateException("Unreachable")
      }
  }

  /** Used by parseDefaulting(). */
  private[format] class DefaultingParser private[format] (
    private val field: TemporalField,
    private val value: Long
  ) extends DateTimePrinterParser {

    def print(context: TTBPDateTimePrintContext, buf: StringBuilder): Boolean = true

    def parse(context: TTBPDateTimeParseContext, text: CharSequence, position: Int): Int = {
      if (context.getParsed(field) == null)
        context.setParsedField(field, value, position, position)
      position
    }
  }

  /** Prints or parses a character literal. */
  final class CharLiteralPrinterParser private[format] (private val literal: Char)
      extends DateTimePrinterParser {

    def print(context: TTBPDateTimePrintContext, buf: StringBuilder): Boolean = {
      buf.append(literal)
      true
    }

    def parse(context: TTBPDateTimeParseContext, text: CharSequence, position: Int): Int = {
      val length: Int = text.length
      if (position == length)
        return ~position
      // Workaround for non-conforming Scala.js behavior
      if (position < 0 || position > length)
        throw new StringIndexOutOfBoundsException

      val ch: Char = text.charAt(position)
      if (!context.charEquals(literal, ch))
        return ~position
      position + 1
    }

    override def toString: String =
      if (literal == '\'') "''"
      else s"'$literal'"
  }

  /** Prints or parses a string literal. */
  private[format] final class StringLiteralPrinterParser private[format] (
    private val literal: String
  ) extends DateTimePrinterParser {

    def print(context: TTBPDateTimePrintContext, buf: StringBuilder): Boolean = {
      buf.append(literal)
      true
    }

    def parse(context: TTBPDateTimeParseContext, text: CharSequence, position: Int): Int = {
      val length: Int = text.length
      if (position > length || position < 0)
        throw new IndexOutOfBoundsException
      else if (!context.subSequenceEquals(text, position, literal, 0, literal.length))
        ~position
      else
        position + literal.length
    }

    override def toString: String = {
      val converted: String = literal.replace("'", "''")
      s"'$converted'"
    }
  }

  /** Prints and parses a numeric date-time field with optional padding. */
  private[format] object NumberPrinterParser {

    /** Array of 10 to the power of n. */
    private[format] val EXCEED_POINTS: Array[Int] =
      Array[Int](0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)
  }

  /**
   * @constructor
   *
   * @param field
   *   the field to print, not null
   * @param minWidth
   *   the minimum field width, from 1 to 19
   * @param maxWidth
   *   the maximum field width, from minWidth to 19
   * @param signStyle
   *   the positive/negative sign style, not null
   * @param subsequentWidth
   *   the width of subsequent non-negative numbers, 0 or greater,
   * -1 if fixed width due to active adjacent parsing
   */
  private[format] class NumberPrinterParser private[format] (
    private[format] val field:           TemporalField,
    private[format] val minWidth:        Int,
    private[format] val maxWidth:        Int,
    private[format] val signStyle:       SignStyle,
    private[format] val subsequentWidth: Int
  ) extends DateTimePrinterParser {

    /**
     * @constructor
     *
     * @param field
     *   the field to print, not null
     * @param minWidth
     *   the minimum field width, from 1 to 19
     * @param maxWidth
     *   the maximum field width, from minWidth to 19
     * @param signStyle
     *   the positive/negative sign style, not null
     */
    private[format] def this(
      field:     TemporalField,
      minWidth:  Int,
      maxWidth:  Int,
      signStyle: SignStyle
    ) = this(field, minWidth, maxWidth, signStyle, 0)

    /**
     * Returns a new instance with fixed width flag set.
     *
     * @return
     *   a new updated printer-parser, not null
     */
    private[format] def withFixedWidth: NumberPrinterParser =
      if (subsequentWidth == -1)
        this
      else
        new NumberPrinterParser(field, minWidth, maxWidth, signStyle, -1)

    /**
     * Returns a new instance with an updated subsequent width.
     *
     * @param subsequentWidth
     *   the width of subsequent non-negative numbers, 0 or greater
     * @return
     *   a new updated printer-parser, not null
     */
    private[format] def withSubsequentWidth(subsequentWidth: Int): NumberPrinterParser =
      new NumberPrinterParser(field,
                              minWidth,
                              maxWidth,
                              signStyle,
                              this.subsequentWidth + subsequentWidth
      )

    def print(context: TTBPDateTimePrintContext, buf: StringBuilder): Boolean = {
      val valueLong: java.lang.Long = context.getValue(field)
      if (valueLong == null)
        return false
      val value: Long               = getValue(context, valueLong)
      val symbols: DecimalStyle     = context.getSymbols
      var str: String               =
        if (value == scala.Long.MinValue) "9223372036854775808" else Math.abs(value).toString
      if (str.length > maxWidth)
        throw new DateTimeException(
          s"Field $field cannot be printed as the value $value exceeds the maximum print width of $maxWidth"
        )
      str = symbols.convertNumberToI18N(str)

      import SignStyle._
      if (value >= 0)
        signStyle match {
          case EXCEEDS_PAD =>
            if (minWidth < 19 && value >= NumberPrinterParser.EXCEED_POINTS(minWidth))
              buf.append(symbols.getPositiveSign)
          case ALWAYS      =>
            buf.append(symbols.getPositiveSign)
          case _           =>
        }
      else
        signStyle match {
          case NORMAL | EXCEEDS_PAD | ALWAYS =>
            buf.append(symbols.getNegativeSign)
          case NOT_NEGATIVE                  =>
            throw new DateTimeException(
              s"Field $field cannot be printed as the value $value cannot be negative according to the SignStyle"
            )
          case _                             =>
        }

      var i: Int = 0
      while (i < minWidth - str.length) {
        buf.append(symbols.getZeroDigit)
        i += 1
      }

      buf.append(str)
      true
    }

    /**
     * Gets the value to output.
     *
     * @param context
     *   the context
     * @param value
     *   the value of the field, not null
     * @return
     *   the value
     */
    protected[format] def getValue(context: TTBPDateTimePrintContext, value: Long): Long =
      if (context != null) value else value

    protected[format] def isFixedWidth(context: TTBPDateTimeParseContext): Boolean =
      if (context != null)
        subsequentWidth == -1 || (subsequentWidth > 0 && minWidth == maxWidth && (signStyle eq SignStyle.NOT_NEGATIVE))
      else
        subsequentWidth == -1 || (subsequentWidth > 0 && minWidth == maxWidth && (signStyle eq SignStyle.NOT_NEGATIVE))

    def parse(context: TTBPDateTimeParseContext, text: CharSequence, position: Int): Int = {
      var _position = position

      val length: Int          = text.length
      if (_position == length)
        return ~_position
      if (_position < 0 || position > length)
        throw new StringIndexOutOfBoundsException
      val sign: Char           = text.charAt(_position)
      var negative: Boolean    = false
      var positive: Boolean    = false
      if (sign == context.getSymbols.getPositiveSign) {
        if (!signStyle.parse(true, context.isStrict, minWidth == maxWidth))
          return ~_position
        positive = true
        _position += 1
      } else if (sign == context.getSymbols.getNegativeSign) {
        if (!signStyle.parse(false, context.isStrict, minWidth == maxWidth))
          return ~_position
        negative = true
        _position += 1
      } else if ((signStyle eq SignStyle.ALWAYS) && context.isStrict)
        return ~_position
      val effMinWidth: Int     = if (context.isStrict || isFixedWidth(context)) minWidth else 1
      val minEndPos: Int       = _position + effMinWidth
      if (minEndPos > length)
        return ~_position
      var effMaxWidth: Int     =
        (if (context.isStrict || isFixedWidth(context)) maxWidth else 9) + Math.max(subsequentWidth,
                                                                                    0
        )
      var total: Long          = 0
      var totalBig: BigInteger = null
      var pos: Int             = _position

      var pass: Int  = 0
      var outerBreak = false
      while (!outerBreak && pass < 2) {
        val maxEndPos: Int = Math.min(pos + effMaxWidth, length)
        var break          = false
        while (!break && pos < maxEndPos) {
          val ch: Char   = text.charAt(pos)
          pos += 1
          val digit: Int = context.getSymbols.convertToDigit(ch)
          if (digit < 0) {
            pos -= 1
            if (pos < minEndPos)
              return ~_position
            break = true
          }
          if (!break)
            if ((pos - _position) > 18) {
              if (totalBig == null)
                totalBig = BigInteger.valueOf(total)
              totalBig = totalBig.multiply(BigInteger.TEN).add(BigInteger.valueOf(digit.toLong))
            } else
              total = total * 10 + digit
        }
        if (subsequentWidth > 0 && pass == 0) {
          val parseLen: Int = pos - _position
          effMaxWidth = Math.max(effMinWidth, parseLen - subsequentWidth)
          pos = _position
          total = 0
          totalBig = null
          pass += 1
        } else
          outerBreak = true
      }
      if (negative)
        if (totalBig != null) {
          if ((totalBig == BigInteger.ZERO) && context.isStrict)
            return ~(_position - 1)
          totalBig = totalBig.negate
        } else {
          if (total == 0 && context.isStrict)
            return ~(_position - 1)
          total = -total
        }
      else if ((signStyle eq SignStyle.EXCEEDS_PAD) && context.isStrict) {
        val parseLen: Int = pos - _position
        if (positive) {
          if (parseLen <= minWidth)
            return ~(_position - 1)
        } else if (parseLen > minWidth)
          return ~_position
      }
      if (totalBig != null) {
        if (totalBig.bitLength > 63) {
          totalBig = totalBig.divide(BigInteger.TEN)
          pos -= 1
        }
        return setValue(context, totalBig.longValue, _position, pos)
      }
      setValue(context, total, _position, pos)
    }

    /**
     * Stores the value.
     *
     * @param context
     *   the context to store into, not null
     * @param value
     *   the value
     * @param errorPos
     *   the position of the field being parsed
     * @param successPos
     *   the position after the field being parsed
     * @return
     *   the new position
     */
    private[format] def setValue(
      context:    TTBPDateTimeParseContext,
      value:      Long,
      errorPos:   Int,
      successPos: Int
    ): Int =
      context.setParsedField(field, value, errorPos, successPos)

    override def toString: String =
      if (minWidth == 1 && maxWidth == 19 && (signStyle eq SignStyle.NORMAL))
        s"Value($field)"
      else if (minWidth == maxWidth && (signStyle eq SignStyle.NOT_NEGATIVE))
        s"Value($field,$minWidth)"
      else
        s"Value($field,$minWidth,$maxWidth,$signStyle)"
  }

  /** Prints and parses a reduced numeric date-time field. */
  private[format] object ReducedPrinterParser {
    private[format] val BASE_DATE: LocalDate = LocalDate.of(2000, 1, 1)
  }

  /**
   * @constructor
   *
   * @param field
   *   the field to print, validated not null
   * @param minWidth
   *   the field width, from 1 to 10
   * @param maxWidth
   *   the field max width, from 1 to 10
   * @param baseValue
   *   the base value
   * @param baseDate
   *   the base date
   */
  private[format] final class ReducedPrinterParser private[format] (
    field:                 TemporalField,
    minWidth:              Int,
    maxWidth:              Int,
    private val baseValue: Int,
    private val baseDate:  ChronoLocalDate,
    subsequentWidth:       Int
  ) extends NumberPrinterParser(field,
                                minWidth,
                                maxWidth,
                                SignStyle.NOT_NEGATIVE,
                                subsequentWidth
      ) {

    if (minWidth < 1 || minWidth > 10)
      throw new IllegalArgumentException(
        s"The width must be from 1 to 10 inclusive but was $minWidth"
      )
    if (maxWidth < 1 || maxWidth > 10)
      throw new IllegalArgumentException(
        s"The maxWidth must be from 1 to 10 inclusive but was $maxWidth"
      )
    if (maxWidth < minWidth)
      throw new IllegalArgumentException("The maxWidth must be greater than the width")
    if (baseDate == null) {
      if (!field.range.isValidValue(baseValue.toLong))
        throw new IllegalArgumentException("The base value must be within the range of the field")
      if ((baseValue.toLong + NumberPrinterParser.EXCEED_POINTS(minWidth)) > Int.MaxValue)
        throw new DateTimeException(
          "Unable to add printer-parser as the range exceeds the capacity of an int"
        )
    }

    private[format] def this(
      field:     TemporalField,
      minWidth:  Int,
      maxWidth:  Int,
      baseValue: Int,
      baseDate:  ChronoLocalDate
    ) = this(field, minWidth, maxWidth, baseValue, baseDate, 0)

    protected[format] override def getValue(
      context: TTBPDateTimePrintContext,
      value:   Long
    ): Long = {
      val absValue: Long = Math.abs(value)
      var baseValue: Int = this.baseValue
      if (baseDate != null) {
        val chrono: Chronology = Chronology.from(context.getTemporal)
        baseValue = chrono.date(baseDate).get(field)
      }
      if (value >= baseValue && value < baseValue + NumberPrinterParser.EXCEED_POINTS(minWidth))
        return absValue % NumberPrinterParser.EXCEED_POINTS(minWidth)
      absValue % NumberPrinterParser.EXCEED_POINTS(maxWidth)
    }

    private[format] override def setValue(
      context:    TTBPDateTimeParseContext,
      value:      Long,
      errorPos:   Int,
      successPos: Int
    ): Int = {
      var _value = value

      var baseValue: Int = this.baseValue
      if (baseDate != null) {
        val chrono: Chronology = context.getEffectiveChronology
        baseValue = chrono.date(baseDate).get(field)
        context.addChronologyChangedParser(this, _value, errorPos, successPos)
      }
      val parseLen: Int  = successPos - errorPos
      if (parseLen == minWidth && _value >= 0) {
        val range: Long    = NumberPrinterParser.EXCEED_POINTS(minWidth).toLong
        val lastPart: Long = baseValue % range
        val basePart: Long = baseValue - lastPart
        if (baseValue > 0)
          _value = basePart + _value
        else
          _value = basePart - _value
        if (_value < baseValue)
          _value += range
      }
      context.setParsedField(field, _value, errorPos, successPos)
    }

    private[format] override def withFixedWidth: NumberPrinterParser =
      if (subsequentWidth == -1)
        this
      else
        new ReducedPrinterParser(field, minWidth, maxWidth, baseValue, baseDate, -1)

    private[format] override def withSubsequentWidth(subsequentWidth: Int): ReducedPrinterParser =
      new ReducedPrinterParser(field,
                               minWidth,
                               maxWidth,
                               baseValue,
                               baseDate,
                               this.subsequentWidth + subsequentWidth
      )

    protected[format] override def isFixedWidth(context: TTBPDateTimeParseContext): Boolean =
      if (!context.isStrict)
        false
      else
        super.isFixedWidth(context)

    override def toString: String =
      s"ReducedValue($field,$minWidth,$maxWidth,${if (baseDate != null) baseDate else baseValue})"
  }

  /**
   * Prints and parses a numeric date-time field with optional padding.
   *
   * @constructor
   *
   * @param field
   *   the field to output, not null
   * @param minWidth
   *   the minimum width to output, from 0 to 9
   * @param maxWidth
   *   the maximum width to output, from 0 to 9
   * @param decimalPoint
   *   whether to output the localized decimal point symbol
   */
  private[format] final class FractionPrinterParser private[format] (
    private val field:        TemporalField,
    private val minWidth:     Int,
    private val maxWidth:     Int,
    private val decimalPoint: Boolean
  ) extends DateTimePrinterParser {
    Objects.requireNonNull(field, "field")
    if (!field.range.isFixed)
      throw new IllegalArgumentException(s"Field must have a fixed set of values: $field")
    if (minWidth < 0 || minWidth > 9)
      throw new IllegalArgumentException(
        s"Minimum width must be from 0 to 9 inclusive but was $minWidth"
      )
    if (maxWidth < 1 || maxWidth > 9)
      throw new IllegalArgumentException(
        s"Maximum width must be from 1 to 9 inclusive but was $maxWidth"
      )
    if (maxWidth < minWidth)
      throw new IllegalArgumentException(
        s"Maximum width must exceed or equal the minimum width but $maxWidth < $minWidth"
      )

    def print(context: TTBPDateTimePrintContext, buf: StringBuilder): Boolean = {
      val value: java.lang.Long = context.getValue(field)
      if (value == null)
        return false
      val symbols: DecimalStyle = context.getSymbols
      var fraction: BigDecimal  = convertToFraction(value)
      if (fraction.scale == 0) {
        if (minWidth > 0) {
          if (decimalPoint)
            buf.append(symbols.getDecimalSeparator)
          var i: Int = 0
          while (i < minWidth) {
            buf.append(symbols.getZeroDigit)
            i += 1
          }
        }
      } else {
        val outputScale: Int = Math.min(Math.max(fraction.scale, minWidth), maxWidth)
        fraction = fraction.setScale(outputScale, RoundingMode.FLOOR)
        var str: String      = fraction.toPlainString.substring(2)
        str = symbols.convertNumberToI18N(str)
        if (decimalPoint)
          buf.append(symbols.getDecimalSeparator)
        buf.append(str)
      }
      true
    }

    def parse(context: TTBPDateTimeParseContext, text: CharSequence, position: Int): Int = {
      var _position = position

      val effectiveMin: Int    = if (context.isStrict) minWidth else 0
      val effectiveMax: Int    = if (context.isStrict) maxWidth else 9
      val length: Int          = text.length
      if (_position == length)
        return if (effectiveMin > 0) ~_position else _position
      if (decimalPoint) {
        if (text.charAt(_position) != context.getSymbols.getDecimalSeparator)
          return if (effectiveMin > 0) ~_position else _position
        _position += 1
      }
      val minEndPos: Int       = _position + effectiveMin
      if (minEndPos > length)
        return ~_position
      val maxEndPos: Int       = Math.min(_position + effectiveMax, length)
      var total: Int           = 0
      var pos: Int             = _position
      var break                = false
      while (!break && pos < maxEndPos) {
        val ch: Char   = text.charAt(pos)
        pos += 1
        val digit: Int = context.getSymbols.convertToDigit(ch)
        if (digit < 0) {
          if (pos < minEndPos)
            return ~_position
          pos -= 1
          break = true
        }
        if (!break)
          total = total * 10 + digit
      }
      val fraction: BigDecimal = new BigDecimal(total).movePointLeft(pos - _position)
      val value: Long          = convertFromFraction(fraction)
      context.setParsedField(field, value, _position, pos)
    }

    /**
     * Converts a value for this field to a fraction between 0 and 1.
     *
     * The fractional value is between 0 (inclusive) and 1 (exclusive). It can only be returned if
     * the {@link TemporalField#range() value range} is fixed. The fraction is obtained by
     * calculation from the field range using 9 decimal places and a rounding mode of {@link
     * RoundingMode#FLOOR FLOOR}. The calculation is inaccurate if the values do not run
     * continuously from smallest to largest.
     *
     * For example, the second-of-minute value of 15 would be returned as 0.25, assuming the
     * standard definition of 60 seconds in a minute.
     *
     * @param value
     *   the value to convert, must be valid for this rule
     * @return
     *   the value as a fraction within the range, from 0 to 1, not null
     * @throws DateTimeException
     *   if the value cannot be converted to a fraction
     */
    private def convertToFraction(value: Long): BigDecimal = {
      val range: ValueRange    = field.range
      range.checkValidValue(value, field)
      val minBD: BigDecimal    = BigDecimal.valueOf(range.getMinimum)
      val rangeBD: BigDecimal  =
        BigDecimal.valueOf(range.getMaximum).subtract(minBD).add(BigDecimal.ONE)
      val valueBD: BigDecimal  = BigDecimal.valueOf(value).subtract(minBD)
      val fraction: BigDecimal = valueBD.divide(rangeBD, 9, RoundingMode.FLOOR)
      if (fraction.compareTo(BigDecimal.ZERO) == 0) BigDecimal.ZERO else fraction.stripTrailingZeros
    }

    /**
     * Converts a fraction from 0 to 1 for this field to a value.
     *
     * The fractional value must be between 0 (inclusive) and 1 (exclusive). It can only be returned
     * if the {@link TemporalField#range() value range} is fixed. The value is obtained by
     * calculation from the field range and a rounding mode of {@link RoundingMode#FLOOR FLOOR}. The
     * calculation is inaccurate if the values do not run continuously from smallest to largest.
     *
     * For example, the fractional second-of-minute of 0.25 would be converted to 15, assuming the
     * standard definition of 60 seconds in a minute.
     *
     * @param fraction
     *   the fraction to convert, not null
     * @return
     *   the value of the field, valid for this rule
     * @throws DateTimeException
     *   if the value cannot be converted
     */
    private def convertFromFraction(fraction: BigDecimal): Long = {
      val range: ValueRange   = field.range
      val minBD: BigDecimal   = BigDecimal.valueOf(range.getMinimum)
      val rangeBD: BigDecimal =
        BigDecimal.valueOf(range.getMaximum).subtract(minBD).add(BigDecimal.ONE)
      fraction.multiply(rangeBD).setScale(0, RoundingMode.FLOOR).add(minBD).longValueExact
    }

    override def toString: String = {
      val decimal: String = if (decimalPoint) ",DecimalPoint" else ""
      s"Fraction($field,$minWidth,$maxWidth$decimal)"
    }
  }

  /**
   * Prints or parses field text.
   *
   * @constructor
   *
   * @param field
   *   the field to output, not null
   * @param textStyle
   *   the text style, not null
   * @param provider
   *   the text provider, not null
   */
  private[format] final class TextPrinterParser private[format] (
    private val field:     TemporalField,
    private val textStyle: TextStyle,
    private val provider:  TTBPDateTimeTextProvider
  ) extends DateTimePrinterParser {

    /**
     * The cached number printer parser. Immutable and volatile, so no synchronization needed.
     */
    @volatile
    private var _numberPrinterParser: NumberPrinterParser = null

    def print(context: TTBPDateTimePrintContext, buf: StringBuilder): Boolean = {
      val value: java.lang.Long = context.getValue(field)
      if (value == null)
        return false
      val text: String          = provider.getText(field, value, textStyle, context.getLocale)
      if (text == null)
        return numberPrinterParser.print(context, buf)
      buf.append(text)
      true
    }

    def parse(context: TTBPDateTimeParseContext, parseText: CharSequence, position: Int): Int = {
      val length: Int      = parseText.length
      if (position < 0 || position > length)
        throw new IndexOutOfBoundsException
      val style: TextStyle = if (context.isStrict) textStyle else null
      val it               = provider.getTextIterator(field, style, context.getLocale)
      if (it != null) {
        while (it.hasNext) {
          val entry          = it.next()
          val itText: String = entry._1
          if (context.subSequenceEquals(itText, 0, parseText, position, itText.length))
            return context.setParsedField(field, entry._2, position, position + itText.length)
        }
        if (context.isStrict)
          return ~position
      }
      numberPrinterParser.parse(context, parseText, position)
    }

    /**
     * Create and cache a number printer parser.
     * @return
     *   the number printer parser for this field, not null
     */
    private def numberPrinterParser: NumberPrinterParser = {
      if (_numberPrinterParser == null)
        _numberPrinterParser = new NumberPrinterParser(field, 1, 19, SignStyle.NORMAL)
      _numberPrinterParser
    }

    override def toString: String =
      if (textStyle eq TextStyle.FULL)
        s"Text($field)"
      else
        s"Text($field,$textStyle)"
  }

  /** Prints or parses an ISO-8601 instant. */
  private[format] object InstantPrinterParser {
    private val SECONDS_PER_10000_YEARS: Long = 146097L * 25L * 86400L
    private val SECONDS_0000_TO_1970: Long    = ((146097L * 5L) - (30L * 365L + 7L)) * 86400L
  }

  private[format] final class InstantPrinterParser private[format] (
    private val fractionalDigits: Int
  ) extends DateTimePrinterParser {

    def print(context: TTBPDateTimePrintContext, buf: StringBuilder): Boolean = {
      val inSecs: java.lang.Long = context.getValue(ChronoField.INSTANT_SECONDS)
      var inNanos: Long          = 0L
      if (context.getTemporal.isSupported(ChronoField.NANO_OF_SECOND))
        inNanos = context.getTemporal.getLong(ChronoField.NANO_OF_SECOND)
      if (inSecs == null)
        return false
      val inSec: Long            = inSecs
      var inNano: Int            = ChronoField.NANO_OF_SECOND.checkValidIntValue(inNanos)
      if (inSec >= -InstantPrinterParser.SECONDS_0000_TO_1970) {
        val zeroSecs: Long     =
          inSec - InstantPrinterParser.SECONDS_PER_10000_YEARS + InstantPrinterParser.SECONDS_0000_TO_1970
        val hi: Long           = Math.floorDiv(zeroSecs, InstantPrinterParser.SECONDS_PER_10000_YEARS) + 1
        val lo: Long           = Math.floorMod(zeroSecs, InstantPrinterParser.SECONDS_PER_10000_YEARS)
        val ldt: LocalDateTime = LocalDateTime.ofEpochSecond(
          lo - InstantPrinterParser.SECONDS_0000_TO_1970,
          0,
          ZoneOffset.UTC
        )
        if (hi > 0)
          buf.append('+').append(hi)
        buf.append(ldt)
        if (ldt.getSecond == 0)
          buf.append(":00")
      } else {
        val zeroSecs: Long     = inSec + InstantPrinterParser.SECONDS_0000_TO_1970
        val hi: Long           = zeroSecs / InstantPrinterParser.SECONDS_PER_10000_YEARS
        val lo: Long           = zeroSecs % InstantPrinterParser.SECONDS_PER_10000_YEARS
        val ldt: LocalDateTime = LocalDateTime.ofEpochSecond(
          lo - InstantPrinterParser.SECONDS_0000_TO_1970,
          0,
          ZoneOffset.UTC
        )
        val pos: Int           = buf.length
        buf.append(ldt)
        if (ldt.getSecond == 0)
          buf.append(":00")
        if (hi < 0)
          if (ldt.getYear == -10000)
            buf.replace(pos, pos + 2, java.lang.Long.toString(hi - 1))
          else if (lo == 0)
            buf.insert(pos, hi)
          else
            buf.insert(pos + 1, Math.abs(hi))
      }
      if (fractionalDigits == -2) {
        if (inNano != 0) {
          buf.append('.')
          if (inNano % 1000000 == 0)
            buf.append(Integer.toString((inNano / 1000000) + 1000).substring(1))
          else if (inNano % 1000 == 0)
            buf.append(Integer.toString((inNano / 1000) + 1000000).substring(1))
          else
            buf.append(Integer.toString(inNano + 1000000000).substring(1))
        }
      } else if (fractionalDigits > 0 || (fractionalDigits == -1 && inNano > 0)) {
        buf.append('.')
        var div: Int = 100000000
        var i: Int   = 0
        while ((fractionalDigits == -1 && inNano > 0) || i < fractionalDigits) {
          val digit: Int = inNano / div
          buf.append((digit + '0').toChar)
          inNano = inNano - (digit * div)
          div = div / 10
          i += 1
        }
      }
      buf.append('Z')
      true
    }

    def parse(context: TTBPDateTimeParseContext, text: CharSequence, position: Int): Int = {
      val newContext: TTBPDateTimeParseContext = context.copy
      val minDigits: Int                       = if (fractionalDigits < 0) 0 else fractionalDigits
      val maxDigits: Int                       = if (fractionalDigits < 0) 9 else fractionalDigits
      val parser: CompositePrinterParser       =
        new DateTimeFormatterBuilder()
          .append(DateTimeFormatter.ISO_LOCAL_DATE)
          .appendLiteral('T')
          .appendValue(ChronoField.HOUR_OF_DAY, 2)
          .appendLiteral(':')
          .appendValue(ChronoField.MINUTE_OF_HOUR, 2)
          .appendLiteral(':')
          .appendValue(ChronoField.SECOND_OF_MINUTE, 2)
          .appendFraction(ChronoField.NANO_OF_SECOND, minDigits, maxDigits, true)
          .appendLiteral('Z')
          .toFormatter
          .toPrinterParser(false)
      val pos: Int                             = parser.parse(newContext, text, position)
      if (pos < 0)
        return pos
      val yearParsed: Long                     = newContext.getParsed(ChronoField.YEAR)
      val month: Int                           = newContext.getParsed(ChronoField.MONTH_OF_YEAR).intValue
      val day: Int                             = newContext.getParsed(ChronoField.DAY_OF_MONTH).intValue
      var hour: Int                            = newContext.getParsed(ChronoField.HOUR_OF_DAY).intValue
      val min: Int                             = newContext.getParsed(ChronoField.MINUTE_OF_HOUR).intValue
      val secVal: java.lang.Long               = newContext.getParsed(ChronoField.SECOND_OF_MINUTE)
      val nanoVal: java.lang.Long              = newContext.getParsed(ChronoField.NANO_OF_SECOND)
      var sec: Int                             = if (secVal != null) secVal.intValue else 0
      val nano: Int                            = if (nanoVal != null) nanoVal.intValue else 0
      val year: Int                            = yearParsed.toInt % 10000
      var days: Int                            = 0
      if (hour == 24 && min == 0 && sec == 0 && nano == 0) {
        hour = 0
        days = 1
      } else if (hour == 23 && min == 59 && sec == 60) {
        context.setParsedLeapSecond()
        sec = 59
      }
      var instantSecs: Long                    = 0L
      try {
        val ldt: LocalDateTime =
          LocalDateTime.of(year, month, day, hour, min, sec, 0).plusDays(days.toLong)
        instantSecs = ldt.toEpochSecond(ZoneOffset.UTC)
        instantSecs += Math.multiplyExact(yearParsed / 10000L,
                                          InstantPrinterParser.SECONDS_PER_10000_YEARS
        )
      } catch {
        case _: RuntimeException => return ~position
      }
      var successPos: Int                      = pos
      successPos =
        context.setParsedField(ChronoField.INSTANT_SECONDS, instantSecs, position, successPos)
      context.setParsedField(ChronoField.NANO_OF_SECOND, nano.toLong, position, successPos)
    }

    override def toString: String = "Instant()"
  }

  /** Prints or parses an offset ID. */
  private[format] object OffsetIdPrinterParser {
    private[format] val PATTERNS: Array[String]            = Array[String]("+HH",
                                                                "+HHmm",
                                                                "+HH:mm",
                                                                "+HHMM",
                                                                "+HH:MM",
                                                                "+HHMMss",
                                                                "+HH:MM:ss",
                                                                "+HHMMSS",
                                                                "+HH:MM:SS"
    )
    private[format] val INSTANCE_ID: OffsetIdPrinterParser =
      new OffsetIdPrinterParser("Z", "+HH:MM:ss")
  }

  /**
   * @constructor
   *
   * @param noOffsetText
   *   the text to use for UTC, not null
   * @param pattern
   *   the pattern
   */
  private[format] final class OffsetIdPrinterParser private[format] (
    private val noOffsetText: String,
    pattern:                  String
  ) extends DateTimePrinterParser {
    Objects.requireNonNull(noOffsetText, "noOffsetText")
    Objects.requireNonNull(pattern, "pattern")

    private val `type`: Int = checkPattern(pattern)

    private def checkPattern(pattern: String): Int = {
      var i: Int = 0
      while (i < OffsetIdPrinterParser.PATTERNS.length) {
        if (OffsetIdPrinterParser.PATTERNS(i) == pattern)
          return i
        i += 1
      }
      throw new IllegalArgumentException(s"Invalid zone offset pattern: $pattern")
    }

    def print(context: TTBPDateTimePrintContext, buf: StringBuilder): Boolean = {
      val offsetSecs: java.lang.Long = context.getValue(ChronoField.OFFSET_SECONDS)
      if (offsetSecs == null)
        return false
      val totalSecs: Int             = Math.toIntExact(offsetSecs)
      if (totalSecs == 0)
        buf.append(noOffsetText)
      else {
        val absHours: Int   = Math.abs((totalSecs / 3600) % 100)
        val absMinutes: Int = Math.abs((totalSecs / 60) % 60)
        val absSeconds: Int = Math.abs(totalSecs % 60)
        val bufPos: Int     = buf.length
        var output: Int     = absHours
        buf
          .append(if (totalSecs < 0) "-" else "+")
          .append((absHours / 10 + '0').toChar)
          .append((absHours % 10 + '0').toChar)
        if (`type` >= 3 || (`type` >= 1 && absMinutes > 0)) {
          buf
            .append(if ((`type` % 2) == 0) ":" else "")
            .append((absMinutes / 10 + '0').toChar)
            .append((absMinutes % 10 + '0').toChar)
          output += absMinutes
          if (`type` >= 7 || (`type` >= 5 && absSeconds > 0)) {
            buf
              .append(if ((`type` % 2) == 0) ":" else "")
              .append((absSeconds / 10 + '0').toChar)
              .append((absSeconds % 10 + '0').toChar)
            output += absSeconds
          }
        }
        if (output == 0) {
          buf.setLength(bufPos)
          buf.append(noOffsetText)
        }
      }
      true
    }

    def parse(context: TTBPDateTimeParseContext, text: CharSequence, position: Int): Int = {
      val length: Int      = text.length
      val noOffsetLen: Int = noOffsetText.length
      if (noOffsetLen == 0) {
        if (position == length)
          return context.setParsedField(ChronoField.OFFSET_SECONDS, 0, position, position)
      } else {
        if (position == length)
          return ~position
        if (context.subSequenceEquals(text, position, noOffsetText, 0, noOffsetLen))
          return context.setParsedField(ChronoField.OFFSET_SECONDS,
                                        0,
                                        position,
                                        position + noOffsetLen
          )
      }
      val sign: Char       = text.charAt(position)
      if (sign == '+' || sign == '-') {
        val negative: Int     = if (sign == '-') -1 else 1
        val array: Array[Int] = new Array[Int](4)
        array(0) = position + 1
        if (
          !(parseNumber(array, 1, text, true) || parseNumber(array,
                                                             2,
                                                             text,
                                                             `type` >= 3
          ) || parseNumber(
            array,
            3,
            text,
            false
          ))
        ) {
          val offsetSecs: Long = negative * (array(1) * 3600L + array(2) * 60L + array(3))
          return context.setParsedField(ChronoField.OFFSET_SECONDS, offsetSecs, position, array(0))
        }
      }
      if (noOffsetLen == 0)
        return context.setParsedField(ChronoField.OFFSET_SECONDS,
                                      0,
                                      position,
                                      position + noOffsetLen
        )
      ~position
    }

    /**
     * Parse a two digit zero-prefixed number.
     *
     * @param array
     *   the array of parsed data, 0=pos,1=hours,2=mins,3=secs, not null
     * @param arrayIndex
     *   the index to parse the value into
     * @param parseText
     *   the offset ID, not null
     * @param required
     *   whether this number is required
     * @return
     *   true if an error occurred
     */
    private def parseNumber(
      array:      Array[Int],
      arrayIndex: Int,
      parseText:  CharSequence,
      required:   Boolean
    ): Boolean = {
      if ((`type` + 3) / 2 < arrayIndex)
        return false
      var pos: Int = array(0)
      if ((`type` % 2) == 0 && arrayIndex > 1) {
        if (pos + 1 > parseText.length || parseText.charAt(pos) != ':')
          return required
        pos += 1
      }
      if (pos + 2 > parseText.length)
        return required
      val ch1: Char  = parseText.charAt(pos)
      pos += 1
      val ch2: Char  = parseText.charAt(pos)
      pos += 1
      if (ch1 < '0' || ch1 > '9' || ch2 < '0' || ch2 > '9')
        return required
      val value: Int = (ch1 - 48) * 10 + (ch2 - 48)
      if (value < 0 || value > 59)
        return required
      array(arrayIndex) = value
      array(0) = pos
      false
    }

    override def toString: String = {
      val converted: String = noOffsetText.replace("'", "''")
      s"Offset(${OffsetIdPrinterParser.PATTERNS(`type`)},'$converted')"
    }
  }

  /** Prints or parses a localized offset. */
  private[format] final class LocalizedOffsetPrinterParser(private val style: TextStyle)
      extends DateTimePrinterParser {

    def print(context: TTBPDateTimePrintContext, buf: StringBuilder): Boolean = {
      val offsetSecs: java.lang.Long = context.getValue(ChronoField.OFFSET_SECONDS)
      if (offsetSecs == null)
        return false
      buf.append("GMT")
      if (style eq TextStyle.FULL)
        return new OffsetIdPrinterParser("", "+HH:MM:ss").print(context, buf)
      val totalSecs: Int             = Math.toIntExact(offsetSecs)
      if (totalSecs != 0) {
        val absHours: Int   = Math.abs((totalSecs / 3600) % 100)
        val absMinutes: Int = Math.abs((totalSecs / 60) % 60)
        val absSeconds: Int = Math.abs(totalSecs % 60)
        buf.append(if (totalSecs < 0) "-" else "+").append(absHours)
        if (absMinutes > 0 || absSeconds > 0) {
          buf
            .append(":")
            .append((absMinutes / 10 + '0').toChar)
            .append((absMinutes % 10 + '0').toChar)
          if (absSeconds > 0)
            buf
              .append(":")
              .append((absSeconds / 10 + '0').toChar)
              .append((absSeconds % 10 + '0').toChar)
        }
      }
      true
    }

    def parse(context: TTBPDateTimeParseContext, text: CharSequence, position: Int): Int = {
      var _position = position

      if (!context.subSequenceEquals(text, _position, "GMT", 0, 3))
        return ~_position
      _position += 3
      if (style eq TextStyle.FULL)
        return new OffsetIdPrinterParser("", "+HH:MM:ss").parse(context, text, _position)
      val end: Int      = text.length
      if (_position == end)
        return context.setParsedField(ChronoField.OFFSET_SECONDS, 0, _position, _position)
      val sign: Char    = text.charAt(_position)
      if (sign != '+' && sign != '-')
        return context.setParsedField(ChronoField.OFFSET_SECONDS, 0, _position, _position)
      val negative: Int = if (sign == '-') -1 else 1
      if (_position == end)
        return ~_position
      _position += 1
      var ch: Char      = text.charAt(_position)
      if (ch < '0' || ch > '9')
        return ~_position
      _position += 1
      var hour: Int     = ch - 48
      if (_position != end) {
        ch = text.charAt(_position)
        if (ch >= '0' && ch <= '9') {
          hour = hour * 10 + (ch - 48)
          if (hour > 23)
            return ~_position
          _position += 1
        }
      }
      if (_position == end || text.charAt(_position) != ':') {
        val offset: Int = negative * 3600 * hour
        return context.setParsedField(ChronoField.OFFSET_SECONDS,
                                      offset.toLong,
                                      _position,
                                      _position
        )
      }
      _position += 1
      if (_position > end - 2)
        return ~_position
      ch = text.charAt(_position)
      if (ch < '0' || ch > '9')
        return ~_position
      _position += 1
      var min: Int      = ch - 48
      ch = text.charAt(_position)
      if (ch < '0' || ch > '9')
        return ~_position
      _position += 1
      min = min * 10 + (ch - 48)
      if (min > 59)
        return ~_position
      if (_position == end || text.charAt(_position) != ':') {
        val offset: Int = negative * (3600 * hour + 60 * min)
        return context.setParsedField(ChronoField.OFFSET_SECONDS,
                                      offset.toLong,
                                      _position,
                                      _position
        )
      }
      _position += 1
      if (_position > end - 2)
        return ~_position
      ch = text.charAt(_position)
      if (ch < '0' || ch > '9')
        return ~_position
      _position += 1
      var sec: Int      = ch - 48
      ch = text.charAt(_position)
      if (ch < '0' || ch > '9')
        return ~_position
      _position += 1
      sec = sec * 10 + (ch - 48)
      if (sec > 59)
        return ~_position
      val offset: Int   = negative * (3600 * hour + 60 * min + sec)
      context.setParsedField(ChronoField.OFFSET_SECONDS, offset.toLong, _position, _position)
    }
  }

  /** Prints or parses a zone ID. */
  private[format] object ZoneTextPrinterParser {

    /** The text style to output. */
    private lazy val LENGTH_COMPARATOR: Ordering[Map.Entry[String, String]] =
      new Ordering[Map.Entry[String, String]] {
        override def compare(
          str1: Map.Entry[String, String],
          str2: Map.Entry[String, String]
        ): Int = {
          var cmp: Int = str2.getKey.length - str1.getKey.length
          if (cmp == 0)
            cmp = str1.getKey.compareTo(str2.getKey)
          cmp
        }
      }
  }

  private[format] final class ZoneTextPrinterParser private[format] (
    private val textStyle: TextStyle
  ) extends DateTimePrinterParser {
    Objects.requireNonNull(textStyle, "textStyle")

    def print(context: TTBPDateTimePrintContext, buf: StringBuilder): Boolean = {
      val zone: ZoneId      = context.getValue(TemporalQueries.zoneId)
      if (zone == null)
        return false
      if (zone.normalized.isInstanceOf[ZoneOffset]) {
        buf.append(zone.getId)
        return true
      }
      val temporal          = context.getTemporal
      var daylight: Boolean = false
      if (temporal.isSupported(ChronoField.INSTANT_SECONDS)) {
        val instant: Instant = Instant.ofEpochSecond(temporal.getLong(ChronoField.INSTANT_SECONDS))
        daylight = zone.getRules.isDaylightSavings(instant)
      }
      val tz: TimeZone      = TimeZone.getTimeZone(zone.getId)
      val tzstyle: Int      = if (textStyle.asNormal eq TextStyle.FULL) TimeZone.LONG else TimeZone.SHORT
      val text: String      = tz.getDisplayName(daylight, tzstyle, context.getLocale)
      buf.append(text)
      true
    }

    def parse(context: TTBPDateTimeParseContext, text: CharSequence, position: Int): Int = {
      // TODO:
      //    1. Consider whether we should keep the Java-based implementation on the JVM
      //       and provide this alternative implementation only on other platforms?
      //    2. We need to write tests for this. I flipped LENGTH_COMPARATOR and no test broke.
      val ids        = new java.util.HashMap[String, String]()
      val idIterator = ZoneId.getAvailableZoneIds.iterator()
      while (idIterator.hasNext) {
        val id           = idIterator.next()
        ids.put(id, id)
        val tz: TimeZone = TimeZone.getTimeZone(id)
        val tzstyle: Int =
          if (textStyle.asNormal eq TextStyle.FULL) TimeZone.LONG else TimeZone.SHORT
        val textWinter   = tz.getDisplayName(false, tzstyle, context.getLocale)
        // The null checks are needed as scalajs-locales doesn't include time zone strings
        if (
          textWinter != null && id
            .startsWith("Etc/") || (!textWinter.startsWith("GMT+") && !textWinter
            .startsWith("GMT+"))
        )
          ids.put(textWinter, id)
        else if (textWinter == null)
          ids.put(tz.getDisplayName(false, tzstyle, context.getLocale), id)
        val textSummer   = tz.getDisplayName(true, tzstyle, context.getLocale)
        if (
          textSummer != null && id
            .startsWith("Etc/") || (!textSummer.startsWith("GMT+") && !textSummer
            .startsWith("GMT+"))
        )
          ids.put(textSummer, id)
        else if (textSummer == null)
          ids.put(tz.getDisplayName(true, tzstyle, context.getLocale), id)
      }
      val tmpIds     = new java.util.ArrayList(ids.entrySet)
      Collections.sort[Map.Entry[String, String]](tmpIds, ZoneTextPrinterParser.LENGTH_COMPARATOR)
      val i          = tmpIds.iterator
      while (i.hasNext()) {
        val v            = i.next()
        val name: String = v.getKey
        if (context.subSequenceEquals(text, position, name, 0, name.length)) {
          context.setParsed(ZoneId.of(v.getValue))
          return position + name.length
        }
      }
      ~position
    }

    override def toString: String = s"ZoneText($textStyle)"
  }

  /** Prints or parses a zone ID. */
  private[format] object ZoneIdPrinterParser {

    /** The cached tree to speed up parsing. */
    @volatile
    private var cachedSubstringTree: java.util.Map.Entry[Integer, SubstringTree] = null

    /**
     * Model a tree of substrings to make the parsing easier. Due to the nature of time-zone names,
     * it can be faster to parse based in unique substrings rather than just a character by
     * character match.
     *
     * For example, to parse America/Denver we can look at the first two character "Am". We then
     * notice that the shortest time-zone that starts with Am is America/Nome which is 12 characters
     * long. Checking the first 12 characters of America/Denver gives America/Denv which is a
     * substring of only 1 time-zone: America/Denver. Thus, with just 3 comparisons that match can
     * be found.
     *
     * This structure maps substrings to substrings of a longer length. Each node of the tree
     * contains a length and a map of valid substrings to sub-nodes. The parser gets the length from
     * the root node. It then extracts a substring of that length from the parseText. If the map
     * contains the substring, it is set as the possible time-zone and the sub-node for that
     * substring is retrieved. The process continues until the substring is no longer found, at
     * which point the matched text is checked against the real time-zones.
     *
     * @constructor
     *
     * @param length
     *   The length of the substring this node of the tree contains. Subtrees will have a longer
     *   length.
     */
    private final class SubstringTree(val length: Int) {

      /** Map of a substring to a set of substrings that contain the key. */
      private val substringMap: java.util.Map[CharSequence, SubstringTree] =
        new java.util.HashMap[CharSequence, SubstringTree]

      /** Map of a substring to a set of substrings that contain the key. */
      private val substringMapCI: java.util.Map[String, SubstringTree] =
        new java.util.HashMap[String, SubstringTree]

      def get(substring2: CharSequence, caseSensitive: Boolean): SubstringTree =
        if (caseSensitive) substringMap.get(substring2)
        else
          substringMapCI.get(CasePlatformHelper.toLocaleIndependentLowerCase(substring2.toString))

      /**
       * Values must be added from shortest to longest.
       *
       * @param newSubstring
       *   the substring to add, not null
       */
      @tailrec
      private[format] def add(newSubstring: String): Unit = {
        val idLen: Int = newSubstring.length
        if (idLen == length) {
          substringMap.put(newSubstring, null)
          substringMapCI.put(CasePlatformHelper.toLocaleIndependentLowerCase(newSubstring), null)
          ()
        } else if (idLen > length) {
          val substring: String         = newSubstring.substring(0, length)
          var parserTree: SubstringTree = substringMap.get(substring)
          if (parserTree == null) {
            parserTree = new SubstringTree(idLen)
            substringMap.put(substring, parserTree)
            substringMapCI.put(CasePlatformHelper.toLocaleIndependentLowerCase(substring),
                               parserTree
            )
          }
          parserTree.add(newSubstring)
        }
      }
    }

    /**
     * Builds an optimized parsing tree.
     *
     * @param availableIDs
     *   the available IDs, not null, not empty
     * @return
     *   the tree, not null
     */
    private def prepareParser(availableIDs: java.util.Set[String]): SubstringTree = {
      val ids: java.util.List[String] = new java.util.ArrayList[String](availableIDs)
      Collections.sort(ids, LENGTH_SORT)
      val tree: SubstringTree         = new SubstringTree(ids.get(0).length)
      val idsIterator                 = ids.iterator
      while (idsIterator.hasNext) {
        val id = idsIterator.next()
        tree.add(id)
      }
      tree
    }
  }

  private[format] final class ZoneIdPrinterParser private[format] (
    private val query:       TemporalQuery[ZoneId],
    private val description: String
  ) extends DateTimePrinterParser {

    def print(context: TTBPDateTimePrintContext, buf: StringBuilder): Boolean = {
      val zone: ZoneId = context.getValue(query)
      if (zone == null)
        false
      else {
        buf.append(zone.getId)
        true
      }
    }

    /**
     * This implementation looks for the longest matching string. For example, parsing Etc/GMT-2
     * will return Etc/GMC-2 rather than just Etc/GMC although both are valid.
     *
     * This implementation uses a tree to search for valid time-zone names in the parseText. The top
     * level node of the tree has a length equal to the length of the shortest time-zone as well as
     * the beginning characters of all other time-zones.
     */
    def parse(context: TTBPDateTimeParseContext, text: CharSequence, position: Int): Int = {
      val length: Int                                         = text.length
      if (position > length)
        throw new IndexOutOfBoundsException
      if (position == length)
        return ~position
      val nextChar: Char                                      = text.charAt(position)
      if (nextChar == '+' || nextChar == '-') {
        val newContext: TTBPDateTimeParseContext = context.copy
        val endPos: Int                          = OffsetIdPrinterParser.INSTANCE_ID.parse(newContext, text, position)
        if (endPos < 0)
          return endPos
        val offset: Int                          = newContext.getParsed(ChronoField.OFFSET_SECONDS).longValue.toInt
        context.setParsed(ZoneOffset.ofTotalSeconds(offset))
        return endPos
      } else if (length >= position + 2) {
        val nextNextChar: Char = text.charAt(position + 1)
        if (context.charEquals(nextChar, 'U') && context.charEquals(nextNextChar, 'T')) {
          if (length >= position + 3 && context.charEquals(text.charAt(position + 2), 'C'))
            return parsePrefixedOffset(context, text, position, position + 3)
          return parsePrefixedOffset(context, text, position, position + 2)
        } else if (
          context.charEquals(nextChar, 'G') && length >= position + 3 && context
            .charEquals(nextNextChar, 'M') && context.charEquals(text.charAt(position + 2), 'T')
        )
          return parsePrefixedOffset(context, text, position, position + 3)
      }
      val regionIds: java.util.Set[String]                    = ZoneRulesProvider.getAvailableZoneIds
      val regionIdsSize: Int                                  = regionIds.size
      var cached: java.util.Map.Entry[Integer, SubstringTree] =
        ZoneIdPrinterParser.cachedSubstringTree
      if (cached == null || (cached.getKey != regionIdsSize))
        this synchronized {
          cached = ZoneIdPrinterParser.cachedSubstringTree
          if (cached == null || (cached.getKey != regionIdsSize))
            ZoneIdPrinterParser.cachedSubstringTree = {
              cached = new java.util.AbstractMap.SimpleImmutableEntry[Integer, SubstringTree](
                regionIdsSize,
                ZoneIdPrinterParser.prepareParser(regionIds)
              )
              cached
            }
        }
      var tree: SubstringTree                                 = cached.getValue
      var parsedZoneId: String                                = null
      var lastZoneId: String                                  = null
      var break                                               = false
      while (!break && tree != null) {
        val nodeLength: Int = tree.length
        if (position + nodeLength > length)
          break = true
        else {
          lastZoneId = parsedZoneId
          parsedZoneId = text.subSequence(position, position + nodeLength).toString
          tree = tree.get(parsedZoneId, context.isCaseSensitive)
        }
      }
      var zone: ZoneId                                        = convertToZone(regionIds, parsedZoneId, context.isCaseSensitive)
      if (zone == null) {
        zone = convertToZone(regionIds, lastZoneId, context.isCaseSensitive)
        if (zone == null) {
          if (context.charEquals(nextChar, 'Z')) {
            context.setParsed(ZoneOffset.UTC)
            return position + 1
          }
          return ~position
        }
        parsedZoneId = lastZoneId
      }
      context.setParsed(zone)
      position + parsedZoneId.length
    }

    private def convertToZone(
      regionIds:     java.util.Set[String],
      parsedZoneId:  String,
      caseSensitive: Boolean
    ): ZoneId =
      if (parsedZoneId == null)
        null
      else if (caseSensitive)
        if (regionIds.contains(parsedZoneId)) ZoneId.of(parsedZoneId) else null
      else {
        val regionIdsIterator = regionIds.iterator
        while (regionIdsIterator.hasNext) {
          val regionId = regionIdsIterator.next()
          if (regionId.equalsIgnoreCase(parsedZoneId))
            return ZoneId.of(regionId)
        }
        null
      }

    private def parsePrefixedOffset(
      context:   TTBPDateTimeParseContext,
      text:      CharSequence,
      prefixPos: Int,
      position:  Int
    ): Int = {
      val prefix: String                       = text.subSequence(prefixPos, position).toString.toUpperCase
      val newContext: TTBPDateTimeParseContext = context.copy
      if (position < text.length && context.charEquals(text.charAt(position), 'Z')) {
        context.setParsed(ZoneId.ofOffset(prefix, ZoneOffset.UTC))
        return position
      }
      val endPos: Int                          = OffsetIdPrinterParser.INSTANCE_ID.parse(newContext, text, position)
      if (endPos < 0) {
        context.setParsed(ZoneId.ofOffset(prefix, ZoneOffset.UTC))
        return position
      }
      val offsetSecs: Int                      = newContext.getParsed(ChronoField.OFFSET_SECONDS).longValue.toInt
      val offset: ZoneOffset                   = ZoneOffset.ofTotalSeconds(offsetSecs)
      context.setParsed(ZoneId.ofOffset(prefix, offset))
      endPos
    }

    override def toString: String = description
  }

  /**
   * Prints or parses a chronology.
   *
   * @param textStyle
   *   The text style to output, null means the ID.
   */
  private[format] final class ChronoPrinterParser private[format] (private val textStyle: TextStyle)
      extends DateTimePrinterParser {

    def print(context: TTBPDateTimePrintContext, buf: StringBuilder): Boolean = {
      val chrono: Chronology = context.getValue(TemporalQueries.chronology)
      if (chrono == null)
        return false
      if (textStyle == null)
        buf.append(chrono.getId)
      else {
        val bundle: ResourceBundle = ResourceBundle.getBundle(
          "org.threeten.bp.format.ChronologyText",
          context.getLocale,
          classOf[DateTimeFormatterBuilder].getClassLoader
        )
        try {
          val text: String = bundle.getString(chrono.getId)
          buf.append(text)
        } catch {
          case _: MissingResourceException => buf.append(chrono.getId)
        }
      }
      true
    }

    def parse(context: TTBPDateTimeParseContext, text: CharSequence, position: Int): Int = {
      if (position < 0 || position > text.length)
        throw new IndexOutOfBoundsException
      val chronos: java.util.Iterator[Chronology] = Chronology.getAvailableChronologies.iterator
      var bestMatch: Chronology                   = null
      var matchLen: Int                           = -1
      while (chronos.hasNext) {
        val chrono     = chronos.next()
        val id: String = chrono.getId
        val idLen: Int = id.length
        if (idLen > matchLen && context.subSequenceEquals(text, position, id, 0, idLen)) {
          bestMatch = chrono
          matchLen = idLen
        }
      }
      if (bestMatch == null)
        ~position
      else {
        context.setParsed(bestMatch)
        position + matchLen
      }
    }
  }

  /**
   * Prints or parses a localized pattern.
   *
   * @constructor
   *
   * @param dateStyle
   *   the date style to use, may be null
   * @param timeStyle
   *   the time style to use, may be null
   */
  private[format] final class LocalizedPrinterParser private[format] (
    private val dateStyle: FormatStyle,
    private val timeStyle: FormatStyle
  ) extends DateTimePrinterParser {

    def print(context: TTBPDateTimePrintContext, buf: StringBuilder): Boolean = {
      val chrono = Chronology.from(context.getTemporal)
      formatter(context.getLocale, chrono).toPrinterParser(false).print(context, buf)
    }

    def parse(context: TTBPDateTimeParseContext, text: CharSequence, position: Int): Int = {
      val chrono = context.getEffectiveChronology
      formatter(context.getLocale, chrono).toPrinterParser(false).parse(context, text, position)
    }

    /**
     * Gets the formatter to use.
     *
     * @param locale
     *   the locale to use, not null
     * @return
     *   the formatter, not null
     * @throws IllegalArgumentException
     *   if the formatter cannot be found
     */
    private def formatter(locale: Locale, chrono: Chronology): DateTimeFormatter =
      DateTimeFormatStyleProvider.getInstance.getFormatter(dateStyle, timeStyle, chrono, locale)

    override def toString: String =
      s"Localized(${if (dateStyle != null) dateStyle else ""},${if (timeStyle != null) timeStyle
      else ""})"
  }

  /** Prints or parses a localized pattern. */
  private[format] final class WeekFieldsPrinterParser(
    private val letter: Char,
    private val count:  Int
  ) extends DateTimePrinterParser {

    def print(context: TTBPDateTimePrintContext, buf: StringBuilder): Boolean =
      evaluate(WeekFields.of(context.getLocale)).print(context, buf)

    def parse(context: TTBPDateTimeParseContext, text: CharSequence, position: Int): Int =
      evaluate(WeekFields.of(context.getLocale)).parse(context, text, position)

    private def evaluate(weekFields: WeekFields): DateTimePrinterParser =
      letter match {
        case 'e' =>
          new NumberPrinterParser(weekFields.dayOfWeek, count, 2, SignStyle.NOT_NEGATIVE)
        case 'c' =>
          new NumberPrinterParser(weekFields.dayOfWeek, count, 2, SignStyle.NOT_NEGATIVE)
        case 'w' =>
          new NumberPrinterParser(weekFields.weekOfWeekBasedYear, count, 2, SignStyle.NOT_NEGATIVE)
        case 'W' =>
          new NumberPrinterParser(weekFields.weekOfMonth, 1, 2, SignStyle.NOT_NEGATIVE)
        case 'Y' =>
          if (count == 2)
            new ReducedPrinterParser(weekFields.weekBasedYear,
                                     2,
                                     2,
                                     0,
                                     ReducedPrinterParser.BASE_DATE
            )
          else
            new NumberPrinterParser(weekFields.weekBasedYear,
                                    count,
                                    19,
                                    if (count < 4) SignStyle.NORMAL else SignStyle.EXCEEDS_PAD,
                                    -1
            )
      }

    override def toString: String = {
      val sb: StringBuilder = new StringBuilder(30)
      sb.append("Localized(")
      if (letter == 'Y')
        if (count == 1)
          sb.append("WeekBasedYear")
        else if (count == 2)
          sb.append("ReducedValue(WeekBasedYear,2,2,2000-01-01)")
        else
          sb.append("WeekBasedYear,")
            .append(count)
            .append(",")
            .append(19)
            .append(",")
            .append(if (count < 4) SignStyle.NORMAL else SignStyle.EXCEEDS_PAD)
      else {
        if (letter == 'c' || letter == 'e')
          sb.append("DayOfWeek")
        else if (letter == 'w')
          sb.append("WeekOfWeekBasedYear")
        else if (letter == 'W')
          sb.append("WeekOfMonth")
        sb.append(",")
        sb.append(count)
      }
      sb.append(")")
      sb.toString
    }
  }

}
