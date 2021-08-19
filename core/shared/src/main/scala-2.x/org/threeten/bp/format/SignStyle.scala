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

/**
 * Enumeration of ways to handle the positive/negative sign.
 *
 * The formatting engine allows the positive and negative signs of numbers to be controlled using
 * this enum. See {@link DateTimeFormatterBuilder} for usage.
 *
 * <h3>Specification for implementors</h3> This is an immutable and thread-safe enum.
 */
object SignStyle {

  /**
   * Style to output the sign only if the value is negative.
   *
   * In strict parsing, the negative sign will be accepted and the positive sign rejected. In
   * lenient parsing, any sign will be accepted.
   */
  lazy val NORMAL = new SignStyle("NORMAL", 0)

  /**
   * Style to always output the sign, where zero will output '+'.
   *
   * In strict parsing, the absence of a sign will be rejected. In lenient parsing, any sign will be
   * accepted, with the absence of a sign treated as a positive number.
   */
  lazy val ALWAYS = new SignStyle("ALWAYS", 1)

  /**
   * Style to never output sign, only outputting the absolute value.
   *
   * In strict parsing, any sign will be rejected. In lenient parsing, any sign will be accepted
   * unless the width is fixed.
   */
  lazy val NEVER = new SignStyle("NEVER", 2)

  /**
   * Style to block negative values, throwing an exception on printing.
   *
   * In strict parsing, any sign will be rejected. In lenient parsing, any sign will be accepted
   * unless the width is fixed.
   */
  lazy val NOT_NEGATIVE = new SignStyle("NOT_NEGATIVE", 3)

  /**
   * Style to always output the sign if the value exceeds the pad width. A negative value will
   * always output the '-' sign.
   *
   * In strict parsing, the sign will be rejected unless the pad width is exceeded. In lenient
   * parsing, any sign will be accepted, with the absence of a sign treated as a positive number.
   */
  lazy val EXCEEDS_PAD = new SignStyle("EXCEEDS_PAD", 4)
}

final class SignStyle(name: String, ordinal: Int) extends Enum[SignStyle](name, ordinal) {

  /**
   * Parse helper.
   *
   * @param positive
   *   true if positive sign parsed, false for negative sign
   * @param strict
   *   true if strict, false if lenient
   * @param fixedWidth
   *   true if fixed width, false if not
   * @return
   *   true if valid
   */
  private[format] def parse(positive: Boolean, strict: Boolean, fixedWidth: Boolean): Boolean =
    ordinal match {
      case 0     =>
        !positive || !strict
      case 1 | 4 =>
        true
      case _     =>
        !strict && !fixedWidth
    }
}
