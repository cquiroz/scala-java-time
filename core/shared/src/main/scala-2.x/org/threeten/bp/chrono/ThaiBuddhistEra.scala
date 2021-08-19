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
package org.threeten.bp.chrono

import org.threeten.bp.DateTimeException

object ThaiBuddhistEra {

  /**
   * The singleton instance for the era before the current one, 'Before Buddhist Era', which has the
   * value 0.
   */
  lazy val BEFORE_BE = new ThaiBuddhistEra("BEFORE_BE", 0)

  /** The singleton instance for the current era, 'Buddhist Era', which has the value 1. */
  lazy val BE = new ThaiBuddhistEra("BE", 1)

  lazy val values: Array[ThaiBuddhistEra] = Array(BEFORE_BE, BE)

  /**
   * Obtains an instance of {@code ThaiBuddhistEra} from a value.
   *
   * The current era (from ISO year -543 onwards) has the value 1 The previous era has the value 0.
   *
   * @param thaiBuddhistEra
   *   the era to represent, from 0 to 1
   * @return
   *   the BuddhistEra singleton, never null
   * @throws DateTimeException
   *   if the era is invalid
   */
  def of(thaiBuddhistEra: Int): ThaiBuddhistEra =
    thaiBuddhistEra match {
      case 0 => BEFORE_BE
      case 1 => BE
      case _ => throw new DateTimeException("Era is not valid for ThaiBuddhistEra")
    }

}

/**
 * An era in the Thai Buddhist calendar system.
 *
 * The Thai Buddhist calendar system has two eras.
 *
 * <b>Do not use ordinal() to obtain the numeric representation of a ThaiBuddhistEra instance. Use
 * getValue() instead.</b>
 *
 * <h3>Specification for implementors</h3> This is an immutable and thread-safe enum.
 */
final class ThaiBuddhistEra(name: String, ordinal: Int)
    extends Enum[ThaiBuddhistEra](name, ordinal)
    with Era {

  /**
   * Gets the era numeric value.
   *
   * The current era (from ISO year -543 onwards) has the value 1 The previous era has the value 0.
   *
   * @return
   *   the era value, from 0 (BEFORE_BE) to 1 (BE)
   */
  def getValue: Int = ordinal

}
