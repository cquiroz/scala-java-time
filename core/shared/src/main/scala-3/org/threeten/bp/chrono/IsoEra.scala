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

object IsoEra {

  /**
   * Obtains an instance of {@code IsoEra} from an {@code int} value.
   *
   * {@code IsoEra} is an enum representing the ISO eras of BCE/CE. This factory allows the enum to
   * be obtained from the {@code int} value.
   *
   * @param era
   *   the BCE/CE value to represent, from 0 (BCE) to 1 (CE)
   * @return
   *   the era singleton, not null
   * @throws DateTimeException
   *   if the value is invalid
   */
  def of(era: Int): IsoEra =
    era match {
      case 0 => BCE
      case 1 => CE
      case _ => throw new DateTimeException(s"Invalid era: $era")
    }
}

/**
 * An era in the ISO calendar system.
 *
 * The ISO-8601 standard does not define eras. A definition has therefore been created with two eras
 * \- 'Current era' (CE) for years from 0001-01-01 (ISO) and 'Before current era' (BCE) for years
 * before that.
 *
 * <b>Do not use {@code ordinal()} to obtain the numeric representation of {@code IsoEra} . Use
 * {@code getValue()} instead.</b>
 *
 * <h3>Specification for implementors</h3> This is an immutable and thread-safe enum.
 */
enum IsoEra(name: String, ordinal: Int) extends java.lang.Enum[IsoEra] with Era {

  /**
   * The singleton instance for the era BCE, 'Before Current Era'. The 'ISO' part of the name
   * emphasizes that this differs from the BCE era in the Gregorian calendar system. This has the
   * numeric value of {@code 0} .
   */
  case BCE extends IsoEra("BCE", 0)

  /**
   * The singleton instance for the era CE, 'Current Era'. The 'ISO' part of the name emphasizes
   * that this differs from the CE era in the Gregorian calendar system. This has the numeric value
   * of {@code 1} .
   */
  case CE extends IsoEra("CE", 1)

  /**
   * Gets the numeric era {@code int} value.
   *
   * The era BCE has the value 0, while the era CE has the value 1.
   *
   * @return
   *   the era value, from 0 (BCE) to 1 (CE)
   */
  def getValue: Int = ordinal
}
