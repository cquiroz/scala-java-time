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
package org.threeten.bp.temporal

import org.scalatest.funsuite.AnyFunSuite
import org.threeten.bp.{ AssertionsHelper, LocalDate }

/** Test. */
object TestJulianFields {
  private val JAN01_1970: LocalDate = LocalDate.of(1970, 1, 1)
  private val DEC31_1969: LocalDate = LocalDate.of(1969, 12, 31)
  private val NOV12_1945: LocalDate = LocalDate.of(1945, 11, 12)
  private val JAN01_0001: LocalDate = LocalDate.of(1, 1, 1)
}

class TestJulianFields extends AnyFunSuite with AssertionsHelper {
  def data_samples: List[List[Any]] =
    List(
      List(ChronoField.EPOCH_DAY, TestJulianFields.JAN01_1970, 0L),
      List(JulianFields.JULIAN_DAY, TestJulianFields.JAN01_1970, 2400001L + 40587L),
      List(JulianFields.MODIFIED_JULIAN_DAY, TestJulianFields.JAN01_1970, 40587L),
      List(JulianFields.RATA_DIE, TestJulianFields.JAN01_1970, 710347L + (40587L - 31771L)),
      List(ChronoField.EPOCH_DAY, TestJulianFields.DEC31_1969, -1L),
      List(JulianFields.JULIAN_DAY, TestJulianFields.DEC31_1969, 2400001L + 40586L),
      List(JulianFields.MODIFIED_JULIAN_DAY, TestJulianFields.DEC31_1969, 40586L),
      List(JulianFields.RATA_DIE, TestJulianFields.DEC31_1969, 710347L + (40586L - 31771L)),
      List(ChronoField.EPOCH_DAY, TestJulianFields.NOV12_1945, -24 * 365 - 6 - 31 - 30 + 11),
      List(JulianFields.JULIAN_DAY, TestJulianFields.NOV12_1945, 2431772L),
      List(JulianFields.MODIFIED_JULIAN_DAY, TestJulianFields.NOV12_1945, 31771L),
      List(JulianFields.RATA_DIE, TestJulianFields.NOV12_1945, 710347L),
      List(ChronoField.EPOCH_DAY,
           TestJulianFields.JAN01_0001,
           -24 * 365 - 6 - 31 - 30 + 11 - 710346L
      ),
      List(JulianFields.JULIAN_DAY, TestJulianFields.JAN01_0001, 2431772L - 710346L),
      List(JulianFields.MODIFIED_JULIAN_DAY, TestJulianFields.JAN01_0001, 31771L - 710346L),
      List(JulianFields.RATA_DIE, TestJulianFields.JAN01_0001, 1L)
    )

  test("samples_get") {
    data_samples.foreach {
      case (field: TemporalField) :: (date: LocalDate) :: (expected: Number) :: Nil =>
        assertEquals(date.getLong(field), expected)
      case _                                                                        =>
        fail()
    }
  }

  test("samples_set") {
    data_samples.foreach {
      case (field: TemporalField) :: (date: LocalDate) :: (value: Number) :: Nil =>
        assertEquals(field.adjustInto(LocalDate.MAX, value.longValue), date)
        assertEquals(field.adjustInto(LocalDate.MIN, value.longValue), date)
        assertEquals(field.adjustInto(TestJulianFields.JAN01_1970, value.longValue), date)
        assertEquals(field.adjustInto(TestJulianFields.DEC31_1969, value.longValue), date)
        assertEquals(field.adjustInto(TestJulianFields.NOV12_1945, value.longValue), date)
      case _                                                                     =>
        fail()
    }
  }

  test("toString") {
    assertEquals(JulianFields.JULIAN_DAY.toString, "JulianDay")
    assertEquals(JulianFields.MODIFIED_JULIAN_DAY.toString, "ModifiedJulianDay")
    assertEquals(JulianFields.RATA_DIE.toString, "RataDie")
  }
}
