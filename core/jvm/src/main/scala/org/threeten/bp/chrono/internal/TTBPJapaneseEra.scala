package org.threeten.bp.chrono.internal

import org.threeten.bp.chrono.JapaneseEra
import org.threeten.bp.LocalDate
import org.threeten.bp.DateTimeException
import java.util.Arrays

object TTBPJapaneseEra {

  /**
   * Registers an additional instance of {@code JapaneseEra}. <p> A new Japanese era can begin at
   * any time. This method allows one new era to be registered without the need for a new library
   * version. If needed, callers should assign the result to a static variable accessible across the
   * application. This must be done once, in early startup code. <p> NOTE: This method does not
   * exist in Java SE 8.
   *
   * @param since
   *   the date representing the first date of the era, validated not null
   * @param name
   *   the name
   * @return
   *   the { @code JapaneseEra} singleton, not null
   * @throws DateTimeException
   *   if an additional era has already been registered
   */
  def registerEra(since: LocalDate, name: String): JapaneseEra = {
    val known    = JapaneseEra.KNOWN_ERAS.get
    if (known.length > 5)
      throw new DateTimeException("Only one additional Japanese era can be added")
    require(since != null)
    require(name != null)
    if (!since.isAfter(JapaneseEra.REIWA.since))
      throw new DateTimeException(
        "Invalid since date for additional Japanese era, must be after Reiwa"
      )
    val era      = new JapaneseEra(JapaneseEra.ADDITIONAL_VALUE, since, name)
    val newArray = Arrays.copyOf(known, 6)
    newArray(5) = era
    if (!JapaneseEra.KNOWN_ERAS.compareAndSet(known, newArray))
      throw new DateTimeException("Only one additional Japanese era can be added")
    era
  }

}
