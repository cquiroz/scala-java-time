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

import java.util.Objects

import org.threeten.bp.temporal.ChronoField._
import org.threeten.bp.DateTimeException
import org.threeten.bp.Instant
import org.threeten.bp.LocalDate
import org.threeten.bp.LocalTime
import org.threeten.bp.Period
import org.threeten.bp.ZoneId
import org.threeten.bp.ZoneOffset
import org.threeten.bp.chrono.ChronoLocalDate
import org.threeten.bp.chrono.ChronoLocalDateTime
import org.threeten.bp.chrono.ChronoZonedDateTime
import org.threeten.bp.chrono.Chronology
import org.threeten.bp.chrono.IsoChronology
import org.threeten.bp.temporal.ChronoField
import org.threeten.bp.temporal.TemporalAccessor
import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.temporal.TemporalQueries
import org.threeten.bp.temporal.TemporalQuery

/**
 * Builder that can holds date and time fields and related date and time objects.
 *
 * The builder is used to hold onto different elements of date and time. It is designed as two
 * separate maps: <ul> <li>from {@link TemporalField} to {@code long} value, where the value may be
 * outside the valid range for the field <li>from {@code Class} to {@link TemporalAccessor}, holding
 * larger scale objects like {@code LocalDateTime}. </ul><p>
 *
 * <h3>Specification for implementors</h3> This class is mutable and not thread-safe. It should only
 * be used from a single thread.
 *
 * Creates an empty instance of the builder.
 */
final class DateTimeBuilder() extends TemporalAccessor with Cloneable {

  /**
   * The map of other fields.
   */
  private[format] val fieldValues: java.util.Map[TemporalField, java.lang.Long] =
    new java.util.HashMap[TemporalField, java.lang.Long]

  /**
   * The chronology.
   */
  private[format] var chrono: Chronology = null

  /**
   * The zone.
   */
  private[format] var zone: ZoneId = null

  /**
   * The date.
   */
  private[format] var date: ChronoLocalDate = null

  /**
   * The time.
   */
  private[format] var time: LocalTime = null

  /**
   * The leap second flag.
   */
  private[format] var leapSecond: Boolean = false

  /**
   * The excess days.
   */
  private[format] var excessDays: Period = null

  /**
   * Creates a new instance of the builder with a single field-value.
   *
   * This is equivalent to using {@link #addFieldValue(TemporalField, long)} on an empty builder.
   *
   * @param field
   *   the field to add, not null
   * @param value
   *   the value to add, not null
   */
  def this(field: TemporalField, value: Long) = {
    this()
    addFieldValue(field, value)
  }

  private def getFieldValue0(field: TemporalField): java.lang.Long =
    fieldValues.get(field)

  /**
   * Adds a field-value pair to the builder.
   *
   * This adds a field to the builder. If the field is not already present, then the field-value
   * pair is added to the map. If the field is already present and it has the same value as that
   * specified, no action occurs. If the field is already present and it has a different value to
   * that specified, then an exception is thrown.
   *
   * @param field
   *   the field to add, not null
   * @param value
   *   the value to add, not null
   * @return
   *   {@code this}, for method chaining
   * @throws DateTimeException
   *   if the field is already present with a different value
   */
  private[format] def addFieldValue(field: TemporalField, value: Long): DateTimeBuilder = {
    Objects.requireNonNull(field, "field")
    val old: java.lang.Long = getFieldValue0(field)
    if (old != null && old.longValue != value)
      throw new DateTimeException(s"Conflict found: $field $old differs from $field $value: $this")
    else
      putFieldValue0(field, value)
  }

  private def putFieldValue0(field: TemporalField, value: Long): DateTimeBuilder = {
    fieldValues.put(field, value)
    this
  }

  private[format] def addObject(date: ChronoLocalDate): Unit = this.date = date

  private[format] def addObject(time: LocalTime): Unit = this.time = time

  /**
   * Resolves the builder, evaluating the date and time.
   *
   * This examines the contents of the builder and resolves it to produce the best available date
   * and time, throwing an exception if a problem occurs. Calling this method changes the state of
   * the builder.
   *
   * @param resolverStyle
   *   how to resolve
   * @return
   *   {@code this}, for method chaining
   */
  def resolve(
    resolverStyle:  ResolverStyle,
    resolverFields: java.util.Set[TemporalField]
  ): DateTimeBuilder = {
    if (resolverFields != null)
      fieldValues.keySet.retainAll(resolverFields)
    mergeInstantFields()
    mergeDate(resolverStyle)
    mergeTime(resolverStyle)
    if (resolveFields(resolverStyle)) {
      mergeInstantFields()
      mergeDate(resolverStyle)
      mergeTime(resolverStyle)
    }
    resolveTimeInferZeroes(resolverStyle)
    crossCheck()
    if (excessDays != null && !excessDays.isZero && date != null && time != null) {
      date = date.plus(excessDays)
      excessDays = Period.ZERO
    }
    resolveFractional()
    resolveInstant()
    this
  }

  private def resolveFields(resolverStyle: ResolverStyle): Boolean = {
    var changes: Int = 0
    val entries      = fieldValues.entrySet.iterator
    var break        = false
    while (!break && entries.hasNext) {
      val entry                            = entries.next()
      val targetField: TemporalField       = entry.getKey
      var resolvedObject: TemporalAccessor = targetField.resolve(fieldValues, this, resolverStyle)
      resolvedObject match {
        case czdt: ChronoZonedDateTime[_] =>
          if (zone == null)
            zone = czdt.getZone
          else if (!(zone == czdt.getZone))
            throw new DateTimeException(
              s"ChronoZonedDateTime must use the effective parsed zone: $zone"
            )
          resolvedObject = czdt.toLocalDateTime
        case cld: ChronoLocalDate         =>
          resolveMakeChanges(targetField, cld)
          changes += 1
          break = true
        case lt: LocalTime                =>
          resolveMakeChanges(targetField, lt)
          changes += 1
          break = true
        case cldt: ChronoLocalDateTime[_] =>
          resolveMakeChanges(targetField, cldt.toLocalDate)
          resolveMakeChanges(targetField, cldt.toLocalTime)
          changes += 1
          break = true
        case null                         =>
          if (!fieldValues.containsKey(targetField)) {
            changes += 1
            break = true
          }
        case _                            =>
          throw new DateTimeException(s"Unknown type: ${resolvedObject.getClass.getName}")
      }
    }
    if (changes == 100)
      throw new DateTimeException("Badly written field")
    changes > 0
  }

  private def resolveMakeChanges(targetField: TemporalField, date: ChronoLocalDate): Unit = {
    if (chrono != date.getChronology)
      throw new DateTimeException(
        s"ChronoLocalDate must use the effective parsed chronology: $chrono"
      )
    val epochDay: Long      = date.toEpochDay
    val old: java.lang.Long = fieldValues.put(ChronoField.EPOCH_DAY, epochDay)
    if (old != null && old.longValue != epochDay)
      throw new DateTimeException(
        s"Conflict found: ${LocalDate.ofEpochDay(old)} differs from ${LocalDate.ofEpochDay(epochDay)} while resolving  $targetField"
      )
  }

  private def resolveMakeChanges(targetField: TemporalField, time: LocalTime): Unit = {
    val nanOfDay: Long      = time.toNanoOfDay
    val old: java.lang.Long = fieldValues.put(ChronoField.NANO_OF_DAY, nanOfDay)
    if (old != null && old.longValue != nanOfDay)
      throw new DateTimeException(
        s"Conflict found: ${LocalTime.ofNanoOfDay(old)} differs from $time while resolving  $targetField"
      )
  }

  private def mergeDate(resolverStyle: ResolverStyle): Unit =
    if (chrono.isInstanceOf[IsoChronology])
      checkDate(IsoChronology.INSTANCE.resolveDate(fieldValues, resolverStyle))
    else if (fieldValues.containsKey(EPOCH_DAY))
      checkDate(LocalDate.ofEpochDay(fieldValues.remove(EPOCH_DAY)))

  private def checkDate(date: LocalDate): Unit =
    if (date != null) {
      addObject(date)
      val fields = fieldValues.keySet.iterator
      var break  = false
      while (!break && fields.hasNext) {
        val field = fields.next()
        if (field.isInstanceOf[ChronoField])
          if (field.isDateBased) {
            var val1: Long = 0L
            try val1 = date.getLong(field)
            catch {
              case _: DateTimeException =>
                break = true
            }
            if (!break) {
              val val2: Long = fieldValues.get(field)
              if (val1 != val2)
                throw new DateTimeException(
                  s"Conflict found: Field $field $val1 differs from $field $val2 derived from $date"
                )
            }
          }
      }
    }

  private def mergeTime(resolverStyle: ResolverStyle): Unit = {
    if (fieldValues.containsKey(CLOCK_HOUR_OF_DAY)) {
      val ch: Long = fieldValues.remove(CLOCK_HOUR_OF_DAY)
      if (resolverStyle ne ResolverStyle.LENIENT)
        if ((resolverStyle eq ResolverStyle.SMART) && ch == 0) {}
        else
          CLOCK_HOUR_OF_DAY.checkValidValue(ch)
      addFieldValue(HOUR_OF_DAY, if (ch == 24) 0 else ch)
    }
    if (fieldValues.containsKey(CLOCK_HOUR_OF_AMPM)) {
      val ch: Long = fieldValues.remove(CLOCK_HOUR_OF_AMPM)
      if (resolverStyle ne ResolverStyle.LENIENT)
        if ((resolverStyle eq ResolverStyle.SMART) && ch == 0) {}
        else
          CLOCK_HOUR_OF_AMPM.checkValidValue(ch)
      addFieldValue(HOUR_OF_AMPM, if (ch == 12) 0 else ch)
    }
    if (resolverStyle ne ResolverStyle.LENIENT) {
      if (fieldValues.containsKey(AMPM_OF_DAY))
        AMPM_OF_DAY.checkValidValue(fieldValues.get(AMPM_OF_DAY))
      if (fieldValues.containsKey(HOUR_OF_AMPM))
        HOUR_OF_AMPM.checkValidValue(fieldValues.get(HOUR_OF_AMPM))
    }
    if (fieldValues.containsKey(AMPM_OF_DAY) && fieldValues.containsKey(HOUR_OF_AMPM)) {
      val ap: Long  = fieldValues.remove(AMPM_OF_DAY)
      val hap: Long = fieldValues.remove(HOUR_OF_AMPM)
      addFieldValue(HOUR_OF_DAY, ap * 12 + hap)
    }
    if (fieldValues.containsKey(NANO_OF_DAY)) {
      val nod: Long = fieldValues.remove(NANO_OF_DAY)
      if (resolverStyle ne ResolverStyle.LENIENT)
        NANO_OF_DAY.checkValidValue(nod)
      addFieldValue(SECOND_OF_DAY, nod / 1000000000L)
      addFieldValue(NANO_OF_SECOND, nod % 1000000000L)
    }
    if (fieldValues.containsKey(MICRO_OF_DAY)) {
      val cod: Long = fieldValues.remove(MICRO_OF_DAY)
      if (resolverStyle ne ResolverStyle.LENIENT)
        MICRO_OF_DAY.checkValidValue(cod)
      addFieldValue(SECOND_OF_DAY, cod / 1000000L)
      addFieldValue(MICRO_OF_SECOND, cod % 1000000L)
    }
    if (fieldValues.containsKey(MILLI_OF_DAY)) {
      val lod: Long = fieldValues.remove(MILLI_OF_DAY)
      if (resolverStyle ne ResolverStyle.LENIENT)
        MILLI_OF_DAY.checkValidValue(lod)
      addFieldValue(SECOND_OF_DAY, lod / 1000)
      addFieldValue(MILLI_OF_SECOND, lod % 1000)
    }
    if (fieldValues.containsKey(SECOND_OF_DAY)) {
      val sod: Long = fieldValues.remove(SECOND_OF_DAY)
      if (resolverStyle ne ResolverStyle.LENIENT)
        SECOND_OF_DAY.checkValidValue(sod)
      addFieldValue(HOUR_OF_DAY, sod / 3600)
      addFieldValue(MINUTE_OF_HOUR, (sod / 60) % 60)
      addFieldValue(SECOND_OF_MINUTE, sod      % 60)
    }
    if (fieldValues.containsKey(MINUTE_OF_DAY)) {
      val mod: Long = fieldValues.remove(MINUTE_OF_DAY)
      if (resolverStyle ne ResolverStyle.LENIENT)
        MINUTE_OF_DAY.checkValidValue(mod)
      addFieldValue(HOUR_OF_DAY, mod / 60)
      addFieldValue(MINUTE_OF_HOUR, mod % 60)
    }
    if (resolverStyle ne ResolverStyle.LENIENT) {
      if (fieldValues.containsKey(MILLI_OF_SECOND))
        MILLI_OF_SECOND.checkValidValue(fieldValues.get(MILLI_OF_SECOND))
      if (fieldValues.containsKey(MICRO_OF_SECOND))
        MICRO_OF_SECOND.checkValidValue(fieldValues.get(MICRO_OF_SECOND))
    }
    if (fieldValues.containsKey(MILLI_OF_SECOND) && fieldValues.containsKey(MICRO_OF_SECOND)) {
      val los: Long = fieldValues.remove(MILLI_OF_SECOND)
      val cos: Long = fieldValues.get(MICRO_OF_SECOND)
      addFieldValue(MICRO_OF_SECOND, los * 1000 + (cos % 1000))
    }
    if (fieldValues.containsKey(MICRO_OF_SECOND) && fieldValues.containsKey(NANO_OF_SECOND)) {
      val nos: Long = fieldValues.get(NANO_OF_SECOND)
      addFieldValue(MICRO_OF_SECOND, nos / 1000)
      fieldValues.remove(MICRO_OF_SECOND)
    }
    if (fieldValues.containsKey(MILLI_OF_SECOND) && fieldValues.containsKey(NANO_OF_SECOND)) {
      val nos: Long = fieldValues.get(NANO_OF_SECOND)
      addFieldValue(MILLI_OF_SECOND, nos / 1000000)
      fieldValues.remove(MILLI_OF_SECOND)
    }
    if (fieldValues.containsKey(MICRO_OF_SECOND)) {
      val cos: Long = fieldValues.remove(MICRO_OF_SECOND)
      addFieldValue(NANO_OF_SECOND, cos * 1000L)
      ()
    } else if (fieldValues.containsKey(MILLI_OF_SECOND)) {
      val los: Long = fieldValues.remove(MILLI_OF_SECOND)
      addFieldValue(NANO_OF_SECOND, los * 1000000L)
      ()
    }
  }

  private def resolveTimeInferZeroes(resolverStyle: ResolverStyle): Unit = {
    var hod: java.lang.Long = fieldValues.get(HOUR_OF_DAY)
    val moh: java.lang.Long = fieldValues.get(MINUTE_OF_HOUR)
    val som: java.lang.Long = fieldValues.get(SECOND_OF_MINUTE)
    var nos: java.lang.Long = fieldValues.get(NANO_OF_SECOND)
    if (hod == null)
      return
    if (moh == null && (som != null || nos != null))
      return
    if (moh != null && som == null && nos != null)
      return
    if (resolverStyle ne ResolverStyle.LENIENT) {
      if (hod != null) {
        if (
          (resolverStyle eq ResolverStyle.SMART) && (hod.longValue == 24) && (moh == null || moh.longValue == 0) && (som == null || som.longValue == 0) && (nos == null || nos.longValue == 0)
        ) {
          hod = 0L
          excessDays = Period.ofDays(1)
        }
        val hodVal: Int = HOUR_OF_DAY.checkValidIntValue(hod)
        if (moh != null) {
          val mohVal: Int = MINUTE_OF_HOUR.checkValidIntValue(moh)
          if (som != null) {
            val somVal: Int = SECOND_OF_MINUTE.checkValidIntValue(som)
            if (nos != null) {
              val nosVal: Int = NANO_OF_SECOND.checkValidIntValue(nos)
              addObject(LocalTime.of(hodVal, mohVal, somVal, nosVal))
            } else
              addObject(LocalTime.of(hodVal, mohVal, somVal))
          } else if (nos == null)
            addObject(LocalTime.of(hodVal, mohVal))
        } else if (som == null && nos == null)
          addObject(LocalTime.of(hodVal, 0))
      }
    } else if (hod != null) {
      var hodVal: Long = hod
      if (moh != null)
        if (som != null) {
          if (nos == null)
            nos = 0L
          var totalNanos: Long = Math.multiplyExact(hodVal, 3600000000000L)
          totalNanos = Math.addExact(totalNanos, Math.multiplyExact(moh, 60000000000L))
          totalNanos = Math.addExact(totalNanos, Math.multiplyExact(som, 1000000000L))
          totalNanos = Math.addExact(totalNanos, nos)
          val excessDays: Int  = Math.floorDiv(totalNanos, 86400000000000L).toInt
          val nod: Long        = Math.floorMod(totalNanos, 86400000000000L)
          addObject(LocalTime.ofNanoOfDay(nod))
          this.excessDays = Period.ofDays(excessDays)
        } else {
          var totalSecs: Long = Math.multiplyExact(hodVal, 3600L)
          totalSecs = Math.addExact(totalSecs, Math.multiplyExact(moh, 60L))
          val excessDays: Int = Math.floorDiv(totalSecs, 86400L).toInt
          val sod: Long       = Math.floorMod(totalSecs, 86400L)
          addObject(LocalTime.ofSecondOfDay(sod))
          this.excessDays = Period.ofDays(excessDays)
        }
      else {
        val excessDays: Int = Math.toIntExact(Math.floorDiv(hodVal, 24L))
        hodVal = Math.floorMod(hodVal, 24L)
        addObject(LocalTime.of(hodVal.toInt, 0))
        this.excessDays = Period.ofDays(excessDays)
      }
    }
    fieldValues.remove(HOUR_OF_DAY)
    fieldValues.remove(MINUTE_OF_HOUR)
    fieldValues.remove(SECOND_OF_MINUTE)
    fieldValues.remove(NANO_OF_SECOND)
    ()
  }

  private def mergeInstantFields(): Unit =
    if (fieldValues.containsKey(INSTANT_SECONDS))
      if (zone != null)
        mergeInstantFields0(zone)
      else {
        val offsetSecs: java.lang.Long = fieldValues.get(OFFSET_SECONDS)
        if (offsetSecs != null) {
          val offset: ZoneOffset = ZoneOffset.ofTotalSeconds(offsetSecs.intValue)
          mergeInstantFields0(offset)
        }
      }

  private def mergeInstantFields0(selectedZone: ZoneId): Unit = {
    val instant: Instant                               = Instant.ofEpochSecond(fieldValues.remove(INSTANT_SECONDS))
    val zdt: ChronoZonedDateTime[_ <: ChronoLocalDate] = chrono.zonedDateTime(instant, selectedZone)
    if (date == null)
      addObject(zdt.toLocalDate)
    else
      resolveMakeChanges(INSTANT_SECONDS, zdt.toLocalDate)
    addFieldValue(SECOND_OF_DAY, zdt.toLocalTime.toSecondOfDay.toLong)
    ()
  }

  private def crossCheck(): Unit =
    if (fieldValues.size > 0)
      if (date != null && time != null)
        crossCheck(date.atTime(time))
      else if (date != null)
        crossCheck(date)
      else if (time != null)
        crossCheck(time)

  private def crossCheck(temporal: TemporalAccessor): Unit = {
    val it: java.util.Iterator[java.util.Map.Entry[TemporalField, java.lang.Long]] =
      fieldValues.entrySet.iterator
    val break                                                                      = false
    while (!break && it.hasNext) {
      val entry: java.util.Map.Entry[TemporalField, java.lang.Long] = it.next
      val field: TemporalField                                      = entry.getKey
      val value: Long                                               = entry.getValue
      if (temporal.isSupported(field)) {
        var temporalValue: Long = 0L
        try temporalValue = temporal.getLong(field)
        catch {
          case _: RuntimeException =>
            break
        }
        if (!break) {
          if (temporalValue != value)
            throw new DateTimeException(
              s"Cross check failed: $field $temporalValue vs $field $value"
            )
          it.remove()
        }
      }
    }
  }

  private def resolveFractional(): Unit =
    if (
      time == null && (fieldValues.containsKey(INSTANT_SECONDS) || fieldValues.containsKey(
        SECOND_OF_DAY
      ) || fieldValues.containsKey(SECOND_OF_MINUTE))
    )
      if (fieldValues.containsKey(NANO_OF_SECOND)) {
        val nos: Long = fieldValues.get(NANO_OF_SECOND)
        fieldValues.put(MICRO_OF_SECOND, nos / 1000)
        fieldValues.put(MILLI_OF_SECOND, nos / 1000000)
        ()
      } else {
        fieldValues.put(NANO_OF_SECOND, 0L)
        fieldValues.put(MICRO_OF_SECOND, 0L)
        fieldValues.put(MILLI_OF_SECOND, 0L)
        ()
      }

  private def resolveInstant(): Unit =
    if (date != null && time != null) {
      val offsetSecs = fieldValues.get(OFFSET_SECONDS);
      if (offsetSecs != null) {
        val offset  = ZoneOffset.ofTotalSeconds(offsetSecs.intValue())
        val instant = date.atTime(time).atZone(offset).getLong(ChronoField.INSTANT_SECONDS)
        fieldValues.put(INSTANT_SECONDS, instant)
      } else if (zone != null) {
        val instant = date.atTime(time).atZone(zone).getLong(ChronoField.INSTANT_SECONDS)
        fieldValues.put(INSTANT_SECONDS, instant)
      }
      ()
    }

  /**
   * Builds the specified type from the values in this builder.
   *
   * This attempts to build the specified type from this builder. If the builder cannot return the
   * type, an exception is thrown.
   *
   * @tparam R
   *   the type to return
   * @param type
   *   the type to invoke {@code from} on, not null
   * @return
   *   the extracted value, not null
   * @throws DateTimeException
   *   if an error occurs
   */
  def build[R](tpe: TemporalQuery[R]): R = tpe.queryFrom(this)

  def isSupported(field: TemporalField): Boolean =
    if (field == null)
      false
    else
      fieldValues
        .containsKey(field) || (date != null && date.isSupported(field)) || (time != null && time
        .isSupported(field))

  def getLong(field: TemporalField): Long = {
    Objects.requireNonNull(field, "field")
    val value: java.lang.Long = getFieldValue0(field)
    if (value == null) {
      if (date != null && date.isSupported(field))
        return date.getLong(field)
      if (time != null && time.isSupported(field))
        return time.getLong(field)
      throw new DateTimeException(s"Field not found: $field")
    }
    value
  }

  override def query[R](query: TemporalQuery[R]): R =
    if (query eq TemporalQueries.zoneId)
      zone.asInstanceOf[R]
    else if (query eq TemporalQueries.chronology)
      chrono.asInstanceOf[R]
    else if (query eq TemporalQueries.localDate)
      if (date != null) LocalDate.from(date).asInstanceOf[R] else null.asInstanceOf[R]
    else if (query eq TemporalQueries.localTime)
      time.asInstanceOf[R]
    else if ((query eq TemporalQueries.zone) || (query eq TemporalQueries.offset))
      query.queryFrom(this)
    else if (query eq TemporalQueries.precision)
      null.asInstanceOf[R]
    else
      query.queryFrom(this)

  override def toString: String = {
    val fields =
      if (fieldValues.size > 0)
        s"fields=$fieldValues, "
      else ""
    s"DateTimeBuilder[$fields $chrono, $zone, $date, $time]"
  }
}
