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
package org.threeten.bp.zone

import org.threeten.bp.temporal.ChronoField.YEAR
import org.threeten.bp.temporal.TemporalAdjusters.nextOrSame
import org.threeten.bp.temporal.TemporalAdjusters.previousOrSame
import java.util.{ Collections, Comparator, Objects }
import org.threeten.bp.DayOfWeek
import org.threeten.bp.LocalDate
import org.threeten.bp.LocalDateTime
import org.threeten.bp.LocalTime
import org.threeten.bp.Month
import org.threeten.bp.Year
import org.threeten.bp.ZoneOffset
import org.threeten.bp.chrono.IsoChronology

/**
 * A mutable builder used to create all the rules for a historic time-zone.
 *
 * The rules of a time-zone describe how the offset changes over time. The rules are created by
 * building windows on the time-line within which the different rules apply. The rules may be one of
 * two kinds: <ul> <li>Fixed savings - A single fixed amount of savings from the standard offset
 * will apply.</li> <li>Rules - A set of one or more rules describe how daylight savings changes
 * during the window.</li> </ul><p>
 *
 * <h3>Specification for implementors</h3> This class is a mutable builder used to create zone
 * instances. It must only be used from a single thread. The created instances are immutable and
 * thread-safe.
 *
 * Constructs an instance of the builder that can be used to create zone rules.
 *
 * The builder is used by adding one or more windows representing portions of the time-line. The
 * standard offset from UTC/Greenwich will be constant within a window, although two adjacent
 * windows can have the same standard offset.
 *
 * Within each window, there can either be a {@link #setFixedSavingsToWindow fixed savings amount}
 * or a {@link #addRuleToWindow list of rules} .
 */
class ZoneRulesBuilder() {

  /**
   * The list of windows.
   */
  private val windowList: java.util.List[ZoneRulesBuilder#TZWindow] =
    new java.util.ArrayList[ZoneRulesBuilder#TZWindow]

  /**
   * A map for deduplicating the output.
   */
  private var deduplicateMap: java.util.Map[AnyRef, AnyRef] = null

  private val ruleComparator = new TZRuleComparator()

  /**
   * Adds a window to the builder that can be used to filter a set of rules.
   *
   * This method defines and adds a window to the zone where the standard offset is specified. The
   * window limits the effect of subsequent additions of transition rules or fixed savings. If
   * neither rules or fixed savings are added to the window then the window will default to no
   * savings.
   *
   * Each window must be added sequentially, as the start instant of the window is derived from the
   * until instant of the previous window.
   *
   * @param standardOffset
   *   the standard offset, not null
   * @param until
   *   the date-time that the offset applies until, not null
   * @param untilDefinition
   *   the time type for the until date-time, not null
   * @return
   *   this, for chaining
   * @throws IllegalStateException
   *   if the window order is invalid
   */
  private[zone] def addWindow(
    standardOffset:  ZoneOffset,
    until:           LocalDateTime,
    untilDefinition: ZoneOffsetTransitionRule.TimeDefinition
  ): ZoneRulesBuilder = {
    Objects.requireNonNull(standardOffset, "standardOffset")
    Objects.requireNonNull(until, "until")
    Objects.requireNonNull(untilDefinition, "untilDefinition")
    val window: ZoneRulesBuilder#TZWindow = new TZWindow(standardOffset, until, untilDefinition)
    if (windowList.size > 0) {
      val previous: ZoneRulesBuilder#TZWindow = windowList.get(windowList.size - 1)
      window.validateWindowOrder(previous)
    }
    windowList.add(window)
    this
  }

  /**
   * Adds a window that applies until the end of time to the builder that can be used to filter a
   * set of rules.
   *
   * This method defines and adds a window to the zone where the standard offset is specified. The
   * window limits the effect of subsequent additions of transition rules or fixed savings. If
   * neither rules or fixed savings are added to the window then the window will default to no
   * savings.
   *
   * This must be added after all other windows. No more windows can be added after this one.
   *
   * @param standardOffset
   *   the standard offset, not null
   * @return
   *   this, for chaining
   * @throws IllegalStateException
   *   if a forever window has already been added
   */
  def addWindowForever(standardOffset: ZoneOffset): ZoneRulesBuilder =
    addWindow(standardOffset, LocalDateTime.MAX, ZoneOffsetTransitionRule.TimeDefinition.WALL)

  /**
   * Sets the previously added window to have fixed savings.
   *
   * Setting a window to have fixed savings simply means that a single daylight savings amount
   * applies throughout the window. The window could be small, such as a single summer, or large,
   * such as a multi-year daylight savings.
   *
   * A window can either have fixed savings or rules but not both.
   *
   * @param fixedSavingAmountSecs
   *   the amount of saving to use for the whole window, not null
   * @return
   *   this, for chaining
   * @throws IllegalStateException
   *   if no window has yet been added
   * @throws IllegalStateException
   *   if the window already has rules
   */
  def setFixedSavingsToWindow(fixedSavingAmountSecs: Int): ZoneRulesBuilder = {
    if (windowList.isEmpty)
      throw new IllegalStateException("Must add a window before setting the fixed savings")
    val window: ZoneRulesBuilder#TZWindow = windowList.get(windowList.size - 1)
    window.setFixedSavings(fixedSavingAmountSecs)
    this
  }

  /**
   * Adds a single transition rule to the current window.
   *
   * This adds a rule such that the offset, expressed as a daylight savings amount, changes at the
   * specified date-time.
   *
   * @param transitionDateTime
   *   the date-time that the transition occurs as defined by timeDefintion, not null
   * @param timeDefinition
   *   the definition of how to convert local to actual time, not null
   * @param savingAmountSecs
   *   the amount of saving from the standard offset after the transition in seconds
   * @return
   *   this, for chaining
   * @throws IllegalStateException
   *   if no window has yet been added
   * @throws IllegalStateException
   *   if the window already has fixed savings
   * @throws IllegalStateException
   *   if the window has reached the maximum capacity of 2000 rules
   */
  private[zone] def addRuleToWindow(
    transitionDateTime: LocalDateTime,
    timeDefinition:     ZoneOffsetTransitionRule.TimeDefinition,
    savingAmountSecs:   Int
  ): ZoneRulesBuilder = {
    Objects.requireNonNull(transitionDateTime, "transitionDateTime")
    addRuleToWindow(
      transitionDateTime.getYear,
      transitionDateTime.getYear,
      transitionDateTime.getMonth,
      transitionDateTime.getDayOfMonth,
      null,
      transitionDateTime.toLocalTime,
      false,
      timeDefinition,
      savingAmountSecs
    )
  }

  /**
   * Adds a single transition rule to the current window.
   *
   * This adds a rule such that the offset, expressed as a daylight savings amount, changes at the
   * specified date-time.
   *
   * @param year
   *   the year of the transition, from MIN_VALUE to MAX_VALUE
   * @param month
   *   the month of the transition, not null
   * @param dayOfMonthIndicator
   *   the day-of-month of the transition, adjusted by dayOfWeek, from 1 to 31 adjusted later, or -1
   *   to -28 adjusted earlier from the last day of the month
   * @param time
   *   the time that the transition occurs as defined by timeDefintion, not null
   * @param timeEndOfDay
   *   whether midnight is at the end of day
   * @param timeDefinition
   *   the definition of how to convert local to actual time, not null
   * @param savingAmountSecs
   *   the amount of saving from the standard offset after the transition in seconds
   * @return
   *   this, for chaining
   * @throws DateTimeException
   *   if a date-time field is out of range
   * @throws IllegalStateException
   *   if no window has yet been added
   * @throws IllegalStateException
   *   if the window already has fixed savings
   * @throws IllegalStateException
   *   if the window has reached the maximum capacity of 2000 rules
   */
  private[zone] def addRuleToWindow(
    year:                Int,
    month:               Month,
    dayOfMonthIndicator: Int,
    time:                LocalTime,
    timeEndOfDay:        Boolean,
    timeDefinition:      ZoneOffsetTransitionRule.TimeDefinition,
    savingAmountSecs:    Int
  ): ZoneRulesBuilder =
    addRuleToWindow(year,
                    year,
                    month,
                    dayOfMonthIndicator,
                    null,
                    time,
                    timeEndOfDay,
                    timeDefinition,
                    savingAmountSecs
    )

  /**
   * Adds a multi-year transition rule to the current window.
   *
   * This adds a rule such that the offset, expressed as a daylight savings amount, changes at the
   * specified date-time for each year in the range.
   *
   * @param startYear
   *   the start year of the rule, from MIN_VALUE to MAX_VALUE
   * @param endYear
   *   the end year of the rule, from MIN_VALUE to MAX_VALUE
   * @param month
   *   the month of the transition, not null
   * @param dayOfMonthIndicator
   *   the day-of-month of the transition, adjusted by dayOfWeek, from 1 to 31 adjusted later, or -1
   *   to -28 adjusted earlier from the last day of the month
   * @param dayOfWeek
   *   the day-of-week to adjust to, null if day-of-month should not be adjusted
   * @param time
   *   the time that the transition occurs as defined by timeDefintion, not null
   * @param timeEndOfDay
   *   whether midnight is at the end of day
   * @param timeDefinition
   *   the definition of how to convert local to actual time, not null
   * @param savingAmountSecs
   *   the amount of saving from the standard offset after the transition in seconds
   * @return
   *   this, for chaining
   * @throws DateTimeException
   *   if a date-time field is out of range
   * @throws IllegalArgumentException
   *   if the day of month indicator is invalid
   * @throws IllegalArgumentException
   *   if the end of day midnight flag does not match the time
   * @throws IllegalStateException
   *   if no window has yet been added
   * @throws IllegalStateException
   *   if the window already has fixed savings
   * @throws IllegalStateException
   *   if the window has reached the maximum capacity of 2000 rules
   */
  private[zone] def addRuleToWindow(
    startYear:           Int,
    endYear:             Int,
    month:               Month,
    dayOfMonthIndicator: Int,
    dayOfWeek:           DayOfWeek,
    time:                LocalTime,
    timeEndOfDay:        Boolean,
    timeDefinition:      ZoneOffsetTransitionRule.TimeDefinition,
    savingAmountSecs:    Int
  ): ZoneRulesBuilder = {
    Objects.requireNonNull(month, "month")
    Objects.requireNonNull(time, "time")
    Objects.requireNonNull(timeDefinition, "timeDefinition")
    YEAR.checkValidValue(startYear.toLong)
    YEAR.checkValidValue(endYear.toLong)
    if (dayOfMonthIndicator < -28 || dayOfMonthIndicator > 31 || dayOfMonthIndicator == 0)
      throw new IllegalArgumentException(
        "Day of month indicator must be between -28 and 31 inclusive excluding zero"
      )
    if (timeEndOfDay && !(time == LocalTime.MIDNIGHT))
      throw new IllegalArgumentException("Time must be midnight when end of day flag is true")
    if (windowList.isEmpty)
      throw new IllegalStateException("Must add a window before adding a rule")
    val window: ZoneRulesBuilder#TZWindow = windowList.get(windowList.size - 1)
    window.addRule(startYear,
                   endYear,
                   month,
                   dayOfMonthIndicator,
                   dayOfWeek,
                   time,
                   timeEndOfDay,
                   timeDefinition,
                   savingAmountSecs
    )
    this
  }

  /**
   * Completes the build converting the builder to a set of time-zone rules.
   *
   * Calling this method alters the state of the builder. Further rules should not be added to this
   * builder once this method is called.
   *
   * @param zoneId
   *   the time-zone ID, not null
   * @return
   *   the zone rules, not null
   * @throws IllegalStateException
   *   if no windows have been added
   * @throws IllegalStateException
   *   if there is only one rule defined as being forever for any given window
   */
  def toRules(zoneId: String): ZoneRules = toRules(zoneId, new java.util.HashMap[AnyRef, AnyRef])

  /**
   * Completes the build converting the builder to a set of time-zone rules.
   *
   * Calling this method alters the state of the builder. Further rules should not be added to this
   * builder once this method is called.
   *
   * @param zoneId
   *   the time-zone ID, not null
   * @param deduplicateMap
   *   a map for deduplicating the values, not null
   * @return
   *   the zone rules, not null
   * @throws IllegalStateException
   *   if no windows have been added
   * @throws IllegalStateException
   *   if there is only one rule defined as being forever for any given window
   */
  private[zone] def toRules(
    zoneId:         String,
    deduplicateMap: java.util.Map[AnyRef, AnyRef]
  ): ZoneRules = {
    Objects.requireNonNull(zoneId, "zoneId")
    this.deduplicateMap = deduplicateMap
    if (windowList.isEmpty)
      throw new IllegalStateException("No windows have been added to the builder")
    val standardTransitionList: java.util.List[ZoneOffsetTransition]     =
      new java.util.ArrayList[ZoneOffsetTransition](4)
    val transitionList: java.util.List[ZoneOffsetTransition]             =
      new java.util.ArrayList[ZoneOffsetTransition](256)
    val lastTransitionRuleList: java.util.List[ZoneOffsetTransitionRule] =
      new java.util.ArrayList[ZoneOffsetTransitionRule](2)
    val firstWindow: ZoneRulesBuilder#TZWindow                           = windowList.get(0)
    var loopStandardOffset: ZoneOffset                                   = firstWindow.standardOffset
    var loopSavings: Int                                                 = 0
    if (firstWindow.fixedSavingAmountSecs != null)
      loopSavings = firstWindow.fixedSavingAmountSecs
    val firstWallOffset: ZoneOffset                                      = deduplicate(
      ZoneOffset.ofTotalSeconds(loopStandardOffset.getTotalSeconds + loopSavings)
    )
    var loopWindowStart: LocalDateTime                                   = deduplicate(LocalDateTime.of(Year.MIN_VALUE, 1, 1, 0, 0))
    var loopWindowOffset: ZoneOffset                                     = firstWallOffset
    val windows                                                          = windowList.iterator
    while (windows.hasNext) {
      val window                          = windows.next()
      window.tidy(loopWindowStart.getYear)
      var effectiveSavings: Integer       = window.fixedSavingAmountSecs
      if (effectiveSavings == null) {
        effectiveSavings = 0
        var break = false
        val rules = window.ruleList.iterator
        while (!break && rules.hasNext) {
          val rule                        = rules.next()
          val trans: ZoneOffsetTransition = rule.toTransition(loopStandardOffset, loopSavings)
          if (trans.toEpochSecond > loopWindowStart.toEpochSecond(loopWindowOffset))
            break = true
          if (!break)
            effectiveSavings = rule.savingAmountSecs
        }
      }
      if (loopStandardOffset != window.standardOffset) {
        standardTransitionList.add(
          deduplicate(
            new ZoneOffsetTransition(LocalDateTime.ofEpochSecond(loopWindowStart
                                                                   .toEpochSecond(loopWindowOffset),
                                                                 0,
                                                                 loopStandardOffset
                                     ),
                                     loopStandardOffset,
                                     window.standardOffset
            )
          )
        )
        loopStandardOffset = deduplicate(window.standardOffset)
      }
      val effectiveWallOffset: ZoneOffset = deduplicate(
        ZoneOffset.ofTotalSeconds(loopStandardOffset.getTotalSeconds + effectiveSavings)
      )
      if (loopWindowOffset != effectiveWallOffset) {
        val trans: ZoneOffsetTransition = deduplicate(
          new ZoneOffsetTransition(loopWindowStart, loopWindowOffset, effectiveWallOffset)
        )
        transitionList.add(trans)
      }
      loopSavings = effectiveSavings
      val rules                           = window.ruleList.iterator
      while (rules.hasNext) {
        val rule                        = rules.next()
        val trans: ZoneOffsetTransition = deduplicate(
          rule.toTransition(loopStandardOffset, loopSavings)
        )
        if (
          (trans.toEpochSecond >= loopWindowStart.toEpochSecond(
            loopWindowOffset
          )) && (trans.toEpochSecond < window
            .createDateTimeEpochSecond(
              loopSavings
            )) && (trans.getOffsetBefore != trans.getOffsetAfter)
        ) {
          transitionList.add(trans)
          loopSavings = rule.savingAmountSecs
        }
      }
      val lastRules                       = window.lastRuleList.iterator
      while (lastRules.hasNext) {
        val lastRule                                 = lastRules.next()
        val transitionRule: ZoneOffsetTransitionRule = deduplicate(
          lastRule.toTransitionRule(loopStandardOffset, loopSavings)
        )
        lastTransitionRuleList.add(transitionRule)
        loopSavings = lastRule.savingAmountSecs
      }
      loopWindowOffset = deduplicate(window.createWallOffset(loopSavings))
      loopWindowStart = deduplicate(
        LocalDateTime
          .ofEpochSecond(window.createDateTimeEpochSecond(loopSavings), 0, loopWindowOffset)
      )
    }
    StandardZoneRules(firstWindow.standardOffset,
                      firstWallOffset,
                      standardTransitionList,
                      transitionList,
                      lastTransitionRuleList
    )
  }

  /**
   * Deduplicates an object instance.
   *
   * @tparam T
   *   the generic type
   * @param object
   *   the object to deduplicate
   * @return
   *   the deduplicated object
   */
  private[zone] def deduplicate[T <: AnyRef](`object`: T): T = {
    if (!deduplicateMap.containsKey(`object`))
      deduplicateMap.put(`object`, `object`)
    deduplicateMap.get(`object`).asInstanceOf[T]
  }

  /**
   * A definition of a window in the time-line. The window will have one standard offset and will
   * either have a fixed DST savings or a set of rules.
   *
   * @constructor
   *
   * @param standardOffset
   *   the standard offset applicable during the window, not null
   * @param windowEnd
   *   the end of the window, relative to the time definition, null if forever
   * @param timeDefinition
   *   the time definition for calculating the true end, not null
   */
  private[zone] class TZWindow private[zone] (
    private[zone] val standardOffset: ZoneOffset,
    private val windowEnd:            LocalDateTime,
    private val timeDefinition:       ZoneOffsetTransitionRule.TimeDefinition
  ) {

    /** The fixed amount of the saving to be applied during this window. */
    private[zone] var fixedSavingAmountSecs: Integer = null

    /** The rules for the current window. */
    private[zone] var ruleList: java.util.List[ZoneRulesBuilder#TZRule] =
      new java.util.ArrayList[ZoneRulesBuilder#TZRule]

    /** The latest year that the last year starts at. */
    private var maxLastRuleStartYear: Int = Year.MIN_VALUE

    /** The last rules. */
    private[zone] var lastRuleList: java.util.List[ZoneRulesBuilder#TZRule] =
      new java.util.ArrayList[ZoneRulesBuilder#TZRule]

    /**
     * Sets the fixed savings amount for the window.
     *
     * @param fixedSavingAmount
     *   the amount of daylight saving to apply throughout the window, may be null
     * @throws IllegalStateException
     *   if the window already has rules
     */
    private[zone] def setFixedSavings(fixedSavingAmount: Int): Unit =
      if (ruleList.size > 0 || lastRuleList.size > 0)
        throw new IllegalStateException("Window has DST rules, so cannot have fixed savings")
      else
        this.fixedSavingAmountSecs = fixedSavingAmount

    /**
     * Adds a rule to the current window.
     *
     * @param startYear
     *   the start year of the rule, from MIN_VALUE to MAX_VALUE
     * @param endYear
     *   the end year of the rule, from MIN_VALUE to MAX_VALUE
     * @param month
     *   the month of the transition, not null
     * @param dayOfMonthIndicator
     *   the day-of-month of the transition, adjusted by dayOfWeek, from 1 to 31 adjusted later, or
     *   -1 to -28 adjusted earlier from the last day of the month
     * @param dayOfWeek
     *   the day-of-week to adjust to, null if day-of-month should not be adjusted
     * @param time
     *   the time that the transition occurs as defined by timeDefintion, not null
     * @param timeEndOfDay
     *   whether midnight is at the end of day
     * @param timeDefinition
     *   the definition of how to convert local to actual time, not null
     * @param savingAmountSecs
     *   the amount of saving from the standard offset in seconds
     * @throws IllegalStateException
     *   if the window already has fixed savings
     * @throws IllegalStateException
     *   if the window has reached the maximum capacity of 2000 rules
     */
    private[zone] def addRule(
      startYear:           Int,
      endYear:             Int,
      month:               Month,
      dayOfMonthIndicator: Int,
      dayOfWeek:           DayOfWeek,
      time:                LocalTime,
      timeEndOfDay:        Boolean,
      timeDefinition:      ZoneOffsetTransitionRule.TimeDefinition,
      savingAmountSecs:    Int
    ): Unit = {
      var _endYear          = endYear
      if (fixedSavingAmountSecs != null)
        throw new IllegalStateException("Window has a fixed DST saving, so cannot have DST rules")
      if (ruleList.size >= 2000)
        throw new IllegalStateException("Window has reached the maximum number of allowed rules")
      var lastRule: Boolean = false
      if (_endYear == Year.MAX_VALUE) {
        lastRule = true
        _endYear = startYear
      }
      var year: Int         = startYear
      while (year <= _endYear) {
        val rule: ZoneRulesBuilder#TZRule = new TZRule(year,
                                                       month,
                                                       dayOfMonthIndicator,
                                                       dayOfWeek,
                                                       time,
                                                       timeEndOfDay,
                                                       timeDefinition,
                                                       savingAmountSecs
        )
        if (lastRule) {
          lastRuleList.add(rule)
          maxLastRuleStartYear = Math.max(startYear, maxLastRuleStartYear)
        } else
          ruleList.add(rule)
        year += 1
      }
    }

    /**
     * Validates that this window is after the previous one.
     *
     * @param previous
     *   the previous window, not null
     * @throws IllegalStateException
     *   if the window order is invalid
     */
    private[zone] def validateWindowOrder(previous: ZoneRulesBuilder#TZWindow): Unit =
      if (windowEnd.isBefore(previous.windowEnd))
        throw new IllegalStateException(
          s"Windows must be added in date-time order: $windowEnd < ${previous.windowEnd}"
        )

    /**
     * Adds rules to make the last rules all start from the same year. Also add one more year to
     * avoid weird case where penultimate year has odd offset.
     *
     * @param windowStartYear
     *   the window start year
     * @throws IllegalStateException
     *   if there is only one rule defined as being forever
     */
    private[zone] def tidy(windowStartYear: Int): Unit = {
      if (lastRuleList.size == 1)
        throw new IllegalStateException("Cannot have only one rule defined as being forever")
      if (windowEnd == LocalDateTime.MAX) {
        maxLastRuleStartYear = Math.max(maxLastRuleStartYear, windowStartYear) + 1
        val lastRules = lastRuleList.iterator
        while (lastRules.hasNext) {
          val lastRule = lastRules.next()
          addRule(
            lastRule.year,
            maxLastRuleStartYear,
            lastRule.month,
            lastRule.dayOfMonthIndicator,
            lastRule.dayOfWeek,
            lastRule.time,
            lastRule.timeEndOfDay,
            lastRule.timeDefinition,
            lastRule.savingAmountSecs
          )
          lastRule.year = maxLastRuleStartYear + 1
        }
        if (maxLastRuleStartYear == Year.MAX_VALUE)
          lastRuleList.clear()
        else
          maxLastRuleStartYear += 1
      } else {
        val endYear: Int = windowEnd.getYear
        val lastRules    = lastRuleList.iterator
        while (lastRules.hasNext) {
          val lastRule = lastRules.next()
          addRule(
            lastRule.year,
            endYear + 1,
            lastRule.month,
            lastRule.dayOfMonthIndicator,
            lastRule.dayOfWeek,
            lastRule.time,
            lastRule.timeEndOfDay,
            lastRule.timeDefinition,
            lastRule.savingAmountSecs
          )
        }
        lastRuleList.clear()
        maxLastRuleStartYear = Year.MAX_VALUE
      }
      Collections.sort(ruleList, ruleComparator)
      Collections.sort(lastRuleList, ruleComparator)
      if (ruleList.size == 0 && fixedSavingAmountSecs == null)
        fixedSavingAmountSecs = 0
    }

    /**
     * Checks if the window is empty.
     *
     * @return
     *   true if the window is only a standard offset
     */
    private[zone] def isSingleWindowStandardOffset: Boolean =
      (windowEnd == LocalDateTime.MAX) && (timeDefinition eq ZoneOffsetTransitionRule.TimeDefinition.WALL) && fixedSavingAmountSecs == null && lastRuleList.isEmpty && ruleList.isEmpty

    /**
     * Creates the wall offset for the local date-time at the end of the window.
     *
     * @param savingsSecs
     *   the amount of savings in use in seconds
     * @return
     *   the created date-time epoch second in the wall offset, not null
     */
    private[zone] def createWallOffset(savingsSecs: Int): ZoneOffset =
      ZoneOffset.ofTotalSeconds(standardOffset.getTotalSeconds + savingsSecs)

    /**
     * Creates the offset date-time for the local date-time at the end of the window.
     *
     * @param savingsSecs
     *   the amount of savings in use in seconds
     * @return
     *   the created date-time epoch second in the wall offset, not null
     */
    private[zone] def createDateTimeEpochSecond(savingsSecs: Int): Long = {
      val wallOffset: ZoneOffset = createWallOffset(savingsSecs)
      val ldt: LocalDateTime     = timeDefinition.createDateTime(windowEnd, standardOffset, wallOffset)
      ldt.toEpochSecond(wallOffset)
    }
  }

  /**
   * A definition of the way a local time can be converted to an offset time.
   *
   * @constructor
   *
   * @param year
   *   the year
   * @param month
   *   the month, not null
   * @param dayOfMonthIndicator
   *   the day-of-month of the transition, adjusted by dayOfWeek, from 1 to 31 adjusted later, or -1
   *   to -28 adjusted earlier from the last day of the month
   * @param dayOfWeek
   *   the day-of-week, null if day-of-month is exact
   * @param time
   *   the time, not null
   * @param timeEndOfDay
   *   whether midnight is at the end of day
   * @param timeDefinition
   *   the time definition, not null
   * @param savingAmountSecs
   *   the savings amount in seconds
   */
  protected[zone] class TZRule private[zone] (
    private[zone] var year:                Int,
    private[zone] var month:               Month,
    private[zone] var dayOfMonthIndicator: Int,
    private[zone] var dayOfWeek:           DayOfWeek,
    private[zone] var time:                LocalTime,
    private[zone] var timeEndOfDay:        Boolean,
    private[zone] var timeDefinition:      ZoneOffsetTransitionRule.TimeDefinition,
    private[zone] var savingAmountSecs:    Int
  ) {

    /**
     * Converts this to a transition.
     *
     * @param standardOffset
     *   the active standard offset, not null
     * @param savingsBeforeSecs
     *   the active savings in seconds
     * @return
     *   the transition, not null
     */
    private[zone] def toTransition(
      standardOffset:    ZoneOffset,
      savingsBeforeSecs: Int
    ): ZoneOffsetTransition = {
      var date: LocalDate         = toLocalDate
      date = deduplicate(date)
      val ldt: LocalDateTime      = deduplicate(LocalDateTime.of(date, time))
      val wallOffset: ZoneOffset  = deduplicate(
        ZoneOffset.ofTotalSeconds(standardOffset.getTotalSeconds + savingsBeforeSecs)
      )
      val dt: LocalDateTime       = deduplicate(
        timeDefinition.createDateTime(ldt, standardOffset, wallOffset)
      )
      val offsetAfter: ZoneOffset = deduplicate(
        ZoneOffset.ofTotalSeconds(standardOffset.getTotalSeconds + savingAmountSecs)
      )
      new ZoneOffsetTransition(dt, wallOffset, offsetAfter)
    }

    /**
     * Converts this to a transition rule.
     *
     * @param standardOffset
     *   the active standard offset, not null
     * @param savingsBeforeSecs
     *   the active savings before the transition in seconds
     * @return
     *   the transition, not null
     */
    private[zone] def toTransitionRule(
      standardOffset:    ZoneOffset,
      savingsBeforeSecs: Int
    ): ZoneOffsetTransitionRule = {
      if (dayOfMonthIndicator < 0)
        if (month ne Month.FEBRUARY)
          dayOfMonthIndicator = month.maxLength - 6
      if (
        timeEndOfDay && dayOfMonthIndicator > 0 && !(dayOfMonthIndicator == 28 && (month eq Month.FEBRUARY))
      ) {
        val date: LocalDate = LocalDate.of(2004, month, dayOfMonthIndicator).plusDays(1)
        month = date.getMonth
        dayOfMonthIndicator = date.getDayOfMonth
        if (dayOfWeek != null)
          dayOfWeek = dayOfWeek.plus(1)
        timeEndOfDay = false
      }
      val trans: ZoneOffsetTransition = toTransition(standardOffset, savingsBeforeSecs)
      new ZoneOffsetTransitionRule(month,
                                   dayOfMonthIndicator,
                                   dayOfWeek,
                                   time,
                                   timeEndOfDay,
                                   timeDefinition,
                                   standardOffset,
                                   trans.getOffsetBefore,
                                   trans.getOffsetAfter
      )
    }

    private[zone] def toLocalDate: LocalDate = {
      var date: LocalDate = null
      if (dayOfMonthIndicator < 0) {
        val monthLen: Int = month.length(IsoChronology.INSTANCE.isLeapYear(year.toLong))
        date = LocalDate.of(year, month, monthLen + 1 + dayOfMonthIndicator)
        if (dayOfWeek != null)
          date = date.`with`(previousOrSame(dayOfWeek))
      } else {
        date = LocalDate.of(year, month, dayOfMonthIndicator)
        if (dayOfWeek != null)
          date = date.`with`(nextOrSame(dayOfWeek))
      }
      if (timeEndOfDay)
        date = date.plusDays(1)
      date
    }

  }

  class TZRuleComparator extends Comparator[ZoneRulesBuilder#TZRule] {

    override def compare(that: ZoneRulesBuilder#TZRule, other: ZoneRulesBuilder#TZRule): Int = {
      var cmp: Int = that.year - other.year
      cmp = if (cmp == 0) that.month.compareTo(other.month) else cmp
      if (cmp == 0) {
        val thisDate: LocalDate  = that.toLocalDate
        val otherDate: LocalDate = other.toLocalDate
        cmp = thisDate.compareTo(otherDate)
      }
      cmp = if (cmp == 0) that.time.compareTo(other.time) else cmp
      cmp
    }

  }

}
