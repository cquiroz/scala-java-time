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

import java.io.Serializable
import java.util.{ Collections, Objects }
import org.threeten.bp.Duration
import org.threeten.bp.Instant
import org.threeten.bp.LocalDateTime
import org.threeten.bp.ZoneOffset

/**
 * The rules defining how the zone offset varies for a single time-zone.
 *
 * The rules model all the historic and future transitions for a time-zone. {@link
 * ZoneOffsetTransition} is used for known transitions, typically historic. {@link
 * ZoneOffsetTransitionRule} is used for future transitions that are based on the result of an
 * algorithm.
 *
 * The rules are loaded via {@link ZoneRulesProvider} using a {@link ZoneId} . The same rules may be
 * shared internally between multiple zone IDs.
 *
 * Serializing an instance of {@code ZoneRules} will store the entire set of rules. It does not
 * store the zone ID as it is not part of the state of this object.
 *
 * A rule implementation may or may not store full information about historic and future
 * transitions, and the information stored is only as accurate as that supplied to the
 * implementation by the rules provider. Applications should treat the data provided as representing
 * the best information available to the implementation of this rule.
 *
 * <h3>Specification for implementors</h3> The supplied implementations of this class are immutable
 * and thread-safe.
 */
object ZoneRules {

  /**
   * Obtains an instance of {@code ZoneRules} with full transition rules.
   *
   * @param baseStandardOffset
   *   the standard offset to use before legal rules were set, not null
   * @param baseWallOffset
   *   the wall offset to use before legal rules were set, not null
   * @param standardOffsetTransitionList
   *   the list of changes to the standard offset, not null
   * @param transitionList
   *   the list of transitions, not null
   * @param lastRules
   *   the recurring last rules, size 16 or less, not null
   * @return
   *   the zone rules, not null
   */
  def of(
    baseStandardOffset:           ZoneOffset,
    baseWallOffset:               ZoneOffset,
    standardOffsetTransitionList: java.util.List[ZoneOffsetTransition],
    transitionList:               java.util.List[ZoneOffsetTransition],
    lastRules:                    java.util.List[ZoneOffsetTransitionRule]
  ): ZoneRules = {
    Objects.requireNonNull(baseStandardOffset, "baseStandardOffset")
    Objects.requireNonNull(baseWallOffset, "baseWallOffset")
    Objects.requireNonNull(standardOffsetTransitionList, "standardOffsetTransitionList")
    Objects.requireNonNull(transitionList, "transitionList")
    Objects.requireNonNull(lastRules, "lastRules")
    StandardZoneRules(baseStandardOffset,
                      baseWallOffset,
                      standardOffsetTransitionList,
                      transitionList,
                      lastRules
    )
  }

  /**
   * Obtains an instance of {@code ZoneRules} that always uses the same offset.
   *
   * The returned rules always have the same offset.
   *
   * @param offset
   *   the offset, not null
   * @return
   *   the zone rules, not null
   */
  def of(offset: ZoneOffset): ZoneRules = {
    Objects.requireNonNull(offset, "offset")
    new ZoneRules.Fixed(offset)
  }

  /**
   * Fixed time-zone.
   *
   * @constructor
   *
   * @param offset
   *   the offset, not null
   */
  @SerialVersionUID(-8733721350312276297L)
  private[zone] final class Fixed private[zone] (private val offset: ZoneOffset)
      extends ZoneRules
      with Serializable {

    def isFixedOffset: Boolean = true

    def getOffset(instant: Instant): ZoneOffset = offset

    def getOffset(localDateTime: LocalDateTime): ZoneOffset = offset

    def getValidOffsets(localDateTime: LocalDateTime): java.util.List[ZoneOffset] =
      Collections.singletonList(offset)

    def getTransition(localDateTime: LocalDateTime): ZoneOffsetTransition = null

    def isValidOffset(dateTime: LocalDateTime, offset: ZoneOffset): Boolean = this.offset == offset

    def getStandardOffset(instant: Instant): ZoneOffset = offset

    def getDaylightSavings(instant: Instant): Duration = Duration.ZERO

    def isDaylightSavings(instant: Instant): Boolean = false

    def nextTransition(instant: Instant): ZoneOffsetTransition = null

    def previousTransition(instant: Instant): ZoneOffsetTransition = null

    def getTransitions: java.util.List[ZoneOffsetTransition] =
      Collections.emptyList[ZoneOffsetTransition]

    def getTransitionRules: java.util.List[ZoneOffsetTransitionRule] =
      Collections.emptyList[ZoneOffsetTransitionRule]

    override def equals(obj: Any): Boolean =
      obj match {
        case fixed: Fixed             => (this eq fixed) || offset == fixed.offset
        case rules: StandardZoneRules =>
          rules.isFixedOffset && (offset == rules.getOffset(Instant.EPOCH))
        case _                        => false
      }

    override def hashCode: Int = 1 ^ (31 + offset.hashCode) ^ 1 ^ (31 + offset.hashCode) ^ 1

    override def toString: String = s"FixedRules:$offset"
  }

}

/** @constructor Restricted constructor. */
abstract class ZoneRules private[zone] () {

  /**
   * Checks of the zone rules are fixed, such that the offset never varies.
   *
   * @return
   *   true if the time-zone is fixed and the offset never changes
   */
  def isFixedOffset: Boolean

  /**
   * Gets the offset applicable at the specified instant in these rules.
   *
   * The mapping from an instant to an offset is simple, there is only one valid offset for each
   * instant. This method returns that offset.
   *
   * @param instant
   *   the instant to find the offset for, not null, but null may be ignored if the rules have a
   *   single offset for all instants
   * @return
   *   the offset, not null
   */
  def getOffset(instant: Instant): ZoneOffset

  /**
   * Gets a suitable offset for the specified local date-time in these rules.
   *
   * The mapping from a local date-time to an offset is not straightforward. There are three cases:
   * <ul> <li>Normal, with one valid offset. For the vast majority of the year, the normal case
   * applies, where there is a single valid offset for the local date-time.</li> <li>Gap, with zero
   * valid offsets. This is when clocks jump forward typically due to the spring daylight savings
   * change from "winter" to "summer". In a gap there are local date-time values with no valid
   * offset.</li> <li>Overlap, with two valid offsets. This is when clocks are set back typically
   * due to the autumn daylight savings change from "summer" to "winter". In an overlap there are
   * local date-time values with two valid offsets.</li> </ul><p> Thus, for any given local
   * date-time there can be zero, one or two valid offsets. This method returns the single offset in
   * the Normal case, and in the Gap or Overlap case it returns the offset before the transition.
   *
   * Since, in the case of Gap and Overlap, the offset returned is a "best" value, rather than the
   * "correct" value, it should be treated with care. Applications that care about the correct
   * offset should use a combination of this method, {@link #getValidOffsets(LocalDateTime)} and
   * {@link #getTransition(LocalDateTime)} .
   *
   * @param localDateTime
   *   the local date-time to query, not null, but null may be ignored if the rules have a single
   *   offset for all instants
   * @return
   *   the best available offset for the local date-time, not null
   */
  def getOffset(localDateTime: LocalDateTime): ZoneOffset

  /**
   * Gets the offset applicable at the specified local date-time in these rules.
   *
   * The mapping from a local date-time to an offset is not straightforward. There are three cases:
   * <ul> <li>Normal, with one valid offset. For the vast majority of the year, the normal case
   * applies, where there is a single valid offset for the local date-time.</li> <li>Gap, with zero
   * valid offsets. This is when clocks jump forward typically due to the spring daylight savings
   * change from "winter" to "summer". In a gap there are local date-time values with no valid
   * offset.</li> <li>Overlap, with two valid offsets. This is when clocks are set back typically
   * due to the autumn daylight savings change from "summer" to "winter". In an overlap there are
   * local date-time values with two valid offsets.</li> </ul><p> Thus, for any given local
   * date-time there can be zero, one or two valid offsets. This method returns that list of valid
   * offsets, which is a list of size 0, 1 or 2. In the case where there are two offsets, the
   * earlier offset is returned at index 0 and the later offset at index 1.
   *
   * There are various ways to handle the conversion from a {@code LocalDateTime} . One technique,
   * using this method, would be: <pre> List<ZoneOffset> validOffsets = rules.getOffset(localDT); if
   * (validOffsets.size() == 1) { // Normal case: only one valid offset zoneOffset =
   * validOffsets.get(0); } else { // Gap or Overlap: determine what to do from transition (which
   * will be non-null) ZoneOffsetTransition trans = rules.getTransition(localDT); } </pre>
   *
   * In theory, it is possible for there to be more than two valid offsets. This would happen if
   * clocks to be put back more than once in quick succession. This has never happened in the
   * history of time-zones and thus has no special handling. However, if it were to happen, then the
   * list would return more than 2 entries.
   *
   * @param localDateTime
   *   the local date-time to query for valid offsets, not null, but null may be ignored if the
   *   rules have a single offset for all instants
   * @return
   *   the list of valid offsets, may be immutable, not null
   */
  def getValidOffsets(localDateTime: LocalDateTime): java.util.List[ZoneOffset]

  /**
   * Gets the offset transition applicable at the specified local date-time in these rules.
   *
   * The mapping from a local date-time to an offset is not straightforward. There are three cases:
   * <ul> <li>Normal, with one valid offset. For the vast majority of the year, the normal case
   * applies, where there is a single valid offset for the local date-time.</li> <li>Gap, with zero
   * valid offsets. This is when clocks jump forward typically due to the spring daylight savings
   * change from "winter" to "summer". In a gap there are local date-time values with no valid
   * offset.</li> <li>Overlap, with two valid offsets. This is when clocks are set back typically
   * due to the autumn daylight savings change from "summer" to "winter". In an overlap there are
   * local date-time values with two valid offsets.</li> </ul><p> A transition is used to model the
   * cases of a Gap or Overlap. The Normal case will return null.
   *
   * There are various ways to handle the conversion from a {@code LocalDateTime} . One technique,
   * using this method, would be: <pre> ZoneOffsetTransition trans = rules.getTransition(localDT);
   * if (trans == null) { // Gap or Overlap: determine what to do from transition } else { // Normal
   * case: only one valid offset zoneOffset = rule.getOffset(localDT); } </pre>
   *
   * @param localDateTime
   *   the local date-time to query for offset transition, not null, but null may be ignored if the
   *   rules have a single offset for all instants
   * @return
   *   the offset transition, null if the local date-time is not in transition
   */
  def getTransition(localDateTime: LocalDateTime): ZoneOffsetTransition

  /**
   * Gets the standard offset for the specified instant in this zone.
   *
   * This provides access to historic information on how the standard offset has changed over time.
   * The standard offset is the offset before any daylight saving time is applied. This is typically
   * the offset applicable during winter.
   *
   * @param instant
   *   the instant to find the offset information for, not null, but null may be ignored if the
   *   rules have a single offset for all instants
   * @return
   *   the standard offset, not null
   */
  def getStandardOffset(instant: Instant): ZoneOffset

  /**
   * Gets the amount of daylight savings in use for the specified instant in this zone.
   *
   * This provides access to historic information on how the amount of daylight savings has changed
   * over time. This is the difference between the standard offset and the actual offset. Typically
   * the amount is zero during winter and one hour during summer. Time-zones are second-based, so
   * the nanosecond part of the duration will be zero.
   *
   * @param instant
   *   the instant to find the daylight savings for, not null, but null may be ignored if the rules
   *   have a single offset for all instants
   * @return
   *   the difference between the standard and actual offset, not null
   */
  def getDaylightSavings(instant: Instant): Duration

  /**
   * Checks if the specified instant is in daylight savings.
   *
   * This checks if the standard and actual offsets are the same at the specified instant.
   *
   * @param instant
   *   the instant to find the offset information for, not null, but null may be ignored if the
   *   rules have a single offset for all instants
   * @return
   *   the standard offset, not null
   */
  def isDaylightSavings(instant: Instant): Boolean

  /**
   * Checks if the offset date-time is valid for these rules.
   *
   * To be valid, the local date-time must not be in a gap and the offset must match the valid
   * offsets.
   *
   * @param localDateTime
   *   the date-time to check, not null, but null may be ignored if the rules have a single offset
   *   for all instants
   * @param offset
   *   the offset to check, null returns false
   * @return
   *   true if the offset date-time is valid for these rules
   */
  def isValidOffset(localDateTime: LocalDateTime, offset: ZoneOffset): Boolean

  /**
   * Gets the next transition after the specified instant.
   *
   * This returns details of the next transition after the specified instant. For example, if the
   * instant represents a point where "Summer" daylight savings time applies, then the method will
   * return the transition to the next "Winter" time.
   *
   * @param instant
   *   the instant to get the next transition after, not null, but null may be ignored if the rules
   *   have a single offset for all instants
   * @return
   *   the next transition after the specified instant, null if this is after the last transition
   */
  def nextTransition(instant: Instant): ZoneOffsetTransition

  /**
   * Gets the previous transition before the specified instant.
   *
   * This returns details of the previous transition after the specified instant. For example, if
   * the instant represents a point where "summer" daylight saving time applies, then the method
   * will return the transition from the previous "winter" time.
   *
   * @param instant
   *   the instant to get the previous transition after, not null, but null may be ignored if the
   *   rules have a single offset for all instants
   * @return
   *   the previous transition after the specified instant, null if this is before the first
   *   transition
   */
  def previousTransition(instant: Instant): ZoneOffsetTransition

  /**
   * Gets the complete list of fully defined transitions.
   *
   * The complete set of transitions for this rules instance is defined by this method and {@link
   * #getTransitionRules()}. This method returns those transitions that have been fully defined.
   * These are typically historical, but may be in the future.
   *
   * The list will be empty for fixed offset rules and for any time-zone where there has only ever
   * been a single offset. The list will also be empty if the transition rules are unknown.
   *
   * @return
   *   an immutable list of fully defined transitions, not null
   */
  def getTransitions: java.util.List[ZoneOffsetTransition]

  /**
   * Gets the list of transition rules for years beyond those defined in the transition list.
   *
   * The complete set of transitions for this rules instance is defined by this method and {@link
   * #getTransitions()}. This method returns instances of {@link ZoneOffsetTransitionRule} that
   * define an algorithm for when transitions will occur.
   *
   * For any given {@code ZoneRules} , this list contains the transition rules for years beyond
   * those years that have been fully defined. These rules typically refer to future daylight saving
   * time rule changes.
   *
   * If the zone defines daylight savings into the future, then the list will normally be of size
   * two and hold information about entering and exiting daylight savings. If the zone does not have
   * daylight savings, or information about future changes is uncertain, then the list will be
   * empty.
   *
   * The list will be empty for fixed offset rules and for any time-zone where there is no daylight
   * saving time. The list will also be empty if the transition rules are unknown.
   *
   * @return
   *   an immutable list of transition rules, not null
   */
  def getTransitionRules: java.util.List[ZoneOffsetTransitionRule]

  /**
   * Checks if this set of rules equals another.
   *
   * Two rule sets are equal if they will always result in the same output for any given input
   * instant or local date-time. Rules from two different groups may return false even if they are
   * in fact the same.
   *
   * This definition should result in implementations comparing their entire state.
   *
   * @param otherRules
   *   the other rules, null returns false
   * @return
   *   true if this rules is the same as that specified
   */
  override def equals(otherRules: Any): Boolean

  /**
   * Returns a suitable hash code given the definition of {@code #equals} .
   *
   * @return
   *   the hash code
   */
  override def hashCode: Int
}
