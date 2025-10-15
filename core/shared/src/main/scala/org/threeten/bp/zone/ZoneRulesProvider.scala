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

import java.util.{ Collections, Objects }
import java.util.HashMap
import java.util.Map
import java.util.ArrayList

object ZoneRulesProvider {

  /** The set of loaded providers. */
  private lazy val PROVIDERS: ArrayList[ZoneRulesProvider] =
    new ArrayList[ZoneRulesProvider]

  /** The lookup from zone region ID to provider. */
  private lazy val ZONES: Map[String, ZoneRulesProvider] =
    new HashMap[String, ZoneRulesProvider]()

  /**
   * Gets the set of available zone IDs.
   *
   * These zone IDs are loaded and available for use by {@code ZoneId}.
   *
   * @return
   *   the unmodifiable copy of the set of zone IDs, not null
   */
  def getAvailableZoneIds: java.util.Set[String] = Collections.unmodifiableSet(ZONES.keySet)

  /**
   * Gets the rules for the zone ID.
   *
   * This returns the latest available rules for the zone ID.
   *
   * This method relies on time-zone data provider files that are configured. These are loaded using
   * a {@code ServiceLoader}.
   *
   * The caching flag is designed to allow provider implementations to prevent the rules being
   * cached in {@code ZoneId}. Under normal circumstances, the caching of zone rules is highly
   * desirable as it will provide greater performance. However, there is a use case where the
   * caching would not be desirable, see {@link #provideRules}.
   *
   * @param zoneId
   *   the zone ID as defined by {@code ZoneId}, not null
   * @param forCaching
   *   whether the rules are being queried for caching, true if the returned rules will be cached by
   *   {@code ZoneId}, false if they will be returned to the user without being cached in { @code
   *   ZoneId}
   * @return
   *   the rules, null if {@code forCaching} is true and this is a dynamic provider that wants to
   *   prevent caching in {@code ZoneId}, otherwise not null
   * @throws ZoneRulesException
   *   if rules cannot be obtained for the zone ID
   */
  def getRules(zoneId: String, forCaching: Boolean): ZoneRules = {
    Objects.requireNonNull(zoneId, "zoneId")
    getProvider(zoneId).provideRules(zoneId, forCaching)
  }

  /**
   * Gets the history of rules for the zone ID.
   *
   * Time-zones are defined by governments and change frequently. This method allows applications to
   * find the history of changes to the rules for a single zone ID. The map is keyed by a string,
   * which is the version string associated with the rules.
   *
   * The exact meaning and format of the version is provider specific. The version must follow
   * lexicographical order, thus the returned map will be order from the oldest known rules to the
   * newest available rules. The default 'TZDB' group uses version numbering consisting of the year
   * followed by a letter, such as '2009e' or '2012f'.
   *
   * Implementations must provide a result for each valid zone ID, however they do not have to
   * provide a history of rules. Thus the map will always contain one element, and will only contain
   * more than one element if historical rule information is available.
   *
   * @param zoneId
   *   the zone region ID as used by {@code ZoneId}, not null
   * @return
   *   a modifiable copy of the history of the rules for the ID, sorted from oldest to newest, not
   *   null
   * @throws ZoneRulesException
   *   if history cannot be obtained for the zone ID
   */
  def getVersions(zoneId: String): java.util.NavigableMap[String, ZoneRules] = {
    Objects.requireNonNull(zoneId, "zoneId")
    getProvider(zoneId).provideVersions(zoneId)
  }

  /**
   * Gets the provider for the zone ID.
   *
   * @param zoneId
   *   the zone region ID as used by {@code ZoneId}, not null
   * @return
   *   the provider, not null
   * @throws ZoneRulesException
   *   if the zone ID is unknown
   */
  private def getProvider(zoneId: String): ZoneRulesProvider = {
    val provider: ZoneRulesProvider = ZONES.get(zoneId)
    if (provider == null)
      if (ZONES.isEmpty)
        throw new ZoneRulesException("No time-zone data files registered")
      else
        throw new ZoneRulesException(s"Unknown time-zone ID: $zoneId")
    else
      provider
  }

  /**
   * Registers a zone rules provider.
   *
   * This adds a new provider to those currently available. A provider supplies rules for one or
   * more zone IDs. A provider cannot be registered if it supplies a zone ID that has already been
   * registered. See the notes on time-zone IDs in {@link ZoneId}, especially the section on using
   * the concept of a "group" to make IDs unique.
   *
   * To ensure the integrity of time-zones already created, there is no way to deregister providers.
   *
   * @param provider
   *   the provider to register, not null
   * @throws ZoneRulesException
   *   if a region is already registered
   */
  def registerProvider(provider: ZoneRulesProvider): Unit = {
    Objects.requireNonNull(provider, "provider")
    registerProvider0(provider)
    PROVIDERS.add(provider)
    ()
  }

  /**
   * Registers the provider.
   *
   * @param provider
   *   the provider to register, not null
   * @throws ZoneRulesException
   *   if unable to complete the registration
   */
  private def registerProvider0(provider: ZoneRulesProvider): Unit = {
    val zoneIds = provider.provideZoneIds.iterator
    while (zoneIds.hasNext) {
      val zoneId                 = zoneIds.next()
      Objects.requireNonNull(zoneId, "zoneId")
      val old: ZoneRulesProvider = ZONES.put(zoneId, provider)
      if (old != null)
        throw new ZoneRulesException(
          s"Unable to register zone as one already registered with that ID: $zoneId, currently loading from provider: $provider"
        )
    }
  }

  /**
   * Refreshes the rules from the underlying data provider.
   *
   * This method is an extension point that allows providers to refresh their rules dynamically at a
   * time of the applications choosing. After calling this method, the offset stored in any {@link
   * ZonedDateTime} may be invalid for the zone ID.
   *
   * Dynamic behavior is entirely optional and most providers, including the default provider, do
   * not support it.
   *
   * @return
   *   true if the rules were updated
   * @throws ZoneRulesException
   *   if an error occurs during the refresh
   */
  def refresh: Boolean = {
    var changed: Boolean = false
    val providers        = PROVIDERS.iterator
    while (providers.hasNext) {
      val provider = providers.next()
      changed |= provider.provideRefresh
    }
    changed
  }

  ZoneRulesInitializer.initialize();
}

/**
 * Provider of time-zone rules to the system.
 *
 * This class manages the configuration of time-zone rules. The static methods provide the public
 * API that can be used to manage the providers. The abstract methods provide the SPI that allows
 * rules to be provided.
 *
 * Rules are looked up primarily by zone ID, as used by {@link ZoneId}. Only zone region IDs may be
 * used, zone offset IDs are not used here.
 *
 * Time-zone rules are political, thus the data can change at any time. Each provider will provide
 * the latest rules for each zone ID, but they may also provide the history of how the rules
 * changed.
 *
 * <h3>Specification for implementors</h3> This interface is a service provider that can be called
 * by multiple threads. Implementations must be immutable and thread-safe.
 *
 * Providers must ensure that once a rule has been seen by the application, the rule must continue
 * to be available.
 *
 * Many systems would like to update time-zone rules dynamically without stopping the JVM. When
 * examined in detail, this is a complex problem. Providers may choose to handle dynamic updates,
 * however the default provider does not.
 */
abstract class ZoneRulesProvider protected () {

  /**
   * SPI method to get the available zone IDs.
   *
   * This obtains the IDs that this {@code ZoneRulesProvider} provides. A provider should provide
   * data for at least one region.
   *
   * The returned regions remain available and valid for the lifetime of the application. A dynamic
   * provider may increase the set of regions as more data becomes available.
   *
   * @return
   *   the unmodifiable set of region IDs being provided, not null
   * @throws ZoneRulesException
   *   if a problem occurs while providing the IDs
   */
  protected def provideZoneIds: java.util.Set[String]

  /**
   * SPI method to get the rules for the zone ID.
   *
   * This loads the rules for the region and version specified. The version may be null to indicate
   * the "latest" version.
   *
   * @param regionId
   *   the time-zone region ID, not null
   * @return
   *   the rules, not null
   * @throws DateTimeException
   *   if rules cannot be obtained
   */
  protected def provideRules(regionId: String, forCaching: Boolean): ZoneRules

  /**
   * SPI method to get the history of rules for the zone ID.
   *
   * This returns a map of historical rules keyed by a version string. The exact meaning and format
   * of the version is provider specific. The version must follow lexicographical order, thus the
   * returned map will be order from the oldest known rules to the newest available rules. The
   * default 'TZDB' group uses version numbering consisting of the year followed by a letter, such
   * as '2009e' or '2012f'.
   *
   * Implementations must provide a result for each valid zone ID, however they do not have to
   * provide a history of rules. Thus the map will always contain one element, and will only contain
   * more than one element if historical rule information is available.
   *
   * The returned versions remain available and valid for the lifetime of the application. A dynamic
   * provider may increase the set of versions as more data becomes available.
   *
   * @param zoneId
   *   the zone region ID as used by {@code ZoneId}, not null
   * @return
   *   a modifiable copy of the history of the rules for the ID, sorted from oldest to newest, not
   *   null
   * @throws ZoneRulesException
   *   if history cannot be obtained for the zone ID
   */
  protected def provideVersions(zoneId: String): java.util.NavigableMap[String, ZoneRules]

  /**
   * SPI method to refresh the rules from the underlying data provider.
   *
   * This method provides the opportunity for a provider to dynamically recheck the underlying data
   * provider to find the latest rules. This could be used to load new rules without stopping the
   * JVM. Dynamic behavior is entirely optional and most providers do not support it.
   *
   * This implementation returns false.
   *
   * @return
   *   true if the rules were updated
   * @throws DateTimeException
   *   if an error occurs during the refresh
   */
  protected def provideRefresh: Boolean = false
}
