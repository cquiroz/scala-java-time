package org.threeten.bp.zone

import org.threeten.bp.ZoneOffset

/**
 * Minimal provider for UTC
 */
final class DefaultTzdbZoneRulesProvider extends ZoneRulesProvider {

  override protected def provideZoneIds: java.util.Set[String] = {
    val zones = new java.util.HashSet[String]
    zones.add("UTC")
    zones.add("GMT")
    zones
  }

  override protected def provideRules(regionId: String, forCaching: Boolean): ZoneRules =
    ZoneRules.of(ZoneOffset.UTC,
                 ZoneOffset.UTC,
                 new java.util.ArrayList,
                 new java.util.ArrayList,
                 new java.util.ArrayList
    )

  override protected def provideVersions(
    zoneId: String
  ): java.util.NavigableMap[String, ZoneRules] = {
    val r = new ZoneMap[String, ZoneRules]
    // FIXME the version should be provided by the db
    r.put("2017c", provideRules("UTC", true))
    r
  }
}
