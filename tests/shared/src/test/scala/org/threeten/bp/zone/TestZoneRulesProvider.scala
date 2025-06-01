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

import java.util.Collections

import org.scalatest.funsuite.AnyFunSuite
import org.threeten.bp.{ AssertionsHelper, ZoneOffset }

/** Test ZoneRulesProvider. */
object TestZoneRulesProvider {
  private[zone] class MockTempProvider extends ZoneRulesProvider {
    private[zone] final val rules: ZoneRules = ZoneOffset.of("+01:45").getRules

    def provideZoneIds: java.util.Set[String] =
      new java.util.HashSet[String](Collections.singleton("FooLocation"))

    protected def provideVersions(zoneId: String): java.util.NavigableMap[String, ZoneRules] = {
      val result: java.util.NavigableMap[String, ZoneRules] = new ZoneMap()
      result.put("BarVersion", rules)
      result
    }

    protected def provideRules(zoneId: String, forCaching: Boolean): ZoneRules = {
      if (zoneId == "FooLocation")
        return rules
      throw new ZoneRulesException("Invalid")
    }
  }

}

class TestZoneRulesProvider extends AnyFunSuite with AssertionsHelper {
  test("getAvailableGroupIds") {
    val zoneIds: java.util.Set[String]  = ZoneRulesProvider.getAvailableZoneIds
    assertTrue(zoneIds.contains("Europe/London"))
    try {
      zoneIds.clear()
      fail()
    } catch {
      case _: UnsupportedOperationException =>
      // ignore
    }
    val zoneIds2: java.util.Set[String] = ZoneRulesProvider.getAvailableZoneIds
    assertTrue(zoneIds2.contains("Europe/London"))
  }

  test("contains derived zone ids") {
    val zoneIds: java.util.Set[String] = ZoneRulesProvider.getAvailableZoneIds
    assertTrue(zoneIds.contains("US/Hawaii"))
    assertTrue(zoneIds.contains("Pacific/Honolulu"))
  }

  test("getRules_String") {
    val rules: ZoneRules  = ZoneRulesProvider.getRules("Europe/London", false)
    assertTrue(rules != null)
    val rules2: ZoneRules = ZoneRulesProvider.getRules("Europe/London", false)
    assertTrue(rules2 == rules)
  }

  test("getRules_String_unknownId") {
    assertThrows[ZoneRulesException] {
      ZoneRulesProvider.getRules("Europe/Lon", false)
    }
  }

  test("getRules_String_null") {
    assertThrows[NullPointerException] {
      ZoneRulesProvider.getRules(null, false)
    }
  }

  test("getVersions_String") {
    val versions: java.util.NavigableMap[String, ZoneRules]  =
      ZoneRulesProvider.getVersions("Europe/London")
    assertTrue(versions.size >= 1)
    val rules: ZoneRules                                     = ZoneRulesProvider.getRules("Europe/London", false)
    assertTrue(versions.lastEntry.getValue == rules)
    val copy                                                 = new java.util.HashMap[String, ZoneRules](versions)
    versions.clear()
    assertTrue(versions.size == 0)
    val versions2: java.util.NavigableMap[String, ZoneRules] =
      ZoneRulesProvider.getVersions("Europe/London")
    assertTrue(versions2 == copy)
  }

  test("getVersions_String_unknownId") {
    assertThrows[ZoneRulesException] {
      ZoneRulesProvider.getVersions("Europe/Lon")
    }
  }

  test("getVersions_String_null") {
    assertThrows[NullPointerException] {
      ZoneRulesProvider.getVersions(null)
    }
  }

  test("refresh") {
    assertTrue(!ZoneRulesProvider.refresh)
  }

  test("registerProvider") {
    val pre: java.util.Set[String]  = ZoneRulesProvider.getAvailableZoneIds
    assertTrue(!pre.contains("FooLocation"))
    ZoneRulesProvider.registerProvider(new TestZoneRulesProvider.MockTempProvider)
    val post: java.util.Set[String] = ZoneRulesProvider.getAvailableZoneIds
    assertTrue(post.contains("FooLocation"))
    assertTrue(ZoneRulesProvider.getRules("FooLocation", false) == ZoneOffset.of("+01:45").getRules)
  }
}
