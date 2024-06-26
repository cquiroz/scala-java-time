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
package org.threeten.bp

import java.io.DataInputStream
import java.io.FileInputStream
import java.io.FileNotFoundException
import java.util.Collections
import java.util.GregorianCalendar
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.locks.ReentrantLock
import java.util.regex.Pattern
import java.util.zip.ZipInputStream
import org.threeten.bp.temporal.TemporalAccessor

/**
 * Test Class loading. Use "-verbose:class".
 */
object ClassLoaderChecker {
  def main(args: Array[String]): Unit = {
    var a: Any = new ConcurrentHashMap[AnyRef, AnyRef]
    a.toString
    a = new ReentrantLock
    a.toString
    a = Pattern.compile("hello[a-z][^f]{2}").matcher("goo").matches
    a.toString
    a = new java.util.HashMap[AnyRef, AnyRef]().entrySet
    a.toString
    a = new java.util.HashMap[AnyRef, AnyRef]().values
    a.toString
    a = new java.util.HashMap[AnyRef, AnyRef]().keySet
    a.toString
    a = new java.util.TreeMap[AnyRef, AnyRef]().entrySet
    a.toString
    a = new java.util.TreeMap[AnyRef, AnyRef]().values
    a.toString
    a = new java.util.TreeMap[AnyRef, AnyRef]().keySet
    a.toString
    a = Collections.unmodifiableMap[AnyRef, AnyRef](new java.util.HashMap[AnyRef, AnyRef]).entrySet
    a.toString
    a = Collections.unmodifiableMap[AnyRef, AnyRef](new java.util.HashMap[AnyRef, AnyRef]).values
    a.toString
    a = Collections.unmodifiableMap[AnyRef, AnyRef](new java.util.HashMap[AnyRef, AnyRef]).keySet
    a.toString
    a = Collections.unmodifiableList(new java.util.ArrayList[AnyRef])
    a.toString
    a = Collections.unmodifiableList(new java.util.ArrayList[AnyRef]).iterator
    a.toString
    try a = new DataInputStream(new ZipInputStream(new FileInputStream("/a.zip")))
    catch {
      case _: FileNotFoundException =>
    }
    a.toString
    System.out.println("************************************************************")
    a = classOf[TemporalAccessor]
    System.out.println("************************************************************")
    Month.of(5)
    System.out.println("************************************************************")
    a = classOf[LocalDate]
    System.out.println("************************************************************")
    val d: LocalDate       = LocalDate.of(2011, 12, 20)
    System.out.println("************************************************************")
    val t: LocalTime       = LocalTime.of(12, 20)
    System.out.println("************************************************************")
    val ldt: LocalDateTime = LocalDateTime.of(d, t)
    System.out.println("************************************************************")
    a = classOf[GregorianCalendar]
    System.out.println("************************************************************")
    new GregorianCalendar
    System.out.println("************************************************************")
    ldt.atZone(ZoneId.of("Europe/Paris"))
    System.out.println("************************************************************")
  }
}
