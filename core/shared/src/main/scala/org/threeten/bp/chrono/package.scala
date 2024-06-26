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

/**
 * Support for calendar systems other than the default ISO.
 *
 * The main API is based around the calendar system defined in ISO-8601. This package provides
 * support for alternate systems.
 *
 * The supported calendar systems includes:
 *
 * -{@linkplain org.threeten.bp.chrono.HijrahChronology Hijrah calendar}
 * -{@linkplain org.threeten.bp.chrono.JapaneseChronology Japanese calendar}
 * -{@linkplain org.threeten.bp.chrono.MinguoChronology Minguo calendar}
 * -{@linkplain org.threeten.bp.chrono.ThaiBuddhistChronology Thai Buddhist calendar}
 *
 * It is intended that applications use the main API whenever possible, including code to read and
 * write from a persistent data store, such as a database, and to send dates and times across a
 * network. This package is then used at the user interface level to deal with localized
 * input/output. See {@link org.threeten.bp.chrono.ChronoLocalDate ChronoLocalDate} for a full
 * discussion of the issues.
 *
 * ==Example==
 *
 * This example creates and uses a date in a non-ISO calendar system.
 *
 * <pre> // Print the Thai Buddhist date ChronoLocalDate now1 =
 * ThaiBuddhistChronology.INSTANCE.now(); int day = now1.get(ChronoField.DAY_OF_MONTH); int dow =
 * now1.get(ChronoField.DAY_OF_WEEK); int month = now1.get(ChronoField.MONTH_OF_YEAR); int year =
 * now1.get(ChronoField.YEAR); System.out.printf(" Today is %s %s %d-%s-%d%n",
 * now1.getChronology().getId(), dow, day, month, year);
 *
 * // Enumerate the list of available calendars and print today for each Set&lt;String&gt; names =
 * Chronology.getAvailableIds(); for (String name : names) { Chronology&lt;?&gt; chrono =
 * Chronology.of(name); ChronoLocalDate&lt;?&gt; date = chrono.now(); System.out.printf(" %20s:
 * %s%n", chrono.getId(), date.toString()); }
 *
 * // Print today's date and the last day of the year for the Thai Buddhist Calendar.
 * ChronoLocalDate first = now1 .with(ChronoField.DAY_OF_MONTH, 1) .with(ChronoField.MONTH_OF_YEAR,
 * 1); ChronoLocalDate last = first .plus(1, ChronoUnit.YEARS) .minus(1, ChronoUnit.DAYS);
 * System.out.printf(" %s: 1st of year: %s; end of year: %s%n", last.getChronology().getId(), first,
 * last); </pre>
 */
package object chrono
