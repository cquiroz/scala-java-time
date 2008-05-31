/*
 * Copyright (c) 2007,2008, Stephen Colebourne & Michael Nascimento Santos
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
package javax.time.calendar.field;

import java.io.Serializable;
import java.util.concurrent.atomic.AtomicReferenceArray;

import javax.time.calendar.Calendrical;
import javax.time.calendar.FlexiDateTime;
import javax.time.calendar.IllegalCalendarFieldValueException;
import javax.time.calendar.LocalTime;
import javax.time.calendar.TimeAdjustor;
import javax.time.calendar.DateTimeFieldRule;
import javax.time.calendar.TimeMatcher;
import javax.time.period.Periods;

/**
 * A representation of a second of minute in the ISO-8601 calendar system.
 * <p>
 * SecondOfMinute is an immutable time field that can only store a second of minute.
 * It is a type-safe way of representing a second of minute in an application.
 * <p>
 * Static factory methods allow you to construct instances.
 * The second of minute may be queried using getValue().
 * <p>
 * SecondOfMinute is thread-safe and immutable.
 *
 * @author Michael Nascimento Santos
 * @author Stephen Colebourne
 */
public final class SecondOfMinute
        implements Calendrical, Comparable<SecondOfMinute>, Serializable, TimeAdjustor, TimeMatcher {

    /**
     * The rule implementation that defines how the second of minute field operates.
     */
    public static final DateTimeFieldRule RULE = Rule.INSTANCE;
    /**
     * A serialization identifier for this instance.
     */
    private static final long serialVersionUID = 1L;
    /**
     * Cache of singleton instances.
     */
    private static final AtomicReferenceArray<SecondOfMinute> cache = new AtomicReferenceArray<SecondOfMinute>(60);

    /**
     * The second of minute being represented.
     */
    private final int secondOfMinute;

    //-----------------------------------------------------------------------
    /**
     * Obtains an instance of <code>SecondOfMinute</code>.
     *
     * @param secondOfMinute  the second of minute to represent, from 0 to 59
     * @return the created SecondOfMinute
     * @throws IllegalCalendarFieldValueException if the secondOfMinute is invalid
     */
    public static SecondOfMinute secondOfMinute(int secondOfMinute) {
        try {
            SecondOfMinute result = cache.get(secondOfMinute);
            if (result == null) {
                SecondOfMinute temp = new SecondOfMinute(secondOfMinute);
                cache.compareAndSet(secondOfMinute, null, temp);
                result = cache.get(secondOfMinute);
            }
            return result;
        } catch (IndexOutOfBoundsException ex) {
            throw new IllegalCalendarFieldValueException(
                RULE.getName(), secondOfMinute, RULE.getMinimumValue(), RULE.getMaximumValue());
        }
    }

    //-----------------------------------------------------------------------
    /**
     * Constructs an instance with the specified second of minute.
     *
     * @param secondOfMinute  the second of minute to represent
     */
    private SecondOfMinute(int secondOfMinute) {
        this.secondOfMinute = secondOfMinute;
    }

    /**
     * Resolve the singleton.
     *
     * @return the singleton, never null
     */
    private Object readResolve() {
        return secondOfMinute(secondOfMinute);
    }

    //-----------------------------------------------------------------------
    /**
     * Gets the second of minute value.
     *
     * @return the second of minute, from 0 to 59
     */
    public int getValue() {
        return secondOfMinute;
    }

    //-----------------------------------------------------------------------
    /**
     * Converts this field to a <code>FlexiDateTime</code>.
     *
     * @return the flexible date-time representation for this instance, never null
     */
    public FlexiDateTime toFlexiDateTime() {
        return new FlexiDateTime(RULE, getValue());
    }

    //-----------------------------------------------------------------------
    /**
     * Compares this second of minute instance to another.
     *
     * @param otherSecondOfMinute  the other second of minute instance, not null
     * @return the comparator value, negative if less, postive if greater
     * @throws NullPointerException if otherSecondOfMinute is null
     */
    public int compareTo(SecondOfMinute otherSecondOfMinute) {
        int thisValue = this.secondOfMinute;
        int otherValue = otherSecondOfMinute.secondOfMinute;
        return (thisValue < otherValue ? -1 : (thisValue == otherValue ? 0 : 1));
    }

    //-----------------------------------------------------------------------
    /**
     * Is this instance equal to that specified, evaluating the second of minute.
     *
     * @param otherSecondOfMinute  the other second of minute instance, null returns false
     * @return true if the second of minute is the same
     */
    @Override
    public boolean equals(Object otherSecondOfMinute) {
        if (this == otherSecondOfMinute) {
            return true;
        }
        if (otherSecondOfMinute instanceof SecondOfMinute) {
            return secondOfMinute == ((SecondOfMinute) otherSecondOfMinute).secondOfMinute;
        }
        return false;
    }

    /**
     * A hashcode for the second of minute object.
     *
     * @return a suitable hashcode
     */
    @Override
    public int hashCode() {
        return secondOfMinute;
    }

    /**
     * A string describing the second of minute object.
     *
     * @return a string describing this object
     */
    @Override
    public String toString() {
        return "SecondOfMinute=" + getValue();
    }

    //-----------------------------------------------------------------------
    /**
     * Adjusts a time to have the the second of minute represented by this object,
     * returning a new time.
     * <p>
     * Only the second of minute field is adjusted in the result. The other time
     * fields are unaffected.
     * <p>
     * This instance is immutable and unaffected by this method call.
     *
     * @param time  the time to be adjusted, not null
     * @return the adjusted time, never null
     */
    public LocalTime adjustTime(LocalTime time) {
        if (this == time.getSecondOfMinute()) {
            return time;
        }
        return LocalTime.time(time.getHourOfDay(), time.getMinuteOfHour(), this, time.getNanoOfSecond());
    }

    /**
     * Checks if the input time has the same second of minute that is represented
     * by this object.
     *
     * @param time  the time to match, not null
     * @return true if the time matches, false otherwise
     */
    public boolean matchesTime(LocalTime time) {
        return this == time.getSecondOfMinute();
    }

    //-----------------------------------------------------------------------
    /**
     * Implementation of the rules for the second of minute field.
     */
    private static class Rule extends DateTimeFieldRule implements Serializable {
        /** Singleton instance. */
        private static final DateTimeFieldRule INSTANCE = new Rule();
        /** A serialization identifier for this class. */
        private static final long serialVersionUID = 1L;
        /** Constructor. */
        private Rule() {
            super("SecondOfMinute", Periods.SECONDS, Periods.MINUTES, 0, 59);
        }
        private Object readResolve() {
            return INSTANCE;
        }
        /** {@inheritDoc} */
        @Override
        protected Integer extractValue(FlexiDateTime dateTime) {
            return dateTime.getTime() != null ? dateTime.getTime().getSecondOfMinute().getValue() : null;
        }
    }

}
