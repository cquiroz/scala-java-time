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

import javax.time.calendar.Calendrical;
import javax.time.calendar.FlexiDateTime;
import javax.time.calendar.IllegalCalendarFieldValueException;
import javax.time.calendar.DateTimeFieldRule;

/**
 * A calendrical representation of a second of day.
 * <p>
 * SecondOfDay is an immutable time field that can only store a second of day.
 * It is a type-safe way of representing a second of day in an application.
 * <p>
 * Static factory methods allow you to construct instances.
 * The second of day may be queried using getValue().
 * <p>
 * SecondOfDay is thread-safe and immutable.
 *
 * @author Stephen Colebourne
 */
public final class SecondOfDay implements Calendrical, Comparable<SecondOfDay>, Serializable {

    /**
     * The rule implementation that defines how the second of day field operates.
     */
    public static final DateTimeFieldRule RULE = new Rule();
    /**
     * A serialization identifier for this instance.
     */
    private static final long serialVersionUID = 1L;

    /**
     * The second of day being represented.
     */
    private final int secondOfDay;

    //-----------------------------------------------------------------------
    /**
     * Obtains an instance of <code>SecondOfDay</code>.
     *
     * @param secondOfDay  the second of day to represent, from 0 to 86399
     * @return the created SecondOfDay
     * @throws IllegalCalendarFieldValueException if the secondOfDay is invalid
     */
    public static SecondOfDay secondOfDay(int secondOfDay) {
        RULE.checkValue(secondOfDay);
        return new SecondOfDay(secondOfDay);
    }

    //-----------------------------------------------------------------------
    /**
     * Constructs an instance with the specified second of day.
     *
     * @param secondOfDay  the second of day to represent
     */
    private SecondOfDay(int secondOfDay) {
        this.secondOfDay = secondOfDay;
    }

    //-----------------------------------------------------------------------
    /**
     * Gets the second of day value.
     *
     * @return the second of day, from 0 to 86399
     */
    public int getValue() {
        return secondOfDay;
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
     * Compares this second of day instance to another.
     *
     * @param otherSecondOfDay  the other second of day instance, not null
     * @return the comparator value, negative if less, postive if greater
     * @throws NullPointerException if otherSecondOfDay is null
     */
    public int compareTo(SecondOfDay otherSecondOfDay) {
        int thisValue = this.secondOfDay;
        int otherValue = otherSecondOfDay.secondOfDay;
        return (thisValue < otherValue ? -1 : (thisValue == otherValue ? 0 : 1));
    }

    //-----------------------------------------------------------------------
    /**
     * Is this instance equal to that specified, evaluating the second of day.
     *
     * @param otherSecondOfDay  the other second of day instance, null returns false
     * @return true if the second of day is the same
     */
    @Override
    public boolean equals(Object otherSecondOfDay) {
        if (this == otherSecondOfDay) {
            return true;
        }
        if (otherSecondOfDay instanceof SecondOfDay) {
            return secondOfDay == ((SecondOfDay) otherSecondOfDay).secondOfDay;
        }
        return false;
    }

    /**
     * A hashcode for the second of day object.
     *
     * @return a suitable hashcode
     */
    @Override
    public int hashCode() {
        return secondOfDay;
    }

    /**
     * A string describing the second of day object.
     *
     * @return a string describing this object
     */
    @Override
    public String toString() {
        return "SecondOfDay=" + getValue();
    }

    //-----------------------------------------------------------------------
    /**
     * Implementation of the rules for the second of day field.
     */
    private static class Rule extends DateTimeFieldRule {

        /** Constructor. */
        protected Rule() {
            super("SecondOfDay", null, null, 0, 86399);
        }
    }

}
