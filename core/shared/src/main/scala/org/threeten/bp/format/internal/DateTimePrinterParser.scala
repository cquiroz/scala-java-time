package org.threeten.bp.format.internal

import java.lang.StringBuilder

/**
 * Strategy for printing/parsing date-time information.
 *
 * The printer may print any part, or the whole, of the input date-time object. Typically, a
 * complete print is constructed from a number of smaller units, each outputting a single field.
 *
 * The parser may parse any piece of text from the input, storing the result in the context.
 * Typically, each individual parser will just parse one field, such as the day-of-month, storing
 * the value in the context. Once the parse is complete, the caller will then convert the context to
 * a {@link DateTimeBuilder} to merge the parsed values to create the desired object, such as a
 * {@code LocalDate}.
 *
 * The parse position will be updated during the parse. Parsing will start at the specified index
 * and the return value specifies the new parse position for the next parser. If an error occurs,
 * the returned index will be negative and will have the error position encoded using the complement
 * operator.
 *
 * <h3>Specification for implementors</h3> This interface must be implemented with care to ensure
 * other classes operate correctly. All implementations that can be instantiated must be final,
 * immutable and thread-safe.
 *
 * The context is not a thread-safe object and a new instance will be created for each print that
 * occurs. The context must not be stored in an instance variable or shared with any other threads.
 */
private[format] trait DateTimePrinterParser {

  /**
   * Prints the date-time object to the buffer.
   *
   * The context holds information to use during the print. It also contains the date-time
   * information to be printed.
   *
   * The buffer must not be mutated beyond the content controlled by the implementation.
   *
   * @param context
   *   the context to print using, not null
   * @param buf
   *   the buffer to append to, not null
   * @return
   *   false if unable to query the value from the date-time, true otherwise
   * @throws DateTimeException
   *   if the date-time cannot be printed successfully
   */
  def print(context: TTBPDateTimePrintContext, buf: StringBuilder): Boolean

  /**
   * Parses text into date-time information.
   *
   * The context holds information to use during the parse. It is also used to store the parsed
   * date-time information.
   *
   * @param context
   *   the context to use and parse into, not null
   * @param text
   *   the input text to parse, not null
   * @param position
   *   the position to start parsing at, from 0 to the text length
   * @return
   *   the new parse position, where negative means an error with the error position encoded using
   *   the complement ~ operator
   * @throws NullPointerException
   *   if the context or text is null
   * @throws IndexOutOfBoundsException
   *   if the position is invalid
   */
  def parse(context: TTBPDateTimeParseContext, text: CharSequence, position: Int): Int
}
