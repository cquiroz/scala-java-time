
## Scala Java-Time

![build](https://github.com/cquiroz/scala-java-time/workflows/build/badge.svg)
[![Maven Central](https://img.shields.io/maven-central/v/io.github.cquiroz/scala-java-time_2.11.svg)](https://maven-badges.herokuapp.com/maven-central/io.github.cquiroz/scala-java-time_2.11)
[![Scala.js](https://www.scala-js.org/assets/badges/scalajs-1.0.0.svg)](https://www.scala-js.org/)

This project provides an implementation of the `java.time` package, a date and time library that was added in Java 8.
The implementation is based on the original BSD-licensed reference implementation (before it was contributed to OpenJDK).

#### Usage

The *scala-java-time* library is currently available for Scala (JVM, version 8 and later), Scala.js (JavaScript) and Scala Native (LLVM).
Scala 2.11, Scala 2.12, Scala 2.13 and Scala 3.0.0 are supported.

To get started with SBT, add one (or both) of these dependencies:

- `libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.6.0"`
- `libraryDependencies += "io.github.cquiroz" %%% "scala-java-time-tzdb" % "2.6.0"` (optional)

The second dependency contains all time zones available from IANA Time Zone Database. It can be removed if your code doesn't use timezones.
Note that the timezone db is fairly large and due to the characteristics of the API it’s not very ammenable to optimization.
This database is published every now and then so it may be old. For current version and more possibilities, pleae see the [Time Zone section in the documentation](https://cquiroz.github.io/scala-java-time/#:~:text=Time%20zones).

On Scala.js with Scala 3.10.1 or newer, you can use `scala-java-time-tzdb-scala3-next` instead of `scala-java-time-tzdb`.
It contains the same time zones, compiled with Scala 3.10.1, and also links on WebAssembly without a JavaScript host.
Use only one of the two artifacts: they contain the same classes.


#### Documentation

See the [documentation](http://cquiroz.github.io/scala-java-time/)
