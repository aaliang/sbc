Requirements:
-------------
- SBT
- Java - I am on Java 8, it should work for previous versions but there were some
 string formatting bugs with BigDecimal that were addressed in Java 8 which may or may not have effects
 on different distros of the JDK

Build/run:
----------
Probably most straightforward using sbt:

> $ sbt run
# will run in "repl" mode. No variables are stored, just a continuous run of the below invocation

You can then run expressions via stdin

> $ >>1+2

> $ sbt run "1+3"
# will evaluate the expression provided