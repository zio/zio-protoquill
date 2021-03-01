## Future Dotty-Based Quill Implementation

Compile using sbt `compile` and `test:compile` normally. Currently on M3.

This project should work with metals. To import, go to the
project directory and type `code .` to open VS Code
(note that it will have to be configured beforehand).

The functionality that is currently available is:
 - Equivalent to quill-sql
 - query, insert, delete, update basic functionality
 - basic lift functionality `lift(scalar)` and `insert(lift(caseClass))`.
 - basic parsing of quoted clauses, see `Parser.scala` for details

For further information, watch:
 - [ScQuilL Sessions - Quill, Dotty, and Macros](https://www.youtube.com/watch?v=0PSg__PPjY8&list=PLqky8QybCVQYNZY_MNJpkjFKT-dAdHQDX) - A tutorial on developing Dotty-Quill from scratch (covers quoting, liftables, and liftables).
 - [Quill, Dotty, And The Awesome Power of 'Inline'](https://www.youtube.com/watch?v=SmBpGkIsJIU) - Many examples of new things that can be done with this library that cannot be done with standard Quill.
