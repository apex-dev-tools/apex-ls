# Location Ranges

`Location` retains its existing one-, two-, and four-argument `apply` overloads, `empty` and `all`
sentinels, structural equality and hashing, and four-field serialized representation. Named
factories make new call-site intent explicit:

- `Location.point(line, column)` creates a zero-length point.
- `Location.wholeLine(line)` creates a half-open range over a line's content. Its end column is
  `Int.MaxValue`, an explicit sentinel in the same style as `Location.all`.
- `Location.span(startLine, startColumn, endLine, endColumn)` creates a half-open range whose start
  is inclusive and end is exclusive.

Lines are one-based. Columns are zero-based Unicode code-point offsets, so JVM and Scala.js use the
same coordinates for non-BMP text.

`Location.extract(source, location)` slices source using this coordinate model. It clamps columns
to the line's content before LF, CRLF, or CR terminators. A `wholeLine` range therefore returns the
line content without consuming its following newline. Locations ending beyond the available lines,
including `Location.all`, clamp to the end of the source.
