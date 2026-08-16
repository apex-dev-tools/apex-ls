# XML Element Locations

`XMLElementLike.location` reports the complete lexical range of an XML element when the original
source can be associated safely with the parsed element tree. The range begins at the opening `<`
and ends immediately after the matching closing tag, or immediately after `/>` for a self-closing
element. It is therefore a half-open range suitable for slicing the original file.

Locations use the existing `Location` representation: lines are one-based and columns are
zero-based Unicode code-point offsets. This matches `SourceData` and ANTLR source extraction. In
particular, a non-BMP character occupies one column on both JVM and Scala.js even though a Scala or
JavaScript string stores it as two UTF-16 code units. LF, CRLF, and CR line endings are each treated
as one line break.

Ranges are indexed from the original, untrimmed file. Leading whitespace therefore contributes to
line and column coordinates even though the semantic XML parsers continue to trim it for parser
compatibility. For indexed elements, the retained `line` API is the same as
`location.startLine`. External `XMLElementLike` implementations remain source compatible because
`location` defaults to the existing point location `Location(line)`.

The shared lexical scanner handles quoted attributes, entity references, comments, CDATA,
processing instructions, XML declarations, DOCTYPE internal subsets, namespaces, nesting, and
self-closing tags. The resulting lexical tree is associated with the JVM scala-xml or Scala.js
xmldom tree only when qualified/local names, available resolved namespaces, child order, and the
complete tree shape agree. If scanning or whole-tree association fails, no partial exact ranges
are used and elements retain their previous point-location behavior. Malformed XML diagnostics
also remain point based.

This API does not change `Location` constructors, equality, sentinels, or four-field serialization.
It also does not migrate label generation, SObject generation, or XML validation diagnostics to
consume the new ranges; those consumer changes remain separate work.
