# Diagnostic IDs

Every reported issue has a rule category and a rule ID. Categories (`Syntax`, `Error`, `Missing`,
`Warning`, and `Unused`) describe severity and broad behaviour. IDs identify a kind of finding and
are the stable names intended for configuration and tooling.

IDs use lower-case kebab-case and describe the condition rather than the source location that
reports it. Related messages share an ID when a user would reasonably want to configure them
together. For example, missing top-level and nested types are both `missing-type`. This keeps the
catalogue aligned with configuration choices instead of exposing an ID for every implementation
call site.

The catalogue is being populated incrementally. The first set covers parser errors, missing-name
diagnostics, and unused findings because these are common, actionable groups that users are most
likely to configure independently. An issue not yet assigned a catalogue ID returns its category
name from `Rule.id()`. This fallback preserves useful output while more diagnostic families are
catalogued without making existing messages depend on a flag-day conversion.

## Catalogue

| ID                         | Category | Finding                                                   |
|----------------------------|----------|-----------------------------------------------------------|
| `syntax-error`             | Syntax   | Apex lexer or parser error                                |
| `missing-type`             | Missing  | Type or nested type declaration cannot be found           |
| `inaccessible-type`        | Missing  | Type exists but is not accessible from the reference      |
| `wrong-type-arguments`     | Missing  | Generic type has the wrong number of arguments            |
| `missing-variable-or-type` | Missing  | A member lookup finds neither a variable nor a type        |
| `unknown-sobject-field`    | Missing  | Field is not present on an SObject                         |
| `unknown-field-or-type`    | Missing  | Member lookup finds neither a field nor a nested type      |
| `unused-local-variable`    | Unused   | Local variable has no references                           |
| `unused-type`              | Unused   | Apex class, interface, enum, or nested type is unused      |
| `unused-field`             | Unused   | Apex field or property is unused                           |
| `unused-method`            | Unused   | Apex method is unused                                      |
| `unused-label`             | Unused   | Custom label is unused                                     |

## CLI output

`CheckForIssues` uses the ID as the text diagnostic prefix and as the PMD `rule` attribute. JSON
messages retain `category` and include the ID separately as `id`. For uncatalogued diagnostics,
these ID positions contain the category fallback.
