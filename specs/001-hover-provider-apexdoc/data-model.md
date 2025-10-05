# Data Model — Hover Provider ApexDoc Rendering

## Overview
Hover responses will continue to expose the existing symbol signature text while appending structured ApexDoc details. The formatter produces a deterministic Markdown string derived from an intermediate model so both JVM and Scala.js builds can share fixtures.

## Entities

### HoverDocPayload
| Field | Type | Description |
|-------|------|-------------|
| `signature` | `String` | Existing `Locatable.toString` output for the hovered symbol (class/constructor/method). |
| `apexDoc` | `Option[ApexDocBundle]` | Present when a compliant ApexDoc block is discovered; `None` when no valid documentation exists. |
| `markdown` | `String` | Final Markdown sent to clients, composed from `signature` followed by rendered documentation sections. |
| `location` | `Location` | Source extent already supplied in `HoverItem`; preserved unchanged. |

### ApexDocBundle
| Field | Type | Description |
|-------|------|-------------|
| `summary` | `Option[MarkdownParagraph]` | Cleaned paragraph derived from the free-form ApexDoc description. |
| `parameters` | `Seq[ApexDocParam]` | One entry per valid `@param`. Missing names or descriptions cause the tag to be dropped. |
| `returns` | `Option[MarkdownParagraph]` | Sanitized text extracted from `@return`. Constructors omit this section. |
| `throws` | `Seq[MarkdownParagraph]` | Distinct entries for each `@throws` tag. |
| `seeAlso` | `Seq[SanitizedLink]` | Links derived from `@see`, `@link`, or inline Markdown-style links inside text. |
| `otherTags` | `Seq[ApexDocTag]` | Additional Salesforce-supported tags (e.g., `@author`, `@since`) kept when valid. |

### ApexDocParam
| Field | Type | Description |
|-------|------|-------------|
| `name` | `String` | Apex parameter identifier as declared in the method/constructor signature. |
| `description` | `MarkdownParagraph` | Sanitized explanation text. |

### ApexDocTag
| Field | Type | Description |
|-------|------|-------------|
| `label` | `String` | Canonical tag name (e.g., `author`, `since`). |
| `value` | `MarkdownParagraph` | Sanitized text payload for the tag. |

### MarkdownParagraph
Simple wrapper representing sanitized Markdown-safe text. Escapes HTML entities, normalizes whitespace, and preserves Salesforce-supported inline emphasis.

### SanitizedLink
| Field | Type | Description |
|-------|------|-------------|
| `label` | `String` | Visible link text (falls back to URL when none supplied). |
| `url` | `String` | Whitelisted `http`/`https` URL with encoded brackets/parentheses. |

## Relationships & Constraints
- `HoverDocPayload.markdown` MUST be generated exclusively from the other fields to guarantee deterministic output.
- `ApexDocParam.name` MUST match a method/constructor parameter; mismatches invalidate the tag.
- `SanitizedLink` entries are appended to the Markdown representation as `[label](url)`; invalid schemes downgrade to plain text and are excluded from `seeAlso`.
- Partial ApexDoc blocks result in truncated models: only successfully parsed sections populate the bundle.
- Empty collections (e.g., `throws`, `seeAlso`) are omitted when composing Markdown to avoid blank headers.

## Rendering Rules
1. Begin the hover Markdown with the original signature on line 1.
2. Insert a blank line before ApexDoc content when present.
3. Emit sections in the following order: Summary, Parameters, Returns, Throws, Other Tags, See Also.
4. Each section label is bold (e.g., `**Parameters**`). Parameter entries render as bullet list items with `` `name` — description `` formatting.
5. Sanitized links use Markdown link syntax and escape backticks, brackets, and parentheses inside labels.
6. When no valid ApexDoc content exists, `markdown` equals `signature` to preserve legacy behavior.

## Determinism Considerations
- Parser treats input as UTF-8 and operates on immutable slices of the original source string; no clock or file system reads are performed.
- Sorting is stable: parameters follow method declaration order; other tag collections preserve original appearance order to match user expectations.
