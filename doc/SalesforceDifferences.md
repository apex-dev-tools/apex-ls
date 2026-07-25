# Differences From Salesforce Behaviour

This page records deliberate, known differences between how a project is
analysed and how the Salesforce platform behaves at deploy or runtime. These
are intentional design decisions, not defects; they are documented here so the
behaviour is not mistakenly "corrected", and so a diagnostic that differs from a
successful platform deploy can be understood.

## Package Directories

Each package directory listed in `sfdx-project.json` is treated as an
independent, ordered layer. An object's fields are resolved from the directory
that defines the object together with any earlier directories: a directory may
extend an object defined in an earlier directory, but a fields-only fragment
appearing before the directory that defines the object is not resolved (its
fields are reported as unknown). Where the same object or field is defined in
more than one directory, the last definition is used.

Salesforce instead composes metadata across all package directories into a
single deploy, merging field contributions regardless of directory order. That
change removed a previously useful ability to split metadata across directories
as separate deploys to work around the 10,000 component limit of a single
Metadata API deploy. That composition is deliberately not emulated; each
package directory is treated as if it were deployed independently and in order.

Practical consequences:
- Define an object before extending it: the directory containing the object's
  `.object-meta.xml` must be listed before any directory that only adds fields.
- Re-defining an object in a later directory replaces the earlier definition
  (last wins) and produces a warning.

Related issue: #339.
