# Updating Types For New API Versions

Apex LS depends on two type libraries - [standard-types](https://github.com/apex-dev-tools/standard-types) and [sobject-types](https://github.com/apex-dev-tools/sobject-types). These contain stub definitions for the Salesforce API types. The language server can validate against these types offline without an org.

* Standard types has all the Apex namespaces you find in the docs, like `System`, `Database`, `Schema`.

* SObject types are generated from an org using describe calls. They then get converted into the stub format extending `System.SObject`.

Of course, with every Salesforce release an update is required.

## Update Process for Standard (Platform Types)

We can't get platform type information out of the Tooling API in the same way as deployed types. So the fall-back is reviewing the release notes and then the Apex reference to get namespace, class, and method signatures.

Important areas of the [Release Notes](https://help.salesforce.com/s/articleView?id=release-notes.salesforce_release_notes.htm):

* `Development > Apex`
    * This is additional context of new language features, which can result in a language server and/or parser change.

* `Development > New and Changed Items for Developers > Apex: New and Changed Items`
    * This is the updated namespaces and classes, which can then be looked up in the [Apex Reference Guide](https://developer.salesforce.com/docs/atlas.en-us.apexref.meta/apexref/apex_ref_guide.htm).
    * There are `ConnectApi` class updates which may be noted in an adjacent section.

Other pages under `Development` may describe beta features that have Apex APIs and namespaces, which often have different documentation outside of the main Apex Reference. Typically this is Analytics or AI (Einstein/Wave).

See [standard-types](https://github.com/apex-dev-tools/standard-types) `Development` README section for help making the classes. Also see previous PRs for an example of how updates look. e.g. [API 61](https://github.com/apex-dev-tools/standard-types/pull/26/files).

## Update Process for SObject Types

Once there is a locally published copy of `standard-types` this can be used in `sobject-types`.

SObjects can be retrieved from an org via describe calls, see [sobject-types](https://github.com/apex-dev-tools/sobject-types) `Development` README section for details on set up. The generated files are not perfect, they can contain duplicate fields of either the same type, or different types. Type names can also be wrong.

There will be missing fields that are present in the current checked in types. The changes have to be reviewed and corrected. Many new additions are the same fields added to hundreds of classes, so there is potential to automate. However, diff/compare tools will show overwrites where the generated class adds a line next to another it did not generate - these need to be appended instead.

A file template is available for creating SObject stubs from scratch, though this is rare.

### Org Features

Any missing types can be attributed to the org used. Some SObjects and fields are only available when features are enabled. E.g. switching to a multi currency org adds currency code fields to every object. This is problematic for offline tools, since they need to capture every possible state in advance, and can't be certain if code with certain SObject references will compile without checking the org state first.

The types repo contains a scratch org definition with as many features as possible, though this is cut down from the documented features based on what `sf` cli intrepets as valid. This config needs refreshing as Salesforce add more possible features.

## Final Testing

After updating the library versions, one test in `PlatformTypesValidationTest` will always fail and need updating - counting the number of types.

The main test for the types being built correctly is `All outer types are valid`. Most commonly this will fail with `Reference to non-platform type java.lang...` errors caused by a missing import of the Apex type of the same name.

The test `Exceptions are valid` can also fail if an `extends Exception` was missed on type names ending Exception - all of these classes should be a `System.Exception`.
