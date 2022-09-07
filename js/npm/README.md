# Apex LS

Provides utility functions for handling collections of Salesforce metadata.

## Node API

The library is mostly written in Scala to support dual building for Java & Node. This model works well when you are also using Scala and is usable from Java but is more awkward for Node clients. To overcome this a small part of the library for resolving type names to paths is exposed in a Node friendly NPM module.

To use this, first create a workspace:

    const workspace = Workspaces.get("mydirectory")  // Will throw on errors

Call findType on the workspace:

    const fooPath = workspace.findType("ns001.Foo")  // Returns null if type is unknown

A workspace here is simply the directory containing Salesforce metadata, typically it's the directory in which sfdx-project.json resides.
