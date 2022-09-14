# Apex Language Server

TODO short description - link to longer docs

## Getting Started

### Installation

Scala:

  ```scala
  // Replace %% with %%% to use ScalaJS build
  libraryDependencies += "io.github.apex-dev-tools" %% "apex-ls" % "X.X.X"
  ```

### Usage

TODO features, api use, standalone check

## Development

### Building

This project can be cross built for JVM and JS use. SBT commands are aggregated, but can also be executed separately with `sbt apexlsJVM/[cmd]` or `sbt apexlsJS/[cmd]`.

Available build commands:

* `sbt build` - Creates packaged jar or js bundle for testing and release.
* `sbt apexlsJS/Dev/build` - Creates fast optimised js bundle for debugging.
* `sbt pack` / `sbt "pack [version]"` - Do a local published release of the most recent tag or given value.
  * **WARNING:** This can override the remote releases, clear your `~/.ivy2/local` directory to revert.
* `sbt test` - Execute full test run.
* `sbt clean` - Removes most build files and artifacts.

### Testing

#### Samples

TODO

### Release

Releases are automated via workflow on publishing a release. Create a `v` prefixed tag at the same time on the commit to be released (e.g. `v1.0.0`).

Snapshot releases can also be created at any time by executing the `Publish` workflow on a branch. The versioning will be in the format `X.X.X+Y-yyyy-SNAPSHOT`; the latest tag followed by recent commit info.

## License

The source code forked from [apex-link](https://github.com/nawforce/apex-link) & [pkgforce](https://github.com/nawforce/pkgforce) uses a 3-clause BSD licence. There are two external contributions:

* The Apex Antlr4 grammar was originally from [Tooling-force.com](https://github.com/neowit/tooling-force.com), although the version used is now markedly different from the original.  
* The antlr4c3 CodeCompletionCore.java has been embedded under a MIT licence.
