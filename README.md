# Apex Language Server

The Apex Language Server library provides a collection of tools to aid development of Salesforce Apex projects. Check for errors, find types, suggest code completions and more.

## Getting Started

### Installation

Releases are available from [SonaType](https://s01.oss.sonatype.org). You will need to add the repository to your build tool.

Scala:

  ```scala
  // Add if not present
  ThisBuild / resolvers ++= Resolver.sonatypeOssRepos("releases")

  project.settings(
    // Replace %% with %%% to use ScalaJS build
    libraryDependencies += "io.github.apex-dev-tools" %% "apex-ls" % "X.X.X"
  )
  ```

Maven:

  ```xml
  <!-- In <repositories/> -->
  <repository>
    <id>oss.sonatype.org</id>
    <url>https://s01.oss.sonatype.org/content/repositories/releases</url>
    <releases>
      <enabled>true</enabled>
    </releases>
  </repository>

  <!-- In <dependencies/> -->
  <dependency>
    <groupId>io.github.apex-dev-tools</groupId>
    <artifactId>apex-ls</artifactId>
    <version>X.X.X</version>
  </dependency>
  ```

### Usage

The library can be consumed in JVM and ScalaJS projects, however the features available to each differ. See the JavaDoc for more details on the API. <!-- TODO link to hosted javadoc -->

The jar is also executable without a client:

  ```sh
  # Assuming dep jars are in the same directory
  java -cp "apex-ls*.jar" io.github.apexdevtools.apexls.Main [args] <directory>
  ```

The chosen directory should contain an `sfdx-project.json`. The following arguments are available:

| Argument | Description |
| --- | --- |
| `-json` | Write output as JSON. Logging is suppressed. |
| `-verbose` | Include warnings in log output. |
| `-info` / `-debug` | Change log level from default. |
| `-nocache` | Do not load from or write to an existing apex-ls cache. |
| `-unused` | Display unused value warnings. (Requires `-verbose`) |
| `-depends` | Display apex type dependencies as either csv or json if `-json` is set. |
| `-outlinesingle` / `-outlinemulti` | Use the apex outline parser in single or multi threaded mode. Otherwise uses default ANTLR parser. |

## Development

### Building

The build is a cross project for JS and JVM; SBT commands are aggregated, but can also be executed separately with `sbt apexlsJVM/[cmd]` or `sbt apexlsJS/[cmd]`.

Available build commands:

* `sbt build` - Creates packaged jar or js bundle for testing and release.
* `sbt apexlsJS/Dev/build` - Creates fast optimised js bundle for debugging.
* `sbt pack` / `sbt "pack [version]"` - Do a local published release of the most recent tag or given value.
  * **WARNING:** This can override the remote releases, clear your `~/.ivy2/local` directory to revert.
* `sbt test` - Execute full test run.
* `sbt clean` - Removes most build files and artifacts.

### Testing

In addition to the regular automated tests, we system test against a number of sample projects collected in the [apex-samples](https://github.com/apex-dev-tools/apex-samples) repository. Follow the README instructions in apex-samples to checkout the submodules or use `git clone --recurse-submodules <repo-url>`. To run the tests:

  ```sh
  # Set SAMPLES env var to samples repo location
  export SAMPLES=<abs path to apex-samples>

  # Exec test script
  cd js/npm
  npm run test-samples
  ```

System test failures relating to the snapshots may highlight regressions. Though if an error is expected or the samples have changed, instead use `npm run test-snapshot` to update the snapshots, then commit the changes.

### Release

Releases are automated via workflow on publishing a release. Create a `v` prefixed tag at the same time on the commit to be released (e.g. `v1.0.0`).

Snapshot releases can also be created at any time by executing the `Publish` workflow on a branch. The versioning will be in the format `X.X.X+Y-yyyy-SNAPSHOT`; the latest tag followed by recent commit info.

## License

The source code forked from [apex-link](https://github.com/nawforce/apex-link) & [pkgforce](https://github.com/nawforce/pkgforce) uses a 3-clause BSD licence. Additional contributions:

* The antlr4c3 CodeCompletionCore.java has been embedded under a MIT licence.
