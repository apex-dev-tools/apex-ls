# Apex Language Server

The Apex Language Server library provides a collection of tools to aid development of Salesforce Apex projects. Check for errors, find types, suggest code completions and more.

## Getting Started

### Installation

Releases are available from [Maven Central](https://central.sonatype.com/), included as a default resolver in maven and sbt.

Scala:

  ```scala
  project.settings(
    // Replace %% with %%% to use ScalaJS build
    libraryDependencies += "io.github.apex-dev-tools" %% "apex-ls" % "X.X.X"
  )
  ```

Maven:

  ```xml
  <!-- In <dependencies/> -->
  <dependency>
    <groupId>io.github.apex-dev-tools</groupId>
    <artifactId>apex-ls</artifactId>
    <version>X.X.X</version>
  </dependency>
  ```

### Usage

The library can be consumed in JVM and ScalaJS projects, however the features available to each differ. See the JavaDoc for more details on the API. <!-- TODO link to hosted javadoc -->

The jar is also executable without a client via the commands, `CheckForIssues` and `DependencyReport`:

  ```sh
  # Assuming dep jars are in the same directory
  java -cp "apex-ls*.jar" io.github.apexdevtools.apexls.<CommandName> [args]
  ```

The following arguments are available:

| Argument             | Description                                                                                        | Supported Commands |
|----------------------|----------------------------------------------------------------------------------------------------|--------------------|
| `--format` / `-f`    | Output format: `text (default) \| json \| pmd`                                                     | All                |
| `--logging` / `-l`   | Text output logging level: `none (default) \| info \| debug`                                       | All                |
| `--nocache` / `-n`   | Do not load from or write to an existing apex-ls cache.                                            | All                |
| `--cache-dir` / `-c` | Cache directory path, defaults to `APEXLINK_CACHE_DIR` env or `$HOME/.apexlink_cache`.             | All                |
| `--workspace` / `-w` | Workspace directory path, defaults to current directory. Must contain an `sfdx-project.json` file. | All                |
| `--detail` / `-d`    | Detail level: `errors (default) \| warnings \| unused`                                             | CheckForIssues     |

## Configuration

### sfdx-project.json Options

The Apex Language Server can be configured through the `sfdx-project.json` file using the `plugins.options` section:

```json
{
  "packageDirectories": [...],
  "namespace": "...",
  "plugins": {
    "dependencies": [...],
    "additionalNamespaces": [...],
    "options": {
      "forceIgnoreVersion": "v2"
    }
  }
}
```

#### Available Options

| Option              | Values        | Default | Description                                                                                    |
|---------------------|---------------|---------|------------------------------------------------------------------------------------------------|
| `forceIgnoreVersion` | `"v1"`, `"v2"` | `"v2"`  | Controls which `.forceignore` implementation to use. `v2` provides exact node-ignore 5.3.2 compatibility, `v1` uses the legacy implementation. |

The `options` section supports additional configuration options that may be added in future releases.

## Model Context Protocol (MCP) Support

🧪 **Experimental**: The Apex Language Server now includes experimental support for the [Model Context Protocol (MCP)](https://modelcontextprotocol.io/), enabling AI tools and applications to interact with Apex code analysis capabilities.

The MCP server provides standardized access to:
- **Static Analysis** - Find errors, warnings, and code issues across SFDX projects
- **Find Usages** - Locate all references to Apex identifiers
- **Go to Definition** - Navigate to symbol definitions
- **Test Discovery** - Find test classes impacted by code changes

### Quick Start

Download the standalone MCP server:

```bash
curl -L -o apex-ls-mcp.jar \
  "https://github.com/apex-dev-tools/apex-ls/releases/latest/download/apex-ls-mcp-standalone.jar"
```

Configure with your AI tool (e.g., Claude Desktop):

```json
{
  "mcpServers": {
    "apex-ls": {
      "command": "java",
      "args": ["-jar", "/path/to/apex-ls-mcp.jar"]
    }
  }
}
```

For complete documentation, installation options, and integration guides, see the [MCP README](mcp/README.md).

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
* `sbt scalafmtAll` - Reformat code files using scalafmt

### Testing

In addition to the regular automated tests, we system test against a number of sample projects collected in the [apex-samples](https://github.com/apex-dev-tools/apex-samples) repository. Follow the README instructions in apex-samples to checkout the submodules. To run the tests:

  ```sh
  # Set SAMPLES env var to samples repo location
  export SAMPLES=<abs path to apex-samples>

  # Exec test script
  cd js/npm
  npm run test-samples
  ```

System test failures relating to the snapshots may highlight regressions. Though if an error is expected or the samples have changed, instead use `npm run test-snapshot` to update the snapshots, then commit the changes.

The tag version of apex-samples used by builds is set in the [build file](.github/workflows/Build.yml).

### Release

Releases are automated via workflow on publishing a release. Create a `v` prefixed tag at the same time on the commit to be released (e.g. `v1.0.0`).

Snapshot releases can also be created at any time by executing the `Publish` workflow on a branch. The versioning will be in the format `X.X.X+Y-yyyy-SNAPSHOT`; the latest tag followed by recent commit info.

### Updating To New API Version

See [doc page](doc/API_Updates.md).

## License

The source code forked from [apex-link](https://github.com/nawforce/apex-link) & [pkgforce](https://github.com/nawforce/pkgforce) uses a 3-clause BSD licence. Additional contributions:

* The antlr4c3 CodeCompletionCore.java has been embedded under a MIT licence.
