# Outline Parser

This is a snapshot of the outline parser taken to integrate with apexlink and pkgforce.

Apex 'Outline' parser. This parser extracts an outline from Apex class files. The outline provides structural information about the class and its inner classes without
needing to parse code blocks so performance much better than a full parser for Apex making it ideal for use when indexing or similar activities.

## Getting Started

### Prerequisites

- Java JDK 1.8

  - OpenJDK installed via brew is recommended
    ```sh
    brew tap adoptopenjdk/openjdk
    brew install --cask adoptopenjdk8
    ```
  - For the correct java version to be used, JAVA_HOME must be set accordingly:
    - E.g. To always select JDK 1.8, add the following to your bash/zsh profile
      ```sh
      export JAVA_HOME=$(/usr/libexec/java_home -v 1.8)
      ```

- [Scala build tool](https://www.scala-sbt.org/)

```sh
brew install sbt
```

## Building

```sh
sbt build
```

This will generate two outputs:

- jvm/target/scala-2.13/parser-assembly-0.0.1.jar
- js/target/scala-2.13/parser-fastopt/main.js

## Usage

### JVM

To parse a single file and have the output verified against the ANTLR parsers run:

    java -jar jvm/target/scala-2.13/parser-assembly-0.0.1.jar <Apex file path>

You can also pass a directory path to test the parser against multiple cls files at once. By default parallel parsing is used, to parse the files sequentially use '-seq'. To turn off the comaprison with the ANTLR parser output used '-notest'. The '-display' option can be used to get detailed information on the parse output.

### Node

To run the node.js version try:

    node js/target/scala-2.13/parser-fastopt/main.js <Apex file path>

You can also provide a directory to the parser to test multiple files at once. This client is simpler than for JVM and does not support options at the moment.

# Developing

## Intellij

The Scala Intellij plugin has a number of useful features and integration that make it
highly recommended. Ensure you have installed and enabled it first.

The Intellij project files are ignored in this repository, so for a clean repo we need
to import and create an sbt project from it.

1. In the IDE, `File > Close Project` if there is an existing project open.
1. Select `Import Project` from the main menu, selecting this repo directory.
1. Then `Import project from external model`, selecting `sbt`.
1. Finally we need to select a JDK version if not already defaulted.
1. You can enable auto import if desired to download dependencies as needed.

After the initial sbt project load you should now be able to start development.

### Building under Windows

Install the following in addition to the requirements above `Git For Windows` and `nvm-windows`.

The build commands documented above work from within a `Git Bash` session.
