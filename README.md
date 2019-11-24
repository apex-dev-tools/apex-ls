
## ApexLink

ApexLink is a SFDX CLI plugin & Java library for static analysis of Salesforce Apex code aimed at improving developer productivity. The core library is useful for any number of analysis problems while the CLI plugin acts as a demo of capability while the features of the library are built out.
 
### SFDX CLI

To install the CLI plugin (from npm)

    sfdx plugins:install apexlink

To perform a simple validity check use:

    sfdx apexlink:check <directory>

This parses and performs semantic checks on the code and reports any errors, such as types not being found. The library contains a pretty comprehensive set of platform types that it validates against.

More complex validations can be performed that support namespaced packages and multiple source directories, see the command help for more details. Currently this command does not require an sfdx project, if you omit the directory it will search the current directory for metadata.  

### Unused fields, properties & methods

You can use the check command to report on unused fields, properties and methods of Apex classes. 

    sfdx apexlink:check --zombies <directory>

This analysis currently may return false positives for:
*  Fields & Properties only bound to SOQL queries
*  Properties only used by Visualforce Pages
*  Fields, properties & methods only referenced from triggers.   

### Class dependencies

The check command can also report Apex class dependencies with:

    sfdx apexlink:check --depends --json <directory>

If you omit the --json the dependency report is returned as CSV records. Understanding dependencies is useful when analysing [cold start behaviours](https://nawforce.blog/2019/02/25/apex-cold-starts-and-class-caching-misses/) but it has also been written with a view to supporting the identification of dead code/metadata. 

### Parallel test execution

There is a WIP command included for parallel test execution

    sfdx apexlink:retest
    
 For more information on this please see read [this post](https://nawforce.blog/2019/06/09/parallel-unit-testing-via-sfdx-cli/)   


### Building

To create a jar use:

    mvn package
     
### Maven

To use the jar in a maven project add the following to your pom.xml

    <dependency>
        <groupId>com.github.nawforce</groupId>
        <artifactId>apexlink</artifactId>
        <version>0.5</version>
    </dependency>

### Source & Licenses

ApexLink is written in a combination of Java and Scala but should run on any fairly recent JVM. Please let me know if you have trouble building or running it. All the source code included uses a 3-clause BSD license. The only third-party component included is the Apex Antlr4 grammar originally from [Tooling-force.com](https://github.com/neowit/tooling-force.com), although the version used is now markedly different from the original.  

