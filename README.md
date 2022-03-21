## ff-apex-ls

This repo contains a FinancialForce fork of the open source [apex-link](https://github.com/nawforce/apex-link). Before forking the [pkgforce](https://github.com/nawforce/pkgforce) library was merged into apex-link to make it easier to refactor functionality between these libraries. The merging of these libraries is intended to be temporary; they will be split again to allow for independent versioning and re-branding when the FinancialForce versions are made open-source. Releases < 2.4 have been made from the nawforce repos, this repo is being used for subsequent releases.

Refactoring is focusing on two broad areas:

* Adding support for an apex.db like model inside the pkgforce library that uses the outline parser.
* Introducing the Apex Jorje parser as an alternative to the ‘apex-parser’ module.

The outline parser has been directly embedded into pkgforce for this work; we expect to open source this as a separate module later. The Apex Jorje jar is being pulled in via a maven artefact. 

### apex.db alternative

Work on an alternative to apex.db is being exposed via [MDIndex](https://github.com/financialforcedev/ff-apex-ls/blob/master/pkgforce/jvm/src/main/java/com/nawforce/pkgforce/api/MDIndex.java) which is based on ApexIndex from Jorje. Currently you can construct an index using the outline parser over very large metadata projects in a few seconds. [MDIndexTest](https://github.com/financialforcedev/ff-apex-ls/blob/master/pkgforce/jvm/src/test/scala/com/nawforce/runtime/api/MDIndexTest.scala) provides some examples of how to use.

The naming as ‘MDIndex’ is deliberate as we expect to include other types of metadata relevant to Apex semantic analysis later on. This is similar to how apex-link currently works in that different metadata types are indexed together using the type names they expose to Apex, e.g. Schema.Foo__c for a custom object.  

The apex-link library relies on a metadata containment hierarchy to isolate different layers of metadata. This is used for a number of purposes such as supporting the analysis of extension packages over base packages, 1GP multi-package directory analysis support and 2GP package analysis. The hierarchy used in apex-link is an ‘Org’ containing ‘Packages’  which contain ‘Modules’ with reverse deploy ordering of children. For the MDIndex model we have adjusted this to be Index->Package->Module but hide the internal structure via acting on the top-most module. This means searches will collate results over the hierarchy and present a consolidated view of the metadata even though internally we are managing multiple layers of metadata to better support advanced semantic analysis later on. 

The apex-link library is not yet making use of MDIndex but can use the Outline parser directly. Once MDIndex has matured we intend to make use of it directly within apex-link as it will allow significant simplification of some of our ‘type finding’ logic. To reach that point we need to show compatibility with ApexIndex using Jorge.

### Jorje Parser Compatibility

The Jorje parser/compiler presents an AST which has a higher level of abstraction around types than our own apex-parser. To understand the differences better we are working on the ability to compare outputs so that we know the input of the indexing processes are comparable. Initially we are treating Jorje as a reference implementation for testing purposes but intend to swap over parsers once compatibility has been reached. 

The main test client for this work is [OutputComparisonTest](https://github.com/financialforcedev/ff-apex-ls/blob/master/pkgforce/jvm/src/test/scala/com/nawforce/runtime/sfparser/OutputComparisonTest.scala) which utilises [SFParser](https://github.com/financialforcedev/ff-apex-ls/blob/master/pkgforce/jvm/src/main/scala/com/nawforce/runtime/sfparser/SFParser.scala) & [OutlineParser](https://github.com/financialforcedev/ff-apex-ls/blob/master/pkgforce/ff-apex-outline-parser/shared/src/main/scala/com/financialforce/oparser/OutlineParser.scala).
 
### Gulp CLI

A separate item that is also in progress is a CLI which downloads the additional metadata from an org that is needed for semantic analysis. This is currently part of the [apex-link-sfdx-cli](https://github.com/nawforce/apex-link-sfdx-cli) but to enable reuse we plan that the core logic will be re-packaged as a NPM module.


### Building
The library is split into two modules 'apex-link' & 'pkgforce', the apex-link library depends on pkgforce. Each have their own pom.xml files that you can use to build. To build both use:

    mvn clean install -Dgpg.skip

We recommend using IntelliJ for development work because of its excellent Scala support. Project files for IntelliJ are also included.

pkgforce can be cross built for use on node.js. To support this it also has an sbt build process. This is run automatically as part of the outer project maven build.

### Source & Licences
The source code forked from apex-link & pkgforce uses a  3-clause BSD licence. There are two external contributions, 

* The Apex Antlr4 grammar was originally from [Tooling-force.com](https://github.com/neowit/tooling-force.com), although the version used is now markedly different from the original.  
* The antlr4c3 CodeCompletionCore.java has been embedded under a MIT licence.

Licensing for new contributions from FinancialForce employees is TBD.

