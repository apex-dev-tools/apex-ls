# MDIndex

Work on an alternative to apex.db is being exposed via [MDIndex](https://github.com/financialforcedev/ff-apex-ls/blob/master/pkgforce/jvm/src/main/java/com/nawforce/pkgforce/api/MDIndex.java) which is based on ApexIndex from Jorje. Currently, you can construct an index using the outline parser over very large metadata projects in a few seconds. [MDIndexTest](https://github.com/financialforcedev/ff-apex-ls/blob/master/pkgforce/jvm/src/test/scala/com/nawforce/runtime/api/MDIndexTest.scala) provides some examples of how to use.

The naming as ‘MDIndex’ is deliberate as we expect to include other types of metadata relevant to Apex semantic analysis later on. Once MDIndex has matured we intend to make use of it directly within apex-link as it will allow significant simplification of some of our ‘type finding’ logic. To reach that point we need to show compatibility with ApexIndex using Jorge.

## IPM

IPM is an object that implements a three layer tree consisting of a root Index which contains Packages, each of which contains Modules. This is structure is central to how we access and manage metadata while maintaining and understanding of the package structure defined in sfdx-project.json so that we can support the semantic analysis of extension packages over base packages, 1GP multi-package directory analysis support and 2GP package analysis

Ordering is used with packages within the index and modules within the packages to reflect deploy ordering. When using MDIndex we collate results over the hierarchy as needed so that is presents a consolidated view of the types/metadata available.

## TriHierarchy

IPM is an implementation of the generic TriHierarchy abstract class. The TriHierarchy is just used as means to maintain consistency with how apex-link implements a similar model. This is important to allowing apex-link and this implementation to share utility code when dealing with workspaces. We expect we will be able to remove TriHierarchy at some point to simplify the model.



