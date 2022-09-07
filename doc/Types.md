# Types

The representation of types is central to the processing model we use. In short, we model all metadata of interest in the form of type declarations and these declarations mirror how that metadata appears to Apex code. For examples, labels are exposed for analysis via a type declaration for System.Label and SObject are exposed via Schema.* type declarations.

Historically these type declarations have been implemented entirely within apex-link but with the advent of the outline parser and our wish to share this approach more widely we are re-building the type declaration model within this library so that it can be exposed from type index structure (see IPM) which is significantly quicker to construct than the apex-link representations. Over time, we expect to deprecate the apex-link model in favour of this approach.

## Outline parser

The outline parser represents types using the traits ITypeDeclaration & IMutableTypeDeclaration:

```txt
ITypeDeclaration (T)
└───IMutableTypeDeclaration (T)
```

Mutability is useful in the parser to allow type declarations to be built up incrementally during the parsing of a file. It is very much less desirable after parsing, so we cast away the mutability. We use this model to avoid having a mutable->immutable conversion which would add cost.

The outline parser is implemented with generics, so we can specialise the actual type declaration class that is generated. To construct the concrete classes we pass a factory.

Although we are now supporting multiple Apex parsers we use the outline parser type declarations model as our common model. This is also to support our performance goals, needing to convert outline parser output to another format could add significant costs on large workspaces.  

One thing to note when looking at these interfaces is that some fields may be null. This is counter to our normal practice of using Option for such types but is done to allow us to save the memory used be wrapping lots of objects with Some.

## Module Type Declarations

Our model places types within modules (see IPM), to support this we extend the outline parser traits with versions which carry a module field:

```txt
ITypeDeclaration (T)
└───IModuleTypeDeclaration (T)
└───IMutableTypeDeclaration (T) with IModuleTypeDeclaration
    └───IMutableModuleTypeDeclaration (T)
```

Note: the use of 'with' to use IModuleTypeDeclaration as a mixin, see [mixin-class-composition](https://docs.scala-lang.org/tour/mixin-class-composition.html) for background on this pattern.

The IModuleTypeDeclaration trait introduces the module field which derived concrete classes are required to define. In some test cases it may be appropriate to use a null module value but in normal use we always expect it to be set.

## TypeDeclaration

All of this leads us to our implementation of type declarations:

```txt
IMutableModuleTypeDeclaration (T)
└───TypeDeclaration (C)
    └───ClassTypeDeclaration (C)
    └───InterfaceTypeDeclaration (C)
    └───EnumTypeDeclaration (C)
```

The Class, Interface and Enum variants were initially used to allow specialisation but that has been removed in favour of allowing TypeDeclaration to store all three forms. The variants still exist for backwards compatibility reasons and may be removed.

There are a couple of things worth noting in how this is done. For enum constants we encode them as static fields in TypeDeclaration with a type of the enclosing enum. For interface extends type references we encode them as the implements type ref list in TypeDeclaration. We also add a nature field to TypeDeclaration which can be used to differentiate class, interface and enum type declarations as needed.

## IModuleTypeDeclaration

While TypeDeclaration provides the central implementation for Apex defined types we use IModuleTypeDeclaration as our common interface for handling type declarations. We can see this when considering platform & SObject types currently.

```txt
IModuleTypeDeclaration (T)
└───PlatformTypeDeclaration (C)
    └───GenericPlatformTypeDeclaration (C)
└───SObjectTypeDeclaration (C)
```

We expect that this list will grow substantially as more metadata types are implemented in this model.
