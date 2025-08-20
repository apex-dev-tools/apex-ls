Unused analysis is a useful way to help minimise redundant code in your codebase. This is important for all developers to help reduce cognitive load but it is specifically helpful for newer developers as identifying redundant code can be much harder when new to a code base. This guide provides some background on how unused code is located and the rules used to decide if and how to flag code as unused. We use the term class here but the same rules are applied to interfaces & enum types as well.

Unused analysis is performed directly after any changes are made. Classes which have errors from the primary analysis are excluded from unused analysis to reduce the number of diagnostics visible. Once a class is clear of errors you may see unused warnings appear. For non-test classes a declaration is considered unused if it is not used by some other non-test code. We exclude Visualforce controllers and those that have class level warning suppression (see later) from analysis. Visualforce controllers are excluded currently due to limitations in our handling of references to Apex code in pages and components.

# Library Project Handling
When a project is configured as a library project (by setting `"library": true` in the `plugins` section of `sfdx-project.json`), public methods and fields are treated as part of the library's public API and are not flagged as unused. This prevents unnecessary warnings for library code that is intended to be consumed by external projects. Private methods and fields within library projects continue to generate unused warnings when not referenced internally, helping maintain clean internal code.

Example configuration:
```json
{
  "packageDirectories": [{"path": "force-app"}],
  "plugins": {
    "library": true
  }
}
```

With this configuration:
- Public methods and fields will not generate unused warnings (as they're part of the library API)
- Private methods and fields will still generate unused warnings if not used internally
- Global methods and fields continue to be excluded from unused warnings (as they already were)

The analysis primarily concerns checking each field & method to see if there are any references to these. Certain types and fields are assumed to have references, such as global methods, even if none are present in the code base directly. When the only caller of a method is within the method these will be flagged as unused but we can’t as yet detect redundant calling loops such as two methods which are only called by each other.

# Class Level Handling
There are broadly two ways to think about a class being used. There may be direct references in the code base to it or we may infer it is being used by seeing that some field or method in the class is being used. Due to dynamic construction either or both of these may be the case.

To reduce warning noise where all fields, methods and inner classes (for outer classes) are unused the unused warning(s) can be replaced by a single warning directly on the class. The process can happen with both inner and outer classes. There are some cases where this summarization will not be used

* The class has direct use, e.g. via a new expression.
* The class is global.
* The class is an empty outer class.

# Method Shadowing
When working with superclasses and interfaces you may notice that a call to say an interface method is needed to remove an unused warning from a class. When calculating if a method is unused we look not only for callers to the method but also callers to any method that it may be ‘shadowing’ in the sense of replacing.

This model is essentially assuming that if you create a method that shadows another that is not itself unused then there is some way to invoke that method. In practice this may not be the case but it's an assumption we need to use currently to avoid generating a lot of false positive warnings.

# Warning suppression
You may come across cases where an unused warning is not valid or even just not wanted yet. To avoid the warning you can use one of two forms of suppression annotation on either the method of containing class.

The most specific annotation to use is ‘“@SuppressWarnings('Unused')”. You can also use “ @SuppressWarnings(‘PMD’)” since PMD also has some unused capabilities. 

