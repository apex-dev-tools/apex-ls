# Property-Based Testing

Property-based testing is available via ScalaCheck integration. This complements the existing example-based unit tests by generating many inputs automatically and checking that properties hold across all of them.

## When to Use Property Tests

Property tests work well when:

- **The input space is large** - TypeName resolution, expression evaluation, type coercion
- **Rules can be stated as properties** - "valid identifiers are accepted", "parsing then printing is identity"
- **You want to find edge cases** - generators explore boundaries you might not think of

Stick with unit tests when:

- **Specific examples matter** - regression tests for known bugs
- **The property is hard to state** - sometimes "expected output for this input" is clearer
- **The input space is small** - a few hand-picked cases may suffice

## Writing Property Tests

Extend `ScalaCheckPropertyChecks` and use `forAll`:

```scala
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import _root_.org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class MyPropertyTest extends AnyFunSuite with ScalaCheckPropertyChecks {

  test("property holds for all inputs") {
    forAll(Gen.alphaStr) { s =>
      whenever(s.nonEmpty) {
        // assert property
      }
    }
  }
}
```

Note the `_root_` prefix on the import - this avoids conflict with apex-ls's own `org` package.

For JVM tests that need `TestHelper`, use the `PropertyTestHelper` trait which combines both.

## Generator Strategies

### Targeted Generators

Build generators that produce specific classes of input:

```scala
// Generate identifiers starting with underscore (known invalid)
val startsWithUnderscore: Gen[String] = for {
  rest <- Gen.alphaNumStr.suchThat(_.nonEmpty)
} yield "_" + rest

test("identifiers starting with underscore are rejected") {
  forAll(startsWithUnderscore) { id =>
    assert(Identifier.isLegalIdentifier(Name(id)).isDefined)
  }
}
```

This requires understanding the rules upfront but gives precise coverage of known edge cases.

### Fuzzing

Generate arbitrary inputs and verify the function behaves consistently with a spec:

```scala
test("arbitrary strings handled consistently with spec") {
  forAll(Gen.asciiStr) { s =>
    whenever(s.nonEmpty) {
      val result = validate(s)
      result match {
        case Valid   => assert(isValidBySpec(s))
        case Invalid => assert(!isValidBySpec(s))
      }
    }
  }
}
```

This catches unexpected behaviors but most inputs may be uninteresting.

### Combined Approach

Use both: targeted generators for known patterns, plus a fuzzing test as a safety net. When fuzzing finds a bug, add that case to the unit test suite as a regression anchor.

## Configuring Test Count

The default is 100 cases per property. Adjust based on input space complexity:

```scala
implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
  PropertyCheckConfiguration(minSuccessful = 250)
```

Guidelines:
- **Simple input space** (identifiers, names): 100 is sufficient
- **Complex combinatorial space** (nested types, expressions): 250-500 provides more thorough search
- **Critical properties**: increase for higher assurance

Note: more cases means more search, not statistical confidence. You're looking for counterexamples, not estimating a population parameter.

## See Also

- `IdentifierPropertyTest` - example property tests for identifier validation
- [ScalaCheck User Guide](https://scalacheck.org/documentation.html)
- [ScalaTest + ScalaCheck](https://www.scalatest.org/plus/scalacheck)
