# Testing Documentation

This document describes the testing strategy for the apex-ls-mcp NPM package.

## Automated Tests

The project includes comprehensive automated tests organized into three categories:

### Unit Tests (`npm run test:unit`)
- **Location**: `test/unit/`
- **Coverage**: Individual functions and modules
- **Tests**: 18 tests covering downloader, config, and java-check functionality

### Integration Tests (`npm run test:integration`) 
- **Location**: `test/integration/`
- **Coverage**: Component interaction and workflow testing
- **Tests**: 8 tests covering install process and basic integration

### System Tests (`npm run test:system`)
- **Location**: `test/system/`
- **Coverage**: Advanced scenarios and Java environment testing
- **Tests**: 7 tests covering unique edge cases not covered by integration tests

### Full Test Suite (`npm test`)
- Runs all 51 automated tests
- Includes TypeScript compilation and build verification
- **Total Coverage**: 51 passing tests, 0 skipped

## Manual Testing

For end-to-end system verification that requires real network access and Java environment testing, use the manual test script:

### Running Manual Tests

```bash
npm run test:manual
```

This script performs the following verification:

1. **Build Verification** - Ensures the project builds successfully
2. **Install Functionality** - Tests JAR download and caching
3. **Cache Management** - Verifies cache directory and file creation
4. **Version Management** - Tests version file handling
5. **Cached Install** - Verifies skip behavior when JAR is cached
6. **Version Upgrade** - Tests re-download when version changes
7. **CLI Integration** - Tests CLI execution (if Java is available)

### Manual Test Features

- **Isolated Environment**: Uses temporary directories for testing
- **Real Network**: Downloads actual JAR files from GitHub releases
- **Cross-Platform**: Works on macOS, Linux, and Windows
- **Java Detection**: Automatically detects and tests Java integration
- **Clean Teardown**: Automatically cleans up test artifacts

### When to Use Manual Tests

Use manual testing when:
- Verifying end-to-end functionality before releases
- Testing in new environments or CI systems
- Validating network-dependent features
- Confirming Java integration works correctly
- Debugging installation issues

## Testing Strategy

The testing approach is designed to handle the unique challenges of testing an NPM package that downloads versioned JAR files:

### CI/CD Strategy
- **GitHub Actions runs ONLY unit tests** (`npm run test:unit`)
- **Integration and system tests are excluded from CI** to avoid version dependency issues
- **CI uses `npm ci --ignore-scripts`** to skip JAR downloads during build

### Development Workflow
- **Unit tests** - Always safe to run, no external dependencies
- **Integration tests** - May fail during development when target JAR version doesn't exist yet
- **System tests** - Require actual released JARs, used for pre-release validation
- **Manual tests** - Full end-to-end verification with real network calls

### Version Dependency Handling
The package faces a chicken-and-egg problem: tests often run against JAR versions that haven't been released yet. Our solution:

- **Unit tests**: Test core logic without network dependencies (CI-safe)
- **Integration tests**: Use version patterns instead of hardcoded versions
- **System tests**: Accept that they may fail during development
- **Manual tests**: Use actual releases for final validation

### Version Override for Testing

To test against a known released version instead of the development version in package.json:

```bash
# Test integration tests against version 5.10.0
npm run test:with-version 5.10.0 integration

# Test system tests against version 5.10.0
npm run test:with-version 5.10.0 system

# Test both integration and system tests
npm run test:with-version 5.10.0

# Or set environment variable directly
export APEX_LS_MCP_VERSION=5.10.0
npm run test:integration
```

This allows testing against released versions while keeping the next development version in package.json.

### When Tests May Fail
- **Integration/System tests will fail** when package.json references a JAR version that doesn't exist yet
- **This is expected behavior** during development cycles
- **Use version override** to test against known released versions
- **Tests become valid** once the corresponding JAR version is released

### Testing Best Practices
- **Always run unit tests** before commits (no version dependencies)
- **Use version override** to test integration/system tests against released versions
- **Run integration tests** after JAR releases to verify compatibility
- **Use manual tests** for comprehensive pre-release validation
- **Don't block development** on integration test failures for unreleased versions

### Environment Variables
- `APEX_LS_MCP_VERSION` - Override the JAR version for testing (e.g., "5.10.0")

This strategy ensures development velocity while maintaining comprehensive test coverage when it matters most.