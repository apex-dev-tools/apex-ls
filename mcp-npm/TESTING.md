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

The testing approach combines automated and manual testing:

- **Automated tests** provide fast feedback and catch regressions
- **Manual tests** verify real-world scenarios and integration points
- **Unit tests** ensure individual components work correctly
- **Integration tests** verify component interactions
- **System tests** test error handling and edge cases
- **Manual tests** validate end-to-end user experience

This comprehensive approach ensures both development velocity and production reliability.