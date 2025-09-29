#!/bin/bash

# Test script to run integration/system tests with a specific version override
# Usage: ./scripts/test-with-version.sh <version> [test-type]
# Example: ./scripts/test-with-version.sh 5.10.0 integration

set -e

if [ $# -lt 1 ]; then
    echo "Usage: $0 <version> [test-type]"
    echo "  version: JAR version to test against (e.g., 5.10.0)"
    echo "  test-type: 'integration', 'system', or 'all' (default: all)"
    echo ""
    echo "Examples:"
    echo "  $0 5.10.0"
    echo "  $0 5.10.0 integration"
    echo "  $0 5.10.0 system"
    exit 1
fi

VERSION=$1
TEST_TYPE=${2:-all}

echo "Testing apex-ls-mcp with version override: $VERSION"
echo "Test type: $TEST_TYPE"
echo ""

# Set environment variable to override version
export APEX_LS_MCP_VERSION=$VERSION

# Build first
echo "Building project..."
npm run build

echo ""
echo "Running tests with version $VERSION..."

case $TEST_TYPE in
    "integration")
        npm run test:integration
        ;;
    "system")
        npm run test:system
        ;;
    "all")
        npm run test:integration
        npm run test:system
        ;;
    *)
        echo "Invalid test type: $TEST_TYPE"
        echo "Valid types: integration, system, all"
        exit 1
        ;;
esac

echo ""
echo "Tests completed successfully with version $VERSION"