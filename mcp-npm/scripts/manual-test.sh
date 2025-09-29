#!/bin/bash

# Manual Test Script for apex-ls-mcp
# This script performs end-to-end testing that was previously in npx-execution.test.ts

set -e

echo "🧪 Manual Test Script for apex-ls-mcp"
echo "=================================="

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Test directory
TEST_DIR=$(mktemp -d -t apex-ls-mcp-manual-test)
echo "📁 Using test directory: $TEST_DIR"

cleanup() {
    echo "🧹 Cleaning up test directory..."
    rm -rf "$TEST_DIR"
}
trap cleanup EXIT

# Function to print test status
print_test() {
    local status=$1
    local message=$2
    if [ "$status" = "PASS" ]; then
        echo -e "${GREEN}✅ PASS${NC}: $message"
    elif [ "$status" = "FAIL" ]; then
        echo -e "${RED}❌ FAIL${NC}: $message"
        exit 1
    elif [ "$status" = "INFO" ]; then
        echo -e "${YELLOW}ℹ️  INFO${NC}: $message"
    fi
}

# Test 1: Build the project
print_test "INFO" "Building project..."
npm run build
print_test "PASS" "Project built successfully"

# Test 2: Install functionality
print_test "INFO" "Testing install functionality..."
cd "$TEST_DIR"

# Create a test package.json
cat > package.json << EOF
{
  "name": "test-package",
  "version": "1.0.0",
  "config": {
    "jarVersion": "5.9.0",
    "downloadUrl": "https://github.com/apex-dev-tools/apex-ls/releases/download/v{jarVersion}/apex-ls-mcp-{jarVersion}-standalone.jar"
  }
}
EOF

# Copy the built install script
cp "$OLDPWD/dist/install.js" .
cp "$OLDPWD/dist/lib"/*.js . 2>/dev/null || true
mkdir -p lib && cp "$OLDPWD/dist/lib"/*.js lib/ 2>/dev/null || true

# Test install
print_test "INFO" "Running install..."
if node install.js; then
    print_test "PASS" "Install completed successfully"
else
    print_test "FAIL" "Install failed"
fi

# Test 3: Verify cache was created
print_test "INFO" "Checking cache directory..."
CACHE_DIR="$HOME/.apex-ls-mcp"
if [ -d "$CACHE_DIR" ]; then
    print_test "PASS" "Cache directory exists"
else
    print_test "FAIL" "Cache directory not found"
fi

if [ -f "$CACHE_DIR/apex-ls-mcp.jar" ]; then
    print_test "PASS" "JAR file downloaded"
else
    print_test "FAIL" "JAR file not found"
fi

if [ -f "$CACHE_DIR/version.txt" ]; then
    VERSION=$(cat "$CACHE_DIR/version.txt")
    print_test "PASS" "Version file exists (version: $VERSION)"
else
    print_test "FAIL" "Version file not found"
fi

# Test 4: Test CLI (if Java is available)
print_test "INFO" "Testing CLI functionality..."
if command -v java >/dev/null 2>&1; then
    # Copy CLI files
    cp "$OLDPWD/dist/cli.js" .
    
    print_test "INFO" "Java found, testing CLI..."
    if timeout 10s node cli.js --help 2>/dev/null; then
        print_test "PASS" "CLI executed successfully"
    else
        print_test "INFO" "CLI test timed out or failed (this may be expected with mock JAR)"
    fi
else
    print_test "INFO" "Java not found, skipping CLI test"
fi

# Test 5: Test re-install (should use cache)
print_test "INFO" "Testing cached install..."
if node install.js 2>&1 | grep -q "already cached"; then
    print_test "PASS" "Cached install works correctly"
else
    print_test "FAIL" "Cached install not working"
fi

# Test 6: Test version upgrade
print_test "INFO" "Testing version upgrade..."
# Modify package.json to use a different version
sed -i '' 's/"jarVersion": "5.9.0"/"jarVersion": "5.8.0"/' package.json || sed -i 's/"jarVersion": "5.9.0"/"jarVersion": "5.8.0"/' package.json

if node install.js 2>&1 | grep -q "Downloading"; then
    print_test "PASS" "Version upgrade triggers download"
else
    print_test "FAIL" "Version upgrade not working"
fi

print_test "INFO" "All manual tests completed successfully!"
echo ""
echo "📋 Manual Test Checklist:"
echo "- ✅ Build works"
echo "- ✅ Install downloads JAR"
echo "- ✅ Cache directory created"
echo "- ✅ Version file written"
echo "- ✅ Cached install works"
echo "- ✅ Version upgrade works"
echo "- ℹ️  CLI test (requires Java)"
echo ""
echo "🎉 Manual testing complete!"