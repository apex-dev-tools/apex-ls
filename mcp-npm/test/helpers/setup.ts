import * as fs from 'fs';
import * as path from 'path';
import * as os from 'os';

// Global test setup

// Ensure we have a clean test environment
beforeEach(() => {
  // Clear any existing environment variables that might affect tests
  delete process.env.APEXLINK_CACHE_DIR;
});

// Cleanup after each test
afterEach(() => {
  // Clean up any test cache directories
  const testCacheDir = path.join(os.tmpdir(), 'apex-ls-mcp-test');
  if (fs.existsSync(testCacheDir)) {
    fs.rmSync(testCacheDir, { recursive: true, force: true });
  }
});