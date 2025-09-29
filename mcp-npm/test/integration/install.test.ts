import * as fs from 'fs';
import * as path from 'path';
// @ts-ignore - will be fixed when install is properly typed
const { install } = require('../../dist/install');
import { MockServer } from '../helpers/mock-server';
import { createTestCacheDir, writeMockPackageJson, mockConsole } from '../helpers/test-utils';

describe('install integration', () => {
  let mockServer: MockServer;
  let testDir: string;
  let originalCwd: string;
  let originalHome: string | undefined;
  let mockHome: string;

  beforeEach(async () => {
    originalCwd = process.cwd();
    originalHome = process.env.HOME;
    
    // Set up test directories
    testDir = createTestCacheDir('install-integration-test');
    mockHome = path.join(testDir, 'home');
    fs.mkdirSync(mockHome, { recursive: true });
    
    // Mock HOME directory
    process.env.HOME = mockHome;
    
    // Set up mock server
    mockServer = new MockServer(9998);
    await mockServer.start();
    
    // Change to test directory with mock package.json
    const packageDir = path.join(testDir, 'package');
    fs.mkdirSync(packageDir, { recursive: true });
    process.chdir(packageDir);
  });

  afterEach(async () => {
    process.chdir(originalCwd);
    process.env.HOME = originalHome;
    await mockServer.stop();
    
    if (fs.existsSync(testDir)) {
      fs.rmSync(testDir, { recursive: true, force: true });
    }
  });

  test('should install JAR successfully on first run', async () => {
    // Set up mock package.json
    writeMockPackageJson(process.cwd(), {
      jarVersion: '1.0.0',
      downloadUrl: `${mockServer.getBaseUrl()}/apex-ls-mcp-{jarVersion}-standalone.jar`
    });

    // Set up mock JAR response
    const jarContent = Buffer.from('PK\x03\x04mock-jar-content');
    mockServer.setJarResponse('/apex-ls-mcp-1.0.0-standalone.jar', jarContent);

    const console = mockConsole();

    await install();

    console.restore();

    // Verify cache directory and files were created
    const expectedCacheDir = path.join(mockHome, '.apex-ls-mcp');
    const expectedJarPath = path.join(expectedCacheDir, 'apex-ls-mcp.jar');
    const expectedVersionFile = path.join(expectedCacheDir, 'version.txt');

    expect(fs.existsSync(expectedCacheDir)).toBe(true);
    expect(fs.existsSync(expectedJarPath)).toBe(true);
    expect(fs.existsSync(expectedVersionFile)).toBe(true);

    // Verify JAR content
    const downloadedContent = fs.readFileSync(expectedJarPath);
    expect(downloadedContent.equals(jarContent)).toBe(true);

    // Verify version file
    const versionContent = fs.readFileSync(expectedVersionFile, 'utf8');
    expect(versionContent.trim()).toBe('1.0.0');

    // Verify console output
    expect(console.logs).toContain('Installing apex-ls-mcp...');
    expect(console.logs).toContain('Downloading apex-ls-mcp v1.0.0...');
    expect(console.logs).toContain('apex-ls-mcp v1.0.0 installed successfully');
  });

  test('should skip download when JAR is already cached', async () => {
    // Set up mock package.json
    writeMockPackageJson(process.cwd(), {
      jarVersion: '1.0.0',
      downloadUrl: `${mockServer.getBaseUrl()}/apex-ls-mcp-{jarVersion}-standalone.jar`
    });

    // Pre-create cache with matching version and valid JAR signature
    const cacheDir = path.join(mockHome, '.apex-ls-mcp');
    fs.mkdirSync(cacheDir, { recursive: true });
    const existingJarContent = Buffer.from('PK\x03\x04existing jar content');
    fs.writeFileSync(path.join(cacheDir, 'apex-ls-mcp.jar'), existingJarContent);
    fs.writeFileSync(path.join(cacheDir, 'version.txt'), '1.0.0');

    const console = mockConsole();

    await install();

    console.restore();

    // Verify it used the cached version
    expect(console.logs).toContain('Installing apex-ls-mcp...');
    expect(console.logs).toContain('apex-ls-mcp v1.0.0 already cached');
    expect(console.logs).not.toContain('Downloading apex-ls-mcp v1.0.0...');
  });

  test('should re-download when version changes', async () => {
    // Set up mock package.json with new version
    writeMockPackageJson(process.cwd(), {
      jarVersion: '2.0.0',
      downloadUrl: `${mockServer.getBaseUrl()}/apex-ls-mcp-{jarVersion}-standalone.jar`
    });

    // Pre-create cache with old version
    const cacheDir = path.join(mockHome, '.apex-ls-mcp');
    fs.mkdirSync(cacheDir, { recursive: true });
    fs.writeFileSync(path.join(cacheDir, 'apex-ls-mcp.jar'), 'old jar content');
    fs.writeFileSync(path.join(cacheDir, 'version.txt'), '1.0.0');

    // Set up mock JAR response for new version
    const newJarContent = Buffer.from('PK\x03\x04new-jar-content');
    mockServer.setJarResponse('/apex-ls-mcp-2.0.0-standalone.jar', newJarContent);

    const console = mockConsole();

    await install();

    console.restore();

    // Verify it downloaded the new version
    expect(console.logs).toContain('Installing apex-ls-mcp...');
    expect(console.logs).toContain('Downloading apex-ls-mcp v2.0.0...');
    expect(console.logs).toContain('apex-ls-mcp v2.0.0 installed successfully');

    // Verify new JAR content
    const jarPath = path.join(cacheDir, 'apex-ls-mcp.jar');
    const downloadedContent = fs.readFileSync(jarPath);
    expect(downloadedContent.equals(newJarContent)).toBe(true);

    // Verify version file updated
    const versionFile = path.join(cacheDir, 'version.txt');
    const versionContent = fs.readFileSync(versionFile, 'utf8');
    expect(versionContent.trim()).toBe('2.0.0');
  });

  test('should handle network errors gracefully', async () => {
    writeMockPackageJson(process.cwd(), {
      jarVersion: '1.0.0',
      downloadUrl: `${mockServer.getBaseUrl()}/nonexistent.jar`
    });

    // Don't set up any response - will get 404
    const console = mockConsole();

    await expect(install()).rejects.toThrow();

    console.restore();

    expect(console.errors).toContain('Failed to install apex-ls-mcp: JAR file not found at http://localhost:9998/nonexistent.jar. Please check if the version exists.');
  });

  test('should handle permission errors gracefully', async () => {
    writeMockPackageJson(process.cwd(), {
      jarVersion: '1.0.0',
      downloadUrl: `${mockServer.getBaseUrl()}/apex-ls-mcp-{jarVersion}-standalone.jar`
    });

    // Set up mock JAR response
    const jarContent = Buffer.from('PK\x03\x04permission-test-content');
    mockServer.setJarResponse('/apex-ls-mcp-1.0.0-standalone.jar', jarContent);

    // Create a cache directory with restrictive permissions that should fail
    const restrictedCacheDir = path.join(mockHome, '.apex-ls-mcp');
    fs.mkdirSync(restrictedCacheDir, { recursive: true });
    
    // Create a read-only file that should block writing
    const jarPath = path.join(restrictedCacheDir, 'apex-ls-mcp.jar');
    fs.writeFileSync(jarPath, 'existing content');
    fs.chmodSync(jarPath, 0o444); // Read-only

    const console = mockConsole();

    await expect(install()).rejects.toThrow();

    console.restore();

    // Clean up permissions for teardown
    try {
      fs.chmodSync(jarPath, 0o644);
    } catch (e) {
      // Ignore cleanup errors
    }

    expect(console.errors.some((err: string) => err.includes('Permission denied') || err.includes('EACCES'))).toBe(true);
  });

  test('should create cache directory if it does not exist', async () => {
    writeMockPackageJson(process.cwd(), {
      jarVersion: '1.0.0',
      downloadUrl: `${mockServer.getBaseUrl()}/apex-ls-mcp-{jarVersion}-standalone.jar`
    });

    const jarContent = Buffer.from('PK\x03\x04test-jar');
    mockServer.setJarResponse('/apex-ls-mcp-1.0.0-standalone.jar', jarContent);

    // Ensure cache directory doesn't exist initially
    const cacheDir = path.join(mockHome, '.apex-ls-mcp');
    if (fs.existsSync(cacheDir)) {
      fs.rmSync(cacheDir, { recursive: true });
    }

    await install();

    expect(fs.existsSync(cacheDir)).toBe(true);
    expect(fs.existsSync(path.join(cacheDir, 'apex-ls-mcp.jar'))).toBe(true);
  });

  test('should handle corrupted package.json gracefully', async () => {
    // Write invalid JSON
    fs.writeFileSync(path.join(process.cwd(), 'package.json'), '{invalid json');

    const console = mockConsole();

    await expect(install()).rejects.toThrow();

    console.restore();

    expect(console.errors[0]).toMatch(/Failed to install apex-ls-mcp:/);
  });
});