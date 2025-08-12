import * as fs from 'fs';
import * as path from 'path';
import { spawn } from 'child_process';
import { createTestCacheDir, writeMockPackageJson, createMockJar } from '../helpers/test-utils';
import { MockServer } from '../helpers/mock-server';

// Don't mock child_process for these simpler tests

describe('Simple Integration Tests', () => {
  let testDir: string;
  let originalHome: string | undefined;
  let originalCwd: string;
  let mockHome: string;
  let mockServer: MockServer;

  beforeEach(async () => {
    originalHome = process.env.HOME;
    originalCwd = process.cwd();
    
    // Set up test directories
    testDir = createTestCacheDir('simple-integration-test');
    mockHome = path.join(testDir, 'home');
    fs.mkdirSync(mockHome, { recursive: true });
    
    // Mock HOME directory
    process.env.HOME = mockHome;
    
    // Set up mock server
    mockServer = new MockServer(9994);
    await mockServer.start();
    
    // Create package directory
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

  test('should create proper package structure', () => {
    // Verify package.json exists in parent directory
    const packageJsonPath = path.resolve(__dirname, '../../package.json');
    expect(fs.existsSync(packageJsonPath)).toBe(true);
    
    const packageJson = JSON.parse(fs.readFileSync(packageJsonPath, 'utf8'));
    expect(packageJson.name).toBe('apex-ls-mcp');
    expect(packageJson.bin['apex-ls-mcp']).toBe('./bin/apex-ls-mcp');
    expect(packageJson.config.jarVersion).toBeTruthy();
    expect(packageJson.config.downloadUrl).toContain('{jarVersion}');
  });

  test('should have executable CLI files', () => {
    const binPath = path.resolve(__dirname, '../../bin/apex-ls-mcp');
    const distCliPath = path.resolve(__dirname, '../../dist/cli.js');
    const distInstallPath = path.resolve(__dirname, '../../dist/install.js');
    
    expect(fs.existsSync(binPath)).toBe(true);
    expect(fs.existsSync(distCliPath)).toBe(true);
    expect(fs.existsSync(distInstallPath)).toBe(true);
    
    // Check that files are executable (on Unix systems)
    if (process.platform !== 'win32') {
      const binStats = fs.statSync(binPath);
      
      expect(binStats.mode & parseInt('111', 8)).toBeGreaterThan(0); // Has execute permission
    }
  });

  test('should require all lib modules without errors', () => {
    expect(() => require('../../dist/lib/config')).not.toThrow();
    expect(() => require('../../dist/lib/downloader')).not.toThrow();
    expect(() => require('../../dist/lib/java-check')).not.toThrow();
  });

  test('config module functions work', () => {
    const config = require('../../dist/lib/config');
    
    // Test that functions return reasonable values
    expect(config.getCacheDir()).toBeTruthy();
    expect(config.getJarPath()).toBeTruthy();
    expect(config.getCurrentVersion()).toBeTruthy();
    expect(config.getDownloadUrl()).toContain('github.com');
    expect(config.getDownloadUrl()).toContain('5.9.0'); // Should contain the actual version
  });

  test('install module can be loaded without errors', () => {
    // Just test that the module loads without syntax errors
    expect(() => require('../../dist/install')).not.toThrow();
    
    const { install } = require('../../dist/install');
    expect(typeof install).toBe('function');
  });

  test('java-check module detects Java availability', async () => {
    const javaCheck = require('../../dist/lib/java-check');
    
    // This will check the actual system Java
    const result = await javaCheck.checkJavaVersion();
    
    expect(result).toHaveProperty('available');
    if (result.available) {
      expect(result).toHaveProperty('version');
      expect(result).toHaveProperty('majorVersion');
      expect(result).toHaveProperty('isCompatible');
    }
  });

  test('cache management functions exist', () => {
    const config = require('../../dist/lib/config');
    
    // Test that all expected functions exist
    expect(typeof config.getCacheDir).toBe('function');
    expect(typeof config.getJarPath).toBe('function');
    expect(typeof config.getVersionFilePath).toBe('function');
    expect(typeof config.isJarCached).toBe('function');
    expect(typeof config.getCachedVersion).toBe('function');
    
    // Test that functions return reasonable types
    expect(typeof config.getCacheDir()).toBe('string');
    expect(typeof config.getJarPath()).toBe('string');
    expect(typeof config.getVersionFilePath()).toBe('string');
    expect(typeof config.isJarCached()).toBe('boolean');
  });
});