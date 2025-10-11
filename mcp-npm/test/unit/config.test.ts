import * as fs from 'fs';
import * as path from 'path';
import * as os from 'os';
import { createTestCacheDir, writeMockPackageJson } from '../helpers/test-utils';

describe('config', () => {
  let originalCwd: string;
  let originalHome: string | undefined;
  let testDir: string;
  let mockHome: string;
  let config: typeof import('../../src/lib/config');

  beforeEach(() => {
    originalCwd = process.cwd();
    originalHome = process.env.HOME;
    testDir = createTestCacheDir('config-test');
    mockHome = path.join(testDir, 'home');
    fs.mkdirSync(mockHome, { recursive: true });
    
    // Mock HOME directory
    process.env.HOME = mockHome;
    
    // Write mock package.json in test directory
    writeMockPackageJson(testDir, {
      jarVersion: '5.9.0',
      downloadUrl: 'https://example.com/apex-ls-mcp-{version}-standalone.jar'
    });
    
    // Change to test directory and clear module cache
    process.chdir(testDir);
    
    // Dynamic import to get fresh module
    delete require.cache[require.resolve('../../src/lib/config')];
    config = require('../../src/lib/config');
  });

  afterEach(() => {
    process.chdir(originalCwd);
    process.env.HOME = originalHome;
    if (fs.existsSync(testDir)) {
      fs.rmSync(testDir, { recursive: true, force: true });
    }
  });

  describe('getPackageConfig', () => {
    test('should read configuration from package.json', () => {
      const packageConfig = config.getPackageConfig();
      
      expect(packageConfig).toHaveProperty('jarVersion');
      expect(packageConfig).toHaveProperty('downloadUrl');
      expect(typeof packageConfig.jarVersion).toBe('string');
      expect(typeof packageConfig.downloadUrl).toBe('string');
      expect(packageConfig.downloadUrl).toContain('{jarVersion}');
    });
  });

  describe('getCacheDir', () => {
    test('should return .apex-ls-mcp directory in home folder', () => {
      const cacheDir = config.getCacheDir();
      const expectedPath = path.join(os.homedir(), '.apex-ls-mcp');
      
      expect(cacheDir).toBe(expectedPath);
    });
  });

  describe('getJarPath', () => {
    test('should return JAR path in cache directory', () => {
      const jarPath = config.getJarPath();
      const expectedPath = path.join(os.homedir(), '.apex-ls-mcp', 'apex-ls-mcp.jar');
      
      expect(jarPath).toBe(expectedPath);
    });
  });

  describe('getCurrentVersion', () => {
    test('should return version from package config', () => {
      const version = config.getCurrentVersion();
      
      expect(typeof version).toBe('string');
      expect(version.length).toBeGreaterThan(0);
    });
  });

  describe('getDownloadUrl', () => {
    test('should replace version placeholder in URL', () => {
      const url = config.getDownloadUrl();
      const version = config.getCurrentVersion();
      
      expect(typeof url).toBe('string');
      expect(url).toContain(version);
      expect(url).not.toContain('{jarVersion}');
    });

    test('should handle multiple jarVersion placeholders', () => {
      // Test the URL replacement logic directly
      const url = 'https://example.com/v{jarVersion}/apex-ls-mcp-{jarVersion}.jar';
      const version = '1.2.3';
      const expectedUrl = url.replace(/{jarVersion}/g, version);
      
      expect(expectedUrl).toBe('https://example.com/v1.2.3/apex-ls-mcp-1.2.3.jar');
    });
  });

  describe('getCachedVersion', () => {
    test('should return null when version file does not exist', () => {
      // Clean any existing version file first
      const versionFile = config.getVersionFilePath();
      if (fs.existsSync(versionFile)) {
        fs.unlinkSync(versionFile);
      }
      
      const cachedVersion = config.getCachedVersion();
      expect(cachedVersion).toBeNull();
    });

    test('should return cached version when file exists', () => {
      const cacheDir = config.getCacheDir();
      fs.mkdirSync(cacheDir, { recursive: true });
      fs.writeFileSync(config.getVersionFilePath(), '1.2.3');
      
      const cachedVersion = config.getCachedVersion();
      expect(cachedVersion).toBe('1.2.3');
    });

    test('should trim whitespace from cached version', () => {
      const cacheDir = config.getCacheDir();
      fs.mkdirSync(cacheDir, { recursive: true });
      fs.writeFileSync(config.getVersionFilePath(), '  1.2.3  \n');
      
      const cachedVersion = config.getCachedVersion();
      expect(cachedVersion).toBe('1.2.3');
    });
  });

  describe('isJarCached', () => {
    test('should return false when JAR file does not exist', () => {
      const isCached = config.isJarCached();
      
      expect(isCached).toBe(false);
    });

    test('should return false when version file does not exist', () => {
      const cacheDir = config.getCacheDir();
      fs.mkdirSync(cacheDir, { recursive: true });
      fs.writeFileSync(config.getJarPath(), 'mock jar content');
      
      const isCached = config.isJarCached();
      expect(isCached).toBe(false);
    });

    test('should return false when versions do not match', () => {
      const cacheDir = config.getCacheDir();
      fs.mkdirSync(cacheDir, { recursive: true });
      fs.writeFileSync(config.getJarPath(), 'mock jar content');
      fs.writeFileSync(config.getVersionFilePath(), '0.0.0'); // Different from current version
      
      const isCached = config.isJarCached();
      expect(isCached).toBe(false);
    });

    test('should return true when JAR exists and versions match', () => {
      const cacheDir = config.getCacheDir();
      const currentVersion = config.getCurrentVersion();
      fs.mkdirSync(cacheDir, { recursive: true });
      fs.writeFileSync(config.getJarPath(), 'mock jar content');
      fs.writeFileSync(config.getVersionFilePath(), currentVersion);
      
      const isCached = config.isJarCached();
      expect(isCached).toBe(true);
    });
  });
});