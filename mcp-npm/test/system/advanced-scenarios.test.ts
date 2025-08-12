import * as fs from 'fs';
import * as path from 'path';
import { createTestCacheDir, writeMockPackageJson, mockConsole } from '../helpers/test-utils';
import { MockServer } from '../helpers/mock-server';
// @ts-ignore - will be fixed when install is properly typed
const { install } = require('../../dist/install');

describe('Advanced system scenarios', () => {
  let testDir: string;
  let originalHome: string | undefined;
  let originalCwd: string;
  let mockHome: string;
  let mockServer: MockServer;

  beforeEach(async () => {
    originalHome = process.env.HOME;
    originalCwd = process.cwd();
    
    // Set up test directories
    testDir = createTestCacheDir('advanced-system-test');
    mockHome = path.join(testDir, 'home');
    fs.mkdirSync(mockHome, { recursive: true });
    
    // Mock HOME directory
    process.env.HOME = mockHome;
    
    // Set up mock server
    mockServer = new MockServer(9995);
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

  describe('Network edge cases', () => {
    test('should handle DNS resolution failure', async () => {
      writeMockPackageJson(process.cwd(), {
        jarVersion: '1.0.0',
        downloadUrl: 'http://nonexistent-domain-for-testing.invalid/test.jar'
      });

      const console = mockConsole();
      
      await expect(install()).rejects.toThrow();
      
      console.restore();
      
      expect(console.errors[0]).toContain('Failed to install apex-ls-mcp:');
      expect(console.errors.some((err: string) => err.includes('Network error'))).toBe(true);
    }, 15000); // Longer timeout for DNS resolution

    test('should handle server errors (500, 503)', async () => {
      writeMockPackageJson(process.cwd(), {
        jarVersion: '1.0.0',
        downloadUrl: `${mockServer.getBaseUrl()}/server-error.jar`
      });

      // Mock server error response
      mockServer.setErrorResponse('/server-error.jar', 500, 'Internal Server Error');

      const console = mockConsole();
      
      await expect(install()).rejects.toThrow();
      
      console.restore();
      
      expect(console.errors[0]).toContain('Failed to install apex-ls-mcp:');
      expect(console.errors[0]).toContain('500 Internal Server Error');
    });

    test('should handle redirect scenarios', async () => {
      writeMockPackageJson(process.cwd(), {
        jarVersion: '1.0.0',
        downloadUrl: `${mockServer.getBaseUrl()}/redirect.jar`
      });

      // Mock redirect response that will fail when followed
      mockServer.setRedirectResponse('/redirect.jar', 'http://nonexistent-domain.invalid/actual.jar');
      
      const console = mockConsole();
      
      await expect(install()).rejects.toThrow();
      
      console.restore();
      
      expect(console.errors[0]).toContain('Failed to install apex-ls-mcp:');
      // The redirect will be followed and fail with a network error
      expect(console.errors.some((err: string) => err.includes('Network error') || err.includes('ENOTFOUND'))).toBe(true);
    });
  });

  describe('Cache advanced scenarios', () => {
    test('should recover from corrupted JAR cache', async () => {
      const cacheDir = path.join(mockHome, '.apex-ls-mcp');
      fs.mkdirSync(cacheDir, { recursive: true });
      
      // Create corrupted cache state - version file exists but JAR is corrupted
      fs.writeFileSync(path.join(cacheDir, 'version.txt'), '1.0.0');
      fs.writeFileSync(path.join(cacheDir, 'apex-ls-mcp.jar'), 'corrupted-content');

      writeMockPackageJson(process.cwd(), {
        jarVersion: '1.0.0',
        downloadUrl: `${mockServer.getBaseUrl()}/apex-ls-mcp-{jarVersion}-standalone.jar`
      });

      const correctJarContent = Buffer.from('PK\x03\x04correct-jar-content');
      mockServer.setJarResponse('/apex-ls-mcp-1.0.0-standalone.jar', correctJarContent);

      // Install should detect corruption and re-download
      await install();

      const jarPath = path.join(cacheDir, 'apex-ls-mcp.jar');
      expect(fs.readFileSync(jarPath).equals(correctJarContent)).toBe(true);
    });

    test('should maintain cache consistency across multiple projects', async () => {
      const cacheDir = path.join(mockHome, '.apex-ls-mcp');
      
      // Create first project
      const project1Dir = path.join(testDir, 'project1');
      fs.mkdirSync(project1Dir, { recursive: true });
      process.chdir(project1Dir);
      
      writeMockPackageJson(project1Dir, {
        jarVersion: '1.0.0',
        downloadUrl: `${mockServer.getBaseUrl()}/apex-ls-mcp-{jarVersion}-standalone.jar`
      });

      const jarContent = Buffer.from('PK\x03\x04shared-cache-content');
      mockServer.setJarResponse('/apex-ls-mcp-1.0.0-standalone.jar', jarContent);

      await install();

      // Verify cache was created
      const jarPath = path.join(cacheDir, 'apex-ls-mcp.jar');
      expect(fs.existsSync(jarPath)).toBe(true);
      
      // Create second project with same version
      const project2Dir = path.join(testDir, 'project2');
      fs.mkdirSync(project2Dir, { recursive: true });
      process.chdir(project2Dir);
      
      writeMockPackageJson(project2Dir, {
        jarVersion: '1.0.0',
        downloadUrl: `${mockServer.getBaseUrl()}/apex-ls-mcp-{jarVersion}-standalone.jar`
      });

      const console = mockConsole();

      // This should use the cached version, not download again
      await install();

      console.restore();

      // Verify same cache is used and "already cached" message appears
      expect(fs.existsSync(jarPath)).toBe(true);
      expect(fs.readFileSync(jarPath).equals(jarContent)).toBe(true);
      expect(console.logs).toContain('apex-ls-mcp v1.0.0 already cached');
    });
  });
});