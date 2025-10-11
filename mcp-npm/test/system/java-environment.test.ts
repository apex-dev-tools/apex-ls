import * as fs from 'fs';
import * as path from 'path';
import { spawn } from 'child_process';
import { createTestCacheDir, writeMockPackageJson, createMockJar, mockConsole } from '../helpers/test-utils';

// Mock child_process.spawn for Java environment testing
jest.mock('child_process');
const mockSpawn = spawn as jest.MockedFunction<typeof spawn>;

// @ts-ignore - will be fixed when cli is properly typed
const { runApexLsMcp } = require('../../dist/cli');

describe('Java environment scenarios', () => {
  let testDir: string;
  let originalHome: string | undefined;
  let originalCwd: string;
  let mockHome: string;

  beforeEach(() => {
    originalHome = process.env.HOME;
    originalCwd = process.cwd();
    
    // Set up test directories
    testDir = createTestCacheDir('java-env-test');
    mockHome = path.join(testDir, 'home');
    fs.mkdirSync(mockHome, { recursive: true });
    
    // Mock HOME directory
    process.env.HOME = mockHome;
    
    // Create package directory
    const packageDir = path.join(testDir, 'package');
    fs.mkdirSync(packageDir, { recursive: true });
    process.chdir(packageDir);

    // Clear mocks
    jest.clearAllMocks();
  });

  afterEach(() => {
    process.chdir(originalCwd);
    process.env.HOME = originalHome;
    
    if (fs.existsSync(testDir)) {
      fs.rmSync(testDir, { recursive: true, force: true });
    }
  });

  test('should handle Java not available', async () => {
    // Set up cached JAR (so we get past install check)
    writeMockPackageJson(process.cwd(), {
      jarVersion: '1.0.0',
      downloadUrl: 'https://example.com/test.jar'
    });

    const cacheDir = path.join(mockHome, '.apex-ls-mcp');
    fs.mkdirSync(cacheDir, { recursive: true });
    createMockJar(path.join(cacheDir, 'apex-ls-mcp.jar'));
    fs.writeFileSync(path.join(cacheDir, 'version.txt'), '1.0.0');

    // Mock Java not being available (exit code 1)
    const mockJavaProcess = {
      stderr: { on: jest.fn() },
      on: jest.fn()
    };
    mockSpawn.mockReturnValue(mockJavaProcess as any);

    const console = mockConsole();
    const mockExit = jest.spyOn(process, 'exit').mockImplementation(() => {
      throw new Error('process.exit called');
    });

    const runPromise = runApexLsMcp();

    // Simulate Java not found (non-zero exit)
    const closeCallback = mockJavaProcess.on.mock.calls.find((call: any) => call[0] === 'close')[1];
    closeCallback(1);

    await expect(runPromise).rejects.toThrow('process.exit called');

    console.restore();
    mockExit.mockRestore();

    expect(console.errors[0]).toMatch(/Java is required but not available/);
  });

  test('should handle incompatible Java version (Java 8)', async () => {
    // Set up cached JAR
    writeMockPackageJson(process.cwd(), {
      jarVersion: '1.0.0',
      downloadUrl: 'https://example.com/test.jar'
    });

    const cacheDir = path.join(mockHome, '.apex-ls-mcp');
    fs.mkdirSync(cacheDir, { recursive: true });
    createMockJar(path.join(cacheDir, 'apex-ls-mcp.jar'));
    fs.writeFileSync(path.join(cacheDir, 'version.txt'), '1.0.0');

    // Mock Java 8 being available
    const mockJavaProcess = {
      stderr: { on: jest.fn() },
      on: jest.fn()
    };
    mockSpawn.mockReturnValue(mockJavaProcess as any);

    const console = mockConsole();
    const mockExit = jest.spyOn(process, 'exit').mockImplementation(() => {
      throw new Error('process.exit called');
    });

    const runPromise = runApexLsMcp();

    // Simulate Java 8 version output
    const stderrCallback = mockJavaProcess.stderr.on.mock.calls.find((call: any) => call[0] === 'data')[1];
    stderrCallback(Buffer.from('java version "1.8.0_301"'));
    
    const closeCallback = mockJavaProcess.on.mock.calls.find((call: any) => call[0] === 'close')[1];
    closeCallback(0);

    await expect(runPromise).rejects.toThrow('process.exit called');

    console.restore();
    mockExit.mockRestore();

    expect(console.errors[0]).toMatch(/Java \d+ is installed, but Java 17 or higher is required/);
  });
});