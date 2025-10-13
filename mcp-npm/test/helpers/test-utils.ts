import * as fs from 'fs';
import * as path from 'path';
import * as os from 'os';

interface MockPackageConfig {
  jarVersion?: string;
  downloadUrl?: string;
}

interface MockPackageJson {
  name: string;
  version: string;
  config: {
    jarVersion: string;
    downloadUrl: string;
  };
}

interface MockConsole {
  logs: string[];
  errors: string[];
  restore: () => void;
}

/**
 * Create a temporary test cache directory
 */
export function createTestCacheDir(testName: string): string {
  const testCacheDir = path.join(os.tmpdir(), 'apex-ls-mcp-test', testName);
  if (!fs.existsSync(testCacheDir)) {
    fs.mkdirSync(testCacheDir, { recursive: true });
  }
  return testCacheDir;
}

/**
 * Create a mock package.json for testing
 */
export function createMockPackageJson(config: MockPackageConfig = {}): MockPackageJson {
  const defaultConfig = {
    jarVersion: '5.9.0',
    downloadUrl: 'http://localhost:8888/apex-ls-mcp-{jarVersion}-standalone.jar'
  };
  
  return {
    name: 'apex-ls-mcp',
    version: '1.0.0',
    config: { ...defaultConfig, ...config }
  };
}

/**
 * Write a mock package.json file
 */
export function writeMockPackageJson(targetDir: string, config: MockPackageConfig = {}): string {
  const packageJson = createMockPackageJson(config);
  const packagePath = path.join(targetDir, 'package.json');
  fs.writeFileSync(packagePath, JSON.stringify(packageJson, null, 2));
  return packagePath;
}

/**
 * Create a mock JAR file for testing
 */
export function createMockJar(filePath: string): string {
  const jarContent = Buffer.from('PK\x03\x04mock-jar-content');
  fs.writeFileSync(filePath, jarContent);
  return filePath;
}

/**
 * Mock console methods and capture output
 */
export function mockConsole(): MockConsole {
  const originalLog = console.log;
  const originalError = console.error;
  const logs: string[] = [];
  const errors: string[] = [];
  
  console.log = (...args: any[]) => logs.push(args.join(' '));
  console.error = (...args: any[]) => errors.push(args.join(' '));
  
  return {
    logs,
    errors,
    restore: () => {
      console.log = originalLog;
      console.error = originalError;
    }
  };
}

/**
 * Wait for a specified number of milliseconds
 */
export function sleep(ms: number): Promise<void> {
  return new Promise(resolve => setTimeout(resolve, ms));
}