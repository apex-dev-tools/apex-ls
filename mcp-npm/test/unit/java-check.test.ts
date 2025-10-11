import { spawn } from 'child_process';
import { checkJavaVersion, validateJavaRequirements } from '../../src/lib/java-check';

// Mock child_process.spawn
jest.mock('child_process');

const mockSpawn = spawn as jest.MockedFunction<typeof spawn>;

interface MockChildProcess {
  stderr: { on: jest.Mock };
  on: jest.Mock;
}

describe('java-check', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('checkJavaVersion', () => {
    test('should detect Java 17 as available and compatible', async () => {
      const mockJava: MockChildProcess = {
        stderr: { on: jest.fn() },
        on: jest.fn()
      };

      mockSpawn.mockReturnValue(mockJava as any);

      // Simulate Java 17 output
      const javaVersionPromise = checkJavaVersion();
      
      // Trigger stderr data event
      const stderrCallback = mockJava.stderr.on.mock.calls.find(call => call[0] === 'data')![1];
      stderrCallback(Buffer.from('openjdk version "17.0.1" 2021-10-19'));
      
      // Trigger close event with success
      const closeCallback = mockJava.on.mock.calls.find(call => call[0] === 'close')![1];
      closeCallback(0);

      const result = await javaVersionPromise;
      expect(result).toEqual({
        available: true,
        version: '17',
        majorVersion: 17,
        isCompatible: true,
        fullVersion: 'version "17.0.1"'
      });
    });

    test('should detect Java 8 as available but incompatible', async () => {
      const mockJava: MockChildProcess = {
        stderr: { on: jest.fn() },
        on: jest.fn()
      };

      mockSpawn.mockReturnValue(mockJava as any);

      const javaVersionPromise = checkJavaVersion();
      
      const stderrCallback = mockJava.stderr.on.mock.calls.find(call => call[0] === 'data')![1];
      stderrCallback(Buffer.from('java version "1.8.0_301"'));
      
      const closeCallback = mockJava.on.mock.calls.find(call => call[0] === 'close')![1];
      closeCallback(0);

      const result = await javaVersionPromise;
      expect(result).toEqual({
        available: true,
        version: '1',
        majorVersion: 8,
        isCompatible: false,
        fullVersion: 'version "1.8.0_301"'
      });
    });

    test('should detect Java 21 as available and compatible', async () => {
      const mockJava: MockChildProcess = {
        stderr: { on: jest.fn() },
        on: jest.fn()
      };

      mockSpawn.mockReturnValue(mockJava as any);

      const javaVersionPromise = checkJavaVersion();
      
      const stderrCallback = mockJava.stderr.on.mock.calls.find(call => call[0] === 'data')![1];
      stderrCallback(Buffer.from('openjdk version "21.0.0" 2023-09-19'));
      
      const closeCallback = mockJava.on.mock.calls.find(call => call[0] === 'close')![1];
      closeCallback(0);

      const result = await javaVersionPromise;
      expect(result).toEqual({
        available: true,
        version: '21',
        majorVersion: 21,
        isCompatible: true,
        fullVersion: 'version "21.0.0"'
      });
    });

    test('should handle Java not found', async () => {
      const mockJava: MockChildProcess = {
        stderr: { on: jest.fn() },
        on: jest.fn()
      };

      mockSpawn.mockReturnValue(mockJava as any);

      const javaVersionPromise = checkJavaVersion();
      
      const closeCallback = mockJava.on.mock.calls.find(call => call[0] === 'close')![1];
      closeCallback(1); // Non-zero exit code

      const result = await javaVersionPromise;
      expect(result).toEqual({
        available: false,
        error: 'Java not found in PATH'
      });
    });

    test('should handle spawn error', async () => {
      const mockJava: MockChildProcess = {
        stderr: { on: jest.fn() },
        on: jest.fn()
      };

      mockSpawn.mockReturnValue(mockJava as any);

      const javaVersionPromise = checkJavaVersion();
      
      const errorCallback = mockJava.on.mock.calls.find(call => call[0] === 'error')![1];
      errorCallback(new Error('spawn java ENOENT'));

      const result = await javaVersionPromise;
      expect(result).toEqual({
        available: false,
        error: 'spawn java ENOENT'
      });
    });

    test('should handle unparseable version output', async () => {
      const mockJava: MockChildProcess = {
        stderr: { on: jest.fn() },
        on: jest.fn()
      };

      mockSpawn.mockReturnValue(mockJava as any);

      const javaVersionPromise = checkJavaVersion();
      
      const stderrCallback = mockJava.stderr.on.mock.calls.find(call => call[0] === 'data')![1];
      stderrCallback(Buffer.from('invalid version output'));
      
      const closeCallback = mockJava.on.mock.calls.find(call => call[0] === 'close')![1];
      closeCallback(0);

      const result = await javaVersionPromise;
      expect(result).toEqual({
        available: false,
        error: 'Unable to parse Java version'
      });
    });
  });

  describe('validateJavaRequirements', () => {
    test('should pass validation for compatible Java version', async () => {
      const mockJava: MockChildProcess = {
        stderr: { on: jest.fn() },
        on: jest.fn()
      };

      mockSpawn.mockReturnValue(mockJava as any);

      const validationPromise = validateJavaRequirements();
      
      const stderrCallback = mockJava.stderr.on.mock.calls.find(call => call[0] === 'data')![1];
      stderrCallback(Buffer.from('openjdk version "17.0.1"'));
      
      const closeCallback = mockJava.on.mock.calls.find(call => call[0] === 'close')![1];
      closeCallback(0);

      const result = await validationPromise;
      expect(result).toEqual({
        available: true,
        version: '17',
        majorVersion: 17,
        isCompatible: true,
        fullVersion: 'version "17.0.1"'
      });
    });

    test('should throw error when Java not available', async () => {
      const mockJava: MockChildProcess = {
        stderr: { on: jest.fn() },
        on: jest.fn()
      };

      mockSpawn.mockReturnValue(mockJava as any);

      const validationPromise = validateJavaRequirements();
      
      const closeCallback = mockJava.on.mock.calls.find(call => call[0] === 'close')![1];
      closeCallback(1);

      await expect(validationPromise).rejects.toThrow(
        'Java is required but not available: Java not found in PATH\nPlease install Java 17 or higher.'
      );
    });

    test('should throw error when Java version incompatible', async () => {
      const mockJava: MockChildProcess = {
        stderr: { on: jest.fn() },
        on: jest.fn()
      };

      mockSpawn.mockReturnValue(mockJava as any);

      const validationPromise = validateJavaRequirements();
      
      const stderrCallback = mockJava.stderr.on.mock.calls.find(call => call[0] === 'data')![1];
      stderrCallback(Buffer.from('java version "1.8.0_301"'));
      
      const closeCallback = mockJava.on.mock.calls.find(call => call[0] === 'close')![1];
      closeCallback(0);

      await expect(validationPromise).rejects.toThrow(
        'Java 8 is installed, but Java 17 or higher is required.\nPlease upgrade your Java installation.'
      );
    });
  });
});