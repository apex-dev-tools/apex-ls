import * as fs from 'fs';
import * as path from 'path';
import { downloadJar } from '../../dist/lib/downloader';
import { MockServer } from '../helpers/mock-server';
import { createTestCacheDir } from '../helpers/test-utils';

describe('downloader', () => {
  let mockServer: MockServer;
  let testDir: string;

  beforeEach(async () => {
    mockServer = new MockServer(9999);
    await mockServer.start();
    testDir = createTestCacheDir('downloader-test');
  });

  afterEach(async () => {
    await mockServer.stop();
    if (fs.existsSync(testDir)) {
      fs.rmSync(testDir, { recursive: true, force: true });
    }
  });

  describe('downloadJar', () => {
    test('should download JAR file successfully', async () => {
      const jarContent = Buffer.from('PK\x03\x04test-jar-content');
      mockServer.setJarResponse('/test.jar', jarContent);
      
      const destinationPath = path.join(testDir, 'downloaded.jar');
      const url = `${mockServer.getBaseUrl()}/test.jar`;
      
      const result = await downloadJar(url, destinationPath);
      
      expect(result).toBe(destinationPath);
      expect(fs.existsSync(destinationPath)).toBe(true);
      
      const downloadedContent = fs.readFileSync(destinationPath);
      expect(downloadedContent.equals(jarContent)).toBe(true);
    });

    test('should create destination directory if it does not exist', async () => {
      const jarContent = Buffer.from('PK\x03\x04test-jar-content');
      mockServer.setJarResponse('/test.jar', jarContent);
      
      const nestedDir = path.join(testDir, 'nested', 'deep');
      const destinationPath = path.join(nestedDir, 'downloaded.jar');
      const url = `${mockServer.getBaseUrl()}/test.jar`;
      
      await downloadJar(url, destinationPath);
      
      expect(fs.existsSync(destinationPath)).toBe(true);
      expect(fs.existsSync(nestedDir)).toBe(true);
    });

    test('should throw error for 404 response', async () => {
      mockServer.setErrorResponse('/notfound.jar', 404, 'Not Found');
      
      const destinationPath = path.join(testDir, 'downloaded.jar');
      const url = `${mockServer.getBaseUrl()}/notfound.jar`;
      
      await expect(downloadJar(url, destinationPath)).rejects.toThrow(
        'JAR file not found at http://localhost:9999/notfound.jar. Please check if the version exists.'
      );
      
      expect(fs.existsSync(destinationPath)).toBe(false);
    });

    test('should throw error for 500 response', async () => {
      mockServer.setErrorResponse('/error.jar', 500, 'Internal Server Error');
      
      const destinationPath = path.join(testDir, 'downloaded.jar');
      const url = `${mockServer.getBaseUrl()}/error.jar`;
      
      await expect(downloadJar(url, destinationPath)).rejects.toThrow(
        'Failed to download: 500 Internal Server Error'
      );
      
      expect(fs.existsSync(destinationPath)).toBe(false);
    });

    test('should clean up partial download on error', async () => {
      // Mock a scenario where download starts but fails
      const destinationPath = path.join(testDir, 'downloaded.jar');
      const invalidUrl = 'http://localhost:9998/nonexistent.jar'; // Wrong port
      
      await expect(downloadJar(invalidUrl, destinationPath)).rejects.toThrow();
      
      // Verify no partial file is left behind
      expect(fs.existsSync(destinationPath)).toBe(false);
    });

    test('should throw error for empty download', async () => {
      mockServer.setResponse('/empty.jar', 200, Buffer.alloc(0));
      
      const destinationPath = path.join(testDir, 'downloaded.jar');
      const url = `${mockServer.getBaseUrl()}/empty.jar`;
      
      await expect(downloadJar(url, destinationPath)).rejects.toThrow(
        'Downloaded file is empty'
      );
      
      expect(fs.existsSync(destinationPath)).toBe(false);
    });

    test('should handle network errors gracefully', async () => {
      const destinationPath = path.join(testDir, 'downloaded.jar');
      // Use localhost with wrong port instead of unreachable IP for faster failure
      const unreachableUrl = 'http://localhost:9998/nonexistent.jar'; // Wrong port, should fail quickly
      
      await expect(downloadJar(unreachableUrl, destinationPath)).rejects.toThrow();
      expect(fs.existsSync(destinationPath)).toBe(false);
    }, 5000); // 5 second timeout

    test('should verify downloaded file has content', async () => {
      const jarContent = Buffer.from('PK\x03\x04significant-jar-content-here');
      mockServer.setJarResponse('/test.jar', jarContent);
      
      const destinationPath = path.join(testDir, 'downloaded.jar');
      const url = `${mockServer.getBaseUrl()}/test.jar`;
      
      await downloadJar(url, destinationPath);
      
      const stats = fs.statSync(destinationPath);
      expect(stats.size).toBeGreaterThan(0);
      expect(stats.size).toBe(jarContent.length);
    });
  });
});