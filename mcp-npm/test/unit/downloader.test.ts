import * as fs from 'fs';
import * as path from 'path';
import { downloadJar } from '../../dist/lib/downloader';
import { createTestCacheDir } from '../helpers/test-utils';
import { Readable } from 'stream';

// Mock global fetch
const mockFetch = jest.fn() as jest.MockedFunction<typeof fetch>;
global.fetch = mockFetch;

// Helper function to create a readable stream from buffer
function createReadableStream(buffer: Buffer): Readable {
  const readable = new Readable();
  readable.push(buffer);
  readable.push(null);
  return readable;
}

describe('downloader', () => {
  let testDir: string;

  beforeEach(() => {
    testDir = createTestCacheDir('downloader-test');
    jest.clearAllMocks();
  });

  afterEach(() => {
    if (fs.existsSync(testDir)) {
      fs.rmSync(testDir, { recursive: true, force: true });
    }
  });

  describe('downloadJar', () => {
    test('should download JAR file successfully', async () => {
      const jarContent = Buffer.from('PK\x03\x04test-jar-content');
      
      mockFetch.mockResolvedValue({
        ok: true,
        status: 200,
        statusText: 'OK',
        body: createReadableStream(jarContent)
      } as any);
      
      const destinationPath = path.join(testDir, 'downloaded.jar');
      const url = 'https://example.com/test.jar';
      
      const result = await downloadJar(url, destinationPath);
      
      expect(result).toBe(destinationPath);
      expect(fs.existsSync(destinationPath)).toBe(true);
      
      const downloadedContent = fs.readFileSync(destinationPath);
      expect(downloadedContent.equals(jarContent)).toBe(true);
    });

    test('should create destination directory if it does not exist', async () => {
      const jarContent = Buffer.from('PK\x03\x04test-jar-content');
      
      mockFetch.mockResolvedValue({
        ok: true,
        status: 200,
        statusText: 'OK',
        body: createReadableStream(jarContent)
      } as any);
      
      const nestedDir = path.join(testDir, 'nested', 'deep');
      const destinationPath = path.join(nestedDir, 'downloaded.jar');
      const url = 'https://example.com/test.jar';
      
      await downloadJar(url, destinationPath);
      
      expect(fs.existsSync(destinationPath)).toBe(true);
      expect(fs.existsSync(nestedDir)).toBe(true);
    });

    test('should throw error for 404 response', async () => {
      const url = 'https://example.com/notfound.jar';
      
      mockFetch.mockResolvedValue({
        ok: false,
        status: 404,
        statusText: 'Not Found'
      } as any);
      
      const destinationPath = path.join(testDir, 'downloaded.jar');
      
      await expect(downloadJar(url, destinationPath)).rejects.toThrow(
        `JAR file not found at ${url}. Please check if the version exists.`
      );
      
      expect(fs.existsSync(destinationPath)).toBe(false);
    });

    test('should throw error for 500 response', async () => {
      const url = 'https://example.com/error.jar';
      
      mockFetch.mockResolvedValue({
        ok: false,
        status: 500,
        statusText: 'Internal Server Error'
      } as any);
      
      const destinationPath = path.join(testDir, 'downloaded.jar');
      
      await expect(downloadJar(url, destinationPath)).rejects.toThrow(
        'Failed to download: 500 Internal Server Error'
      );
      
      expect(fs.existsSync(destinationPath)).toBe(false);
    });

    test('should clean up partial download on error', async () => {
      const url = 'https://example.com/test.jar';
      
      mockFetch.mockRejectedValue(new Error('Network error'));
      
      const destinationPath = path.join(testDir, 'downloaded.jar');
      
      await expect(downloadJar(url, destinationPath)).rejects.toThrow();
      
      // Verify no partial file is left behind
      expect(fs.existsSync(destinationPath)).toBe(false);
    });

    test('should throw error for empty download', async () => {
      const emptyBuffer = Buffer.alloc(0);
      
      mockFetch.mockResolvedValue({
        ok: true,
        status: 200,
        statusText: 'OK',
        body: createReadableStream(emptyBuffer)
      } as any);
      
      const destinationPath = path.join(testDir, 'downloaded.jar');
      const url = 'https://example.com/empty.jar';
      
      await expect(downloadJar(url, destinationPath)).rejects.toThrow(
        'Downloaded file is empty'
      );
      
      expect(fs.existsSync(destinationPath)).toBe(false);
    });

    test('should handle network errors gracefully', async () => {
      const url = 'https://example.com/test.jar';
      
      mockFetch.mockRejectedValue(new Error('ENOTFOUND'));
      
      const destinationPath = path.join(testDir, 'downloaded.jar');
      
      await expect(downloadJar(url, destinationPath)).rejects.toThrow();
      expect(fs.existsSync(destinationPath)).toBe(false);
    });

    test('should verify downloaded file has content', async () => {
      const jarContent = Buffer.from('PK\x03\x04significant-jar-content-here');
      
      mockFetch.mockResolvedValue({
        ok: true,
        status: 200,
        statusText: 'OK',
        body: createReadableStream(jarContent)
      } as any);
      
      const destinationPath = path.join(testDir, 'downloaded.jar');
      const url = 'https://example.com/test.jar';
      
      await downloadJar(url, destinationPath);
      
      const stats = fs.statSync(destinationPath);
      expect(stats.size).toBeGreaterThan(0);
      expect(stats.size).toBe(jarContent.length);
    });

    test('should handle null response body', async () => {
      mockFetch.mockResolvedValue({
        ok: true,
        status: 200,
        statusText: 'OK',
        body: null
      } as any);
      
      const destinationPath = path.join(testDir, 'downloaded.jar');
      const url = 'https://example.com/test.jar';
      
      await expect(downloadJar(url, destinationPath)).rejects.toThrow(
        'Response body is null'
      );
      
      expect(fs.existsSync(destinationPath)).toBe(false);
    });
  });
});