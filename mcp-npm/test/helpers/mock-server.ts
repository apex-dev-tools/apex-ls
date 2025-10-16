import * as http from 'http';
import * as fs from 'fs';
import * as path from 'path';

interface MockResponse {
  statusCode: number;
  body: string | Buffer;
  headers: Record<string, string>;
}

/**
 * Create a mock HTTP server for testing downloads
 */
export class MockServer {
  private port: number;
  private server: http.Server | null = null;
  private responses: Map<string, MockResponse> = new Map();

  constructor(port: number = 8888) {
    this.port = port;
  }

  /**
   * Set up a response for a specific path
   */
  setResponse(path: string, statusCode: number, body: string | Buffer, headers: Record<string, string> = {}): void {
    this.responses.set(path, {
      statusCode,
      body,
      headers: {
        'Content-Type': 'application/octet-stream',
        ...headers
      }
    });
  }

  /**
   * Set up a JAR file response
   */
  setJarResponse(path: string, jarContent: Buffer | null = null): void {
    const content = jarContent || Buffer.from('PK\x03\x04mock-jar-content');
    this.setResponse(path, 200, content, {
      'Content-Type': 'application/java-archive',
      'Content-Length': content.length.toString()
    });
  }

  /**
   * Set up an error response
   */
  setErrorResponse(path: string, statusCode: number = 404, message: string = 'Not Found'): void {
    this.setResponse(path, statusCode, message, {
      'Content-Type': 'text/plain'
    });
  }

  /**
   * Set up a redirect response for a specific path
   */
  setRedirectResponse(path: string, redirectUrl: string, statusCode: number = 302): void {
    this.setResponse(path, statusCode, 'Found', {
      'Content-Type': 'text/plain',
      'Location': redirectUrl
    });
  }

  /**
   * Start the mock server
   */
  start(): Promise<string> {
    return new Promise((resolve, reject) => {
      this.server = http.createServer((req, res) => {
        const response = this.responses.get(req.url || '');
        
        if (!response) {
          res.writeHead(404, { 'Content-Type': 'text/plain' });
          res.end('Mock server: Path not configured');
          return;
        }

        res.writeHead(response.statusCode, response.headers);
        res.end(response.body);
      });

      this.server.listen(this.port, (err?: Error) => {
        if (err) {
          reject(err);
        } else {
          resolve(`http://localhost:${this.port}`);
        }
      });
    });
  }

  /**
   * Stop the mock server
   */
  stop(): Promise<void> {
    return new Promise((resolve) => {
      if (this.server) {
        this.server.close(() => {
          this.server = null;
          resolve();
        });
      } else {
        resolve();
      }
    });
  }

  /**
   * Get the base URL for the server
   */
  getBaseUrl(): string {
    return `http://localhost:${this.port}`;
  }
}