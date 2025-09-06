import * as fs from 'fs';
import * as path from 'path';
import { pipeline } from 'stream/promises';
import fetch from 'node-fetch';

export async function downloadJar(url: string, destinationPath: string): Promise<string> {
  try {
    const response = await fetch(url);
    
    if (!response.ok) {
      if (response.status === 404) {
        throw new Error(`JAR file not found at ${url}. Please check if the version exists.`);
      }
      throw new Error(`Failed to download: ${response.status} ${response.statusText}`);
    }
    
    // Ensure destination directory exists
    const destinationDir = path.dirname(destinationPath);
    if (!fs.existsSync(destinationDir)) {
      fs.mkdirSync(destinationDir, { recursive: true });
    }
    
    // Download and save the file
    if (!response.body) {
      throw new Error('Response body is null');
    }
    
    await pipeline(response.body, fs.createWriteStream(destinationPath));
    
    // Verify the downloaded file exists and has content
    const stats = fs.statSync(destinationPath);
    if (stats.size === 0) {
      throw new Error('Downloaded file is empty');
    }
    
    return destinationPath;
    
  } catch (error) {
    // Clean up partial download
    if (fs.existsSync(destinationPath)) {
      try {
        fs.unlinkSync(destinationPath);
      } catch (cleanupError) {
        console.warn(`Failed to cleanup partial download at ${destinationPath}: ${cleanupError}`);
      }
    }
    throw error;
  }
}