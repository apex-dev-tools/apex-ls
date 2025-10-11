#!/usr/bin/env node

import * as fs from 'fs';
import * as path from 'path';
import * as os from 'os';
import * as downloader from './lib/downloader';
import * as config from './lib/config';

export async function install(): Promise<void> {
  try {
    console.log('Installing apex-ls-mcp...');
    
    const jarVersion = config.getCurrentVersion();
    const downloadUrl = config.getDownloadUrl();
    
    const cacheDir = config.getCacheDir();
    const jarPath = config.getJarPath();
    const versionFile = config.getVersionFilePath();
    
    // Create cache directory if it doesn't exist
    if (!fs.existsSync(cacheDir)) {
      fs.mkdirSync(cacheDir, { recursive: true });
    }
    
    // Check if we need to download (version mismatch or jar doesn't exist)
    let needsDownload = true;
    if (fs.existsSync(jarPath) && fs.existsSync(versionFile)) {
      const cachedVersion = fs.readFileSync(versionFile, 'utf8').trim();
      if (cachedVersion === jarVersion) {
        // Validate JAR file integrity (basic check for ZIP/JAR signature)
        try {
          const jarBuffer = fs.readFileSync(jarPath);
          const ZIP_SIGNATURE = Buffer.from([0x50, 0x4B, 0x03, 0x04]); // 'PK\x03\x04'
          const isValidJar = jarBuffer.length > 0 && jarBuffer.subarray(0, 4).equals(ZIP_SIGNATURE);

          if (isValidJar) {
            console.log(`apex-ls-mcp v${jarVersion} already cached`);
            needsDownload = false;
          } else {
            console.log(`Cached JAR is corrupted, re-downloading...`);
          }
        } catch (error) {
          console.log(`Error reading cached JAR, re-downloading...`);
        }
      }
    }
    
    if (needsDownload) {
      console.log(`Downloading apex-ls-mcp v${jarVersion}...`);
      await downloader.downloadJar(downloadUrl, jarPath);
      
      // Write version file
      fs.writeFileSync(versionFile, jarVersion);
      console.log(`apex-ls-mcp v${jarVersion} installed successfully`);
    }
    
  } catch (error) {
    const err = error as NodeJS.ErrnoException;
    console.error('Failed to install apex-ls-mcp:', err.message);
    
    if (err.code === 'ENOTFOUND' || err.code === 'ECONNREFUSED') {
      console.error('Network error. Please check your internet connection and try again.');
    } else if (err.code === 'EACCES') {
      console.error('Permission denied. Please check file permissions for your home directory.');
    }
    
    // For programmatic usage (like tests), throw the error instead of exiting
    if (require.main !== module) {
      throw error;
    }
    
    process.exit(1);
  }
}

if (require.main === module) {
  install();
}