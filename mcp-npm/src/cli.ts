#!/usr/bin/env node

import { spawn } from 'child_process';
import * as path from 'path';
import * as fs from 'fs';
import * as config from './lib/config';
import * as javaCheck from './lib/java-check';

export async function runApexLsMcp(): Promise<void> {
  try {
    // Validate Java requirements
    const javaInfo = await javaCheck.validateJavaRequirements();
    
    // Check if JAR is cached
    if (!config.isJarCached()) {
      console.error('apex-ls-mcp JAR not found or version mismatch.');
      console.error('Please run: npm install apex-ls-mcp');
      process.exit(1);
    }
    
    const jarPath = config.getJarPath();
    
    // Spawn Java process with the JAR and pass through all arguments
    const args = ['-jar', jarPath, ...process.argv.slice(2)];
    const child = spawn('java', args, { 
      stdio: 'inherit',
      windowsHide: true
    });
    
    // Handle process exit
    child.on('exit', (code: number | null, signal: NodeJS.Signals | null) => {
      if (signal) {
        process.kill(process.pid, signal);
      } else {
        process.exit(code || 0);
      }
    });
    
    // Handle process errors
    child.on('error', (error: Error) => {
      if ((error as NodeJS.ErrnoException).code === 'ENOENT') {
        console.error('Java not found. Please ensure Java 17+ is installed and in your PATH.');
      } else {
        console.error('Failed to start apex-ls-mcp:', error.message);
      }
      process.exit(1);
    });
    
    // Handle termination signals
    process.on('SIGINT', () => {
      child.kill('SIGINT');
    });
    
    process.on('SIGTERM', () => {
      child.kill('SIGTERM');
    });
    
  } catch (error) {
    console.error('Error running apex-ls-mcp:', (error as Error).message);
    process.exit(1);
  }
}

if (require.main === module) {
  runApexLsMcp();
}