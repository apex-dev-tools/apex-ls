import { spawn } from 'child_process';

export interface JavaInfo {
  available: boolean;
  version?: string;
  majorVersion?: number;
  isCompatible?: boolean;
  fullVersion?: string;
  error?: string;
}

export function checkJavaVersion(): Promise<JavaInfo> {
  return new Promise((resolve) => {
    const java = spawn('java', ['-version'], { stdio: ['ignore', 'ignore', 'pipe'] });
    let versionOutput = '';
    
    java.stderr.on('data', (data: Buffer) => {
      versionOutput += data.toString();
    });
    
    java.on('close', (code: number | null) => {
      if (code !== 0) {
        resolve({ available: false, error: 'Java not found in PATH' });
        return;
      }
      
      try {
        // Parse version from output like: java version "17.0.1" or openjdk version "17.0.1"
        const versionMatch = versionOutput.match(/version "(\d+)\.?\d*\.?\d*[^"]*"/);
        if (!versionMatch || !versionMatch[1] || !versionMatch[0]) {
          resolve({ available: false, error: 'Unable to parse Java version' });
          return;
        }
        
        // Handle Java 8 and below (format: 1.8.0) vs Java 9+ (format: 17.0.1)
        let majorVersion = parseInt(versionMatch[1], 10);
        if (majorVersion === 1) {
          // For Java 8 and below, parse the second number as the major version
          const secondVersionMatch = versionOutput.match(/version "1\.(\d+)/);
          if (secondVersionMatch && secondVersionMatch[1]) {
            majorVersion = parseInt(secondVersionMatch[1], 10);
          }
        }
        const isCompatible = majorVersion >= 17;
        
        resolve({
          available: true,
          version: versionMatch[1],
          majorVersion,
          isCompatible,
          fullVersion: versionMatch[0]
        });
      } catch (error) {
        resolve({ available: false, error: 'Failed to parse Java version output' });
      }
    });
    
    java.on('error', (error: Error) => {
      resolve({ available: false, error: error.message });
    });
  });
}

export async function validateJavaRequirements(): Promise<JavaInfo> {
  const javaInfo = await checkJavaVersion();
  
  if (!javaInfo.available) {
    throw new Error(`Java is required but not available: ${javaInfo.error}\nPlease install Java 17 or higher.`);
  }
  
  if (!javaInfo.isCompatible) {
    throw new Error(`Java ${javaInfo.majorVersion} is installed, but Java 17 or higher is required.\nPlease upgrade your Java installation.`);
  }
  
  return javaInfo;
}