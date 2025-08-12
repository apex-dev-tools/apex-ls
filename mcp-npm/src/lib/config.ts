import * as fs from 'fs';
import * as path from 'path';
import * as os from 'os';

interface PackageConfig {
  jarVersion: string;
  downloadUrl: string;
}

interface PackageJson {
  config: PackageConfig;
}

export function getPackageConfig(): PackageConfig {
  const packageJsonPath = path.join(__dirname, '..', '..', 'package.json');
  const packageJson = JSON.parse(fs.readFileSync(packageJsonPath, 'utf8')) as PackageJson;
  return packageJson.config;
}

export function getCacheDir(): string {
  return path.join(os.homedir(), '.apex-ls-mcp');
}

export function getJarPath(): string {
  return path.join(getCacheDir(), 'apex-ls-mcp.jar');
}

export function getVersionFilePath(): string {
  return path.join(getCacheDir(), 'version.txt');
}

export function getCurrentVersion(): string {
  const config = getPackageConfig();
  return config.jarVersion;
}

export function getDownloadUrl(): string {
  const config = getPackageConfig();
  const version = getCurrentVersion();
  return config.downloadUrl.replace(/{jarVersion}/g, version);
}

export function getCachedVersion(): string | null {
  const versionFile = getVersionFilePath();
  if (fs.existsSync(versionFile)) {
    return fs.readFileSync(versionFile, 'utf8').trim();
  }
  return null;
}

export function isJarCached(): boolean {
  const jarPath = getJarPath();
  const cachedVersion = getCachedVersion();
  const currentVersion = getCurrentVersion();
  
  return fs.existsSync(jarPath) && cachedVersion === currentVersion;
}