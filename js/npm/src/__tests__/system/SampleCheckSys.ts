/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
import chalk from "chalk";
import { spawnSync } from "child_process";
import { readdirSync, lstatSync, writeFileSync, unlinkSync, existsSync } from "fs";
import { basename, resolve, sep } from "path";

const isCategory =
  (...cats: string[]) =>
  (str: string) =>
    cats.some((c) => str.startsWith(c));
const isErrorCat = isCategory("Syntax", "Error", "Missing");
const isWarnCat = isCategory("Warning", "Unused");
const isSyntaxCat = isCategory("Syntax");

// .each runs first before any hooks like beforeAll
function getSamples(): [string, string, string | null][] {
  if (!process.env.SAMPLES) {
    throw new Error(
      "Missing environment variable 'SAMPLES' with path to samples."
    );
  }
  const sampleDir = resolve(process.env.SAMPLES);
  return readdirSync(sampleDir)
    .filter((f) => !/(^|\/)\.[^\/\.]/g.test(f)) // not hidden
    .map((f) => resolve(sampleDir, f)) // full path
    .filter((f) => lstatSync(f).isDirectory())
    .map((d) => findProjectDir(d));
}

function findProjectDir(wd: string): [string, string, string | null] {
  // Returns [project name, directory to test, temporary file path or null]
  const projName = basename(wd);
  const parentDir = wd;
  const nestedDir = resolve(wd, projName);
  
  // Check if parent has sfdx-project.json
  if (existsSync(resolve(parentDir, "sfdx-project.json"))) {
    return [projName, parentDir, null];
  }
  
  // Check if nested has sfdx-project.json  
  if (existsSync(resolve(nestedDir, "sfdx-project.json"))) {
    return [projName, nestedDir, null];
  }
  
  // No sfdx-project.json found - create temporary one in parent
  const tempSfdxProject = createTemporarySfdxProject(parentDir, projName);
  return [projName, tempSfdxProject ? parentDir : nestedDir, tempSfdxProject];
}

function createTemporarySfdxProject(parentDir: string, packageDirName: string): string | null {
  const sfdxProjectPath = resolve(parentDir, "sfdx-project.json");
  
  // Don't create if already exists
  if (existsSync(sfdxProjectPath)) {
    return null;
  }
  
  const sfdxProjectContent = {
    packageDirectories: [
      {
        path: packageDirName,
        default: true
      }
    ],
    sourceApiVersion: "60.0"
  };
  
  try {
    writeFileSync(sfdxProjectPath, JSON.stringify(sfdxProjectContent, null, 2));
    return sfdxProjectPath;
  } catch (error) {
    console.warn(`Failed to create temporary sfdx-project.json at ${sfdxProjectPath}: ${error}`);
    return null;
  }
}

function cleanupTemporaryFile(filePath: string | null): void {
  if (filePath && existsSync(filePath)) {
    try {
      unlinkSync(filePath);
    } catch (error) {
      console.warn(`Failed to cleanup temporary file ${filePath}: ${error}`);
    }
  }
}

function printLogs(
  logs: string[],
  errLogs: string[],
  name: string,
  status: number
): void {
  const prefix = `[${name}](status: ${status}) check output:`;
  if (logs.length) {
    console.log(prefix);
    console.log(
      logs
        .map((msg) => {
          if (isErrorCat(msg)) {
            return chalk.red(msg);
          } else if (isWarnCat(msg)) {
            return chalk.yellow(msg);
          }
          return msg;
        })
        .join("\n")
    );
    console.log(); // \n
  } else if (errLogs.length) {
    console.log(`${prefix} ${chalk.redBright("Exception")}`);
    console.log(errLogs);
    console.log(); // \n
  } else {
    console.log(`${prefix} ${chalk.green("No issues found")}\n`);
  }
}

function applyMessageFilters(msg: string): string {
  if (isSyntaxCat(msg)) {
    // Syntax errors can include large list of tokens
    // that changes on parser update causing failure
    if (msg.length > 250) {
      return msg.split(" expecting {", 2)[0];
    }
  }
  if (isWarnCat(msg)) {
    // msg for ambiguous flips between the related methods
    // need to cut the end off
    if (msg.includes("Ambiguous method call")) {
      return msg.split(",", 2)[0];
    }
  }
  return msg;
}

function prepareSnapshotLogs(logs: string[], path: string): string[][] {
  const headings: number[] = [];

  const snapLogs = logs.reduce((parsed: string[], log: string) => {
    const isHead = log.startsWith(path);

    // Strip unique abs paths
    let msg = log.replaceAll(`${path}${sep}`, "");

    // Modify frequently changing messages
    // to make snapshot deterministic
    if (!isHead) {
      msg = applyMessageFilters(msg);
    }

    const length = parsed.push(msg);

    // track headings for splitting
    isHead && headings.push(length - 1);

    return parsed;
  }, []);

  return headings.map((headIdx, ptrIdx) => {
    const [head, ...msgs] = snapLogs.slice(headIdx, headings[ptrIdx + 1]);
    return [
      head,
      // sort to remove message order changes
      ...msgs.sort(),
    ];
  });
}

describe("Check samples", () => {
  // disable jest wrapped logging
  const jestConsole = console;
  beforeEach(() => {
    global.console = require("console");
  });
  afterEach(() => {
    global.console = jestConsole;
  });

  test.each(getSamples())(
    "Sample: %s",
    async (name, path, tempSfdxProject) => {
      try {
        const jvmCheck = spawnSync(
          "java",
          [
            "-cp",
            "jvm/target/scala-2.13/*:jvm/target/scala-2.13/apex-ls_2.13-*.jar",
            "io.github.apexdevtools.apexls.CheckForIssues",
            "-d", "warnings",
            "-n",
            "-w",
            path,
          ],
          {
            // can only be run from npm dir
            cwd: resolve(process.cwd(), "../.."),
            timeout: 30000,
          }
        );

        // Check it was not cancelled due to timeout
        expect(jvmCheck.signal).toBeNull();

        const status = jvmCheck.status;
        const logs: string[] = jvmCheck.stdout
          .toString("utf8")
          .split("\n")
          .filter((l) => l);

        // only print exceptions, do not snapshot
        const errLogs = jvmCheck.stderr
          .toString("utf8")
          .split("\n")
          .filter((l) => l);

        printLogs(logs, errLogs, name, status);

        const result = {
          status,
          logs: prepareSnapshotLogs(logs, path),
        };

        expect(result).toMatchSnapshot();
      } finally {
        // Always cleanup temporary file
        cleanupTemporaryFile(tempSfdxProject);
      }
    },
    40000
  );
});
