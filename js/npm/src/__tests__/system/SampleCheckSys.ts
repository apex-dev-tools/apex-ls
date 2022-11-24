import chalk from "chalk";
import { spawnSync } from "child_process";
import { readdirSync, lstatSync } from "fs";
import { basename, resolve, sep } from "path"

const isCategory = (...cats: string[]) => (str: string) => cats.some(c => str.startsWith(c));
const isErrorCat = isCategory("Syntax", "Error", "Missing");
const isWarnCat = isCategory("Warning", "Unused");

// .each runs first before any hooks like beforeAll
function getSamples(): string[][] {
    if (!process.env.SAMPLES) {
        throw new Error("Missing environment variable 'SAMPLES' with path to samples.");
    }
    const sampleDir = resolve(process.env.SAMPLES);
    return readdirSync(sampleDir)
        .filter(f => !(/(^|\/)\.[^\/\.]/g).test(f)) // not hidden
        .map(f => resolve(sampleDir, f)) // full path
        .filter(f => lstatSync(f).isDirectory())
        .map(d => findProjectDir(d))
}

function findProjectDir(wd: string): [string, string] {
    // Detects override sfdx-project or descends 1 level into sample
    const projName = basename(wd);
    return [
        projName,
        readdirSync(wd)
            .includes("sfdx-project.json") ? wd : resolve(wd, projName)
    ];
}

function printLogs(logs: string[], name: string, status: number): void {
    if (logs.length) {
        console.log(`[${name}](status: ${status}) check output:`);
        console.log(
            logs.map(l => {
                if (isErrorCat(l)) {
                    return chalk.red(l);
                } else if (isWarnCat(l)) {
                    return chalk.yellow(l);
                }
                return l;
            }).join("\n")
        );
        console.log(); // \n
    } else {
        console.log(
            `[${name}](status: ${status}) check output: ${chalk.green("No issues found")}\n`
        );
    }
}

function prepareSnapshotLogs(logs: string[], path: string): string[][] {
    const headings: number[] = [];

    const snapLogs = logs.reduce((parsed: string[], log: string) => {
        const isHead = log.startsWith(path);

        // Strip unique abs paths
        let msg = log.replaceAll(`${path}${sep}`, "");

        // msg for ambiguous flips between the related methods
        // need to cut the end off
        if (!isHead && msg.includes("Ambiguous method call")) {
            msg = msg.split(",", 2)[0];
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
            ...msgs.sort()
        ];
    })
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

    test.each(getSamples())("Sample: %s", async (name, path) => {

        const jvmCheck = spawnSync(
            "java",
            [
                "-cp",
                "jvm/target/scala-2.13/*:jvm/target/scala-2.13/apex-ls_2.13-*.jar",
                "io.github.apexdevtools.apexls.Main",
                "-verbose",
                "-nocache",
                "-outlinemulti",
                path
            ],
            {
                // can only be run from npm dir
                cwd: resolve(process.cwd(), "../.."),
                timeout: 30000
            }
        );

        // Check it was not cancelled due to timeout
        expect(jvmCheck.signal).toBeNull();

        const status = jvmCheck.status;
        const logs: string[] = jvmCheck.stdout
            .toString("utf8")
            .split("\n")
            .filter(l => l);

        printLogs(logs, name, status);

        const result = {
            status,
            logs: prepareSnapshotLogs(logs, path)
        };

        expect(result).toMatchSnapshot();

    }, 40000);

});
