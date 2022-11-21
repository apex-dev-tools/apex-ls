import chalk from "chalk";
import { spawnSync } from "child_process";
import { readdirSync, lstatSync } from "fs";
import { basename, resolve, sep } from "path"

const errorCats = ["Syntax", "Error", "Missing"];
const warnCats = ["Warning", "Unused"];

describe("Check samples", () => {

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

        // Strip unique abs paths and empty lines for snapshotting
        const logs: string[] = jvmCheck.stdout
            .toString("utf8")
            .split("\n")
            .reduce((a, c) => {
                c && a.push(c.replaceAll(`${path}${sep}`, ""));
                return a;
            }, []);

        if (logs.length) {
            console.log(`[${name}](status: ${status}) check output:`);
            console.log(
                logs.map(l => {
                    if (errorCats.some(cat => l.startsWith(cat))) {
                        return chalk.red(l);
                    } else if (warnCats.some(cat => l.startsWith(cat))) {
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

        const result = {
            status,
            logs
        };

        expect(result).toMatchSnapshot();

    }, 40000);

});
