# Security Policy

## Reporting a vulnerability

Please do not report security issues through public GitHub issues, discussions or pull requests.

Report them privately through GitHub's [private vulnerability reporting](https://github.com/apex-dev-tools/apex-ls/security/advisories/new) instead, which you can also reach from the Security tab of this repository. The report stays visible only to you and the maintainers until a fix is ready and an advisory is published.

It helps to include the affected package (`io.github.apex-dev-tools:apex-ls_2.13` on Maven Central or `@apexdevtools/apex-ls` on npm) and version, a description of the impact, and a minimal project or steps that reproduce the problem.

This project is maintained by a small team, so we can't promise fixed response times, but we aim to acknowledge reports within a week and will keep you updated as we investigate. We are happy to credit reporters in the advisory if you would like that.

## Supported versions

Fixes are made against the latest release and shipped as a new version on Maven Central. The npm package is published less often and is updated when a fix affects it. Older major versions do not normally receive security fixes.

| Version | Supported |
| ------- | --------- |
| 6.x     | Yes       |
| < 6.0   | No        |

## Scope

apex-ls is often run over Salesforce projects the caller does not control, for example in CI or through tools such as PMD that embed it. Issues where the contents of a project being analysed can cause apex-ls to read files its configuration does not ask it to load, make network requests, execute code, or use CPU or memory far out of proportion to the size of the project are in scope. So are problems with the published Maven and npm artifacts themselves.

Diagnostics that differ from what the Salesforce platform reports are bugs rather than vulnerabilities and can be raised as normal issues.
