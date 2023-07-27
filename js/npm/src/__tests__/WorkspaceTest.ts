/*
 Copyright (c) 2019 Kevin Jones, All rights reserved.
 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.
 */
import { Workspaces } from "@apexdevtools/apex-ls";
import { vol } from "memfs";
import { patchFs } from "fs-monkey";

test("Bad directory", () => {
  try {
    Workspaces.get("foo");
    expect(true).toBe(false);
  } catch (err) {
    expect(err.message).toMatch(/.*No directory at .*/);
  }
});

test("Empty directory", () => {
  vol.fromJSON({ "/pkg1/README.md": "" });
  const unpatch = patchFs(vol);
  try {
    const workspace = Workspaces.get("/pkg1");
    expect(workspace).toBeDefined();
    expect(workspace.findType("Foo")).toEqual([]);
  } finally {
    unpatch();
  }
});

test("MDAPI Class", () => {
  vol.fromJSON({ "/pkg2/classes/Foo.cls": "" });
  const unpatch = patchFs(vol);
  try {
    const workspace = Workspaces.get("/pkg2");
    expect(workspace).toBeDefined();
    expect(workspace.findType("Foo")).toEqual(["/pkg2/classes/Foo.cls"]);
  } finally {
    unpatch();
  }
});

test("SFDX Bad Project file", () => {
  vol.fromJSON({ "/pkg3/sfdx-project.json": "" });
  const unpatch = patchFs(vol);
  try {
    const workspace = Workspaces.get("/pkg3");
    expect(true).toBe(false);
  } catch (err) {
    expect(err.message).toMatch(
      "Error: line 1: Failed to parse - ujson.IncompleteParseException: exhausted input"
    );
  } finally {
    unpatch();
  }
});

test("SFDX without namespace", () => {
  vol.fromJSON({
    "/pkg4/sfdx-project.json": '{ "packageDirectories": [{"path": "classes"}]}',
    "/pkg4/classes/Foo.cls": "",
  });
  const unpatch = patchFs(vol);
  try {
    const workspace = Workspaces.get("/pkg4");
    expect(workspace).toBeDefined();
    expect(workspace.findType("Foo")).toEqual(["/pkg4/classes/Foo.cls"]);
  } finally {
    unpatch();
  }
});

test("SFDX with namespace", () => {
  vol.fromJSON({
    "/pkg5/sfdx-project.json":
      '{ "packageDirectories": [{"path": "classes"}], "namespace": "ns001"}',
    "/pkg5/classes/Foo.cls": "",
  });
  const unpatch = patchFs(vol);
  try {
    const workspace = Workspaces.get("/pkg5");
    expect(workspace).toBeDefined();
    expect(workspace.findType("ns001.Foo")).toEqual(["/pkg5/classes/Foo.cls"]);
  } finally {
    unpatch();
  }
});
