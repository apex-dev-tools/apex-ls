/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
declare module "@apexdevtools/apex-ls" {
  export class WorkspaceException {
    message: string;
  }

  export class Workspace {
    findType(name: string): string[];
  }

  export class Workspaces {
    static get(path: string): Workspace;
  }
}
