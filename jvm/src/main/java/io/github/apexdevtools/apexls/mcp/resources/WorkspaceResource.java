/*
 Copyright (c) 2025 Kevin Jones, All rights reserved.
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

package io.github.apexdevtools.apexls.mcp.resources;

import io.github.apexdevtools.apexls.mcp.ScalaBridge;

/**
 * MCP resource for accessing workspace information and metadata.
 */
public class WorkspaceResource {
    
    private final ScalaBridge scalaBridge;
    
    public WorkspaceResource(ScalaBridge scalaBridge) {
        this.scalaBridge = scalaBridge;
    }
    
    // TODO: Implement MCP resource specification
    // This is a placeholder for the actual MCP server integration
}