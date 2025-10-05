# Contract — Hover response with ApexDoc

## Request
```json
{
  "method": "apex/getHover",
  "params": {
    "path": "/workspace/classes/InvoiceProcessor.cls",
    "line": 12,
    "offset": 6,
    "content": null
  }
}
```

## Expected Response
```json
{
  "result": {
    "content": "global class InvoiceProcessor {\n\n**Summary**\nProcesses invoice batches and routes failures for manual review.\n\n**Parameters**\n- `batchSize` — Number of records processed per execution.\n\n**Returns**\nReturns `true` when the batch completes without errors.\n\n**See Also**\n- [Billing Guide](https://example.com/billing)",
    "location": {
      "path": "/workspace/classes/InvoiceProcessor.cls",
      "startLine": 1,
      "startPosition": 7,
      "endLine": 40,
      "endPosition": 1
    }
  },
  "jsonrpc": "2.0",
  "id": 7
}
```

## Notes
- `content` combines the existing signature (`global class InvoiceProcessor`) with sanitized ApexDoc sections.
- External link text is preserved and rendered via `[label](url)` Markdown.
- If the ApexDoc block were missing or malformed, `content` would fall back to just the signature.
