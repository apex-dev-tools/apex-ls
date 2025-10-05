# Contract — Hover response with partially valid ApexDoc

## Request
```json
{
  "method": "apex/getHover",
  "params": {
    "path": "/workspace/classes/InvoiceProcessor.cls",
    "line": 28,
    "offset": 15,
    "content": null
  }
}
```

## Expected Response
```json
{
  "result": {
    "content": "public Boolean processInvoices(List<Id> invoiceIds) {\n\n**Summary**\nExecutes validation for the supplied invoice Ids.\n\n**Parameters**\n- `invoiceIds` — List of invoices targeted for validation.",
    "location": {
      "path": "/workspace/classes/InvoiceProcessor.cls",
      "startLine": 25,
      "startPosition": 3,
      "endLine": 75,
      "endPosition": 1
    }
  },
  "jsonrpc": "2.0",
  "id": 11
}
```

## Notes
- The original ApexDoc contained an invalid `@return` tag (missing description). The formatter drops that tag while preserving the valid summary and parameter sections.
- No trailing blank sections appear when a block collapses to summary + parameters.
