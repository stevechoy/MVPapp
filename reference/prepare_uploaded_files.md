# Validate and Prepare Uploaded Files for LLM Processing

Validates one or more files from a Shiny
[`fileInput`](https://rdrr.io/pkg/shiny/man/fileInput.html) widget
against allowed single- and multi-file extension lists, then either
returns the single file path directly or combines multiple files into
one via
[`combine_uploaded_files`](https://stevechoy.github.io/MVPapp/reference/combine_uploaded_files.md).
On validation failure, a Shiny error notification is shown and `NULL` is
returned.

## Usage

``` r
prepare_uploaded_files(
  files,
  llm_service,
  model_lang,
  use_nonmem2mrgsolve,
  use_nonmem2rx,
  single_file_types,
  multi_file_types
)
```

## Arguments

- files:

  A data frame produced by a Shiny `fileInput` widget, containing at
  minimum the columns `name` (original filename) and `datapath`
  (server-side temporary path).

- llm_service:

  LLM provider name. More extensions are accepted by Dify-workflows.

- model_lang:

  Character. "mrgsolve", "nonmem", "rxode2"

- use_nonmem2mrgsolve:

  Logical. Whether to use nonmem2mrgsolve when translating to mrgsolve,
  if package is present

- use_nonmem2rx:

  Logical. Whether to use nonmem2rx when translating to rxode2, if
  package is present

- single_file_types:

  Character vector of permitted file extensions when exactly one file is
  uploaded, e.g. `c(".pdf", ".docx", ".csv")`. Extensions should include
  the leading dot.

- multi_file_types:

  Character vector of permitted file extensions when more than one file
  is uploaded, e.g. `c(".pdf", ".txt", ".mod", ".ctl")`. Must be a
  subset of `single_file_types`. Extensions should include the leading
  dot.

## Value

A length-one character string giving the path to a ready-to-use file, or
`NULL` if validation failed (in which case a Shiny error notification
has already been shown to the user). For single-file uploads this is
`files$datapath[1]`; for multi-file uploads this is the temporary
combined file produced by
[`combine_uploaded_files`](https://stevechoy.github.io/MVPapp/reference/combine_uploaded_files.md).
Returns a list if either criteria for nonmem2mrgsolve or nonmem2rx has
been met.

## Details

Extensions `.ctl` and `.mod` are treated as equivalent to `.txt` when
checking for mixed-type uploads, since all three are plain-text formats
commonly used in pharmacometric workflows. This normalisation only
affects the mixed-type guard; the original extensions are preserved in
any user-facing notification messages.

Validation is performed in this order:

1.  For a single file: check that its extension is in
    `single_file_types`.

2.  For multiple files: check that every extension is in
    `multi_file_types`.

3.  For multiple files: check that all files share the same normalised
    extension (i.e. no mixing of e.g. `.pdf` and `.txt`).

4.  For multiple files: delegate to
    [`combine_uploaded_files`](https://stevechoy.github.io/MVPapp/reference/combine_uploaded_files.md).

## See also

[`combine_uploaded_files`](https://stevechoy.github.io/MVPapp/reference/combine_uploaded_files.md)
for the multi-file combining logic,
[`fileInput`](https://rdrr.io/pkg/shiny/man/fileInput.html) for the UI
element that produces `files`,
[`showNotification`](https://rdrr.io/pkg/shiny/man/showNotification.html)
for the notification system.

## Examples

``` r
if (FALSE) { # \dontrun{
# Inside a Shiny server function:
observe({
  req(input$pdffile_model_1)

  ready_path <- prepare_uploaded_files(
    files             = input$pdffile_model_1,
    single_file_types = llm_accept_single_types,
    multi_file_types  = llm_accept_multi_types
  )

  req(!is.null(ready_path))
  # pass ready_path to translate_model_code() or send_to_llm()
})
} # }
```
