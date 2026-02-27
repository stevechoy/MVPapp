# Combine Multiple Uploaded Files into a Single File

Merges a list of locally stored files (from a Shiny `fileInput` with
`multiple = TRUE`) into a single temporary file, ready for downstream
processing such as sending to an LLM. Supports PDF (merged page-by-page)
and plain text types including `.txt`, `.mod`, and `.ctl`
(concatenated). On error, a Shiny notification is shown to the user and
`NULL` is returned rather than stopping the session.

## Usage

``` r
combine_uploaded_files(file_paths, file_names)
```

## Arguments

- file_paths:

  Character vector of temporary file paths, drawn from
  `input$<inputId>$datapath` after a Shiny `fileInput` upload.

- file_names:

  Character vector of original file names, drawn from
  `input$<inputId>$name`. Used to determine file type via extension,
  since browser-reported MIME types are unreliable for types such as
  `.mod` and `.ctl`.

## Value

A length-one character string: the path to a temporary file containing
the combined content, or `NULL` if an error occurred (in which case a
Shiny notification is shown). The file lives in
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) and will be cleaned
up at the end of the R session.

## Details

File type is determined from the extension of `file_names` rather than
the browser-reported MIME type, as browsers often report generic
`"application/octet-stream"` for domain-specific extensions such as
`.ctl` and `.mod`.

This function assumes all files share the same extension. If they do
not, a Shiny error notification is shown and `NULL` is returned. It is
expected that this validation is performed by the caller (see the Shiny
server example) before calling this function.

Combination strategy by extension:

- `.pdf`:

  Pages are merged in order using
  [`pdf_combine`](https://docs.ropensci.org/qpdf/reference/qpdf.html).
  Requires the pdftools package.

- `.txt`, `.mod`, `.ctl`:

  Files are read with
  [`readLines`](https://rdrr.io/r/base/readLines.html) and concatenated
  with a newline separator. Output is written as `.txt`.

- All other extensions:

  A Shiny error notification is shown and `NULL` is returned.

## See also

[`fileInput`](https://rdrr.io/pkg/shiny/man/fileInput.html) for the UI
element that produces the input,
[`showNotification`](https://rdrr.io/pkg/shiny/man/showNotification.html)
for the notification system,
[`pdf_combine`](https://docs.ropensci.org/qpdf/reference/qpdf.html) for
the underlying PDF merge.

## Examples

``` r
if (FALSE) { # \dontrun{
# Inside a Shiny server function:
observe({
  req(input$pdffile_model_1)
  files <- input$pdffile_model_1

  combined_path <- if (nrow(files) > 1) {
    combine_uploaded_files(files$datapath, files$name)
  } else {
    files$datapath[1]
  }

  req(!is.null(combined_path))
  send_to_llm(combined_path)
})
} # }
```
