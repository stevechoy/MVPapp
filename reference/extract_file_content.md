# Extract text content from a local file (pdf or text) and append to a prompt

Reads a local PDF or plain text file and appends the extracted content
to the provided instruction prompt, wrapped in delimiters. For PDFs,
text is extracted using
[`pdftools::pdf_text()`](https://docs.ropensci.org/pdftools//reference/pdftools.html).
For plain text files (including `.txt`, `.mod`, and `.ctl`), lines are
read with [`readLines()`](https://rdrr.io/r/base/readLines.html). The
resulting prompt is returned and intended to be passed directly as the
user message to the LLM.

## Usage

``` r
extract_file_content(
  file_path,
  file_name,
  detected_type,
  instruction_prompt,
  debug = TRUE
)
```

## Arguments

- file_path:

  Path to the local file to extract text from

- file_name:

  Original filename including extension, used for debug messaging only

- detected_type:

  MIME type of the file. Use `"application/pdf"` for PDF files and
  `"text/plain"` for all plain text formats

- instruction_prompt:

  Base instruction prompt string to which the extracted file content
  will be appended

- debug:

  If `TRUE`, prints the extracted character count to the console

## Value

A single character string containing `instruction_prompt` followed by
the extracted file content wrapped in `--- FILE CONTENT START ---` and
`--- FILE CONTENT END ---` delimiters
