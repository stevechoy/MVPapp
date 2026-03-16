# Clean and Escape LLM Markdown Responses

This function strips markdown code fences (e.g., `cpp, `mrgsolve) and
any text appearing before or after the code block. It also escapes
backslashes and quotes to ensure the string can be safely wrapped in
double quotes for downstream processing.

## Usage

``` r
clean_llm_response(final_answer)
```

## Arguments

- final_answer:

  A character string containing the raw LLM response.

## Value

A character string stripped of markdown wrappers, with internal
quotes/backslashes escaped, and padded with a single newline at the
start and end.
