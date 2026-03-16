# Send a chat request to a Dify Workflow via httr2 (BI-only)

Handles the full Dify API interaction: optionally uploads a file first,
then sends the instruction prompt as a blocking workflow request and
returns the answer along with token usage metadata.

## Usage

``` r
run_dify_chat(
  instruction_prompt,
  api_key,
  api_upload = NULL,
  api_chat,
  user_id,
  file_path = NULL,
  detected_type = NULL,
  locally_parse_file = TRUE,
  conversation_id = NULL,
  deep_pdfscan = FALSE,
  force_parse = FALSE,
  model_name,
  debug = TRUE
)
```

## Arguments

- instruction_prompt:

  Full prompt string to send as the query, including any extracted file
  content if parsing locally

- api_key:

  Dify API key

- api_upload:

  API URL for the Dify file upload endpoint. Only used when
  `locally_parse_file = FALSE`

- api_chat:

  API URL for the Dify chat/workflow endpoint

- user_id:

  User identifier string passed to the Dify API

- file_path:

  Path to the file on disk. Only used when `locally_parse_file = FALSE`

- detected_type:

  MIME type of the file (e.g. `"application/pdf"`). Only used when
  `locally_parse_file = FALSE`

- locally_parse_file:

  If `TRUE`, skips file upload and sends text content directly in the
  prompt

- conversation_id:

  Default NULL, required for context reuse

- deep_pdfscan:

  Default FALSE, uses Vision to extract images

- force_parse:

  Default FALSE, forces reparsing

- model_name:

  Model name string used as a fallback label in usage_info if the API
  does not return a model identifier

- debug:

  If `TRUE`, prints progress messages to the console

## Value

A named list with the following elements:

- answer:

  Character string containing the raw model response

- conversation_id:

  Dify conversation ID string for the request

- usage_info:

  Named list with `input`, `output`, `total` token counts and `model`
  label
