# Send a chat request via an ellmer provider

Initialises the appropriate ellmer chat object for the given service and
dispatches the prompt, either as text-only or alongside an uploaded
file. Supports Gemini, OpenAI, Claude, OpenRouter, and OpenAI-compatible
endpoints.

## Usage

``` r
run_ellmer_chat(
  service,
  model_name,
  system_prompt,
  optimal_params,
  api_chat = NULL,
  instruction_prompt,
  file_path = NULL,
  detected_type = NULL,
  locally_parse_file = TRUE,
  chat_obj = NULL,
  internal_version = TRUE,
  debug = TRUE
)
```

## Arguments

- service:

  Provider to use. One of `"Gemini"`, `"OpenAI"`, `"Claude"`,
  `"OpenRouter"`, `"OpenAI-Compatible"`, `"DeepSeek"`, `"Apollo"`,
  `"Azure OpenAI"`, `"AWS Bedrock"`

- model_name:

  Model identifier string passed to the ellmer chat constructor

- system_prompt:

  System prompt string defining model behaviour and output format
  constraints

- optimal_params:

  An `ellmer::params()` object controlling temperature, seed, and other
  supported sampling parameters

- api_chat:

  Base URL for the chat endpoint. Only required when
  `service = "openai_compatible"`

- instruction_prompt:

  Full user prompt string, including any extracted file content if
  parsing locally

- file_path:

  Path to the file on disk. Only used when `locally_parse_file = FALSE`

- detected_type:

  MIME type of the file (e.g. `"application/pdf"`). Only used when
  `locally_parse_file = FALSE`

- locally_parse_file:

  If `TRUE`, sends text content in the prompt directly rather than
  uploading the file to the provider

- chat_obj:

  the ellmer chat object for context re-use, default NULL

- internal_version:

  Logical. Changes base_url path, Only relevant for BI

- debug:

  If `TRUE`, prints progress messages and the chat object summary to the
  console

## Value

A named list with the following elements:

- answer:

  Character string containing the raw model response

- chat_obj:

  The ellmer chat object, which can be used for token inspection or
  follow-up turns

- usage_info:

  Named list with `input`, `output`, `total` token counts and `model`
  label
