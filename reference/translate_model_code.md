# Upload, and translate a PDF file to model code via external API

Sends a file to a Dify Workflow or LLM API for translation into model
code. The file should be pre-validated and optionally pre-combined using
`combine_uploaded_files` before being passed to this function.

## Usage

``` r
translate_model_code(
  ready_path,
  file_name = NULL,
  service,
  api_key,
  api_upload = NULL,
  api_chat = NULL,
  user_id = "mrgsolve_translator",
  model_gemini = "gemini-3-flash-preview",
  model_openai = "gpt-5-mini",
  model_anthropic = "claude-haiku-4-5-20251001",
  model_openrouter = "arcee-ai/trinity-large-preview:free",
  model_openai_compatible = "gpt-5-mini",
  model_deepseek = "deepseek-reasoner",
  model_apollo = "gpt-5.2",
  model_azure = "gpt-5.2",
  model_aws = "anthropic.claude-sonnet-4-6",
  display_info = TRUE,
  temperature = 0.1,
  seed = 42,
  locally_parse_file = FALSE,
  model_lang = "mrgsolve",
  deep_pdfscan = FALSE,
  force_parse = FALSE,
  mrgsolve_system_prompt,
  mrgsolve_long_user_prompt,
  mrgsolve_short_user_prompt,
  nonmem_system_prompt,
  nonmem_long_user_prompt,
  nonmem_short_user_prompt,
  rxode2_system_prompt,
  rxode2_long_user_prompt,
  rxode2_short_user_prompt,
  internal_version = TRUE,
  debug = TRUE
)
```

## Arguments

- ready_path:

  Path to the file to be processed. For multi-file uploads, this should
  be the combined output of `combine_uploaded_files`.

- file_name:

  Original filename (used for display in notifications and for MIME type
  detection). If `NULL`, derived from `ready_path`.

- service:

  Choice of "PROD" (BI-only), "EXP" (BI-only), "Gemini", "OpenAI",
  "Claude", "OpenRouter", "OpenAI-Compatible", "DeepSeek", "Apollo"
  (BI-only), "Azure OpenAI"

- api_key:

  API Key, recommended to store it as env var called "ANTHROPIC_API_KEY"
  etc

- api_upload:

  API URL for uploading of files (Dify requires a 2-step process)

- api_chat:

  API URL for chat messages, required when using OpenAI-compatible API

- user_id:

  user id for the request (BI-only)

- model_gemini:

  Model to be used when calling Gemini API

- model_openai:

  Model to be used when calling OpenAI API

- model_anthropic:

  Model to be used when calling Anthropic API

- model_openrouter:

  Model to be used when calling OpenRouter

- model_openai_compatible:

  Model to be used when calling OpenAI-compatible API

- model_deepseek:

  Model to be used when calling DeepSeek

- model_apollo:

  Model to be used when calling Apollo (BI only)

- model_azure:

  Model to be used when calling Azure OpenAI

- model_aws:

  Model to be used when calling AWS Bedrock

- display_info:

  Set to TRUE to show how much time/tokens in Shiny UI when job is
  finished

- temperature:

  Goes from 0 to 1, where 0 is deterministic

- seed:

  seed number for LLMs

- locally_parse_file:

  if TRUE, extracts text locally and includes in prompt instead of
  uploading file

- model_lang:

  Either "mrgsolve" or "nonmem" (changes the prompt)

- deep_pdfscan:

  Use Vision to extract image data (BI only)

- force_parse:

  Force parsing (BI only)

- mrgsolve_system_prompt:

  String for mrgsolve system prompt

- mrgsolve_long_user_prompt:

  String for mrgsolve long user prompt

- mrgsolve_short_user_prompt:

  String for mrgsolve short user prompt

- nonmem_system_prompt:

  String for nonmem system prompt

- nonmem_long_user_prompt:

  String for nonmem long user prompt

- nonmem_short_user_prompt:

  String for nonmem short user prompt

- rxode2_system_prompt:

  String for rxode2 system prompt

- rxode2_long_user_prompt:

  String for rxode2 long user prompt

- rxode2_short_user_prompt:

  String for rxode2 short user prompt

- internal_version:

  Logical. Only relevant for BI

- debug:

  Displays debug messages

## Value

a named list with `answer`, `conversation_id`, and `chat_obj`, or `NULL`
on failure.
