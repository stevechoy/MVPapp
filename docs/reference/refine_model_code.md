# Provide a model code string and its compile error message to be refined

Sends a string containing model code and error message to a Dify
Workflow or LLM API for correction.

## Usage

``` r
refine_model_code(
  model_code,
  error_message,
  context = NULL,
  reuse_context = FALSE,
  service,
  api_key,
  api_upload = NULL,
  api_chat = NULL,
  user_id = "mrgsolve_translator",
  model_gemini = "gemini-3.6-flash",
  model_openai = "gpt-5-mini",
  model_anthropic = "claude-haiku-4-5-20251001",
  model_openrouter = "arcee-ai/trinity-large-preview:free",
  model_openai_compatible = "gpt-5-mini",
  model_deepseek = "deepseek-reasoner",
  model_apollo = "gpt-5.6-terra",
  model_azure = "gpt-5.6-terra",
  model_aws = "anthropic.claude-sonnet-5",
  progress_bar = 0.4,
  max_retries = 2,
  display_info = TRUE,
  temperature = 0,
  seed = 42,
  attempt = 1,
  deep_pdfscan = FALSE,
  force_parse = FALSE,
  system_prompt,
  long_user_prompt,
  short_user_prompt,
  internal_version,
  feedback_success = FALSE,
  debug = TRUE
)
```

## Arguments

- model_code:

  Input model code

- error_message:

  Input compilation error message

- context:

  Argument to hold chat_obj or conversation_id to keep same chat session

- reuse_context:

  Set to TRUE to re-use same conversation_id or chat_obj, more costly in
  terms of time and tokens

- service:

  Choice of "PROD", "EXP", "Gemini", "OpenAI", "Claude", "OpenRouter",
  "OpenAI-Compatible", "DeepSeek", "Apollo", "Azure OpenAI", "AWS
  Bedrock"

- api_key:

  API Key, recommended to store it as env var called "ANTHROPIC_API_KEY"
  etc (Dify only)

- api_upload:

  API URL for uploading of files (Dify requires a 2-step process)

- api_chat:

  API URL for chat messages, required for OpenAI-compatible

- user_id:

  user id for the request

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

  Model to be used when calling Apollo LLM

- model_azure:

  Model to be used when calling Azure OpenAI

- model_aws:

  Model to be used when calling AWS Bedrock

- progress_bar:

  Starting point of Shiny progress bar, relevant for 2nd try

- max_retries:

  Required to calculate a more accurate progress bar

- display_info:

  Set to TRUE to show how much time/tokens in Shiny UI when job is
  finished

- temperature:

  Goes from 0 to 1, where 0 is deterministic, if not reusing context

- seed:

  seed number for LLMs, if not reusing context

- attempt:

  current attempt number

- deep_pdfscan:

  Uses Vision to extract image data (BI only)

- force_parse:

  Force reparsing (BI only)

- system_prompt:

  Character. System prompt

- long_user_prompt:

  Character. Long user prompt

- short_user_prompt:

  Character. Short user prompt

- internal_version:

  Logical. Only relevant for BI

- feedback_success:

  Logical. Only relevant for BI, and if reuse_context = TRUE

- debug:

  Displays debug messages

## Value

a named list of "answer", "conversation_id", "chat_obj"
