# Launch Model Visualization Platform App

This function will launch MVP app and pass along arbitrary parameters
through the `...` parameter to the application. This is done by
modifying the global environment. This function will attempt to clean up
any objects placed into the global environment on exit. If objects exist
prior to calling this function (i.e. `exists(OBJECT)` returns TRUE) then
the value will be reset to it's state prior to calling `run_mvp`.

## Usage

``` r
run_mvp(
  appDir = system.file("shiny", package = "MVPapp"),
  insert_watermark = TRUE,
  authentication_code = NA_character_,
  internal_version = TRUE,
  use_bi_styling = FALSE,
  pw_models_path = NA_character_,
  llm_choices = c("Claude", "Gemini", "OpenAI", "OpenRouter", "OpenAI-Compatible",
    "DeepSeek", "Azure OpenAI", "AWS Bedrock"),
  api_upload = NA_character_,
  api_chat = NA_character_,
  user_id = "mrgsolve_translator",
  user_id_retry = "mrgsolve_translator",
  reuse_context = FALSE,
  model_gemini = "gemini-3-flash-preview",
  model_openai = "gpt-5.4",
  model_anthropic = "claude-sonnet-4-6",
  model_openrouter = "arcee-ai/trinity-large-preview:free",
  model_openai_compatible = "gpt-5.2",
  model_deepseek = "deepseek-reasoner",
  model_apollo = "claude_4_6_sonnet",
  model_azure = "gpt-5.2",
  model_aws = "anthropic.claude-sonnet-4-6",
  temperature = 0,
  llm_seed = 42,
  model_lang = "mrgsolve",
  prompts_path = system.file("shiny/prompts.R", package = "MVPapp"),
  show_debugging_msg = FALSE,
  ...
)
```

## Arguments

- appDir:

  the directory of the application to run.

- insert_watermark:

  Logical. Default TRUE. Set to FALSE to remove "For Internal Use Only"
  text in simulated plots.

- authentication_code:

  Character. Default NA_character\_. Provide a string (e.g., password)
  to password-lock the entire app.

- internal_version:

  Logical. Default TRUE. Setting to FALSE may allow generation of NCA
  reports when hosted on AWS with different access rights.

- use_bi_styling:

  Logical. Default FALSE. Set to TRUE to insert BI logo (deprecated -
  currently inactive).

- pw_models_path:

  Character. Default NA_character\_. Provide a path to source
  password-gated models.

- llm_choices:

  Character. Supported providers are "Claude", "Gemini", "OpenAI",
  "OpenRouter", "OpenAI-Compatible", "Deepseek", "Azure OpenAI", "AWS
  Bedrock"

- api_upload:

  Character. Default NA_character\_. API upload URL when using
  Dify-based workflows

- api_chat:

  Character. Default NA_character\_. API chat URL required when using
  OpenAI-compatible providers

- user_id:

  Character. Default mrgsolve_translator. User ID for LLMs (BI-only).

- user_id_retry:

  Character. Default mrgsolve_translator. User ID for submitting retries
  to LLMs (BI-only).

- reuse_context:

  Logical. Default FALSE. Set to TRUE to re-use same conversation during
  retries.

- model_gemini:

  Character. Default gemini-3-flash-preview. Model for Gemini.

- model_openai:

  Character. Default gpt-5.4. Model for OpenAI / ChatGPT.

- model_anthropic:

  Character. Default claude-sonnet-4-6. Model for Anthropic / Claude.

- model_openrouter:

  Character. Default arcee-ai/trinity-large-preview:free. Model for
  OpenRouter.

- model_openai_compatible:

  Character. Default gpt-5.2. Model for OpenAI-compatible providers.

- model_deepseek:

  Character. Default deepseek-reasoner. Model for DeepSeek.

- model_apollo:

  Character. Default gpt-5.2 (BI-only)

- model_azure:

  Character. Default gpt-5.2. Model for Azure OpenAI.

- model_aws:

  Character. Default anthropic.claude-sonnet-4-6. Model for AWS Bedrock.

- temperature:

  Numeric. Default 0, between 0 (more deterministic) to 1 (more
  creative) (not all models support this).

- llm_seed:

  Numeric. Default 42. Seed number for LLMs (not all models support
  this).

- model_lang:

  Character. Default mrgsolve. Changes model translation language to
  "mrgsolve", "nonmem" (for testing only), "rxode2" (for testing only)

- prompts_path:

  Prompts file. See vignette ("automatic-translation") for more
  information.

- show_debugging_msg:

  Logical. Default FALSE. Set to TRUE to output verbose working messages
  in the console, useful for debugging.

- ...:

  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html)
  parameters,
  [`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html)
  parameters, or parameters to pass to the Shiny app.

## Details

If the user wishes to run the App outside of the function (e.g.
preparing for deployment on Posit Connect), this can be done by
accessing shiny/app.R, which is located inside the folder of where the
package was installed, and modify standalone_mode = TRUE (and setting
these options there as required).

## Note

Adapted from
https://github.com/jbryer/ShinyDemo/blob/master/R/run_shiny_app.R

## See also

<https://stevechoy.github.io/MVPapp/articles/supply-passwords.html>
<https://stevechoy.github.io/MVPapp/articles/automatic-translation.html>
[`chat`](https://ellmer.tidyverse.org/reference/chat-any.html) for LLM
chat interface.

## Examples

``` r
if (FALSE) { # \dontrun{
run_mvp(insert_watermark = FALSE) # remove watermarks
run_mvp(launch.browser = TRUE) # launch app in browser (argument passed to shinyApp)
run_mvp(authentication_code = "some_password") # Password-lock the site,
# could be useful in deployment
run_mvp(pw_models_path = "path/to/your/private/models.R") # see
# "shiny/passworded_models_example.R" on how to set one up
run_mvp(llm_service = "Claude", model_anthropic = "claude-sonnet-4-6", temperature = 0) # Uses Claude only with reproducible results
} # }
```
