# Launch Model Visualization Platform App

`run_mvp()` launches the MVP app. `mvp_app()` returns the app as a Shiny
app object without running it, e.g. for deployment on Posit Connect or
shinyapps.io (an `app.R` containing `MVPapp::mvp_app()` is sufficient).

The app is evaluated in its own environment (whose parent gives access
to MVPapp and its key dependencies), so neither the global environment
nor the search path is modified. Global options changed by the app are
set when it starts and restored when it stops.

## Usage

``` r
mvp_app(
  appDir = system.file("shiny", package = "MVPapp"),
  insert_watermark = TRUE,
  max_dataset_size = 200,
  authentication_code = NA_character_,
  internal_version = TRUE,
  pw_models_path = NA_character_,
  llm_choices = c("Claude", "Gemini", "OpenAI", "OpenRouter", "OpenAI-Compatible",
    "DeepSeek", "Azure OpenAI", "AWS Bedrock"),
  api_upload = NA_character_,
  api_chat = NA_character_,
  user_id = "MVP_user",
  reuse_context = FALSE,
  model_gemini = "gemini-3.8-flash",
  model_openai = "gpt-6.1-sol",
  model_anthropic = "claude-sonnet-5-5",
  model_openrouter = "arcee-ai/trinity-large-preview:free",
  model_openai_compatible = "gpt-5.2",
  model_deepseek = "deepseek-flash",
  model_apollo = "claude_4_6_sonnet",
  model_azure = "gpt-5.2",
  model_aws = "anthropic.claude-sonnet-5",
  temperature = 1,
  llm_seed = 42,
  model_lang = c("mrgsolve", "nonmem", "rxode2"),
  prompts_path = system.file("shiny/prompts.R", package = "MVPapp"),
  show_debugging_msg = FALSE,
  brand_color = NULL,
  brand_text_color = NULL,
  brand_logo = NULL,
  brand_favicon = NULL,
  page_title = NULL,
  ...
)

run_mvp(
  appDir = system.file("shiny", package = "MVPapp"),
  insert_watermark = TRUE,
  max_dataset_size = 200,
  authentication_code = NA_character_,
  internal_version = TRUE,
  pw_models_path = NA_character_,
  llm_choices = c("Claude", "Gemini", "OpenAI", "OpenRouter", "OpenAI-Compatible",
    "DeepSeek", "Azure OpenAI", "AWS Bedrock"),
  api_upload = NA_character_,
  api_chat = NA_character_,
  user_id = "MVP_user",
  reuse_context = FALSE,
  model_gemini = "gemini-3.8-flash",
  model_openai = "gpt-6.1-sol",
  model_anthropic = "claude-sonnet-5-5",
  model_openrouter = "arcee-ai/trinity-large-preview:free",
  model_openai_compatible = "gpt-5.2",
  model_deepseek = "deepseek-flash",
  model_apollo = "claude_4_6_sonnet",
  model_azure = "gpt-5.2",
  model_aws = "anthropic.claude-sonnet-5",
  temperature = 1,
  llm_seed = 42,
  model_lang = c("mrgsolve", "nonmem", "rxode2"),
  prompts_path = system.file("shiny/prompts.R", package = "MVPapp"),
  show_debugging_msg = FALSE,
  brand_color = NULL,
  brand_text_color = NULL,
  brand_logo = NULL,
  brand_favicon = NULL,
  page_title = NULL,
  ...
)
```

## Arguments

- appDir:

  Character. Directory containing the app's `app.R`. Defaults to the
  copy installed with MVPapp and rarely needs changing.

- insert_watermark:

  Logical. Default TRUE. Set to FALSE to remove the "For Internal Use
  Only" watermark from simulation plots and NCA reports.

- max_dataset_size:

  Numeric. Default 200. Maximum upload dataset size in MB.

- authentication_code:

  Character. Default NA_character\_. Provide a string (e.g., password)
  to password-lock the entire app.

- internal_version:

  Logical. Default TRUE. Set to FALSE when hosting the app on a server
  (e.g. AWS) where the working directory is not writable: NCA reports
  are then written to the session's temporary directory, and the help
  text for locked models points to GitHub instead. For internal
  deployments, it also selects the external Apollo endpoint.

- pw_models_path:

  Character. Default NA_character\_. Path to an R file defining
  password-gated models (see
  `system.file("shiny/passworded_models_example.R", package = "MVPapp")`
  and
  [`vignette("supply-passwords")`](https://stevechoy.github.io/MVPapp/articles/supply-passwords.md)).

- llm_choices:

  Character vector of LLM providers offered in the app; the first one is
  selected by default. Defaults to all supported providers: "Claude",
  "Gemini", "OpenAI", "OpenRouter", "OpenAI-Compatible", "DeepSeek",
  "Azure OpenAI" and "AWS Bedrock". Internal-deployment providers (e.g.
  "PMx Co-Modeler", "Apollo") are also accepted.

- api_upload:

  Character. Default NA_character\_. API upload URL when using
  Dify-based workflows (internal deployment only)

- api_chat:

  Character. Default NA_character\_. Base URL required when using
  OpenAI-Compatible providers, e.g. "https://api.moonshot.ai/v1"
  ("/chat/completions" is appended automatically)

- user_id:

  Character. Default MVP_user. User ID for LLMs

- reuse_context:

  Logical. Default FALSE. Set to TRUE to re-use same conversation during
  retries.

- model_gemini:

  Character. Default gemini-3.8-flash. Model for Gemini.

- model_openai:

  Character. Default gpt-6.1-sol. Model for OpenAI / ChatGPT.

- model_anthropic:

  Character. Default claude-sonnet-5-5. Model for Anthropic / Claude.

- model_openrouter:

  Character. Default arcee-ai/trinity-large-preview:free. Model for
  OpenRouter.

- model_openai_compatible:

  Character. Default gpt-5.2. Model for OpenAI-compatible providers.

- model_deepseek:

  Character. Default deepseek-flash. Model for DeepSeek.

- model_apollo:

  Character. Default claude_4_6_sonnet. Model for Apollo (internal
  deployment only).

- model_azure:

  Character. Default gpt-5.2. Model for Azure OpenAI.

- model_aws:

  Character. Default anthropic.claude-sonnet-5. Model for AWS Bedrock.

- temperature:

  Numeric. Default 1. Between 0 (more deterministic) and 1 (more
  creative) (not all models support this).

- llm_seed:

  Numeric. Default 42. Seed number for LLMs (not all models support
  this).

- model_lang:

  Character. Default "mrgsolve" (the first element). Model translation
  language: "mrgsolve", "nonmem" (for testing only) or "rxode2" (for
  testing only).

- prompts_path:

  Character. Path to an R file defining the LLM prompts. Defaults to the
  prompts installed with MVPapp. See
  [`vignette("automatic-translation")`](https://stevechoy.github.io/MVPapp/articles/automatic-translation.md)
  for more information.

- show_debugging_msg:

  Logical. Default FALSE. Set to TRUE to output verbose working messages
  in the console, useful for debugging. AI translation responses are
  then also kept for review (see Details).

- brand_color:

  Character. Default NULL (the default navy). A colour, e.g. `"#F36633"`
  or `"orange"`, used for the navbar, active tab, box headers, table
  headers and loading spinners. See "Branding" in Details.

- brand_text_color:

  Character. Default NULL. Colour of the text on `brand_color` (navbar
  and box headers). By default white or black is chosen for readability.
  Ignored if `brand_color` is NULL.

- brand_logo:

  Character. Default NULL (no logo). Path to an image file (png, jpg,
  svg, ...) or a web URL, shown in the navbar in front of the page
  title.

- brand_favicon:

  Character. Default NULL (the MVP icon shipped with the package). Path
  to an image file (png, ico, svg) or a web URL, used as the browser tab
  icon.

- page_title:

  Character. Default NULL ("Model Visualization Platform (MVP)"). Title
  shown in the navbar. When set, it is also used as the browser tab
  title.

- ...:

  For `run_mvp()`,
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html)
  arguments (e.g. `launch.browser`, `port`). `launch.browser` defaults
  to `TRUE` here, so the app opens in your web browser (also from
  RStudio, which would otherwise use its viewer pane); use
  `launch.browser = FALSE` to turn that off. Any other named arguments
  are passed to the app.

## Value

`run_mvp()` is called for its side effect of launching the app and
returns the value of
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html) invisibly
once the app is closed. `mvp_app()` returns a Shiny app object (class
`shiny.appobj`).

## Details

**Objects kept for debugging.** MVPapp does not write to the global
environment. Objects useful for debugging are kept in an internal
environment, `MVPapp:::.mvp_state`, and remain available in the R
session after the app is closed. List them with
`ls(MVPapp:::.mvp_state)`:

- `last_ev_df$model_1`, `last_ev_df$model_2`: the dosing event tables
  used in each model's most recent simulation (also used for the
  reproducible bundle download).

- `deps_env`: an internal cache used to run the app; not useful for
  debugging.

- With `show_debugging_msg = TRUE`, AI translation responses:
  `llm_result` (initial translation), `llm_refine` (latest retry that
  still failed), `llm_success` (Dify's reply when told the code
  compiled), `branch_model` / `branch_refine` (raw answers before
  cleaning) and `ellmer_chat_response` / `dify_chat_response` (raw chat
  responses).

**Branding.** For a company deployment, the title, colour, logo and
favicon can be set when launching, without editing any package files.
Each is optional; with all of them left at `NULL` the app looks as it
always did.

    run_mvp(brand_color   = "#F36633",
            brand_logo    = "path/to/company_logo.png",
            brand_favicon = "path/to/favicon.png",
            page_title    = "Company Model Visualization Platform")

Image files are read from where they are and served by the app, so they
do not need to be inside the package (a web URL also works). A logo with
a transparent background and a height of about 30 pixels or more works
best. `brand_color` replaces the navy in the navbar, the active tab, the
headers and borders of the panels, the data table headers and the
loading spinners. The same arguments work with `mvp_app()` for hosted
deployments.

**Hosting and security.** MVPapp evaluates R and C++ model code supplied
in the browser, so anyone who can open the app can run code on the
server under the server account. `authentication_code` and the model
passwords are a convenience to limit access to the interface and to hide
models, not a security boundary: host the app only for trusted users, in
a sandboxed account or container. The same applies to AI translation:
its output is compiled automatically, and compiling mrgsolve code can
itself run commands, so do not translate documents from untrusted
sources on a shared server.

Without installing MVPapp (e.g. where package installation is
restricted), the development version can also be launched from GitHub,
provided its dependencies are installed:
`shiny::runGitHub("MVPapp", "stevechoy", subdir = "inst/shiny")`.

## See also

<https://stevechoy.github.io/MVPapp/articles/supply-passwords.html>
<https://stevechoy.github.io/MVPapp/articles/automatic-translation.html>
[`chat`](https://ellmer.tidyverse.org/reference/chat-any.html) for LLM
chat interface.

## Examples

``` r
if (interactive()) {
  run_mvp(insert_watermark = FALSE) # remove watermarks

  # Other examples:
  # run_mvp(launch.browser = FALSE) # do not open a browser (argument passed to runApp)
  # run_mvp(authentication_code = "some_password") # Password-lock the site,
  #                                                # could be useful in deployment
  # run_mvp(pw_models_path = "path/to/your/private/models.R") # see
  #   system.file("shiny/passworded_models_example.R", package = "MVPapp")
  # run_mvp(llm_choices = "Claude", model_anthropic = "claude-sonnet-5-5",
  #         temperature = 0) # Uses Claude only with reproducible results
}

# App object, e.g. as the only line of an app.R file for deployment:
app <- mvp_app(insert_watermark = FALSE)
```
