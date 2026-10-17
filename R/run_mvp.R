#' @name run_mvp
#'
#' @title
#' Launch Model Visualization Platform App
#'
#' @description
#' `run_mvp()` launches the MVP app. `mvp_app()` returns the app as a Shiny app
#' object without running it, e.g. for deployment on Posit Connect or
#' shinyapps.io (an `app.R` containing `MVPapp::mvp_app()` is sufficient).
#'
#' The app is evaluated in its own environment (whose parent gives access to
#' MVPapp and its key dependencies), so neither the global environment nor the
#' search path is modified. Global options changed by the app are set when it
#' starts and restored when it stops.
#'
#' @param appDir the directory of the application to run.
#' @param insert_watermark Logical. Default TRUE. Set to FALSE to remove "For Internal Use Only" text in simulated plots.
#' @param max_dataset_size Numeric. Default 200. Maximum upload dataset size in MB.
#' @param authentication_code Character. Default NA_character_. Provide a string (e.g., password) to password-lock the entire app.
#' @param internal_version Logical. Default TRUE. Setting to FALSE may allow generation of NCA reports when hosted on AWS with different access rights.
#' @param pw_models_path Character. Default NA_character_. Provide a path to source password-gated models.
#' @param llm_choices Character. Supported providers are "Claude", "Gemini", "OpenAI", "OpenRouter", "OpenAI-Compatible", "Deepseek", "Azure OpenAI", "AWS Bedrock"
#' @param api_upload Character. Default NA_character_. API upload URL when using Dify-based workflows (BI-only)
#' @param api_chat Character. Default NA_character_. API chat URL required when using OpenAI-Compatible providers
#' @param user_id Character. Default MVP_user. User ID for LLMs
#' @param reuse_context Logical. Default FALSE. Set to TRUE to re-use same conversation during retries.
#' @param model_gemini Character. Default gemini-3.8-flash. Model for Gemini.
#' @param model_openai Character. Default gpt-6.1-sol. Model for OpenAI / ChatGPT.
#' @param model_anthropic Character. Default claude-sonnet-5-5. Model for Anthropic / Claude.
#' @param model_openrouter Character. Default arcee-ai/trinity-large-preview:free. Model for OpenRouter.
#' @param model_openai_compatible Character. Default gpt-5.2. Model for OpenAI-compatible providers.
#' @param model_deepseek Character. Default deepseek-flash. Model for DeepSeek.
#' @param model_apollo Character. Default gpt-5.2 (BI-only)
#' @param model_azure Character. Default gpt-5.2. Model for Azure OpenAI.
#' @param model_aws Character. Default anthropic.claude-sonnet-5. Model for AWS Bedrock.
#' @param temperature Numeric. Default 0, between 0 (more deterministic) to 1 (more creative) (not all models support this).
#' @param llm_seed Numeric. Default 42. Seed number for LLMs (not all models support this).
#' @param model_lang Character. Default mrgsolve. Changes model translation language to "mrgsolve", "nonmem" (for testing only), "rxode2" (for testing only)
#' @param prompts_path Prompts file. See vignette ("automatic-translation") for more information.
#' @param show_debugging_msg Logical. Default FALSE. Set to TRUE to output verbose working messages in the console, useful for debugging.
#'   AI translation responses are then also kept for review (see Details).
#' @param ... For `run_mvp()`, [shiny::runApp()] arguments (e.g. `launch.browser`,
#'        `port`). Any other named arguments are passed to the app.
#'
#' @details
#' **Objects kept for debugging.** MVPapp does not write to the global
#' environment. Objects useful for debugging are kept in an internal
#' environment, `MVPapp:::.mvp_state`, and remain available in the R session
#' after the app is closed. List them with `ls(MVPapp:::.mvp_state)`:
#' * `last_ev_df$model_1`, `last_ev_df$model_2`: the dosing event tables used
#'   in each model's most recent simulation (also used for the reproducible
#'   bundle download).
#' * With `show_debugging_msg = TRUE`, AI translation responses: `llm_result`
#'   (initial translation), `llm_refine` (latest retry that still failed),
#'   `llm_success` (retry that compiled), `branch_model` / `branch_refine` (raw
#'   answers before cleaning) and `ellmer_chat_response` /
#'   `dify_chat_response` (raw chat responses).
#'
#' Without installing MVPapp (e.g. where package installation is restricted),
#' the app can also be launched from a tagged release on GitHub, provided its
#' dependencies are installed:
#' `shiny::runGitHub("MVPapp", "stevechoy", ref = "v1.0.0", subdir = "inst/shiny")`.
#'
#' @returns `run_mvp()` is called for its side effect of launching the app and
#'   returns the value of [shiny::runApp()] invisibly once the app is closed.
#'   `mvp_app()` returns a Shiny app object (class `shiny.appobj`).
#'
#' @examples
#' if (interactive()) {
#'   run_mvp(insert_watermark = FALSE) # remove watermarks
#'
#'   # Other examples:
#'   # run_mvp(launch.browser = TRUE) # launch app in browser (argument passed to runApp)
#'   # run_mvp(authentication_code = "some_password") # Password-lock the site,
#'   #                                                # could be useful in deployment
#'   # run_mvp(pw_models_path = "path/to/your/private/models.R") # see
#'   #   "shiny/passworded_models_example.R" on how to set one up
#'   # run_mvp(llm_choices = "Claude", model_anthropic = "claude-sonnet-4-6",
#'   #         temperature = 0) # Uses Claude only with reproducible results
#' }
#'
#' # App object, e.g. as the only line of an app.R file for deployment:
#' app <- mvp_app(insert_watermark = FALSE)
#' @note
#' Adapted from https://github.com/jbryer/ShinyDemo/blob/master/R/run_shiny_app.R
#' @seealso
#' \url{https://stevechoy.github.io/MVPapp/articles/supply-passwords.html}
#' \url{https://stevechoy.github.io/MVPapp/articles/automatic-translation.html}
#' \code{\link[ellmer]{chat}} for LLM chat interface.
#' @export
mvp_app <- function(appDir                  = system.file("shiny", package = "MVPapp"),
                    insert_watermark        = TRUE,
                    max_dataset_size        = 200,
                    authentication_code     = NA_character_,
                    internal_version        = TRUE,
                    pw_models_path          = NA_character_,
                    llm_choices             = c("Claude", "Gemini", "OpenAI", "OpenRouter", "OpenAI-Compatible", "DeepSeek", "Azure OpenAI", "AWS Bedrock"),
                    api_upload              = NA_character_,
                    api_chat                = NA_character_, 
                    user_id                 = "MVP_user",
                    reuse_context           = FALSE, # Re-use same conversation to keep original context for better re-iteration answers
                    model_gemini            = "gemini-3.8-flash",
                    model_openai            = "gpt-6.1-sol", # "gpt-5-mini"
                    model_anthropic         = "claude-sonnet-5-5", 
                    model_openrouter        = "arcee-ai/trinity-large-preview:free",  # "openrouter/free"
                    model_openai_compatible = "gpt-5.2",
                    model_deepseek          = "deepseek-flash",
                    model_apollo            = "claude_4_6_sonnet", # For BI-only
                    model_azure             = "gpt-5.2",
                    model_aws               = "anthropic.claude-sonnet-5",
                    temperature             = 1,
                    llm_seed                = 42,
                    model_lang              = c("mrgsolve", "nonmem", "rxode2"),
                    prompts_path            = system.file("shiny/prompts.R", package = "MVPapp"),
                    show_debugging_msg      = FALSE,
                    ...) {

  # Collect the app settings (all named arguments except appDir) plus any extra
  # named arguments passed via ...
  params <- mget(setdiff(names(formals(mvp_app)), c("appDir", "...")), envir = environment())
  extra  <- list(...)
  extra  <- extra[nzchar(names(extra) %||% character(length(extra)))]
  params <- c(params, extra[!names(extra) %in% names(params)])

  # Evaluate the app in its own environment instead of the global environment.
  # Its parent exposes MVPapp (including internal helpers), the package
  # datasets, and the exports of shiny, shinyBS, dplyr, mrgsolve, ggplot2,
  # rlang and magrittr, without attaching any packages.
  app_env <- new.env(parent = mvp_deps_env())
  for (i in names(params)) {
    if (!is.null(params[[i]])) {
      assign(i, params[[i]], envir = app_env)
    }
  }

  # shinyBS registers its JS/CSS resource path when attached; register it
  # explicitly so popovers work without calling library(shinyBS)
  shiny::addResourcePath("sbs", system.file("www", package = "shinyBS"))

  app_file <- file.path(appDir, "app.R")
  if (!file.exists(app_file)) {
    stop("Could not find app.R in appDir: ", appDir, call. = FALSE)
  }
  eval(parse(file = app_file, keep.source = FALSE, encoding = "UTF-8"), envir = app_env)
}

#' @rdname run_mvp
#' @export
run_mvp <- function() {
  # Same arguments as mvp_app() (formals copied below); split ... into
  # shiny::runApp() arguments and app settings
  args <- mget(setdiff(names(formals(mvp_app)), "..."), envir = environment())
  dots <- list(...)
  dot_names   <- names(dots) %||% character(length(dots))
  is_run_arg  <- dot_names %in% names(formals(shiny::runApp))
  app <- do.call(mvp_app, c(args, dots[!is_run_arg]))
  invisible(do.call(shiny::runApp, c(list(appDir = app), dots[is_run_arg])))
}
formals(run_mvp) <- formals(mvp_app)
