#' @name run_mvp
#'
#' @title
#' Launch Model Visualization Platform App
#'
#' @description
#' This function will launch MVP app and pass along arbitrary parameters through
#' the \code{...} parameter to the application. The app is evaluated in its own
#' environment (whose parent gives access to MVPapp and its key dependencies),
#' so neither the global environment nor the search path is modified. Global
#' options changed for the app are restored when it closes.
#'
#' @param appDir the directory of the application to run.
#' @param insert_watermark Logical. Default TRUE. Set to FALSE to remove "For Internal Use Only" text in simulated plots.
#' @param max_dataset_size Numeric. Default 200. Maximum upload dataset size in MB.
#' @param authentication_code Character. Default NA_character_. Provide a string (e.g., password) to password-lock the entire app.
#' @param internal_version Logical. Default TRUE. Setting to FALSE may allow generation of NCA reports when hosted on AWS with different access rights.
#' @param use_bi_styling Logical. Default FALSE. Set to TRUE to insert BI logo (deprecated - currently inactive).
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
#' @param ... [shiny::runApp()] parameters, [shiny::shinyApp()] parameters,
#'        or parameters to pass to the Shiny app.
#'
#' @details
#' If the user wishes to run the App outside of the function (e.g. preparing for
#' deployment on Posit Connect), this can be done by accessing shiny/app.R,
#' which is located inside the folder of where the package was installed, and modify
#' standalone_mode = TRUE (and setting these options there as required).
#'
#' @returns Called for its side effect of launching the Shiny app. Returns the
#'   value returned by [shiny::runApp()] invisibly, after the app is closed.
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
#' @note
#' Adapted from https://github.com/jbryer/ShinyDemo/blob/master/R/run_shiny_app.R
#' @seealso
#' \url{https://stevechoy.github.io/MVPapp/articles/supply-passwords.html}
#' \url{https://stevechoy.github.io/MVPapp/articles/automatic-translation.html}
#' \code{\link[ellmer]{chat}} for LLM chat interface.
#' @export
run_mvp <- function(appDir                  = system.file("shiny", package = "MVPapp"),
                    insert_watermark        = TRUE,
                    max_dataset_size        = 200,
                    authentication_code     = NA_character_,
                    internal_version        = TRUE,
                    use_bi_styling          = FALSE,
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
  
# Create a list of the parameters
  params <- list(
    insert_watermark        = insert_watermark,
    max_dataset_size        = max_dataset_size,
    authentication_code     = authentication_code,
    internal_version        = internal_version,
    use_bi_styling          = use_bi_styling,
    pw_models_path          = pw_models_path,
    llm_choices             = llm_choices,
    api_upload              = api_upload,             
    api_chat                = api_chat,
    user_id                 = user_id,
    reuse_context           = reuse_context,
    model_gemini            = model_gemini,
    model_openai            = model_openai,
    model_anthropic         = model_anthropic,
    model_openrouter        = model_openrouter,
    model_openai_compatible = model_openai_compatible,
    model_deepseek          = model_deepseek,
    model_apollo            = model_apollo,
    model_azure             = model_azure,
    model_aws               = model_aws,
    temperature             = temperature,
    llm_seed                = llm_seed,
    model_lang              = model_lang,
    prompts_path            = prompts_path,
    show_debugging_msg      = show_debugging_msg
  )

  # Handle additional parameters passed via ...
  additional_params <- list(...)
  runApp_params     <- names(formals(shiny::runApp))
  shinyApp_params   <- names(formals(shiny::shinyApp))

  runApp_args <- additional_params[names(additional_params) %in% runApp_params]
  extra_app_args <- additional_params[!names(additional_params) %in% c(runApp_params, shinyApp_params)]
  params <- c(params, extra_app_args[!names(extra_app_args) %in% names(params)])

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

  old_options <- options(
    scipen = 3, # avoid scientific notation, good up until 3e-07 / 1e+08
    DT.options = list(pageLength = 20, language = list(search = 'Filter:'), scrollX = TRUE), # dataTable options
    shiny.maxRequestSize = max_dataset_size * 1024^2 # Maximum file upload size
  )
  on.exit(options(old_options), add = TRUE)

  app_file <- file.path(appDir, "app.R")
  if (!file.exists(app_file)) {
    stop("Could not find app.R in appDir: ", appDir, call. = FALSE)
  }
  app <- eval(parse(file = app_file, keep.source = FALSE, encoding = "UTF-8"), envir = app_env)

  runApp_args$appDir <- app
  invisible(do.call(shiny::runApp, runApp_args))
}
