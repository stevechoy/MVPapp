#' @name run_mvp
#'
#' @title
#' Launch Model Visualization Platform App
#'
#' @description
#' This function will launch MVP app and pass along arbitrary parameters through
#' the \code{...} parameter to the application. This is done by modifying the
#' global environment. This function will attempt to clean up any objects placed
#' into the global environment on exit. If objects exist prior to calling this
#' function (i.e. \code{exists(OBJECT)} returns TRUE) then the value will be reset
#' to it's state prior to calling \code{run_mvp}.
#'
#' @param appDir the directory of the application to run.
#' @param insert_watermark Logical. Default TRUE. Set to FALSE to remove "For Internal Use Only" text in simulated plots.
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
#' @param model_openai Character. Default gpt-5.6-terra. Model for OpenAI / ChatGPT.
#' @param model_anthropic Character. Default claude-sonnet-5. Model for Anthropic / Claude.
#' @param model_openrouter Character. Default arcee-ai/trinity-large-preview:free. Model for OpenRouter.
#' @param model_openai_compatible Character. Default gpt-5.2. Model for OpenAI-compatible providers.
#' @param model_deepseek Character. Default deepseek-v4-flash. Model for DeepSeek.
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
#' @examples
#' \dontrun{
#' run_mvp(insert_watermark = FALSE) # remove watermarks
#' run_mvp(launch.browser = TRUE) # launch app in browser (argument passed to shinyApp)
#' run_mvp(authentication_code = "some_password") # Password-lock the site,
#' # could be useful in deployment
#' run_mvp(pw_models_path = "path/to/your/private/models.R") # see
#' # "shiny/passworded_models_example.R" on how to set one up
#' run_mvp(llm_service = "Claude", model_anthropic = "claude-sonnet-4-6", temperature = 0) # Uses Claude only with reproducible results
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
                    model_openai            = "gpt-5.6-terra", # "gpt-5-mini"
                    model_anthropic         = "claude-sonnet-5", 
                    model_openrouter        = "arcee-ai/trinity-large-preview:free",  # "openrouter/free"
                    model_openai_compatible = "gpt-5.2",
                    model_deepseek          = "deepseek-v4-flash",
                    model_apollo            = "claude_4_6_sonnet", # For BI-only
                    model_azure             = "gpt-5.2",
                    model_aws               = "anthropic.claude-sonnet-5",
                    temperature             = 0,
                    llm_seed                = 42,
                    model_lang              = c("mrgsolve", "nonmem", "rxode2"),
                    prompts_path            = system.file("shiny/prompts.R", package = "MVPapp"),
                    show_debugging_msg      = FALSE,
                    ...) {
  
# Create a list of the parameters
  params <- list(
    insert_watermark        = insert_watermark,
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

  shinyApp_args <- list()
  runApp_args   <- list()

  # Handle additional parameters passed via ...
  additional_params <- list(...)
  runApp_params <- names(formals(shiny::runApp))
  shinyApp_params <- names(formals(shiny::shinyApp))

  runApp_args <- additional_params[names(additional_params) %in% runApp_params]
  shinyApp_args <- additional_params[names(additional_params) %in% shinyApp_params]
  app_args <- params[!names(params) %in% c(runApp_params, shinyApp_params)]

  # Set parameters in the global environment
  reset_params <- list()
  added_params <- character()  # Track which parameters are added to the global environment
  for (i in names(params)) {
    if (!is.null(params[[i]])) {
      if (i %in% ls_all()) {
        reset_params[[i]] <- get(i)
      }
      .GlobalEnv[[i]] <- params[[i]]
      added_params <- c(added_params, i)  # Track added parameter
    }
  }

  # Ensure cleanup on exit
  on.exit({
    # Remove only the objects that were added
    if (length(added_params) > 0) {
      rm(list = added_params, envir = .GlobalEnv)
    }
    # Restore pre-existing objects
    for (i in names(reset_params)) {
      .GlobalEnv[[i]] <- reset_params[[i]]
    }
  }, add = TRUE)

  # Loading packages here to side-step JS compatibility issues
  library(shiny) # 1.7.5.1
  library(shinyBS) # 0.61.1 # this needs to be reloaded to make popovers work
  library(dplyr) # 1.1.3 # required for data code editor
  library(mrgsolve) # 1.5.2 # required for sim code editor

  default_options <- options()
  options(scipen=3) # Set the penalty to a high value to avoid scientific notation, this value is good up until 3e-07 / 1e+08
  options(DT.options = list(pageLength = 20, language = list(search = 'Filter:'), scrollX = T)) # dataTable options
  options(shiny.maxRequestSize = 200*1024^2) # Maximum file upload size

  runApp_args$appDir <- appDir
  do.call(shiny::runApp, runApp_args)
}

#' List All Objects
#'
#' This function returns a character vector of all objects available. Unlike
#' [ls()] this function will loop through all environments from the current
#' environment to \code{.GlobalEnv}. This will also verify that the object
#' is indeed available from the current environment using the [exists()]
#' function call.
#'
#' @return a character vector with the name of all objects available.
#' @note
#' Copied from https://github.com/jbryer/ShinyDemo, R/ls_all.R
ls_all <- function() {
  objs <- character()
  i <- 1
  repeat {
    local_objs <- ls(parent.frame(i))
    # Confirm the object is available from the current environment
    for(j in local_objs) {
      if(exists(j)) {
        objs <- c(objs, j)
      }
    }
    if(identical(parent.frame(i), .GlobalEnv)) {
      break
    } else {
      i <- i + 1
    }
  }
  return(objs)
}

