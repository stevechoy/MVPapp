# AI-assisted model translation (LLM providers)

#-------------------------------------------------------------------------------
#' @name translate_model_code
#'
#' @title Upload, and translate a PDF file to model code via external API
#'
#' @description
#' Sends a file to a Dify Workflow or LLM API for translation into model code.
#' The file should be pre-validated and optionally pre-combined using
#' \code{combine_uploaded_files} before being passed to this function.
#'
#' @param ready_path Path to the file to be processed. For multi-file uploads,
#'   this should be the combined output of \code{combine_uploaded_files}.
#' @param file_name Original filename (used for display in notifications and
#'   for MIME type detection). If \code{NULL}, derived from \code{ready_path}.
#' @param service Choice of "PROD" (internal deployment only), "EXP" (internal deployment only), "Gemini", "OpenAI", "AWS Bedrock",
#'  "Claude", "OpenRouter", "OpenAI-Compatible", "DeepSeek", "Apollo" (internal deployment only), "Azure OpenAI"
#' @param api_key API Key, recommended to store it as env var called "ANTHROPIC_API_KEY" etc
#' @param api_upload API URL for uploading of files (Dify requires a 2-step process)
#' @param api_chat API URL for chat messages, required when using OpenAI-compatible API
#' @param user_id user id for the request (internal deployment only)
#' @param model_gemini Model to be used when calling Gemini API
#' @param model_openai Model to be used when calling OpenAI API
#' @param model_anthropic Model to be used when calling Anthropic API
#' @param model_openrouter Model to be used when calling OpenRouter
#' @param model_openai_compatible Model to be used when calling OpenAI-compatible API
#' @param model_deepseek Model to be used when calling DeepSeek
#' @param model_apollo Model to be used when calling Apollo (internal deployment only)
#' @param model_azure Model to be used when calling Azure OpenAI
#' @param model_aws Model to be used when calling AWS Bedrock
#' @param display_info Set to TRUE to show how much time/tokens in Shiny UI when job is finished
#' @param temperature Goes from 0 to 1, where 0 is deterministic
#' @param seed seed number for LLMs
#' @param locally_parse_file if TRUE, extracts text locally and includes in prompt instead of uploading file
#' @param model_lang Either "mrgsolve" or "nonmem" (changes the prompt)
#' @param deep_pdfscan Use Vision to extract image data (internal deployment only)
#' @param force_parse Force parsing (internal deployment only)
#' @param mrgsolve_system_prompt String for mrgsolve system prompt
#' @param mrgsolve_long_user_prompt String for mrgsolve long user prompt
#' @param mrgsolve_short_user_prompt String for mrgsolve short user prompt
#' @param nonmem_system_prompt String for nonmem system prompt
#' @param nonmem_long_user_prompt String for nonmem long user prompt
#' @param nonmem_short_user_prompt String for nonmem short user prompt
#' @param rxode2_system_prompt String for rxode2 system prompt
#' @param rxode2_long_user_prompt String for rxode2 long user prompt
#' @param rxode2_short_user_prompt String for rxode2 short user prompt
#' @param internal_version Logical. Only relevant for internal deployments
#' @param max_tokens Numeric. Maximum output tokens.
#' @param debug Displays debug messages and keeps the responses in MVPapp:::.mvp_state
#'
#' @returns a named list with \code{answer}, \code{conversation_id}, and
#'   \code{chat_obj}, or \code{NULL} on failure.
#' @importFrom shiny setProgress incProgress showNotification
#' @importFrom stats setNames
#' @importFrom rlang %||%
#' @noRd
#-------------------------------------------------------------------------------

translate_model_code <- function(ready_path,
                                 file_name = NULL,
                                 service,
                                 api_key,
                                 api_upload = NULL,
                                 api_chat = NULL,
                                 user_id = "mrgsolve_translator",
                                 model_gemini = "gemini-3.8-flash",
                                 model_openai = "gpt-5-mini",
                                 model_anthropic = "claude-haiku-4-5-20251001",
                                 model_openrouter = "arcee-ai/trinity-large-preview:free",
                                 model_openai_compatible = "gpt-5-mini",
                                 model_deepseek = "deepseek-flash",
                                 model_apollo = "gpt-5.6-terra",
                                 model_azure = "gpt-5.6-terra",
                                 model_aws = "anthropic.claude-sonnet-5",
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
                                 max_tokens = 128000,
                                 debug = TRUE
) {
  rlang::check_installed(c("ellmer", "httr2", "mime"), reason = "to use `translate_model_code()`.")

  file_path <- ready_path
  file_name <- file_name %||% basename(ready_path)

  is_text_file <- grepl("\\.(txt|mod|ctl|cpp|lst)$", file_name, ignore.case = TRUE)
  
  if(is_text_file) {
    detected_type <- "text/plain"
    new_path <- paste0(file_path, ".txt")
    file.copy(file_path, new_path)
    file_path <- new_path
    locally_parse_file <- TRUE
    safely_showNotification(paste0("Text file detected, will be parsed locally to bypass file upload."))
  } else {
    detected_type <- mime::guess_type(file_name)
  }

  model_name <- switch(EXPR = service,
                       "PROD"              = "[PROD model]",
                       "PMx Co-Modeler"    = "[Apollo model]",
                       "EXP"               = "[EXP model]",
                       "Fast"              = "[Apollo model]",
                       "Gemini"            = model_gemini,
                       "OpenAI"            = model_openai,
                       "Claude"            = model_anthropic,
                       "OpenRouter"        = model_openrouter,
                       "OpenAI-Compatible" = model_openai_compatible,
                       "DeepSeek"          = model_deepseek,
                       "Apollo"            = model_apollo,
                       "Test Provider"     = model_apollo,
                       "Azure OpenAI"      = model_azure,
                       "AWS Bedrock"       = model_aws)
  
  if(debug) {
    message(paste0("translate_model_code: 0) model_name is: ", model_name))
  }
  
  ## Allow list to explicitly list some popular legacy models that supports temperature settings
  models_yes_temperature <- c("claude-sonnet-4-6", "claude-opus-4-6", "claude-haiku-4-5", "claude-haiku-4-5-20251001",
                              "gemini-3.7-flash", "gemini-3.6-flash", "gemini-3.5-flash",
                              "gpt-5.2")
  
  if(model_name %in% models_yes_temperature) {
    optimal_params <- ellmer::params(temperature = temperature, seed = seed, max_tokens = max_tokens)
  } else {
    safely_showNotification(paste0(model_name, " does not support temperature setting."), type = "warning")
    optimal_params <- ellmer::params(seed = seed, max_tokens = max_tokens)
  }
  
  if(debug) {
    message("translate_model_code: 1) API key check")
  }
  
  if(model_lang == "mrgsolve") {
    system_prompt     <- mrgsolve_system_prompt
    long_user_prompt  <- mrgsolve_long_user_prompt
    short_user_prompt <- mrgsolve_short_user_prompt
  }
  
  if(model_lang == "nonmem") {
    system_prompt     <- nonmem_system_prompt
    long_user_prompt  <- nonmem_long_user_prompt
    short_user_prompt <- nonmem_short_user_prompt
  }
  
  if(model_lang == "rxode2") {
    system_prompt     <- rxode2_system_prompt
    long_user_prompt  <- rxode2_long_user_prompt
    short_user_prompt <- rxode2_short_user_prompt    
  }
  
  if(service %in% dify_services) {
    instruction_prompt <- short_user_prompt
  } else {
    instruction_prompt <- long_user_prompt
  }
  
  # ── Service-level format guard (still relevant for single-file non-PDF/text uploads) ──
  if (service %in% c("DeepSeek", "Apollo", "Test Provider")) {
    safely_showNotification(
      paste0(service, " currently only supports local parsing."),
      type = "warning"
    )
    locally_parse_file <- TRUE
  }
  
  if (service %in% c("OpenAI", "OpenRouter", "OpenAI-Compatible", "DeepSeek", "Apollo", "Test Provider") &&
      !detected_type %in% c("application/pdf", "text/plain")) {
    safely_showNotification(
      paste0(service, " currently only supports PDF or plain text format."),
      type = "error"
    )
    return(NULL)
  }
  
  if(debug) {
    message("translate_model_code: 2) result tryCatch")
  }
  
  # --- Initialize output variables outside tryCatch for clarity ---
  usage_info      <- list(input = 0, output = 0, total = 0, model = "unknown")
  conversation_id <- ""
  chat_obj        <- NULL
  elapsed         <- NA
  tokens_per_sec  <- "N/A"
  
  result <- tryCatch({
    
    safely_withProgress(message = 'Processing...', value = 0, {
      
      # --- Local file parsing ---
      if(locally_parse_file) {
        
        safely_incProgress(0.1, detail = paste("Parsing", file_name, "locally..."))
        
        instruction_prompt <- paste0(
          instruction_prompt,
          "\n\n--- FILE CONTENT START ---\n",
          extract_file_content(file_path, file_name, detected_type, debug),
          "\n--- FILE CONTENT END ---"
        )
        
      } else {
        safely_incProgress(0.1, detail = paste("Uploading", file_name, "..."))
      }
      
      start_time <- Sys.time()
      safely_incProgress(0.3, detail = paste0("Generating ", model_lang, " code using ", model_name, " (please be patient, takes ~1 min)..."))

      branch_result <- if (service %in% dify_services) {
        
        if(debug) message("translate_model_code: 3) Dify branch")
        run_dify_chat(
          conversation_id    = NULL,
          instruction_prompt = instruction_prompt,
          api_key            = api_key,
          api_upload         = api_upload,
          api_chat           = api_chat,
          user_id            = user_id,
          file_path          = file_path,
          detected_type      = detected_type,
          locally_parse_file = locally_parse_file,
          deep_pdfscan       = deep_pdfscan,
          force_parse        = force_parse,
          model_name         = model_name,
          debug              = debug
        )
      } else {
        
        if(debug) message("translate_model_code: 3) Ellmer branch")
        run_ellmer_chat(
          chat_obj           = NULL,
          service            = service,
          model_name         = model_name,
          system_prompt      = system_prompt,
          optimal_params     = optimal_params,
          api_chat           = api_chat,
          instruction_prompt = instruction_prompt,
          file_path          = file_path,
          detected_type      = detected_type,
          locally_parse_file = locally_parse_file,
          internal_version   = internal_version,
          debug              = debug
        )
      } # end BRANCH B
      
      if(debug) {.mvp_state$branch_model <- branch_result}

      elapsed <- round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 1)
      
      if(debug) message(paste0("translate_model_code: raw answer: ", branch_result$answer)) # Raw answer before cleaning
      
      # Return EVERYTHING needed after the tryCatch as a named list
      list(
        answer          = branch_result$answer,
        conversation_id = branch_result$conversation_id %||% "",
        chat_obj        = branch_result$chat_obj,
        elapsed         = elapsed,
        usage_info      = branch_result$usage_info
      )
      
    }) # end of withProgress
    
  }, error = function(e) {
    msg <- llm_error_message(e)
    if(debug) message("--- TRANSLATION ERROR ---\n", msg)
    safely_showNotification(paste("Translation Failed:", msg), type = "error", duration = NULL)
    return(NULL)
  })
  
  # Unpack cleanly after tryCatch
  if(!is.null(result)) {
    
    elapsed        <- result$elapsed
    usage_info     <- result$usage_info
    tokens_per_sec <- if(is.numeric(usage_info$total) && usage_info$total > 0) {
      round(usage_info$total / elapsed, 1)
    } else { "N/A" }
    
    if(display_info) {
      safely_showNotification(
        ui = shiny::tagList(
          shiny::tags$div(
            style = "font-size: 14px;",
            shiny::tags$b("Translation Complete"),
            shiny::tags$hr(style = "margin: 5px 0;"),
            shiny::tags$p(paste0("\u23f1 Time: ", elapsed, " seconds")),
            shiny::tags$p(paste0("\U0001f916 Model: ", usage_info$model)),
            shiny::tags$p(paste0("\U0001f39f Tokens: ", usage_info$total, " (", tokens_per_sec, " t/s)")),
            shiny::tags$p(style = "font-size: 11px; color: #666;",
                          paste0("In: ", usage_info$input, " | Out: ", usage_info$output))
          )
        ),
        type     = "message",
        duration = 25
      )
    }
    
    list_of_output <- list(
      answer          = result$answer,
      conversation_id = result$conversation_id,
      chat_obj        = result$chat_obj
    )
    
    if(debug) .mvp_state$llm_result <- list_of_output
    
    list_of_output$answer <- clean_llm_response(list_of_output$answer)
    
    return(list_of_output)
    
  } else {
    return(NULL)
  }
}

#-------------------------------------------------------------------------------
#' @name refine_model_code
#'
#' @title Provide a model code string and its compile error message to be refined
#'
#' @description
#' Sends a string containing model code and error message to a Dify Workflow or
#' LLM API for correction.
#'
#' @param model_code Input model code
#' @param error_message Input compilation error message
#' @param context Argument to hold chat_obj or conversation_id to keep same chat session
#' @param reuse_context Set to TRUE to re-use same conversation_id or chat_obj, more costly in terms of time and tokens
#' @param service Choice of "PROD", "EXP", "Gemini", "OpenAI", "Claude", "OpenRouter",
#'  "OpenAI-Compatible", "DeepSeek", "Apollo", "Azure OpenAI", "AWS Bedrock"
#' @param api_key API Key, recommended to store it as env var called "ANTHROPIC_API_KEY" etc (Dify only)
#' @param api_upload API URL for uploading of files (Dify requires a 2-step process)
#' @param api_chat API URL for chat messages, required for OpenAI-compatible
#' @param user_id user id for the request
#' @param model_gemini Model to be used when calling Gemini API
#' @param model_openai Model to be used when calling OpenAI API
#' @param model_anthropic Model to be used when calling Anthropic API
#' @param model_openrouter Model to be used when calling OpenRouter
#' @param model_openai_compatible Model to be used when calling OpenAI-compatible API
#' @param model_deepseek Model to be used when calling DeepSeek
#' @param model_apollo Model to be used when calling Apollo LLM
#' @param model_azure Model to be used when calling Azure OpenAI
#' @param model_aws Model to be used when calling AWS Bedrock
#' @param progress_bar Starting point of Shiny progress bar, relevant for 2nd try
#' @param max_retries Required to calculate a more accurate progress bar
#' @param display_info Set to TRUE to show how much time/tokens in Shiny UI when job is finished
#' @param temperature Goes from 0 to 1, where 0 is deterministic, if not reusing context
#' @param seed seed number for LLMs, if not reusing context
#' @param attempt current attempt number
#' @param deep_pdfscan Uses Vision to extract image data (internal deployment only)
#' @param force_parse Force reparsing (internal deployment only)
#' @param system_prompt Character. System prompt
#' @param long_user_prompt Character. Long user prompt
#' @param short_user_prompt Character. Short user prompt
#' @param internal_version Logical. Only relevant for internal deployments
#' @param feedback_success Logical. Only relevant for internal deployments, and if reuse_context = TRUE
#' @param max_tokens Numeric. Maximum output tokens.
#' @param debug Displays debug messages and keeps the responses in MVPapp:::.mvp_state
#'
#' @returns a named list of "answer", "conversation_id", "chat_obj"
#' @importFrom stats setNames
#' @importFrom rlang %||%
#' @noRd
#-------------------------------------------------------------------------------

refine_model_code <- function(model_code,
                              error_message,
                              context = NULL, # New argument to hold chat_obj or conversation_id
                              reuse_context = FALSE,
                              service,
                              api_key,
                              api_upload = NULL,
                              api_chat = NULL,
                              user_id = "mrgsolve_translator",
                              model_gemini = "gemini-3.8-flash",
                              model_openai = "gpt-5-mini",
                              model_anthropic = "claude-haiku-4-5-20251001",
                              model_openrouter = "arcee-ai/trinity-large-preview:free",
                              model_openai_compatible = "gpt-5-mini",
                              model_deepseek = "deepseek-flash",
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
                              max_tokens = 128000,
                              debug = TRUE
) {
  rlang::check_installed(c("ellmer", "httr2"), reason = "to use `refine_model_code()`.")
  
  model_name <- switch(EXPR = service,
                       "PROD"              = "[PROD model]",
                       "PMx Co-Modeler"    = "[Apollo model]",
                       "EXP"               = "[EXP model]",
                       "Fast"              = "[Apollo model]",
                       "Gemini"            = model_gemini,
                       "OpenAI"            = model_openai,
                       "Claude"            = model_anthropic,
                       "OpenRouter"        = model_openrouter,
                       "OpenAI-Compatible" = model_openai_compatible,
                       "DeepSeek"          = model_deepseek,
                       "Apollo"            = model_apollo,
                       "Test Provider"     = model_apollo,
                       "Azure OpenAI"      = model_azure,
                       "AWS Bedrock"       = model_aws)
  
  ## Allow list to explicitly list some popular legacy models that supports temperature settings
  models_yes_temperature <- c("claude-sonnet-4-6", "claude-opus-4-6", "claude-haiku-4-5", "claude-haiku-4-5-20251001",
                              "gemini-3.7-flash", "gemini-3.6-flash", "gemini-3.5-flash",
                              "gpt-5.2")
  
  if(model_name %in% models_yes_temperature) {
    optimal_params <- ellmer::params(temperature = temperature, seed = seed, max_tokens = max_tokens)
  } else {
    #safely_showNotification(paste0(model_name, " does not support temperature setting."), type = "warning")
    optimal_params <- ellmer::params(seed = seed, max_tokens = max_tokens)
  }
  
  # Calculate the width of one retry segment (e.g., 0.2 if max_retries is 3)
  if(!feedback_success) {
    segment_width <- (1 - 0.4) / max_retries 
  } else {
    segment_width <- 0 # Setting progress bar to 0 when relaying success messages
  }

  if(service %in% dify_services) {
    instruction_prompt <- short_user_prompt
  } else {
    instruction_prompt <- long_user_prompt
  }
  
  # Ensure error_message is a single string, not a vector of lines (i.e. if error comes from out$stderr)
  if (is.character(error_message) && length(error_message) > 1) {
    error_message <- paste(error_message, collapse = "\n")
  }
  
  retry_prompt <- paste0(
    instruction_prompt,
    "\n\n",
    "<model_code>\n",
    model_code, "\n",
    "</model_code>\n\n",
    "<error_message>\n",
    error_message, "\n",
    "</error_message>"
  )
  
  if(reuse_context) {
    safely_showNotification("Keeping same conversation to improve results...")
    if(feedback_success) {
      retry_prompt <- paste0(
        "Model compiled successfully!"
      )
      safely_showNotification(paste0(retry_prompt, " Feeding back to ", service, "."))
    }
  }
  
  if(debug) {message(paste0("refine_model_code: 1) ", retry_prompt))}
  
  result <- tryCatch({
    
    if(feedback_success) {
      process_msg <- "Relaying success..."
    } else {
      process_msg <- "Processing..."
    }
    
    safely_withProgress(message = process_msg, value = progress_bar, {
      
      if(!feedback_success) {
        safely_incProgress(segment_width * 0.33, detail = paste("Re-iterating after failed compilation..."))
      }
      
      start_time <- Sys.time()
      
      # --- BRANCH A: DIFY (Manual httr2 Logic) ---
      branch_result <- if(service %in% dify_services) {
        run_dify_chat(
          conversation_id    = if(reuse_context) context$conversation_id else NULL,
          instruction_prompt = retry_prompt,
          api_key            = api_key,
          api_chat           = api_chat,
          user_id            = user_id,
          locally_parse_file = TRUE,
          model_name         = model_name,
          deep_pdfscan       = FALSE,
          force_parse        = FALSE,
          debug              = debug
        )
      } else {
        run_ellmer_chat(
          chat_obj           = if(reuse_context) context$chat_obj else NULL,
          system_prompt      = system_prompt,
          instruction_prompt = retry_prompt,
          api_chat           = api_chat,
          locally_parse_file = TRUE,
          optimal_params     = optimal_params,
          service            = service,
          model_name         = model_name,
          internal_version   = internal_version,
          debug              = debug
        )
      }
      
      if(debug) {.mvp_state$branch_refine <- branch_result}
      
      if(!feedback_success) {
        safely_incProgress(segment_width * 0.33, detail = paste("Cleaning response..."))
      }
      
      # Return everything needed after tryCatch as a named list
      list(
        answer          = branch_result$answer,
        conversation_id = branch_result$conversation_id %||% "",
        chat_obj        = branch_result$chat_obj,
        elapsed         = round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 1),
        usage_info      = branch_result$usage_info
      )
      
    }) # end of withProgress
    
  }, error = function(e) {
    # Single unified error handler
    msg <- llm_error_message(e)
    
    if(debug) message("--- REFINEMENT ERROR ---\n", msg)
    safely_showNotification(paste("Refinement Failed:", msg), type = "error", duration = NULL)
    return(NULL)  # Graceful failure
  })
  
  # Unpack and display notification outside tryCatch
  if(!is.null(result)) {
    
    elapsed        <- result$elapsed
    usage_info     <- result$usage_info
    tokens_per_sec <- if(is.numeric(usage_info$total) && usage_info$total > 0) {
      round(usage_info$total / elapsed, 1)
    } else { "N/A" }
    
    if(debug) message(paste0("refine_model_code: 4) conversation_id: ", result$conversation_id))
    
    if(display_info) {
      safely_showNotification(
        ui = shiny::tagList(
          shiny::tags$div(
            style = "font-size: 14px;",
            shiny::tags$b(paste0("Retry Complete (#", attempt, ")")),
            shiny::tags$hr(style = "margin: 5px 0;"),
            shiny::tags$p(paste0("\u23f1 Time: ", elapsed, " seconds")),
            shiny::tags$p(paste0("\U0001f916 Model: ", usage_info$model)),
            shiny::tags$p(paste0("\U0001f39f Tokens: ", usage_info$total, " (", tokens_per_sec, " t/s)")),
            shiny::tags$p(style = "font-size: 11px; color: #666;",
                          paste0("In: ", usage_info$input, " | Out: ", usage_info$output))
          )
        ),
        type     = "message",
        duration = 25
      )
    }
  
    list_of_output <- list(
      answer          = result$answer,
      conversation_id = result$conversation_id,
      chat_obj        = result$chat_obj
    )
    
    # Keep responses for review when debugging (see ?run_mvp, "Objects kept for debugging")
    if(debug && !feedback_success) .mvp_state$llm_refine <- list_of_output
    if(debug && feedback_success) .mvp_state$llm_success <- list_of_output
    
    list_of_output$answer <- clean_llm_response(list_of_output$answer)
    
    if(feedback_success) {
      return(model_code) ## Not that it matters, since we're calling this function for its side effect when relaying success
    } else {
      return(list_of_output)      
    }
    
  } else {
    return(NULL)
  }
}

#-------------------------------------------------------------------------------
#' @name clean_llm_response
#'
#' @title Clean and Escape LLM Markdown Responses
#'
#' @description
#' This function strips markdown code fences (e.g., ```cpp, ```mrgsolve) and 
#' any text appearing before or after the code block. It also escapes 
#' backslashes and quotes to ensure the string can be safely wrapped in 
#' double quotes for downstream processing.
#'
#' @param final_answer A character string containing the raw LLM response.
#'
#' @return A character string stripped of markdown wrappers, with internal 
#'   quotes/backslashes escaped, and padded with a single newline at the 
#'   start and end.
#' @importFrom stringr str_replace_all str_replace fixed
#' @noRd
#------------------------------------------------------------------------------- 

clean_llm_response <- function(final_answer) {
  
  orig_answer <- final_answer
  
  # 1. Clean the Markdown (Keep these as regex)
  # Removes everything including and before the first code fence (```), and then
  # remove everything including and after the last code fence
  final_answer <- stringr::str_replace(final_answer, "(?s)^.*?```[a-zA-Z]*\\s*\\n?", "")
  final_answer <- stringr::str_replace(final_answer, "(?s)\\n?\\s*```[\\s\\S]*$", "")

  
  # 2. Perform all escapes in one pass using a named vector
  # Uses fixed() for maximum performance on large strings
  # The answer is placed inside a double-quoted R string in the code editor, so
  # backslashes must be escaped first, then quotes
  replaces <- c("\\" = "\\\\",
                "\"" = "\\\"",
                "'" = "\\'")
  
  final_answer <- stringr::str_replace_all(final_answer, stringr::fixed(replaces))
  
  # 3. Final Polish: Trim whitespace and add intentional newlines
  final_answer <- paste0("\n", trimws(final_answer), "\n")
  
  if(final_answer == "\n\n") {
    return(orig_answer) # Failsafe, return original answer if it's empty after cleaning
  } else {
    return(final_answer)    
  }
} 

#-------------------------------------------------------------------------------
#' @name extract_file_content
#'
#' @title Extract text content from a local file (pdf or text)
#'
#' @description
#' Reads a local PDF or plain text file and returns its text. For PDFs, text is
#' extracted using \code{pdftools::pdf_text()}. For plain text files (including
#' \code{.txt}, \code{.mod}, and \code{.ctl}), lines are read with
#' \code{readLines()}. The caller wraps the text in delimiters and appends it to
#' the user prompt.
#'
#' @param file_path Path to the local file to extract text from
#' @param file_name Original filename including extension, used for debug
#'   messaging only
#' @param detected_type MIME type of the file. Use \code{"application/pdf"} for
#'   PDF files and \code{"text/plain"} for all plain text formats
#' @param debug If \code{TRUE}, prints the file name and the extracted
#'   character count to the console
#'
#' @returns A single character string containing the extracted text
#'
#' @noRd
#-------------------------------------------------------------------------------

extract_file_content <- function(file_path,
                                 file_name,
                                 detected_type,
                                 debug = TRUE) {
  
  if(debug) message("extract_file_content: parsing '", file_name, "' locally")
  
  extracted_text <- if(detected_type == "application/pdf") {
    rlang::check_installed("pdftools", reason = "to extract text from PDF files.")
    paste(pdftools::pdf_text(file_path), collapse = "\n\n")
  } else {
    paste(readLines(file_path, warn = FALSE), collapse = "\n")
  }
  
  if(debug) message(paste0("Extracted text length: ", nchar(extracted_text), " characters"))
  
  return(extracted_text)
}

#-------------------------------------------------------------------------------
#' @name run_dify_chat
#'
#' @title Send a chat request to a Dify Workflow via httr2 (internal deployment only)
#'
#' @description
#' Handles the full Dify API interaction: optionally uploads a file first,
#' then sends the instruction prompt as a blocking workflow request and returns
#' the answer along with token usage metadata.
#'
#' @param instruction_prompt Full prompt string to send as the query, including
#'   any extracted file content if parsing locally
#' @param api_key Dify API key
#' @param api_upload API URL for the Dify file upload endpoint. Only used when
#'   \code{locally_parse_file = FALSE}
#' @param api_chat API URL for the Dify chat/workflow endpoint
#' @param user_id User identifier string passed to the Dify API
#' @param file_path Path to the file on disk. Only used when
#'   \code{locally_parse_file = FALSE}
#' @param detected_type MIME type of the file (e.g. \code{"application/pdf"}).
#'   Only used when \code{locally_parse_file = FALSE}
#' @param locally_parse_file If \code{TRUE}, skips file upload and sends text
#'   content directly in the prompt
#' @param conversation_id Default NULL, required for context reuse
#' @param deep_pdfscan Default FALSE, uses Vision to extract images
#' @param force_parse Default FALSE, forces reparsing
#' @param model_name Model name string used as a fallback label in usage_info
#'   if the API does not return a model identifier
#' @param debug If \code{TRUE}, prints progress messages to the console
#'
#' @returns A named list with the following elements:
#' \describe{
#'   \item{answer}{Character string containing the raw model response}
#'   \item{conversation_id}{Dify conversation ID string for the request}
#'   \item{usage_info}{Named list with \code{input}, \code{output}, \code{total}
#'     token counts and \code{model} label}
#' }
#' @importFrom rlang %||%
#' @noRd
#-------------------------------------------------------------------------------

run_dify_chat <- function(instruction_prompt,
                          api_key,
                          api_upload         = NULL,
                          api_chat,
                          user_id,
                          file_path          = NULL, 
                          detected_type      = NULL,
                          locally_parse_file = TRUE,
                          conversation_id    = NULL,   # NEW: for context reuse
                          deep_pdfscan       = FALSE,
                          force_parse        = FALSE,
                          model_name,
                          debug = TRUE) {
  rlang::check_installed(c("httr2", "curl"), reason = "to use `run_dify_chat()`.")
  
  chat_body <- if(locally_parse_file) {
    
    if(debug) message("run_dify_chat: locally_parse_file == TRUE")
    
    if(!is.null(conversation_id)) {
      
      if(debug) message("run_dify_chat: conversation_id is not NULL")
      
      list(
        query = instruction_prompt,
        response_mode = "blocking",
        user = user_id,
        conversation_id = conversation_id, # USE CONTEXT HERE
        inputs = setNames(list(), character(0)) # Force an empty dictionary {} instead of an empty array []
      )
    } else {
      list(
        query = instruction_prompt,
        response_mode = "blocking",
        user = user_id,
        inputs = setNames(list(), character(0)) # Force an empty dictionary {} instead of an empty array []
      )
    }
  } else {
    upload_data <- httr2::request(api_upload) %>%
      httr2::req_headers(Authorization = paste("Bearer", api_key)) %>%
      httr2::req_body_multipart(
        user = user_id,
        file = curl::form_file(file_path, type = detected_type)
      ) %>%
      httr2::req_retry(max_tries = 3) %>%
      httr2::req_perform() %>%
      httr2::resp_body_json()
    
    if(debug) message("run_dify_chat: upload_data formed")
    
    list(
      inputs = list(
        deep_pdfscan = tolower(as.character(deep_pdfscan)),
        force_parse = tolower(as.character(force_parse)),
        fileInput = list(list(
          transfer_method = "local_file",
          upload_file_id  = upload_data$id,
          type            = "document"
        ))
      ),
      query         = instruction_prompt,
      response_mode = "blocking",
      user          = user_id,
      files         = list(list(
        type            = "document",
        transfer_method = "local_file",
        upload_file_id  = upload_data$id
      ))
    )
  }
  
  if(debug) message("run_dify_chat: sending request")
  
  chat_resp <- httr2::request(api_chat) %>%
    httr2::req_headers(
      Authorization  = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) %>%
    httr2::req_body_json(chat_body) %>%
    httr2::req_retry(max_tries = 2) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()
  
  if(debug) .mvp_state$dify_chat_response <- chat_resp
  
  list(
    answer          = chat_resp$answer,
    conversation_id = chat_resp$conversation_id,
    usage_info      = list(
      input  = chat_resp$metadata$usage$prompt_tokens     %||% "N/A",
      output = chat_resp$metadata$usage$completion_tokens %||% "N/A",
      total  = chat_resp$metadata$usage$total_tokens      %||% "N/A",
      model  = chat_resp$metadata$usage$model             %||% model_name
    )
  )
}

#-------------------------------------------------------------------------------
# Services that talk to a Dify-style endpoint (internal deployment only); every
# service check in this package uses this one vector
#-------------------------------------------------------------------------------
dify_services <- c("PROD", "EXP", "PMx Co-Modeler", "Fast")

#-------------------------------------------------------------------------------
# File extensions nonmem2rx can use (control stream / listing plus supporting
# files); shared by the upload widgets and prepare_uploaded_files()
#-------------------------------------------------------------------------------
nonmem2rx_file_types <- c(".ctl", ".mod", ".lst", ".ext", ".phi", ".cov", ".cor", ".coi", ".xml", ".grd", ".shk")

#-------------------------------------------------------------------------------
#' @name run_ellmer_chat
#'
#' @title Send a chat request via an ellmer provider
#'
#' @description
#' Initialises the appropriate ellmer chat object for the given service and
#' dispatches the prompt, either as text-only or alongside an uploaded file.
#' Supports Gemini, OpenAI, Claude, OpenRouter, and OpenAI-compatible endpoints.
#'
#' @param service Provider to use. One of \code{"Gemini"}, \code{"OpenAI"},
#'   \code{"Claude"}, \code{"OpenRouter"}, \code{"OpenAI-Compatible"}, \code{"DeepSeek"},
#'    \code{"Apollo"}, \code{"Azure OpenAI"}, \code{"AWS Bedrock"}
#' @param model_name Model identifier string passed to the ellmer chat constructor
#' @param system_prompt System prompt string defining model behaviour and output
#'   format constraints
#' @param optimal_params An \code{ellmer::params()} object controlling
#'   temperature, seed, and other supported sampling parameters
#' @param api_chat Base URL for the chat endpoint. Only required when
#'   \code{service = "openai_compatible"}
#' @param instruction_prompt Full user prompt string, including any extracted
#'   file content if parsing locally
#' @param file_path Path to the file on disk. Only used when
#'   \code{locally_parse_file = FALSE}
#' @param detected_type MIME type of the file (e.g. \code{"application/pdf"}).
#'   Only used when \code{locally_parse_file = FALSE}
#' @param locally_parse_file If \code{TRUE}, sends text content in the prompt
#'   directly rather than uploading the file to the provider
#' @param chat_obj the ellmer chat object for context re-use, default NULL
#' @param internal_version Logical. Changes base_url path, Only relevant for internal deployments
#' @param debug If \code{TRUE}, prints progress messages and the chat object
#'   summary to the console
#'
#' @returns A named list with the following elements:
#' \describe{
#'   \item{answer}{Character string containing the raw model response}
#'   \item{chat_obj}{The ellmer chat object, which can be used for token
#'     inspection or follow-up turns}
#'   \item{usage_info}{Named list with \code{input}, \code{output}, \code{total}
#'     token counts and \code{model} label}
#' }
#' @importFrom rlang %||%
#' @noRd
#-------------------------------------------------------------------------------
run_ellmer_chat <- function(service,
                            model_name,
                            system_prompt,
                            optimal_params,
                            api_chat            = NULL,
                            instruction_prompt,
                            file_path           = NULL,
                            detected_type       = NULL,
                            locally_parse_file  = TRUE,
                            chat_obj            = NULL,   # NEW: for context reuse
                            internal_version    = TRUE,
                            debug = TRUE) {
  rlang::check_installed("ellmer", reason = "to use `run_ellmer_chat()`.")
  
  apollo_url <- ifelse(internal_version, Sys.getenv("APOLLO_BASE_URL"), Sys.getenv("APOLLO_BASE_URL_EXT"))
  
  # Reuse existing chat object if provided, otherwise create a new one
  chat_obj <- if(!is.null(chat_obj)) {
    chat_obj
  } else {
    switch(EXPR = service,
           "Gemini"            = ellmer::chat_google_gemini(model = model_name, system = system_prompt, params = optimal_params),
           "OpenAI"            = ellmer::chat_openai(model = model_name, system = system_prompt, params = optimal_params),
           "Claude"            = if(!locally_parse_file) {
             ellmer::chat_claude(model = model_name, system = system_prompt, params = optimal_params,
                                 beta_headers = "files-api-2025-04-14")
           } else {
             ellmer::chat_claude(model = model_name, system = system_prompt, params = optimal_params)
           },
           "OpenRouter"        = ellmer::chat_openrouter(model = model_name, system = system_prompt, params = optimal_params),
           "OpenAI-Compatible" = ellmer::chat_openai_compatible(base_url = api_chat,
                                                                 # without this, ellmer would use OPENAI_API_KEY
                                                                 credentials = function() Sys.getenv("OPENAI_COMPATIBLE_API_KEY"),
                                                                 model = model_name, system = system_prompt, params = optimal_params),
           "DeepSeek"          = ellmer::chat_deepseek(model = model_name, system = system_prompt, params = optimal_params),
           "Apollo"            = if(internal_version) {
             ellmer::chat_openai_compatible(base_url = paste0(apollo_url, "/llm-api"),
                                            credentials = get_apollo_token, # zero-argument function
                                            model = model_name, system = system_prompt, params = optimal_params)
           } else {
             ellmer::chat_openai_compatible(base_url = paste0(apollo_url, "/llm-api"),
                                            credentials = get_apollo_token_ext, # zero-argument function
                                            model = model_name, system = system_prompt, params = optimal_params)             
           },
           "Test Provider"      = if(internal_version) {
             ellmer::chat_openai_compatible(base_url = paste0(apollo_url, "/llm-api"),
                                            credentials = get_apollo_token, # zero-argument function
                                            model = model_name, system = system_prompt, params = optimal_params)
           } else {
             ellmer::chat_openai_compatible(base_url = paste0(apollo_url, "/llm-api"),
                                            credentials = get_apollo_token_ext, # zero-argument function
                                            model = model_name, system = system_prompt, params = optimal_params)             
           },
           "Azure OpenAI"      = ellmer::chat_azure_openai(model = model_name, system = system_prompt, params = optimal_params),
           "AWS Bedrock"       = ellmer::chat_aws_bedrock(model = model_name, system = system_prompt, params = optimal_params)
    )
  }
  
  final_answer <- if(locally_parse_file) {
    if(debug) message(paste0("run_ellmer_chat: ", service, " text-only"))
    chat_obj$chat(instruction_prompt, echo = FALSE)
    
  } else {
    if(debug) message(paste0("run_ellmer_chat: ", service, " with file upload"))
    
    upload_file <- switch(EXPR = service,
                          "Gemini"            = ellmer::content_pdf_file(file_path),
                          #"Gemini"            = ellmer::google_upload(path = file_path, mime_type = detected_type), # seems not work as nice as content_pdf_file
                          "Claude"            = ellmer::claude_file_upload(path = file_path, beta_headers = "files-api-2025-04-14"),
                          "OpenAI"            = ellmer::content_pdf_file(file_path),
                          "OpenRouter"        = ellmer::content_pdf_file(file_path),
                          "OpenAI-Compatible" = ellmer::content_pdf_file(file_path),
                          "DeepSeek"          = ellmer::content_pdf_file(file_path),  # DeepSeek does not support file upload, the logic to guard against this happens in translate_model_code
                          "Apollo"            = ellmer::content_pdf_file(file_path),  # Does not support file upload
                          "Test Provider"     = ellmer::content_pdf_file(file_path),  # Does not support file upload
                          "Azure OpenAI"      = ellmer::content_pdf_file(file_path),
                          "AWS Bedrock"       = ellmer::content_pdf_file(file_path),
    )
    
    if(debug && service %in% c("Claude")) {
      message(paste0("run_ellmer_chat: upload_file - ", upload_file@uri, ", type: ", upload_file@mime_type))
    }
    chat_obj$chat(instruction_prompt, upload_file, echo = FALSE)
  }
  
  if(debug) { message("run_ellmer_chat: chat_obj:"); message(paste(utils::capture.output(print(chat_obj)), collapse = "\n")) }
  
  if(debug) .mvp_state$ellmer_chat_response <- final_answer
  
  usage_df <- chat_obj$get_tokens()
  
  list(
    answer     = final_answer,
    chat_obj   = chat_obj,
    usage_info = list(
      input  = sum(usage_df$input),
      output = sum(usage_df$output),
      total  = sum(usage_df$input) + sum(usage_df$output),
      model  = chat_obj$get_model()
    )
  )
}

#' @name prepare_uploaded_files
#' 
#' @title Validate and Prepare Uploaded Files for LLM Processing
#'
#' @description
#' Validates one or more files from a Shiny \code{\link[shiny]{fileInput}}
#' widget against allowed single- and multi-file extension lists, then either
#' returns the single file path directly or combines multiple files into one
#' via \code{\link{combine_uploaded_files}}. On validation failure, a Shiny
#' error notification is shown and \code{NULL} is returned.
#'
#' @param files A data frame produced by a Shiny \code{fileInput} widget,
#'   containing at minimum the columns \code{name} (original filename) and
#'   \code{datapath} (server-side temporary path).
#' @param llm_service LLM provider name. More extensions are accepted by Dify-workflows.
#' @param model_lang Character. "mrgsolve", "nonmem", "rxode2"
#' @param use_nonmem2mrgsolve Logical. Whether to use nonmem2mrgsolve when translating to mrgsolve, if package is present
#' @param use_nonmem2rx Logical. Whether to use nonmem2rx when translating to rxode2, if package is present
#' @param single_file_types Character vector of permitted file extensions when
#'   exactly one file is uploaded, e.g. \code{c(".pdf", ".docx", ".csv")}.
#'   Extensions should include the leading dot.
#' @param multi_file_types Character vector of permitted file extensions when
#'   more than one file is uploaded, e.g. \code{c(".pdf", ".txt", ".mod",
#'   ".ctl")}. Must be a subset of \code{single_file_types}. Extensions should
#'   include the leading dot.
#'
#' @return A length-one character string giving the path to a ready-to-use
#'   file, or \code{NULL} if validation failed (in which case a Shiny error
#'   notification has already been shown to the user). For single-file uploads
#'   this is \code{files$datapath[1]}; for multi-file uploads this is the
#'   temporary combined file produced by \code{\link{combine_uploaded_files}}.
#'   Returns a list if either criteria for nonmem2mrgsolve or nonmem2rx has been met.
#'
#' @details
#' Extensions \code{.ctl} and \code{.mod} are treated as equivalent to
#' \code{.txt} when checking for mixed-type uploads, since all three are
#' plain-text formats commonly used in pharmacometric workflows. This
#' normalisation only affects the mixed-type guard; the original extensions
#' are preserved in any user-facing notification messages.
#'
#' Validation is performed in this order:
#' \enumerate{
#'   \item For a single file: check that its extension is in
#'     \code{single_file_types}.
#'   \item For multiple files: check that every extension is in
#'     \code{multi_file_types}.
#'   \item For multiple files: check that all files share the same normalised
#'     extension (i.e. no mixing of e.g. \code{.pdf} and \code{.txt}).
#'   \item For multiple files: delegate to
#'     \code{\link{combine_uploaded_files}}.
#' }
#'
#' @examples
#' \dontrun{
#' # Inside a Shiny server function:
#' observe({
#'   req(input$pdffile_model_1)
#'
#'   ready_path <- prepare_uploaded_files(
#'     files             = input$pdffile_model_1,
#'     single_file_types = llm_accept_single_types,
#'     multi_file_types  = llm_accept_multi_types
#'   )
#'
#'   req(!is.null(ready_path))
#'   # pass ready_path to translate_model_code() or send_to_llm()
#' })
#' }
#'
#' @seealso
#' \code{\link{combine_uploaded_files}} for the multi-file combining logic,
#' \code{\link[shiny]{fileInput}} for the UI element that produces \code{files},
#' \code{\link[shiny]{showNotification}} for the notification system.
#'
#' @importFrom shiny showNotification req
#' @importFrom tools file_ext
#'
#' @noRd
prepare_uploaded_files <- function(files,
                                   llm_service,
                                   model_lang,
                                   use_nonmem2mrgsolve,
                                   use_nonmem2rx,
                                   single_file_types,
                                   multi_file_types) {
  
  # Determine accepted file types based on llm choice
  accept_types <- if (llm_service %in% dify_services) {
    single_file_types  # Dify supports text too
  } else {
    multi_file_types  # default for other providers (PDF or text-equivalent)
  }
  
  if(model_lang == "mrgsolve" && use_nonmem2mrgsolve && requireNamespace("nonmem2mrgsolve", quietly = TRUE)) {
    accept_types <- c(accept_types, ".ext")
  }
  
  nonmem2rx_types <- nonmem2rx_file_types
  use_rx <- model_lang == "rxode2" && use_nonmem2rx && nonmem2rx_available()
  if (use_rx) {
    accept_types <- unique(c(accept_types, nonmem2rx_types))
  }
  
  # ── Helper: normalise interchangeable plain-text extensions ──────────────────
  normalize_ext <- function(ext) {
    ifelse(ext %in% c(".ctl", ".mod", ".cpp", ".lst"), ".txt", ext)
  }
  
  exts            <- tolower(paste0(".", tools::file_ext(files$name)))
  normalized_exts <- normalize_ext(exts)
  
  # ── nonmem2rx: one control stream (.ctl/.mod) or listing (.lst), optionally
  #    with supporting files (.ext, .phi, .cov, .xml, ...) ──────────────────────
  # nonmem2rx looks for supporting files next to the control stream by base
  # name, so all uploads are copied under their original names into one folder.
  if (use_rx && all(exts %in% nonmem2rx_types) &&
      any(exts %in% c(".ctl", ".mod", ".lst"))) {

    is_ctl  <- exts %in% c(".ctl", ".mod")
    is_lst  <- exts == ".lst"
    primary <- if (sum(is_ctl) == 1) {
      which(is_ctl)
    } else if (sum(is_ctl) == 0 && sum(is_lst) == 1) {
      which(is_lst)
    } else {
      NA_integer_
    }

    if (is.na(primary)) {
      safely_showNotification(
        paste0("For nonmem2rx, please upload one control stream (.ctl/.mod) or one ",
               "listing file (.lst), optionally with its supporting files ",
               "(e.g. .ext, .phi, .cov, .xml) using the same base name."),
        type     = "error",
        duration = NULL
      )
      return(invisible(NULL))
    }

    work_dir <- tempfile("nonmem2rx_")
    dir.create(work_dir)
    file.copy(files$datapath, file.path(work_dir, files$name))

    return(list(
      primary_path = file.path(work_dir, files$name[primary]),
      dir          = work_dir,
      path         = files$datapath[primary],
      ext          = exts[primary],
      name         = files$name[primary]
    ))
  }

  # ── Single file ───────────────────────────────────────────────────────────────
  if (nrow(files) == 1) {
    
    # A supporting file (.ext, .phi, ...) is only useful together with its
    # control stream; on its own it would be sent to the provider as a binary file
    supporting_only <- setdiff(nonmem2rx_types, c(".ctl", ".mod", ".lst", single_file_types, multi_file_types))
    if (exts %in% supporting_only) {
      safely_showNotification(
        paste0("A '", exts, "' file can only be uploaded together with its NONMEM control stream (.ctl/.mod) or listing (.lst) of the same name."),
        type     = "error",
        duration = NULL
      )
      return(invisible(NULL))
    }
    
    if (!exts %in% accept_types) {
      safely_showNotification(
        paste0("Unsupported file type: '", exts, "'. ",
               "Supported types are: ",
               paste(accept_types, collapse = ", ")),
        type     = "error",
        duration = NULL
      )
      return(invisible(NULL))
    }
    
    return(files$datapath[1])
  }
  
  # ── NONMEM pair: one .ctl/.mod + one .ext → list for nonmem2mrgsolve ─────────
  # Must be checked before the general multi-file logic so .ext doesn't get
  # rejected by the multi_file_types guard.
  if (nrow(files) == 2 && model_lang == "mrgsolve" && use_nonmem2mrgsolve && requireNamespace("nonmem2mrgsolve", quietly = TRUE)) {
    
    is_model_ext <- exts %in% c(".ctl", ".mod")
    is_ext_ext   <- exts == ".ext"
    
    if (sum(is_model_ext) == 1 && sum(is_ext_ext) == 1) {
      
      model_path <- files$datapath[is_model_ext]
      ext_path   <- files$datapath[is_ext_ext]
      
      return(list(
        model = model_path,
        ext   = ext_path
      ))
    }
  }
  
  # ── Multiple files ────────────────────────────────────────────────────────────
  
  # Guard: all extensions must be in the multi-file supported list
  unsupported <- exts[!exts %in% multi_file_types]
  if (length(unsupported) > 0) {
    safely_showNotification(
      paste0("When uploading multiple files, only ",
             paste(multi_file_types, collapse = ", "),
             " are supported. Unsupported file(s): ",
             paste(unique(unsupported), collapse = ", ")),
      type     = "error",
      duration = NULL
    )
    return(invisible(NULL))
  }
  
  # Guard: all files must share the same normalised extension
  if (length(unique(normalized_exts)) > 1) {
    safely_showNotification(
      paste0("All files must be the same type when uploading multiple files. ",
             "Found: ", paste(unique(exts), collapse = ", ")),
      type     = "error",
      duration = NULL
    )
    return(invisible(NULL))
  }
  
  # Combine and return
  combine_uploaded_files(files$datapath, files$name)
}

#' @name combine_uploaded_files
#' 
#' @title Combine Multiple Uploaded Files into a Single File
#'
#' @description
#' Merges a list of locally stored files (from a Shiny \code{fileInput} with
#' \code{multiple = TRUE}) into a single temporary file, ready for downstream
#' processing such as sending to an LLM. Supports PDF (merged page-by-page)
#' and plain text types including \code{.txt}, \code{.mod}, \code{.ctl}, and \code{.cpp}
#' (concatenated). On error, a Shiny notification is shown to the user and
#' \code{NULL} is returned rather than stopping the session.
#'
#' @param file_paths Character vector of temporary file paths, drawn from
#'   \code{input$<inputId>$datapath} after a Shiny \code{fileInput} upload.
#' @param file_names Character vector of original file names, drawn from
#'   \code{input$<inputId>$name}. Used to determine file type via extension,
#'   since browser-reported MIME types are unreliable for types such as
#'   \code{.mod} and \code{.ctl}.
#'
#' @return A length-one character string: the path to a temporary file
#'   containing the combined content, or \code{NULL} if an error occurred (in
#'   which case a Shiny notification is shown). The file lives in
#'   \code{tempdir()} and will be cleaned up at the end of the R session.
#'
#' @details
#' File type is determined from the extension of \code{file_names} rather than
#' the browser-reported MIME type, as browsers often report generic
#' \code{"application/octet-stream"} for domain-specific extensions such as
#' \code{.ctl} and \code{.mod}.
#'
#' This function assumes all files share the same extension. If they do not,
#' a Shiny error notification is shown and \code{NULL} is returned. It is
#' expected that this validation is performed by the caller (see the Shiny
#' server example) before calling this function.
#'
#' Combination strategy by extension:
#' \describe{
#'   \item{\code{.pdf}}{Pages are merged in order using
#'     \code{\link[pdftools]{pdf_combine}}. Requires the \pkg{pdftools} package.}
#'   \item{\code{.txt}, \code{.mod}, \code{.ctl}}{Files are read with
#'     \code{\link{readLines}} and concatenated with a newline separator.
#'     Output is written as \code{.txt}.}
#'   \item{All other extensions}{A Shiny error notification is shown and
#'     \code{NULL} is returned.}
#' }
#'
#' @examples
#' \dontrun{
#' # Inside a Shiny server function:
#' observe({
#'   req(input$pdffile_model_1)
#'   files <- input$pdffile_model_1
#'
#'   combined_path <- if (nrow(files) > 1) {
#'     combine_uploaded_files(files$datapath, files$name)
#'   } else {
#'     files$datapath[1]
#'   }
#'
#'   req(!is.null(combined_path))
#'   send_to_llm(combined_path)
#' })
#' }
#'
#' @seealso
#' \code{\link[shiny]{fileInput}} for the UI element that produces the input,
#' \code{\link[shiny]{showNotification}} for the notification system,
#' \code{\link[pdftools]{pdf_combine}} for the underlying PDF merge.
#'
#' @importFrom shiny showNotification
#'
#' @noRd
combine_uploaded_files <- function(file_paths, file_names) {
  
  # ── Validate inputs ──────────────────────────────────────────────────────────
  if (length(file_paths) != length(file_names)) {
    shiny::showNotification(
      "Internal error: file_paths and file_names must be the same length.",
      type     = "error",
      duration = NULL
    )
    return(invisible(NULL))
  }
  
  # ── Resolve extensions from original file names ──────────────────────────────
  exts <- tolower(paste0(".", tools::file_ext(file_names)))
  
  # Normalize interchangeable plain-text extensions before uniqueness check
  normalize_ext <- function(ext) {
    ifelse(ext %in% c(".ctl", ".mod", ".cpp", ".lst"), ".txt", ext)
  }
  normalized_exts <- normalize_ext(exts)
  
  if (length(unique(normalized_exts)) > 1) {
    shiny::showNotification(
      paste0("All uploaded files must share the same extension. ",
             "Found: ", paste(unique(exts), collapse = ", ")),
      type     = "error",
      duration = NULL
    )
    return(invisible(NULL))
  }
  
  ext <- normalized_exts[1]
  
  # ── PDF ──────────────────────────────────────────────────────────────────────
  if (ext == ".pdf") {
    rlang::check_installed("pdftools", reason = "to combine PDF files.")
    output_path <- tempfile(fileext = ".pdf")
    pdftools::pdf_combine(file_paths, output = output_path)
    return(output_path)
  }
  
  # ── Plain text: .txt, .mod, .ctl, .cpp ──────────────────────────────────────
  if (ext %in% c(".txt", ".mod", ".ctl", ".cpp", ".lst")) {
    output_path <- tempfile(fileext = ".txt")
    combined_text <- sapply(file_paths, readLines, warn = FALSE) %>%
      unlist() %>%
      paste(collapse = "\n")
    writeLines(combined_text, output_path)
    return(output_path)
  }
  
  # ── Fallback: unsupported type ───────────────────────────────────────────────
  shiny::showNotification(
    paste0("Unsupported file type for multi-file combining: '", ext, "'. ",
           "Supported types are: .pdf, .txt, .mod, .ctl, .cpp"),
    type     = "error",
    duration = NULL
  )
  return(invisible(NULL))
}

#' @name get_api_key
#' 
#' @title Retrieves the stored API key from a provider in .Renviron File
#'
#' @description
#' Retrieves the API key for a specified LLM service from environment variables.
#' Set environment variables with \code{usethis::edit_r_environ()}.
#'
#' @param llm_service Name of LLM provider
#' 
#' @return A character string containing the API key, or \code{""} if the
#'   environment variable is not set.
#'
#' @noRd
get_api_key <- function(llm_service) {
  switch(EXPR = llm_service, # edit with usethis::edit_r_environ()
         "PROD"              = Sys.getenv("DIFY_API_KEY"),
         "PMx Co-Modeler"    = Sys.getenv("DIFY_API_KEY"),
         "EXP"               = Sys.getenv("DIFY_API_KEY_2"),
         "Fast"              = Sys.getenv("DIFY_API_KEY_2"),
         "Claude"            = Sys.getenv("ANTHROPIC_API_KEY"),
         "Gemini"            = Sys.getenv("GEMINI_API_KEY"),
         "OpenAI"            = Sys.getenv("OPENAI_API_KEY"),
         "OpenRouter"        = Sys.getenv("OPENROUTER_API_KEY"),
         "OpenAI-Compatible" = Sys.getenv("OPENAI_COMPATIBLE_API_KEY"),
         "DeepSeek"          = Sys.getenv("DEEPSEEK_API_KEY"),
         "Apollo"            = Sys.getenv("APOLLO_CLIENT_SECRET"),
         "Test Provider"     = Sys.getenv("APOLLO_CLIENT_SECRET"),
         "Azure OpenAI"      = Sys.getenv("AZURE_OPENAI_API_KEY"),
         "AWS Bedrock"       = "dummy"
  )
}

#' @name get_api_key_name
#' 
#' @title Returns the designated API key name from a provider in .Renviron File
#'
#' @description
#' Retrieves the name of the API key for a specified LLM service from environment variables.
#' Set environment variables with \code{usethis::edit_r_environ()}.
#'
#' @param llm_service Name of LLM provider
#' 
#' @return A character string containing the API key name
#'
#' @noRd
get_api_key_name <- function(llm_service) {
  switch(EXPR = llm_service,
         "PROD"              = "DIFY_API_KEY",
         "PMx Co-Modeler"    = "DIFY_API_KEY",
         "EXP"               = "DIFY_API_KEY_2",
         "Fast"              = "DIFY_API_KEY_2",
         "Claude"            = "ANTHROPIC_API_KEY",
         "Gemini"            = "GEMINI_API_KEY",
         "OpenAI"            = "OPENAI_API_KEY",
         "OpenRouter"        = "OPENROUTER_API_KEY",
         "OpenAI-Compatible" = "OPENAI_COMPATIBLE_API_KEY",
         "DeepSeek"          = "DEEPSEEK_API_KEY",
         "Apollo"            = "APOLLO_CLIENT_SECRET", # Also needs APOLLO_CLIENT_ID, APOLLO_TOKEN_URL, APOLLO_BASE_URL
         "Test Provider"     = "APOLLO_CLIENT_SECRET", # Also needs APOLLO_CLIENT_ID, APOLLO_TOKEN_URL, APOLLO_BASE_URL
         "Azure OpenAI"      = "AZURE_OPENAI_API_KEY",
         "AWS Bedrock"       = "dummy"
  )
}

#' Get Apollo OAuth Token
#'
#' Requests a short-lived OAuth access token from the Axway gateway using
#' client credentials. Requires \code{APOLLO_CLIENT_ID}, \code{APOLLO_CLIENT_SECRET},
#' and \code{APOLLO_TOKEN_URL} to be set in \code{.Renviron}.
#' Use \code{usethis::edit_r_environ()} to set them. Only relevant for internal deployments.
#' @noRd

#'
#' @return A character string containing the Bearer access token.
#'
#' @examples
#' \dontrun{
#' token <- get_apollo_token()
#' }
#' 
#' 
#' @noRd
get_apollo_token <- function() {
  rlang::check_installed("httr2", reason = "to use `get_apollo_token()`.")

  response <- httr2::request(Sys.getenv("APOLLO_TOKEN_URL")) %>%
    httr2::req_body_form(
      client_id     = Sys.getenv("APOLLO_CLIENT_ID"),
      client_secret = Sys.getenv("APOLLO_CLIENT_SECRET"),
      grant_type    = "client_credentials"
    ) %>%
    httr2::req_perform()
  
  httr2::resp_body_json(response)$access_token
}

#' Get Apollo OAuth Token (External)
#'
#' Requests a short-lived OAuth access token from the Axway gateway using
#' client credentials. Requires \code{APOLLO_CLIENT_ID}, \code{APOLLO_CLIENT_SECRET},
#' and \code{APOLLO_TOKEN_URL} to be set in \code{.Renviron}.
#' Use \code{usethis::edit_r_environ()} to set them. Only relevant for internal deployments.
#' @noRd

#'
#' @return A character string containing the Bearer access token.
#'
#' @examples
#' \dontrun{
#' token <- get_apollo_token()
#' }
#' 
#' 
#' @noRd
get_apollo_token_ext <- function() {
  rlang::check_installed("httr2", reason = "to use `get_apollo_token_ext()`.")
  
  response <- httr2::request(Sys.getenv("APOLLO_TOKEN_URL_EXT")) %>%
    httr2::req_body_form(
      client_id     = Sys.getenv("APOLLO_CLIENT_ID"),
      client_secret = Sys.getenv("APOLLO_CLIENT_SECRET"),
      grant_type    = "client_credentials"
    ) %>%
    httr2::req_perform()
  
  httr2::resp_body_json(response)$access_token
}

#-------------------------------------------------------------------------------
#' @name nonmem2rx_available
#'
#' @title Is the optional nonmem2rx package installed?
#'
#' @returns `TRUE` or `FALSE`.
#' @noRd
#-------------------------------------------------------------------------------
nonmem2rx_available <- function() {
  requireNamespace("nonmem2rx", quietly = TRUE)
}

#-------------------------------------------------------------------------------
#' @name llm_error_message
#'
#' @title Builds the error message shown when an LLM request fails
#'
#' @description
#' Uses only the condition itself. `httr2::last_response()` is shared by the
#' whole R process, so on a multi-user server it could hold another session's
#' response (and it is stale for errors that are not HTTP errors).
#'
#' @param e a condition
#'
#' @returns a single character string
#' @noRd
#-------------------------------------------------------------------------------

llm_error_message <- function(e) {
  msg <- conditionMessage(e)
  if (inherits(e, "httr2_http") && !is.null(e$resp)) {
    body <- tryCatch(httr2::resp_body_json(e$resp), error = function(i) NULL)
    detail <- if (is.list(body)) {
      if (is.list(body$error)) body$error$message else body$message
    }
    if (is.null(detail)) {
      detail <- tryCatch(httr2::resp_body_string(e$resp), error = function(i) NULL)
    }
    if (is.character(detail) && length(detail) == 1 && nzchar(detail) &&
        !grepl(detail, msg, fixed = TRUE)) {
      msg <- paste(msg, detail)
    }
  }
  msg
}
