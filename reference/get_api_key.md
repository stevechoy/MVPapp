# Retrieves the stored API key from a provider in .Renviron File

Retrieves the API key for a specified LLM service from environment
variables. Set environment variables with `usethis::edit_r_environ()`.

## Usage

``` r
get_api_key(llm_service)
```

## Arguments

- llm_service:

  Name of LLM provider

## Value

A character string containing the API key, or `""` if the environment
variable is not set.
