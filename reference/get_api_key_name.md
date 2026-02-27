# Returns the designated API key name from a provider in .Renviron File

Retrieves the name of the API key for a specified LLM service from
environment variables. Set environment variables with
`usethis::edit_r_environ()`.

## Usage

``` r
get_api_key_name(llm_service)
```

## Arguments

- llm_service:

  Name of LLM provider

## Value

A character string containing the API key name
