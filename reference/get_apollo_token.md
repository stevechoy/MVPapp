# Get Apollo OAuth Token

Requests a short-lived OAuth access token from the Axway gateway using
client credentials. Requires `APOLLO_CLIENT_ID`, `APOLLO_CLIENT_SECRET`,
and `APOLLO_TOKEN_URL` to be set in `.Renviron`. Use
`usethis::edit_r_environ()` to set them. Only relevant for BI.

## Usage

``` r
get_apollo_token()
```

## Value

A character string containing the Bearer access token.

## Examples

``` r
if (FALSE) { # \dontrun{
token <- get_apollo_token()
} # }
```
