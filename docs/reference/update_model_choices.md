# Shorthand function to update list of models and default values by input password

Shorthand function to update list of models and default values by input
password

## Usage

``` r
update_model_choices(input_password, session, model_list = model_examples_list)
```

## Arguments

- input_password:

  From the UI (input\$password)

- session:

  Shiny session (don't change this)

- model_list:

  List of models (don't change this)

## Value

a logical TRUE/FALSE if password is valid

*Note* this function will be re-defined if passworded models is sourced
externally, i.e. when provided as an argument for run_mvp(pw_models_path
= "path/to/your/private/models.R")

## Note

input\$model_select & input\$model_select2 are updated as a side effect
