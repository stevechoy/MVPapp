# shorthand function to switch model code based on choice of model input, and append model name

shorthand function to switch model code based on choice of model input,
and append model name

## Usage

``` r
model_switch_conditions(input_model_select, mcode_model_choice)
```

## Arguments

- input_model_select:

  From the UI (input\$model_select, input\$model_select2)

- mcode_model_choice:

  What to append at the end of the model code

## Value

a switch for 'model_input' / 'model_input2'

## Note

This function will be re-defined if passworded models is sourced
externally, i.e. when provided as an argument for run_mvp(pw_models_path
= "path/to/your/private/models.R").

An example passworded model called "Test Passworded Model" is included
by default as an example, which can be unlocked by using the password
"test".
