# Function to plot variability exposure data

Function to plot variability exposure data

## Usage

``` r
plot_iiv_exp_data(
  input_dataset,
  yvar = "yvar",
  ylab = yvar,
  model_1_name = "",
  model_2_name = "",
  model_1_color = "#F8766D",
  model_2_color = "#7570B3",
  show_stats = TRUE,
  xlab = "",
  title = ""
)
```

## Arguments

- input_dataset:

  Input dataset of Model 1 and/or Model 2 that contains MODEL ID CMIN
  CAVG CMAX AUC

- yvar:

  Name of Y-variable (exposure metric) to be plotted, string

- ylab:

  Optional name for yvar

- model_1_name:

  Optional name for Model 1

- model_2_name:

  Optional name for Model 2

- model_1_color:

  Color for Model 1

- model_2_color:

  Color for Model 2

- show_stats:

  Display texts of stats for each box plot

- xlab:

  Optional name for xvar

- title:

  Title for plot

## Value

a ggplot object
