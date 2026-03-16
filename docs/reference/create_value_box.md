# Short-hand for creating NCA metrics valueBoxes

Short-hand for creating NCA metrics valueBoxes

## Usage

``` r
create_value_box(
  input_dataset,
  name_ends_with,
  value_box_subtitle,
  width = infoBox_width,
  color,
  sigdig = 4,
  dp = FALSE
)
```

## Arguments

- input_dataset:

  dataset to extract metrics from

- name_ends_with:

  name of metrics column that ends with x (string)

- value_box_subtitle:

  name of subtitle in value box

- width:

  width of valueBox

- color:

  color of valueBox

- sigdig:

  significant digits to output

- dp:

  if TRUE, uses decimal rounding instead

## Value

a shinydashboard valueBox
