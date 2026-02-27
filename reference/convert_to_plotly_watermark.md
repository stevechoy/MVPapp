# Converts a ggplot object to plotly object and then apply watermark, and other options

Converts a ggplot object to plotly object and then apply watermark, and
other options

## Usage

``` r
convert_to_plotly_watermark(
  ggplot_object,
  opacity = 0.05,
  font_name = "Arial",
  format = "png",
  filename = "newplot",
  width = NULL,
  height = NULL,
  debug = FALSE,
  try_tooltip = FALSE,
  plotly_watermark = TRUE
)
```

## Arguments

- ggplot_object:

  a ggplot object

- opacity:

  the alpha or transparency, goes from 0 to 1

- font_name:

  font name as a string

- format:

  one of "png", "svg", "jpeg", "webp"

- filename:

  name of output file when saved

- width:

  width in pixels. Default NULL - uses current resolution

- height:

  height in pixels. Default NULL - uses current resolution

- debug:

  Set to TRUE to show debug messages

- try_tooltip:

  Testing tooltip display option, relevant for tornado plots

- plotly_watermark:

  Set to TRUE to insert "For Internal Use Only"

## Value

a plotly object
