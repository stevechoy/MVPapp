# Prepare multiple NONMEM files for nonmem2rx conversion

Copies all uploaded NONMEM files into a single temp directory with their
original filenames preserved, then identifies the primary file for
nonmem2rx based on priority: .xml \> .lst \> .ctl/.mod

## Usage

``` r
prepare_nonmem2rx_files(files)
```

## Arguments

- files:

  Data frame from Shiny fileInput (columns: name, datapath)

## Value

A list with \$primary_path (the main file for nonmem2rx) and \$dir (the
temp directory containing all files), or NULL on failure
