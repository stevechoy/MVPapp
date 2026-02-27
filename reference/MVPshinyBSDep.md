# Internal HTML Dependency for shinyBS

This object represents the internal HTML dependency for the `shinyBS`
package. It is used to include the required JavaScript and CSS files for
`shinyBS` functionality without relying on unexported objects from the
`shinyBS` package.

## Usage

``` r
MVPshinyBSDep
```

## Format

A list with the following elements:

- name:

  Character string specifying the name of the dependency (`"shinyBS"`).

- version:

  Character string specifying the version of the dependency
  (`"0.61.1"`).

- src:

  A list specifying the source location of the dependency. Contains:

  - `href`: Character string specifying the relative path (`"sbs"`).

- meta:

  `NULL`, indicating no metadata is provided.

- script:

  Character string specifying the JavaScript file (`"shinyBS.js"`).

- stylesheet:

  Character string specifying the CSS file (`"shinyBS.css"`).

- head:

  `NULL`, indicating no additional HTML is provided for the `<head>`
  section.

- attachment:

  `NULL`, indicating no attachments are provided.

- package:

  `NULL`, indicating no specific package is associated.

- all_files:

  Logical value (`TRUE`) indicating that all files are included.

## Details

The `shinyBSDep` object is a list that defines the HTML dependency for
`shinyBS`. It includes metadata such as the name, version, source files,
and other attributes. The object is stored internally to avoid direct
use of `shinyBS:::shinyBSDep`, which is discouraged by CRAN guidelines.

## Note

The `shinyBSDep` object is marked as internal and is not exported. It is
intended for use within the package to avoid reliance on unexported
objects from `shinyBS`.
