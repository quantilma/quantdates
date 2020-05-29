# quantdates
R package that provides useful tools for manipulating dates and counting days in a financial context.

## Features

* Get Business Days for London (UK), New York (US) and Bogotá (CO).
* Get Holidays Days for London (UK), New York (US) and Bogotá (CO).
* Manipulate dates
    + Add days considerign business and holidays for the London, New York or Bogotá calendars.
    + Time difference between two dates considering leap years.
* Day counting in financial context:
    + 30/360
    + ACT/365
    + ACT/360
    + ACT/365L
    + NL/365
    + ACT/ACT-ISDA
    + ACT/ACT-AFB
    
## Installation

To install the latest development builds directly from GitHub, run this instead:

```r
if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("quantilma/quantdates")
```

## Guidelines for contributing

Currently we are developing the guidelines for contibutions. 

## License

The shiny package as a whole is licensed under the GPLv3. See the [LICENSE](LICENSE) file for more details.
    
