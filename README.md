# quantdates
Functions to manipulate dates and count days for quantitative finance analysis. The 'quantdates' package considers leap, holidays and business days for relevant calendars in a financial context to simplify quantitative finance calculations, consistent with International Swaps and Derivatives Association (ISDA) regulations.

## Features

* Get Business Days for London (UK), New York (US) and Bogota (CO).
* Get Holidays Days for London (UK), New York (US) and Bogota (CO).
* Manipulate dates
    + Add days considering business and holidays for the London, New York or Bogota calendars.
    + Number of days  between two dates considering leap years.
    + Get last day of month.
    + Transform numeric dates from R or Excel into dates. 
* Day counting in financial context:
    + 30/360.
    + ACT/365.
    + ACT/360.
    + ACT/365L.
    + NL/365.
    + ACT/ACT-ISDA.
    + ACT/ACT-AFB.
    
## Installation

To install the latest development builds directly from GitHub, run this instead:

```r
if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("quantilma/quantdates")
```

## Examples
```r

# AddDate allows Date input as character object
AddDate(date = '2019-10-04',addDays=14,addMonths=2,addYears=3)

# Add business days for Bogota (CO)
AddBusinessDays(date = Sys.Date(),numDate = 15,loc = 'BOG')

# Return last day of the month for the date
LastDayOfMonth(date = '2020-02-03')

# Returns business days within given range for the location and Dates as character
BusinessDays(loc='BOG', from='2020-10-10', to='2020-11-10')

# Transform numeric dates from Excel
NumExcel2DateR(as.numeric(Sys.Date()))

# Transform numeric dates from R
NumR2DateR(as.numeric(Sys.Date()))

# Number of days between two dates considering leap days.
difftime_leap_year(tfinal='2023-03-05',tinitial='2019-02-28',leapDatesIn=TRUE)

# Number of days between two dates skipping leap days.
difftime_leap_year(tfinal='2023-03-05',tinitial='2019-02-28',leapDatesIn=FALSE)

# Number of business days between two dates in Bogota (CO). 
difftime_business(tfinal='2023-03-08',tinitial='2019-02-28',wd=wdBOG)

# Financial day count between two dates according to ACT/365 convention
day_count(tfinal='2023-03-08',tinitial='2019-02-28',convention='ACT/365')
```

## Guidelines for contributing

Currently we are developing the guidelines for contributions. 

## License

The quantdates package as a whole is licensed under the GPLv3.
    
