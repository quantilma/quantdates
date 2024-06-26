% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CountingDates.R
\name{day_count}
\alias{day_count}
\title{day_count}
\source{
International Swaps and Derivatives Association - ISDA.
}
\usage{
day_count(tfinal, tinitial, convention = "ACT/365")
}
\arguments{
\item{tfinal}{Final date.}

\item{tinitial}{Initial date.}

\item{convention}{Character that specifies the convention. See details.}
}
\value{
Number of years between the specified dates according to the convention.
}
\description{
Function to count the number of years between two dates according to
the given convention.
}
\details{
The convention accepts the following values:
\itemize{
    \item 30/360.
    \deqn{DayCount = \frac{360\times(Y_2-Y_1)+30\times (M_2-M_1) + (D_2-D_1)}{360}}{DayCount = (360*(Y_2-Y_1)+30*(M_2-M_1) + (D_2-D_1))/360}
    Here the dates are in the following format
    \itemize{
    \item tfinal = \eqn{Y_2}-\eqn{M_2}-\eqn{D_2} (YYYY-MM-DD).
    \item tinitial = \eqn{Y_1}-\eqn{M_1}-\eqn{D_1} (YYYY-MM-DD).
    }
    It is important to note that
    \itemize{
    \item \eqn{D_1=\min(D_1,30)}
    \item If \eqn{D_1=30} then \eqn{D_2=\min(D_2,30)}
    }

    \item ACT/365 (Default).
    \deqn{DayCount = \frac{Days(tintial, tfinal)}{365}}

    Also known as ACT/365 Fixed.

    \item ACT/360.
    \deqn{DayCount = \frac{Days(tintial, tfinal)}{360}}

    \item ACT/365L.
    \deqn{DayCount = \frac{Days(tintial, tfinal)}{DiY}}

    If February 29 is in the range from Date1 (exclusive) to Date2 (inclusive),
   then DiY = 366, else DiY = 365.

    \item NL/365.

    If February 29 is not in the period then actual number of days between
    dates is used. Else actual number of days minus 1 is used. Day count basis = 365.

    \item ACT/ACT-ISDA.
    \deqn{DayCount = \frac{Days\; not\; in\; leap\; year}{365} + \frac{Days\; in\; leap\; year}{366}}

    \item ACT/ACT-AFB.
    \deqn{DayCount = \frac{Days(tintial, tfinal)}{DiY}}

    The basic rule is that if February 29 is in the range from Date1 (inclusive)
     to Date2 (exclusive), then DiY = 366, else DiY = 365.

     If the period from Date1 to Date2 is more than one year, the calculation
     is split into two parts:
     \itemize{
     \item The number of complete years, counted back from the last day of the period.
     \item The remaining initial stub, calculated using the basic rule.
     }


}
}
\examples{
#Function accepts Dates as Dates or as characters.
day_count(tfinal='2023-03-08',tinitial='2019-02-28',convention='ACT/365')
day_count(tfinal=as.Date('2023-03-08'),tinitial=as.Date('2019-02-28'),convention='ACT/360')
day_count(tfinal='2023-03-08',tinitial=as.Date('2019-02-28'),convention='30/360')
day_count(tfinal='2023-03-08',tinitial='2019-02-28',convention='NL/365')
day_count(tfinal='2023-03-08',tinitial='2019-02-28',convention='ACT/ACT-ISDA')
day_count(tfinal='2023-03-08',tinitial='2019-02-28',convention='ACT/ACT-AFB')

}
\references{
International Swaps and Derivatives Association. (2006). 2006 ISDA definitions. New York, N.Y: International Swaps and Derivatives Association.
}
\author{
Julian Chitiva
}
