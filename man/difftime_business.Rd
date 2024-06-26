% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CountingDates.R
\name{difftime_business}
\alias{difftime_business}
\title{difftime_business}
\usage{
difftime_business(tfinal, tinitial, wd = wdBOG)
}
\arguments{
\item{tfinal}{Final date, it must be a business day.}

\item{tinitial}{Initial date, it must be a business day.}

\item{wd}{Vector of dates with business days. The default are the business
days of Bogota. See details}
}
\value{
Number of days between the specified dates.
}
\description{
difftime_business
}
\details{
wd refers to the business days of a specific location:
 \itemize{
     \item wdNYGB for New York Government Bonds Market.
     \item wdNY for New York Stock Exchange Market.
     \item wdLDN for London.
     \item wdBOG for Bogota.
}
}
\examples{
#Function accepts Dates as Dates or as characters.
difftime_business(tfinal='2023-03-08',tinitial='2019-02-28',wd=wdBOG)
difftime_business(tfinal=as.Date('2023-03-08'),tinitial=as.Date('2019-02-28'),wd=wdBOG)
difftime_business(tfinal='2023-03-08',tinitial=as.Date('2019-02-28'),wd=wdLDN)
difftime_business(tfinal='2023-03-08',tinitial='2019-02-28',wd=wdNY)
difftime_business(tfinal='2023-03-08',tinitial='2019-02-28',wd=wdNYGB)

}
\author{
Diego Jara and Juan Pablo Bermudez

Function to count the number of business days between two dates.
}
