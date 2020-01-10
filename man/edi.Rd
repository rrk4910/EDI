\name{edi}
\alias{edi}
\title{Calculation of Effective Drought Index (EDI)
}
\description{
The EDI function computes the Effective Drought Index (EDI) (Byun and Wilhite, 1999) from a time series daily or monthly precipitation data.
}
\usage{
edi(datatype, data)
}

\arguments{
\item{datatype }{type of precipitation data i.e. daily or monthly.}
\item{data}{Vector of time series precipitation data.}
}

\value{


$EDI-Values	  Series of effective drought index (EDI).


}

\details{
The Effective Drought Index (EDI) have been calculated from the effective precipitation (EP), which is summed value with time-dependent reduction function.
}
\references{

Byun H-R, Wilhite DA (1999) Objective quantification of drought severity and duration. Journal of climate, 12:2747–2756. https://doi.org/10.1175/1520-0442(1999)012<2747:OQODSA>2.0.CO;2.

Byun H-R, Lee D-K (2002) Defining three rainy seasons and the hydrological summer monsoon in Korea using available water resources index. Journal of the Meteorological Society of Japan 80:33–44.


}

\author{
Rajeev Ranjan Kumar, K N Singh, D C Mishra and Neeraj Budhlakoti
}

\examples{

library(EDI)
data(EDI_data)
datatype<-"daily"
#datatype<-"monthly"
EDI<-edi(datatype,EDI_data)
EDI
}
