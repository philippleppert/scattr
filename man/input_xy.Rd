\name{input_xy}

\alias{input_xy}

\title{Define Variables to be treated as numeric}

\usage{
input_xy()

## Default S3 method:
input_xy(variable = NULL)
}

\arguments{
\item{variable}{A character vector. List variables to be treated as "numeric".}


}
\description{
All encoded variables appear in the "X-axis Variable" and "Y-axis Variable" selection field of the shiny app ScattR.

Reset the encoding with input_xy().
}
\examples{
input_xy(variable = c("Berichtsjahr","ID"))
}
