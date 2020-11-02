\name{input_mouseclick}

\alias{input_mouseclick}

\title{Define Variables to be treated as character}

\usage{
input_mouseclick()

## Default S3 method:
input_mouseclick(variable = NULL)
}

\arguments{
\item{variable}{A character vector. List variables to be treated as "character".}


}
\description{
All encoded variables appear in the "Mouse click" selection field of the shiny app ScattR.

Reset the encoding with input_mouseclick().
}
\examples{
input_mouseclick(variable = c("Firm_name","Firm_id"))
}
