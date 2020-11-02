\name{input_color_group}

\alias{input_color_group}

\title{Define Variables to be treated as factor}

\usage{
input_color_group()

## Default S3 method:
input_color_group(variable = NULL)
}

\arguments{
\item{variable}{A character vector. List variables to be treated as "factor".}


}
\description{
All encoded variables appear in the "Color by", "Group vertical by" and "Group horizontal by" selection  fields of the shiny app ScattR.

Reset the encoding with input_color_group().
}
\examples{
input_color_group(variable = c("NACE_division","NUTS_2"))
}
