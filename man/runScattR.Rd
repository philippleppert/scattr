\name{runScattR}
\alias{runScattR}
\title{Start the Shiny Application ScattR}

\usage{
runScattR()

## Mozilla Firefox is recommended.
}

\description{
This function starts the shiny app ScattR in a new window or tab of the default internet browser. Hierarachical structures of the data may be defined in advance with \link[scattr]{define_hierarchy}. Custom variable assignment for the application's selection fields may be defined with \link[scattr]{input_xy} (X/Y-Axis), \link[scattr]{input_mouseclick} (Mouse click) or \link[scattr]{input_color_group} (Color, Group vertical/horizontal).
}

\examples{
runScattR()
}
