\name{define_hierarchy}

\alias{define_hierarchy}

\title{Define Hierarchical Structures}

\usage{
define_hierarchy()

## Default S3 method:
define_hierarchy(layer1 = NULL, layer2 = NULL, layer3 = NULL)
}

\arguments{
\item{layer1}{A character value. Represents the first (highest) level of the hierarchy.}
\item{layer2}{A character value. Represents the second level of the hierarchy.}
\item{layer3}{A character value. Represents the third level of the hierarchy.}

}
\description{
Up to three layers can be defined in order to address hierarchical structures of the data. By default it is assumed that no hierarchies are present and the tick box "Break-up. The defined layers are passed to the shiny application ScattR and appear by selecting "Break Up Hierarchy" is without effect. It is recommended to arrange multiple layers in a logical structure such that a lower layer can be deduced from a higher layer.

Reset the hierarchy definition with define_hierarchy().
}
\examples{
define_hierarchy(layer1 = "NACE_section", layer2 = "NACE_divison", layer3 = "NACE_group")
}
