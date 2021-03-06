% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/institutions.R
\name{make_reponses_table}
\alias{make_reponses_table}
\title{Make a table with all answers to one questions}
\usage{
make_reponses_table(x)
}
\arguments{
\item{x}{}
}
\value{
a tibble
}
\description{
Make a table with all answers to one questions
}
\examples{
library(xml2)
library(dplyr)
library(purrr)
list_institutions <- read_xml(x = "raw-data/Institutions.xml") \%>\% as_list()
make_reponses_table(x = list_institutions$themes[[1]]$questions[[1]])
map_df(extract2(list_institutions$themes[[1]], "questions"), make_reponses_table) \%>\% glimpse()
}

