% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/psi.R
\name{psi_table}
\alias{psi_table}
\title{Population Stability Index Table}
\usage{
psi_table(df, df_column = "DATA", count_observed, count_estimated, simplfy = T)
}
\arguments{
\item{df}{Frequency tables for variables (nested tibble)}

\item{df_column}{Nested tibble column name (Default is DATA)}

\item{count_observed}{The column name of count observation variable. (character)}

\item{count_estimated}{The column name of count estimate variable. (character)}

\item{simplfy}{if is TRUE, return only PSI value, otherwise detail.}
}
\description{
Population Stability Index table
}
