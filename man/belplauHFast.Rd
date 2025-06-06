% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{belplauHFast}
\alias{belplauHFast}
\title{Calculate belief, disbelief, unknown, plausibility, plausibility ratio}
\arguments{
\item{MACC}{Vector of masses e.g. x$m}

\item{W2}{Description matrix e.g. x$tt}

\item{h}{Hypotheses to be tested, same format as x$tt}

\item{display_progress}{Whether to show progress bar}
}
\value{
A matrix of \code{M} rows by 5 columns is returned, where \code{M} is the number of hypothesis tested: \itemize{
 \item Column 1: the degree of Belief \code{bel};
 \item Column 2: the degree of Disbellief (belief in favor of the contrary hypothesis) \code{disbel};
 \item Column 3: the degree of Epistemic uncertainty \code{unc};
 \item Column 4: the degree of Plausibility \code{plau};
 \item Column 5: the Plausibility ratio \code{rplau}.
   }
}
\description{
Calculate belief, disbelief, unknown, plausibility, plausibility ratio
}
\examples{
1
}
