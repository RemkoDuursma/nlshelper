% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_gam.R
\name{plot_gam}
\alias{plot_gam}
\title{Plot a generalized additive model}
\usage{
plot_gam(x, y, g = NULL, data, fittype = c("gam", "lm"), kgam = 4,
  R = NULL, log = "", axes = TRUE, fitoneline = FALSE,
  points.col = NULL, lines.col = NULL, ci.col = "#D3D3D3B3",
  xlab = NULL, ylab = NULL, band = TRUE, plotit = TRUE, add = FALSE,
  npred = 101, lwd = 2, ...)
}
\arguments{
\item{x}{Variable for X axis (unquoted)}

\item{y}{Variable for Y axis (unquoted)}

\item{g}{Variable for grouping (unquoted); optional}

\item{data}{Dataframe containing x and y}

\item{fittype}{Either 'gam' (default), or 'lm' in which case a simple linear model is fit - useful for comparison.}

\item{kgam}{the \code{k} parameter for smooth terms in gam.}

\item{R}{An optional random effect (quoted)}

\item{log}{Whether to add log axes for x or y (but no transformations are done).}

\item{axes}{Logical (default TRUE), whether to add axes to the plot.}

\item{fitoneline}{Whether to fit only one curve to the entire dataset, regardless of whether a grouping variable was defined. Default FALSE.}

\item{points.col}{Colours of the points, can be a vector (one value for each group, if present).}

\item{lines.col}{Colours of the lines, can be a vector (one value for each group, if present).}

\item{ci.col}{Colour of the confidence band, if plotted. Defaults to a transparent grey colour.}

\item{xlab}{X-axis label}

\item{ylab}{Y-axis label}

\item{band}{Logical. If true, plots the confidence band (as a transparent polygon).}

\item{plotit}{Logical (default TRUE); if FALSE, suppresses the plot.}

\item{add}{Logical (default FALSE), if TRUE, adds to an existing plot.}

\item{npred}{Number of x values to use for prediction}

\item{lwd}{Line thickness (see \code{\link{par}})}

\item{\dots}{Further arguments passed to \code{plot} or \code{points}, for example to change colour of plotting symbols.}
}
\description{
This is a simple wrapper to fit and plot a basic type of generalized additive model. The fitted model is of the form \code{gam(Y ~ s(X, k))}, which can be fitted by a specified grouping variable (using the \code{g} argument). Also supported is an optional random effect, in which case the model fitted is \code{gamm(Y ~ s(X, k=k), random = list(R=~1), data=dfr)}.
}
\details{
In either case, the \code{k} parameter necessary for the GAM fit can be set using the \code{kgam} argument. See\code{\link{choose.k}} for details on this setting (it is important you don't just use the default value here!).
}
\examples{
data(Loblolly)
plot_gam(age, height, data=Loblolly)
plot_gam(age, height, Seed, data=Loblolly, band=FALSE, lines.col="black")
plot_gam(age, height, Seed, data=Loblolly, band=FALSE, lines.col="black", fittype="lm")

data(ChickWeight)
plot_gam(Time, weight, Diet, R="Chick", data=ChickWeight, lines.col=rainbow(4))
}
