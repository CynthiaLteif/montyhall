% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-problem.R
\name{select_door}
\alias{select_door}
\title{Select a door}
\usage{
select_door()
}
\arguments{
\item{...}{no arguments are used by the function.}
}
\value{
The function returns a number between 1 and 3 representing the selected door.
}
\description{
The select_door() function randomly selects one door from the three doors.
}
\details{
The function uses sample() function from R to randomly select one door from the three doors (1, 2, 3) and returns the selected door as a number between 1 and 3.
}
\examples{
  select_door()

}
