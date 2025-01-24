% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/COCA_app.R
\name{COCA_app}
\alias{COCA_app}
\title{COCA Shiny Application}
\usage{
COCA_app()
}
\value{
This function does not return a value. It launches a Shiny application.
}
\description{
A Shiny app for identifying COCA subtypes based on miRNA expression data.
}
\details{
COCA_app

A Shiny application to identify COCA Subtypes from miRNA Expression.
This function sets up and launches a Shiny app that allows users to input
miRNA feature files, predict COCA subtypes, and view the results.

This Shiny application is designed to identify COCA subtypes from miRNA expression data.
The app checks and installs necessary packages, sets up the UI, and handles user input to predict
COCA subtypes using a pre-trained neural network model. The app includes a contact page for user
feedback and support, and an informative home page with a carousel and updates.

It first checks and installs any necessary packages. Then, it sets up the user interface (UI)
and server logic. The UI consists of multiple pages, including a home page and a contact page.
The server logic handles user input, data validation, model prediction, and feedback form submission.
}
\author{
Zaoqu Liu; Email: liuzaoqu@163.com
}
