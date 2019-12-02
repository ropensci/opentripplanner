---
title: 'OpenTripPlanner for R'
authors:
- affiliation: 1
  name: Malcolm Morgan
  orcid: 0000-0002-9488-9183
- affiliation: 2
  name: Marcus Young
  orcid: 0000-0003-4627-1116
- affiliation: 1
  name: Robin Lovelace
  orcid: 0000-0001-5679-6536
- affiliation: 1
  name: Layik Hama
  orcid: 0000-0003-1912-4890
date: "27 Nov 2019"
output:
  html_document: default
  pdf_document: default
tags:
- R
- transport
- geospatial
- transit
- OpenTripPlanner
- OpenStreetMap
affiliations:
- index: 1
  name: Institute for Transport Studies, University of Leeds, UK
- index: 2
  name: Transportation Research Group, University of Southampton, UK
bibliography: paper.bib
---

<!--
generate citations (in R)
refs = RefManageR::ReadZotero(group = "418217", .params = list(collection = "JFR868KJ", limit = 100))
RefManageR::WriteBib(refs, "paper.bib")
citr::tidy_bib_file(rmd_file = "paper.md", messy_bibliography = "paper.bib")
-->

# Summary

**opentripplanner** provides functions that enable multi-modal routing in R.
It provides an interface to the OpenTripPlanner (OTP) Java routing service, which allows calculation of routes on large transport networks, locally or via calls to a remote server.
The package contains three groups of functions for: (1) setting up and managing a local instance of OTP; (2) connecting to OTP locally or remotely; and (3) sending requests to the OTP API and importing the results and converting them into appropriate classes for further analysis.

# Motivation

Routing, the process of calculating paths between points in geographic space that follow a transport network, is a fundamental part of transport planning and vital to solving many real-world transport problems.
The outputs of a routing service, which can calculate many routes over a large area, comprises coordinates and other data representing a movement that is in some way 'optimal', based on a range of criteria (that the user should understand and be able to change).
Routing services, such as the one provided by Google Maps, are a well-known and increasingly vital component of personal travel planning for many people [@bast_fast_2010].
Less well-known, but perhaps equally important, is that routing services are also key to understanding aggregate travel patterns and guiding policy and commercial decisions [@giusti_new_2018].
To meet this need for route planning capabilities, a wide range of both proprietary and open source tools have been created.

# Functionality

[OpenTripPlanner](https://www.opentripplanner.org/) (OTP) is written in Java and designed to work with [Open Street Map](https://www.openstreetmap.org) (OSM) data for road-based modes (Car, Bike, Walking) and [General Transit Feed Specification]( https://developers.google.com/transit/gtfs/) (GTFS) data for public transit (Bus, Train, Metro).
OTP is unusual among open source routing tools in its ability to account for a wide range of modes of travel, e.g. bicycle hire, and support for complex multi-stage multi-modal journeys such as park and ride. 
However, OTP’s primary purpose is to support public-facing websites such as TriMet; thus its analytical capabilities are limited.
Conversely, the R language is well suited to statistical and spatial analysis but has no route planning capabilities.

The OpenTripPlanner for R package aims to bridge the gap between OTP and R by supplying simple ways for R to connect to OTP either on a local machine or on a remote server, via OTP’s API.
The package has been designed to ease bulk routing by allowing the input of multiple origins and destinations as two column matrices of longitude-latitude pairs.
The package also supports multi-core operation to take advantage of OTP’s multicore functionality.
Results are returned in the widely used [`sf` data frame](https://CRAN.R-project.org/package=sf) format.
Although performance is dependant on the size of the map being routed over, it typically can achieve more than 10 routes per second per core.

The package has been developed from a set of R functions that formed part of an intermediate-level [OTP tutorial](https://github.com/marcusyoung/otp-tutorial/raw/master/intro-otp.pdf) as part of research at [Centre for Research into Energy Demand Solutions]( https://www.creds.ac.uk/) and the [Institute of Transport Studies](https://environment.leeds.ac.uk/transport).

# Reproducible demonstration

Example data for the Isle of Wight, UK is provided with the package. The example below uses this data to demonstrate the basic functionality of the package. A full explanation is provided in the [package vignettes](https://docs.ropensci.org/opentripplanner/articles/opentripplanner.html)

First, download the demo data and OTP.

``` r
library(opentripplanner)                 # Load Package
path_data <- file.path(tempdir(), "OTP") # Create folder for data
dir.create(path_data)
path_otp <- otp_dl_jar(path_data)        # Download OTP
otp_dl_demo(path_data)                   # Download demo data
```

Second, build the OTP graph, start up OTP server and connect to the server

``` r
log <- otp_build_graph(otp = path_otp,     # Build Graph
                       dir = path_data)
otp_setup(otp = path_otp, dir = path_data) # Start OTP
otpcon <- otp_connect()                    # Connect R to OTP
```

Finally, find routes

``` r
route <- otp_plan(otpcon, 
                 fromPlace = c(-1.17502, 50.64590), 
                 toPlace = c(-1.15339, 50.72266))
```

# References

