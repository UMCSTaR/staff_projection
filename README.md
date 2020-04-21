# Staff Skill Projection

The COVID staffing projection tool can be used to calculate your hospital’s staffing needs over time, based on your projected patient census. Please visit [covidstaffing.org](https://www.covidstaffing.org/) for more information on our staffing tools.

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for **development** and **testing** purposes. See **contributing** for more information on how you can help us build the COVID staffing tool. See **deployment** for notes on how to deploy the project on a live system.

### Prerequisites

This application is written in [R](https://www.r-project.org/) and uses [Shiny](https://github.com/rstudio/shiny) to build and deploy the web application.  We assume you have basic knowledge of both.

### Settings up the development environment

This should detail how to setup a local environment that enables developers to keep all packages version segregated from their main machine. This will make it easy to setup and avoid conflicts due in dependencies due to version miss-matches between development environments.   

### Running

The following packages should be added to an install script, along with their intended version, and references from there.

Imports
- shiny
- tidyverse
- highcharter
- rhandsontable
- shinyjs
- plotly
- readxl

Depends
- glue
- forcats
- stringr
- dplyr
- purrr
- readr
- tidyr
- tibble
- ggplot2
- stats
- graphics
- grDevices
- utils
- datasets
- methods
- base

#### Using RStudio
To run the application locally, you can install the packages above in RStudio, and use the function `runApp()` to start.

Note: You may need to install **devtools** and **tidyext** from the console manually:
```
install.package("devtools")
devtools::install_github('m-clark/tidyext')
```

#### Using a Shiny server
Instructions on how to run the application on a shiny server.

### Further information

The COVID-19 pandemic poses an unprecedented challenge to health care systems and their staff
In response, our [multi-institutional collaborative](https://www.covidstaffing.org/) is developing tools to support your hospital in:

- 1) Projecting your frontline workforce needs;
- 2) Redeploying your clinical teams; and
- 3) Protecting the health and well-being of your providers.

We are software developers, administrators, engineers, big data analysts, social scientists, and clinicians dedicated to supporting hospitals and front line providers during the COVID-19 crisis.

Our goal is not to duplicate work that others have already begun, but rather to combine our work with that of others. We welcome your feedback and suggestions as we navigate this difficult time together.

If you are also working in this domain and interested in collaborating, please [contact us](mailto:info@covidstaffing.org).

### License

This project is licensed under the MIT license.
