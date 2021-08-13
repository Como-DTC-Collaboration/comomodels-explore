# comoexplore
This is a shiny app accompanied with the R package [`comomodels`](https://github.com/Como-DTC-Collaboration/como-models/) to interactively explore the model behaviors of in response to different transmission parameters.

Currently we support three sub-models: 

* **SEIRD** - basic Susceptible-Exposed-Infected-Recovered-Dead model;
* **SEIaImIsRD** - a symptom-compartment SEIRD model that divide infectious population (I) into asymptomatic (Ia), mild (Im) and severe (Is) groups according to the severity of symptoms;
* **SEIRDAge** - an age-structured SEIRD model that separate the population into different age ranges.

## Development
To contribute to this repository, read [DEVELOPMENT.md](https://github.com/Como-DTC-Collaboration/comomodels-explore/blob/main/DEVELOPMENT.md)

## Installations
### Dependencies
To check which R packages are required by the package, please refer to [DESCRIPTION](https://github.com/Como-DTC-Collaboration/comomodels-explore/blob/main/DESCRIPTION) or the conda environment list [comomodels-explore.yml](https://github.com/Como-DTC-Collaboration/comomodels-explore/blob/main/envs/comomodels-explore.yml)

#### Install using `conda`

If you have a number of dependencies not installed yet, instead of manually installing each of them, you can save time and energy by installing all dependencies in a `conda` environment:

1. [Install conda](https://docs.conda.io/projects/conda/en/latest/user-guide/install/index.html)

2. Create a package-specific conda environment:
```
# clone the repository with git, go to the cloned directory:
git clone git@github.com:Como-DTC-Collaboration/comomodels-explore.git
cd comomodels-explore

# create a new conda environment (here with name "comoexplore") with all dependencies listed the provided .yml file:
conda env create -f envs/comomodels-explore.yml

# All dependenceis required by this package are now installed in comoexplore. Now activate the environment:
conda activate comoexplore
```

## How to run the app (linux)
The app is **NOT deployed** yet, so you have to run it locally:

* Method 1: the easiest way is to directly download and run the remote version of the app in one command:

```
# start an R environment, then:
# 1. use runGitHub
shiny::runGitHub("comomodels-explore", "Como-DTC-Collaboration", ref = "main")

# 2. alternatively, run a tar or zip file
shiny::runUrl("https://github.com/Como-DTC-Collaboration/comomodels-explore/archive/main.tar.gz")
shiny::runUrl("https://github.com/Como-DTC-Collaboration/comomodels-explore/archive/main.zip")
```

* Method 2: you can first clone the git repository, then run the app:

```
# (in terminal)
# clone the repository with git, go to the cloned directory:
git clone git@github.com:Como-DTC-Collaboration/comomodels-explore.git
cd comomodels-explore/

# in terminal, start an R environment
Rscript comoexplore.Rproj
    
# in the R environment, make sure your working directory is comomodels-explore, then load all dependencies and run the app:
setwd(".")
devtools::load_all()

## if you are using an interactive R session (e.g. RStudio):
shiny::runApp(appDir = "R")
## otherwise, set `launch.browser=FALSE` if you are using a non-interactive R session and manually launch the browser in the next step:
shiny::runApp(appDir = "R", launch.browser=FALSE)
```

Either way, a local URL will appear on your R console, e.g.:
```
Listening on http://127.0.0.1:5554
```

`R/app.R` will be automatically run and pop out a web interface. If not, you can manually copy-and-paste the above URL to any web server. 

You can now play around with the parameters on the web interface.

