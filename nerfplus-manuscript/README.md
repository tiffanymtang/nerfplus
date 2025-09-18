
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Network-assisted Random Forest+ (NeRF+)

[Network-assisted Random Forest+]() (NeRF+) is a flexible and
interpretable machine learning model for incorporating network data
alongside node-level covariate information. Briefly, NeRF+ extends a
generalization of random forests (RF) called RF+ [(Agarwal et
al. 2025)](https://arxiv.org/pdf/2307.01932) to the network-assisted
regression setting by incorporating both a network cohesion penalty and
network embeddings as additional covariates. Using this approach, NeRF+
inherits both the flexibility and interpretability of RFs while allowing
researchers to easily incorporate network information in their model to
further improve predictive performance.

For more details, check out [Tang, T. M., Levina, E., Zhu, J.
“Interpretable Network-assisted Random Forest+.” (2025).]()

## Project Structure

This directory contains all of the code necessary to reproduce the
analysis and figures in [Tang et al. (2025)]().

Moreover, to facilitate reproducibility of this work, we leveraged
[`renv`](https://rstudio.github.io/renv/articles/renv.html) to create a
reproducible environment and
[`simChef`](https://yu-group.github.io/simChef/) to create a
reproducible and transparent simulation workflow. Thus, to install the
necessary dependencies and reproduce the simulation, the following steps
are recommended:

1.  Clone this repository.
2.  Open the `nerfplus-manuscript.Rproj` in RStudio.
3.  Run the following code in the R console:

``` r
# install.packages("renv")
renv::restore()
```

4.  See `job_scripts/driver.sh` to run each of the simulation driver
    scripts, located in the `meals/` directory.
5.  Render all of the figures/results by running the `scripts/figures.R`
    script.

## Citation

``` r
```
