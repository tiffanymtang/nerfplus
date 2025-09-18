
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Causal Distillation Trees

[Causal Distillation Trees](https://arxiv.org/abs/2502.07275) (CDT) is a
novel machine learning method for estimating interpretable subgroups in
causal inference. CDT allows researchers to fit *any* machine learning
model of their choice to estimate the individual-level treatment effect,
and then leverages a simple, second-stage tree-based model to “distill”
the estimated treatment effect into meaningful subgroups. As a result,
CDT inherits the improvements in predictive performance from black-box
machine learning models while preserving the interpretability of a
simple decision tree.

![](../causalDT/man/figures/cdt_diagram.png)

Briefly, CDT is a two-stage learner that first fits a teacher model
(e.g., a black-box metalearner) to estimate individual-level treatment
effects and secondly fits a student model (e.g., a decision tree) to
predict the estimated individual-level treatment effects, in effect
distilling the estimated individual-level treatment effects and
producing interpretable subgroups. This two-stage learner is learned
using the training data. Finally, using the estimated subgroups, the
subgroup average treatment effects are honestly estimated with a
held-out estimation set.

For more details, check out [Huang, M., Tang, T. M., Kenney, A. M.
“Distilling heterogeneous treatment effects: Stable subgroup estimation
in causal inference.” (2025).](https://arxiv.org/abs/2502.07275)

## Project Structure

This directory contains all of the code necessary to reproduce the
analysis and figures in [Huang et
al. (2025)](https://arxiv.org/abs/2502.07275).

Moreover, to facilitate reproducibility of this work, we leveraged
[`renv`](https://rstudio.github.io/renv/articles/renv.html) to create a
reproducible environment and
[`simChef`](https://yu-group.github.io/simChef/) to create a
reproducible and transparent simulation workflow. Thus, to install the
necessary dependencies and reproduce the simulation, the following steps
are recommended:

1.  Clone this repository.
2.  Open the `causalDT-manuscript.Rproj` in RStudio.
3.  Run the following code in the R console:

``` r
# install.packages("renv")
renv::restore()
```

4.  Run each of the simulation driver scripts in the `meals/` directory.
5.  Render all of the results into a single summary `simChef`
    documentation by running the `meals/00_render_docs.R` script.

These steps will generate all of the figures and results from the
mansucript. The rendered `simChef` documentation can be found [here](https://tiffanymtang.github.io/causalDT/simulation_results.html).

## Citation

``` r
@article{huang2025distilling,
  title={Distilling heterogeneous treatment effects: Stable subgroup estimation in causal inference}, 
  author={Melody Huang and Tiffany M. Tang and Ana M. Kenney},
  year={2025},
  eprint={2502.07275},
  archivePrefix={arXiv},
  primaryClass={stat.ME},
  url={https://arxiv.org/abs/2502.07275}, 
}
```
