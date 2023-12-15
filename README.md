---
editor_options: 
  markdown: 
    wrap: 72
---

# lme4: Mixed-effects models in R.

## Assignment

### Link to original Repository

[lme4 link](https://github.com/lme4/lme4)

### Link to website repository

[Public Repository with
website](https://github.com/jhu-statprogramming-fall-2023/biostat777-project3-part1-alepigna12)

### 5 edits:

-   Base font

-   Heading font

-   Code font

-   Background color

-   Foreground color

### Package Name

lme4: Mixed-effects models in R.

### Authors

-   Douglas Bates: Author.

-   Martin Maechler: Author.

-   Ben Bolker: Author, maintainer.

-   Steven Walker: Author.

-   Rune Haubo Bojesen Christensen: Contributor.

-   Henrik Singmann: Contributor.

-   Bin Dai: Contributor.

-   Fabian Scheipl: Contributor.

-   Gabor Grothendieck: Contributor.

-   Peter Green: Contributor.

-   John Fox: Contributor.

-   Alexander Bauer: Contributor.

-   Pavel N. Krivitsky: Contributor, copyright holder. shared copyright
    on simulate.formula

-   Emi Tanaka: Contributor.

### Website Creator

Alessio Pignatelli (Did not write the package, authors are listed above)

### Package Goal

Implementation of generalized linear mixed models (GLMMs) and nonlinear
mixed models (NLMMs)

### Alphabetical List of functions

-   allFit() - Refit a fitted model with all available optimizers
-   Arabidopsis - Arabidopsis clipping/fertilization data
-   bootMer() - Model-based (Semi-)Parametric Bootstrap for Mixed Models
-   cake - Breakage Angle of Chocolate Cakes
-   cbpp - Contagious bovine pleuropneumonia
-   checkConv() - Extended Convergence Checking
-   confint(<merMod>) confint(<thpr>) - Compute Confidence Intervals for
    Parameters of a [ng]lmer Fit
-   convergence Assessing Convergence for Fitted Models
-   devcomp() - Extract the deviance component list
-   devfun2() - Deviance Function in Terms of Standard
    Deviations/Correlations
-   drop1(<merMod>) - Drop all possible single fixed-effect terms from a
    mixed effect model
-   dummy() - Dummy variables (experimental)
-   Dyestuff Dyestuff2 - Yield of dyestuff by batch
-   expandDoubleVerts() - Expand terms with '\|\|' notation into
    separate '\|' terms
-   factorize() - Attempt to convert grouping variables to factors
-   findbars() - Determine random-effects expressions from a formula
-   fixef(<merMod>) - Extract fixed-effects estimates
-   fortify.merMod() getData(<merMod>) - add information to data based
    on a fitted model
-   getME() - Extract or Get Generalized Components from a Fitted Mixed
    Effects Model
-   GHrule() - Univariate Gauss-Hermite quadrature rule
-   glmer.nb() - Fitting Negative Binomial GLMMs
-   glmer() - Fitting Generalized Linear Mixed-Effects Models
-   glmerLaplaceHandle() - Handle for glmerLaplace
-   glmFamily-class - Class "glmFamily" - a reference class for family
-   glmFamily() - Generator object for the glmFamily class
-   golden() - Class "golden" and Generator for Golden Search Optimizer
    Class
-   GQdk() - GQN Sparse Gaussian / Gauss-Hermite Quadrature grid
-   grouseticks - Data on red grouse ticks from Elston et al. 2001
-   hatvalues(<merMod>) Diagonal elements of the hat matrix
-   influence(<merMod>) cooks.distance(\<influence.merMod\>)
    dfbeta(\<influence.merMod\>) dfbetas(\<influence.merMod\>) -
    Influence Diagnostics for Mixed-Effects Models
-   InstEval - University Lecture/Instructor Evaluations by Students at
    ETH
-   isNested() - Is f1 nested within f2?
-   isREML() isLMM() isNLMM() isGLMM() - Check characteristics of models
-   isSingular() - Test Fitted Model for (Near) Singularity
-   lme4 lme4-package - Linear, generalized linear, and nonlinear mixed
    models
-   lme4_testlevel() - Detect testing level for lme4 examples and tests
-   lmer() - Fit Linear Mixed-Effects Models
-   lmerControl() glmerControl() nlmerControl() .makeCC() - Control of
    Mixed Model Fitting
-   lmList() - Fit List of lm or glm Objects with a Common Model
-   lmList4-class show,lmList4-method - Class "lmList4" of 'lm' Objects
    on Common Model
-   glmResp-class lmerResp-class lmResp-class nlsResp-class - Reference
    Classes for Response Modules, "(lm\|glm\|nls\|lmer)Resp"
-   lmResp() - Generator objects for the response classes
-   anova(<merMod>) as.function(<merMod>) coef(<merMod>)
    deviance(<merMod>) REMLcrit() extractAIC(<merMod>) family(<merMod>)
    formula(<merMod>) fitted(<merMod>) logLik(<merMod>) nobs(<merMod>)
    ngrps(<merMod>) terms(<merMod>) vcov(<merMod>) model.frame(<merMod>)
    model.matrix(<merMod>) print(<merMod>) summary(<merMod>)
    print(\<summary.merMod\>) update(<merMod>) weights(<merMod>) - Class
    "merMod" of Fitted Mixed-Effect Models
-   merPredD-class - Class "merPredD" - a Dense Predictor Reference
    Class
-   merPredD() - Generator object for the merPredD class
-   mkMerMod() - Create a 'merMod' Object
-   mkRespMod() - Create an lmerResp, glmResp or nlsResp instance
-   mkReTrms() mkNewReTrms() - Make Random Effect Terms: Create Z,
    Lambda, Lind, etc.
-   mkParsTemplate() mkDataTemplate() - Make templates suitable for
    guiding mixed model simulations
-   mkVarCorr() - Make Variance and Correlation Matrices from theta
-   lFormula() mkLmerDevfun() optimizeLmer() glFormula() mkGlmerDevfun()
    optimizeGlmer() updateGlmerDevfun() - Modular Functions for Mixed
    Model Fits
-   namedList() - Self-naming list function
-   NelderMead() - Class "NelderMead" of Nelder-Mead optimizers and its
    Generator
-   Nelder_Mead() - Nelder-Mead Optimization of Parameters, Possibly
    (Box) Constrained
-   ngrps() - Number of Levels of a Factor or a "merMod" Model
-   nlformula() - Manipulate a Nonlinear Model Formula
-   nlmer() - Fitting Nonlinear Mixed-Effects Models
-   nloptwrap() nlminbwrap() - Wrappers for additional optimizers
-   nobars() - Omit terms separated by vertical bars in a formula
-   Pastes - Paste strength by batch and cask
-   Penicillin - Variation in penicillin testing
-   plot(<merMod>) qqmath(<merMod>) - Diagnostic Plots for 'merMod' Fits
-   xyplot(<thpr>) densityplot(<thpr>) splom(<thpr>) - Mixed-Effects
    Profile Plots (Regular / Density / Pairs)
-   predict(<merMod>) - Predictions from a model at new data values
-   profile(<merMod>) as.data.frame(<thpr>) log(<thpr>) logProf()
    varianceProf() - Profile method for merMod objects
-   mcmcsamp pvalues - Getting p-values for fitted models
-   ranef(<merMod>) dotplot(\<ranef.mer\>) qqmath(\<ranef.mer\>)
    as.data.frame(\<ranef.mer\>) - Extract the modes of the random
    effects
-   refit() - Refit a (merMod) Model with a Different Response
-   refitML() - Refit a Model by Maximum Likelihood Criterion
-   rePCA() - PCA of random-effects covariance matrix
-   rePos-class - Class "rePos"
-   rePos() - Generator object for the rePos (random-effects positions)
    class
-   residuals(<merMod>) residuals(<lmResp>) residuals(<glmResp>) -
    residuals of merMod objects
-   sigma(<merMod>) - Extract Residual Standard Deviation 'Sigma'
-   simulate(<formula>) - A simulate Method for formula objects that
    dispatches based on the Left-Hand Side
-   simulate(<merMod>) .simulateFun() - Simulate Responses From merMod
    Object
-   sleepstudy - Reaction times in a sleep deprivation study
-   subbars() "Sub[stitute] Bars"
-   troubleshooting - Troubleshooting
-   llikAIC() methTitle() .prt.methTit() .prt.family() .prt.resids()
    .prt.call() .prt.aictab() .prt.grps() .prt.warn() .prt.VC()
    formatVC() - Print and Summary Method Utilities for Mixed Effects
-   VarCorr(<merMod>) as.data.frame(\<VarCorr.merMod\>)
    print(\<VarCorr.merMod\>) - Extract Variance and Correlation
    Components
-   mlist2vec() vec2mlist() vec2STlist() sdcor2cov() cov2sdcor()
    Vv_to_Cv() Sv_to_Cv() Cv_to_Vv() Cv_to_Sv() - Convert between
    representations of (co-)variance structures
-   VerbAgg - Verbal Aggression item responses

### Simple Example Usage: lmer for linear mixed-effects model

```r
# Install and load the lme4 package
# install.packages("lme4")
library(lme4)

# Generate some example data
set.seed(123)
data <- data.frame(
  Group = rep(c("A", "B"), each = 5),
  Score = rnorm(10),
  Subject = factor(rep(1:5, 2))
)

# Fit a linear mixed-effects model
model <- lmer(Score ~ Group + (1 | Subject), data = data)

# Print the model summary
summary(model)
```

<!-- badges: start -->

[![R-CMD-check](https://github.com/lme4/lme4/workflows/R-CMD-check/badge.svg)](https://github.com/lme4/lme4/actions)
[![cran
version](http://www.r-pkg.org/badges/version/lme4)](https://cran.r-project.org/package=lme4)
[![downloads](http://cranlogs.r-pkg.org/badges/lme4)](http://cranlogs.r-pkg.org/badges/lme4)
[![total
downloads](http://cranlogs.r-pkg.org/badges/grand-total/lme4)](http://cranlogs.r-pkg.org/badges/grand-total/lme4)
<!-- badges: start -->

## Recent/release notes

-   See the [NEWS
    file](https://github.com/lme4/lme4/blob/master/inst/NEWS.Rd)

## Where to get help

-   [r-sig-mixed-models\@r-project.org](https://stat.ethz.ch/mailman/listinfo/r-sig-mixed-models)
    for questions about `lme4` usage and more general mixed model
    questions; please read the info page, and subscribe, before posting
    ... (note that the mailing list does not support images or
    large/non-text attachments)
-   <https://github.com/lme4/lme4/issues> for bug, infelicity, and
    wishlist reporting
-   The [lme4 tag on
    StackOverflow](https://stackoverflow.com/questions/tagged/lme4) for
    programming-related or the [lme4-nlme tag on
    CrossValidated](https://stats.stackexchange.com/questions/tagged/lme4-nlme)
    for statistics-related questions
-   maintainer e-mail only for urgent/private communications

## Support

If you choose to support `lme4` development financially, you can
contribute to a fund at McMaster University (home institution of one of
the developers)
[here](https://secureca.imodules.com/s/1439/17/giving/form.aspx?sid=1439&gid=1&pgid=770&cid=1618&dids=2413&bledit=1&appealcode=18C9).
The form will say that you are donating to the "Global Coding Fund";
this fund is available for use by the developers, under McMaster's
research spending rules. We plan to use the funds, as available, to pay
students to do maintenance and development work. There is no way to
earmark funds or set up a bounty to direct funding toward particular
features, but you can e-mail the maintainers and suggest priorities for
your donation.

## Features

-   Efficient for large data sets, using algorithms from the
    [Eigen](http://eigen.tuxfamily.org/index.php?title=Main_Page) linear
    algebra package via the
    [RcppEigen](https://cran.r-project.org/package=RcppEigen) interface
    layer.
-   Allows arbitrarily many nested and crossed random effects.
-   Fits generalized linear mixed models (GLMMs) and nonlinear mixed
    models (NLMMs) via Laplace approximation or adaptive Gauss-Hermite
    quadrature; GLMMs allow user-defined families and link functions.
-   Incorporates likelihood profiling and parametric bootstrapping.

## Installation

### On current R (\>= 3.0.0)

-   From CRAN (stable release 1.0.+)
-   Development version from Github:

```         
library("devtools"); install_github("lme4/lme4",dependencies=TRUE)
```

(This requires `devtools` \>= 1.6.1, and installs the "master"
(development) branch.) This approach builds the package from source,
i.e. `make` and compilers must be installed on your system -- see the R
FAQ for your operating system; you may also need to install dependencies
manually. Specify `build_vignettes=FALSE` if you have trouble because
your system is missing some of the `LaTeX/texi2dvi` tools. \*
Development binaries from `lme4` r-forge repository:

```         
install.packages("lme4",
   repos=c("http://lme4.r-forge.r-project.org/repos",
          getOption("repos")[["CRAN"]]))
```

(these source and binary versions are updated manually, so may be out of
date; if you believe they are, please contact the maintainers).

### On old R (pre-3.0.0)

It is possible to install (but not easily to check) `lme4` at least as
recently as 1.1-7.

-   make sure you have *exactly* these package versions: `Rcpp` 0.10.5,
    `RcppEigen` 3.2.0.2
-   for installation, use `--no-inst`; this is necessary in order to
    prevent R from getting hung up by the `knitr`-based vignettes
-   running `R CMD check` is difficult, but possible if you hand-copy
    the contents of the `inst` directory into the installed package
    directory ...

### Of `lme4.0`

-   `lme4.0` is a maintained version of lme4 back compatible to CRAN
    versions of lme4 0.99xy, mainly for the purpose of *reproducible
    research and data analysis* which was done with 0.99xy versions of
    lme4.
-   there have been
    [some](http://stackoverflow.com/questions/23662589/r-reverting-to-lme4-0-and-still-getting-inconsistent-results)
    [reports](http://hlplab.wordpress.com/2014/06/24/more-on-old-and-new-lme4/)
    of problems with `lme4.0` on R version 3.1; if someone has a
    specific reproducible example they'd like to donate, please contact
    the maintainers.
-   Notably, `lme4.0` features `getME(<mod>, "..")` which is compatible
    (as much as sensibly possible) with the current `lme4`'s version of
    `getME()`.
-   You can use the `convert_old_lme4()` function to take a fitted
    object created with `lme4` \<1.0 and convert it for use with
    `lme4.0`.
-   It currently resides on R-forge, and you should be able to install
    it with

```         
install.packages("lme4.0",
                 repos=c("http://lme4.r-forge.r-project.org/repos",
                         getOption("repos")[["CRAN"]]))
```

(if the binary versions are out of date or unavailable for your system,
please contact the maintainers).
