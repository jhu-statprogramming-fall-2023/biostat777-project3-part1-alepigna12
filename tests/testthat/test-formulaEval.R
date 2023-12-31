library("testthat")
library("lme4")

context("data= argument and formula evaluation")

## intercept context-dependent errors ... it's too bad that
##  these errors differ between devtools::test() and
##  R CMD check, but finding the difference is too much
##  of a nightmare
## n.b. could break in other locales *if* we ever do internationalization ...
data_RE <- "(bad 'data'|variable lengths differ)"

test_that("glmerFormX", {
    set.seed(101)
    n <- 50
    x <- rbinom(n, 1, 1/2)
    y <- rnorm(n)
    z <- rnorm(n)
    r <- sample(1:5, size=n, replace=TRUE)
    d <- data.frame(x,y,z,r)

    F <- "z"
    rF <- "(1|r)"
    modStr <- (paste("x ~", "y +", F, "+", rF))
    modForm <- as.formula(modStr)

    ## WARNING: these drop/environment tests are extremely sensitive to environment
    ## they may fail/not fail, or fail differently, within a "testthat" environment vs.
    ##   when run interactively
    expect_that(m_data.3 <- glmer( modStr , data=d, family="binomial"), is_a("glmerMod"))
    expect_that(m_data.4 <- glmer( "x ~ y + z + (1|r)" , data=d, family="binomial"), is_a("glmerMod"))
    ## interactively: (interactive() is TRUE {i.e. doesn't behave as I would expect} within testing environment ...
    ## if (interactive()) {
    ## AICvec <- c(77.0516381151634, 75.0819116367084, 75.1915023640827)
    ## expect_equal(drop1(m_data.3)$AIC,AICvec)
    ## expect_equal(drop1(m_data.4)$AIC,AICvec)
    ## } else {
        ## in test environment [NOT test_
        expect_error(drop1(m_data.3),data_RE)
        expect_error(drop1(m_data.4),data_RE)
    ##}
})

test_that("glmerForm", {
    set.seed(101)
    n <- 50
    x <- rbinom(n, 1, 1/2)
    y <- rnorm(n)
    z <- rnorm(n)
    r <- sample(1:5, size=n, replace=TRUE)
    d <- data.frame(x,y,z,r)

    F <- "z"
    rF <- "(1|r)"
    modStr <- (paste("x ~", "y +", F, "+", rF))
    modForm <- as.formula(modStr)

    ## formulas have environments associated, but character vectors don't
    ## data argument not specified:
    ## should work, but documentation warns against it

    expect_that(m_nodata.0 <- glmer( x ~ y + z + (1|r) , family="binomial"), is_a("glmerMod"))
    expect_that(m_nodata.1 <- glmer( as.formula(modStr) , family="binomial"), is_a("glmerMod"))
    expect_that(m_nodata.2 <- glmer( modForm , family="binomial"), is_a("glmerMod"))
    expect_that(m_nodata.3 <- glmer( modStr , family="binomial"), is_a("glmerMod"))
    expect_that(m_nodata.4 <- glmer( "x ~ y + z + (1|r)" , family="binomial"), is_a("glmerMod"))

    ## apply drop1 to all of these ...
    m_nodata_List <- list(m_nodata.0,
                          m_nodata.1,m_nodata.2,m_nodata.3,m_nodata.4)
    d_nodata_List <- lapply(m_nodata_List,drop1)

    rm(list=c("x","y","z","r"))

    ## data argument specified
    expect_that(m_data.0 <- glmer( x ~ y + z + (1|r) , data=d, family="binomial"), is_a("glmerMod"))
    expect_that(m_data.1 <- glmer( as.formula(modStr) , data=d, family="binomial"), is_a("glmerMod"))
    expect_that(m_data.2 <- glmer( modForm , data=d, family="binomial"), is_a("glmerMod"))
    expect_that(m_data.3 <- glmer( modStr , data=d, family="binomial"), is_a("glmerMod"))
    expect_that(m_data.4 <- glmer( "x ~ y + z + (1|r)" , data=d, family="binomial"), is_a("glmerMod"))

    ff <- function() {
        set.seed(101)
        n <- 50
        x <- rbinom(n, 1, 1/2)
        y <- rnorm(n)
        z <- rnorm(n)
        r <- sample(1:5, size=n, replace=TRUE)
        d2 <- data.frame(x,y,z,r)
        glmer( x ~ y + z + (1|r), data=d2, family="binomial")
    }
    m_data.5 <- ff()

    ff2 <- function() {
        set.seed(101)
        n <- 50
        x <- rbinom(n, 1, 1/2)
        y <- rnorm(n)
        z <- rnorm(n)
        r <- sample(1:5, size=n, replace=TRUE)
        glmer( x ~ y + z + (1|r), family="binomial")
    }
    m_data.6 <- ff2()


    m_data_List <- list(m_data.0,m_data.1,m_data.2,m_data.3,m_data.4,m_data.5,m_data.6)
    badNums <- 4:5
    d_data_List <- lapply(m_data_List[-badNums],drop1)

    ## these do NOT fail if there is a variable 'd' living in the global environment --
    ## they DO fail in the testthat context
    expect_error(drop1(m_data.3),data_RE)
    expect_error(drop1(m_data.4),data_RE)

    ## expect_error(lapply(m_data_List[4],drop1))
    ## expect_error(lapply(m_data_List[5],drop1))
    ## d_data_List <- lapply(m_data_List,drop1,evalhack="parent")  ## fails on element 1
    ## d_data_List <- lapply(m_data_List,drop1,evalhack="formulaenv")  ## fails on element 4
    ## d_data_List <- lapply(m_data_List,drop1,evalhack="nulldata")  ## succeeds
    ## drop1(m_data.5,evalhack="parent") ## 'd2' not found
    ## drop1(m_data.5,evalhack="nulldata") ## 'x' not found (d2 is in environment ...)
    ## should we try to make update smarter ... ??

    ## test equivalence of (i vs i+1) for all models, all drop1() results
    for (i in 1:(length(m_nodata_List)-1)) {
        expect_equivalent(m_nodata_List[[i]],m_nodata_List[[i+1]])
        expect_equivalent(d_nodata_List[[i]],d_nodata_List[[i+1]])
    }

    expect_equivalent(m_nodata_List[[1]],m_data_List[[1]])
    expect_equivalent(d_nodata_List[[1]],d_data_List[[1]])

    for (i in 1:(length(m_data_List)-1)) {
        expect_equivalent(m_data_List[[i]],m_data_List[[i+1]])
    }
    ## allow for dropped 'bad' vals
    for (i in 1:(length(d_data_List)-1)) {
        expect_equivalent(d_data_List[[i]],d_data_List[[i+1]])
    }

})


test_that("lmerForm", {

    set.seed(101)

    x <- rnorm(10)
    y <- rnorm(10)
    z <- rnorm(10)
    r <- sample(1:3, size=10, replace=TRUE)
    d <- data.frame(x,y,z,r)

    ## example from Joehanes Roeby
    m2 <- suppressWarnings(lmer(x ~ y + z + (1|r), data=d))
    ff <- function() {
        m1 <- suppressWarnings(lmer(x ~ y + z + (1|r), data=d))
        return(anova(m1))
    }

    ff1 <- Reaction ~ Days + (Days|Subject)
    fm1 <- lmer(ff1, sleepstudy)
    fun <- function () {
        ff1 <- Reaction ~ Days + (Days|Subject)
        fm1 <- suppressWarnings(lmer(ff1, sleepstudy))
        return (anova(fm1))
    }
    anova(m2)
    ff()
    expect_equal(anova(m2),ff())
    anova(fm1)
    fun()
    expect_equal(anova(fm1),fun())

    ## test deparsing of long RE terms
    varChr <- paste0("varname_",outer(letters,letters,paste0)[1:100])
    rvars <- varChr[1:9]
    form <- as.formula(paste("y ~",paste(varChr,collapse="+"),
                             "+",
                             paste0("(",paste(rvars,collapse="+"),"|f)")))
    ff <- lme4:::reOnly(form)
    environment(ff) <- .GlobalEnv
    expect_equal(ff,
     ~(varname_aa + varname_ba + varname_ca + varname_da + varname_ea +
       varname_fa + varname_ga + varname_ha + varname_ia | f))
})

test_that("lapply etc.", {
    ## copied from dplyr
    failwith <- function (default = NULL, f, quiet = FALSE) {
        function(...) {
            out <- default
            try(out <- f(...), silent = quiet)
            out
        }
    }
    lmer_fw    <- failwith(NULL,function(...) lmer(...)   ,quiet=TRUE)
    expect_is(lmer_fw(Yield ~ 1|Batch, Dyestuff, REML = FALSE),
              "merMod")
    ## GH 369
    listOfFormulas <- list(
        cbind(incidence, size - incidence) ~ 1 +  (1 | herd),
        cbind(incidence, size - incidence) ~ period +  (1 | herd))
    expect_is(lapply(listOfFormulas,glmer,family=binomial,data=cbpp),"list")
})

test_that("formula and data validation work with do.call() in artificial environment", {
    ## This ensures compatibility of lmer when it's called from the
    ## C-level Rf_eval() with an environment that doesn't exist on the
    ## stack (i.e. C implementation in magrittr 2.0)
    e <- new.env()
    e$. <- mtcars
    expect_is(
        do.call(lme4::lmer, list("disp ~ (1 | cyl)", quote(.)), envir = e),
        "merMod"
    )

    fn <- function(data) {
        lme4::lmer("disp ~ (1 | cyl)", data = data)
    }
    expect_is(
        do.call(fn, list(quote(.)), envir = e),
        "merMod"
    )
})

test_that("correct environment on reOnly()", {
  ## GH 654
  f <- Reaction ~ Days + (1 | Subject)
  e <- environment(f)
  m <- lmer(f, data = sleepstudy)
  expect_identical(environment(formula(m)), e) # TRUE
  expect_identical(environment(formula(m, fixed.only = TRUE)), e) # TRUE
  expect_identical(ee <- environment(formula(m, random.only = TRUE)), e) # FALSE
})
