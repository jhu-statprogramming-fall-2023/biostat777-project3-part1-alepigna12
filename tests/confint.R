if (lme4:::testLevel() > 1 || .Platform$OS.type != "windows") withAutoprint({
    library("lme4")
    library("testthat")

    L <- load(system.file("testdata", "lme-tst-fits.rda",
                          package="lme4", mustWork=TRUE))
    ## -> "fit_*" objects

    fm1 <- fit_sleepstudy_2
    c0  <- confint(fm1, method="Wald")
    c0B <- confint(fm1, method="Wald",parm="Days")
    expect_equal(c0["Days",],c0B["Days",])
    expect_equal(c(c0B),c(7.437592,13.496980),tolerance=1e-6)
    set.seed(101)

    for (bt in c("norm", "basic", "perc")) {
        suppressWarnings(
            confint(fm1, method="boot", boot.type=bt, nsim=10,quiet=TRUE))
    }
    for (bt in c("stud","bca","junk")) {
        expect_error(confint(fm1, method="boot", boot.type=bt, nsim=10),
                     "should be one of")
    }
    if((testLevel <- lme4:::testLevel()) > 1) {
        pr1.56 <- profile(fm1, which = 5:6)
        c1 <- confint(pr1.56, method="profile")
        expect_equal(c0[5:6,],c1,tolerance=2e-3)  ## expect Wald and profile _reasonably_ close
        print(c1,digits=3)
        ##
        c6 <- confint(pr1.56, "Days")
        expect_equal(c1[2, , drop=FALSE], c6)
        c2  <- confint(fm1,method="boot",nsim=50,parm=5:6)
        ## expect_error(confint(fm1,method="boot",nsim=50,parm="Days"),
        ##  "must be specified as an integer")
        expect_equal(c1,c2,tolerance=2e-2)
        print(c2,digits=3)
    }
    if (testLevel > 10) {
        print(c1B <- confint(fm1, method="profile"))
        print(c2B <- confint(fm1, method="boot"))
        expect_equal(unname(c1B), unname(c2B), tolerance=2e-2)
    }
}) ## skip if windows/testLevel<1
