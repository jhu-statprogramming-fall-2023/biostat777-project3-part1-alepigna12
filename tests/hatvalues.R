if (.Platform$OS.type != "windows") withAutoprint({

    library(lme4)
    source(system.file("testdata", "lme-tst-funs.R", package="lme4", mustWork=TRUE))# -> unn()

    m <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
    bruteForceHat <- function(object) {
        with(getME(object, c("Lambdat", "Lambda", "Zt", "Z", "q", "X")), {
            ## cp:= the cross product block matrix in (17) and (18):
            W <- Diagonal(x = weights(object))
            I <- Diagonal(q)
            A.21 <- t(X) %*% W %*% Z %*% Lambda
            cp <- rbind(cbind(Lambdat %*% Zt %*% W %*% Z %*% Lambda + I, t(A.21)),
                        cbind(A.21, t(X) %*% W %*% X))
            mm <- cbind(Z %*% Lambda, X)
            ## a bit efficient: both cp and mm are typically quite sparse
            ## mm %*% solve(as.matrix(cp)) %*% t(mm)
            mm %*% solve(cp, t(mm), sparse=FALSE)
        })
    }


    str(H <- bruteForceHat(m))

    set.seed(7)
    ii <- sample(nrow(sleepstudy), 500, replace=TRUE)
    m2 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy[ii, ])

    
    stopifnot(all.equal(diag(H),
                        unn(hatvalues(m)),  tol= 1e-14),
              all.equal(diag(bruteForceHat(m2)),
                        unn(hatvalues(m2)), tol= 1e-14)
              )
}) ## skip on windows (for speed)
