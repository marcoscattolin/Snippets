library(mgcv)
library(nlme)
library(httr)

#############################################
## Functions for derivatives of GAM models ##
#############################################
Deriv <- function(mod, n = 200, eps = 1e-7, newdata) {
        if(isTRUE(inherits(mod, "list")))
                mod <- mod$gam
        m.terms <- attr(terms(mod), "term.labels")
        if(missing(newdata)) {
                newD <- sapply(model.frame(mod)[, m.terms, drop = FALSE],
                               function(x) seq(min(x), max(x), length = n))
                names(newD) <- m.terms
        } else {
                newD <- newdata
        }
        X0 <- predict(mod, data.frame(newD), type = "lpmatrix")
        newD <- newD + eps
        X1 <- predict(mod, data.frame(newD), type = "lpmatrix")
        Xp <- (X1 - X0) / eps
        Xp.r <- NROW(Xp)
        Xp.c <- NCOL(Xp)
        ## dims of bs
        bs.dims <- sapply(mod$smooth, "[[", "bs.dim") - 1
        # number of smooth terms
        t.labs <- attr(mod$terms, "term.labels")
        nt <- length(t.labs)
        ## list to hold the derivatives
        lD <- vector(mode = "list", length = nt)
        names(lD) <- t.labs
        for(i in seq_len(nt)) {
                Xi <- Xp * 0
                want <- grep(t.labs[i], colnames(X1))
                Xi[, want] <- Xp[, want]
                df <- Xi %*% coef(mod)
                df.sd <- rowSums(Xi %*% mod$Vp * Xi)^.5
                lD[[i]] <- list(deriv = df, se.deriv = df.sd)
                ## Xi <- Xp * 0 ##matrix(0, nrow = Xp.r, ncol = Xp.c)
                ## J <- bs.dims[i]
                ## Xi[,(i-1) * J + 1:J + 1] <- Xp[,(i-1) * J + 1:J +1]
                ## df <- Xi %*% coef(mod)
                ## df.sd <- rowSums(Xi %*% mod$Vp * Xi)^.5
                ## lD[[i]] <- list(deriv = df, se.deriv = df.sd)
        }
        class(lD) <- "Deriv"
        lD$gamModel <- mod
        lD$eps <- eps
        lD$eval <- newD - eps
        return(lD)
}

confint.Deriv <- function(object, term, alpha = 0.05, ...) {
        l <- length(object) - 3
        term.labs <- names(object[seq_len(l)])
        if(missing(term))
                term <- term.labs
        Term <- match(term, term.labs)
        ##term <- term[match(term, term.labs)]
        if(any(miss <- is.na(Term)))
                stop(paste("'term'", term[miss], "not a valid model term."))
        ## if(is.na(term))
        ##     stop("'term' not a valid model term.")
        res <- vector(mode = "list", length = length(term))
        names(res) <- term
        residual.df <- length(object$gamModel$y) - sum(object$gamModel$edf)
        tVal <- qt(1 - (alpha/2), residual.df)
        ## tVal <- qt(1 - (alpha/2), object$gamModel$df.residual)
        for(i in seq_along(term)) {
                upr <- object[[term[i]]]$deriv + tVal * object[[term[i]]]$se.deriv
                lwr <- object[[term[i]]]$deriv - tVal * object[[term[i]]]$se.deriv
                res[[term[i]]] <- list(upper = drop(upr), lower = drop(lwr))
        }
        res$alpha = alpha
        res
}

signifD <- function(x, d, upper, lower, eval = 0) {
        miss <- upper > eval & lower < eval
        incr <- decr <- x
        want <- d > eval
        incr[!want | miss] <- NA
        want <- d < eval
        decr[!want | miss] <- NA
        list(incr = incr, decr = decr)
}

plot.Deriv <- function(x, alpha = 0.05, polygon = TRUE,
                       sizer = FALSE, term, eval = 0, lwd = 3,
                       col = "lightgrey", border = col,
                       ylab, xlab, ...) {
        l <- length(x) - 3
        ## get terms and check specified (if any) are in model
        term.labs <- names(x[seq_len(l)])
        if(missing(term))
                term <- term.labs
        Term <- match(term, term.labs)
        if(any(miss <- is.na(Term)))
                stop(paste("'term'", term[miss], "not a valid model term."))
        if(all(is.na(Term)))
                stop("All terms in 'term' not found in model.")
        l <- sum(!miss)
        nplt <- n2mfrow(l)
        ## tVal <- qt(1 - (alpha/2), x$gamModel$df.residual)
        residual.df <- length(x$gamModel$y) - sum(x$gamModel$edf)
        tVal <- qt(1 - (alpha/2), residual.df)
        if(missing(ylab))
                ylab <- expression(italic(hat(f)*"'"*(x)))
        if(missing(xlab)) {
                xlab <- attr(terms(x$gamModel), "term.labels")[Term]
                names(xlab) <- xlab
        }
        layout(matrix(seq_len(l), nrow = nplt[1], ncol = nplt[2]))
        CI <- confint(x, term = term, alpha = alpha)
        for(i in seq_along(term)) {
                ## for(i in seq_len(l)) {
                upr <- CI[[term[i]]]$upper
                lwr <- CI[[term[i]]]$lower
                ylim <- range(upr, lwr)
                plot(x$eval[,term[i]], x[[term[i]]]$deriv, type = "n",
                     ylim = ylim, ylab = ylab, xlab = xlab[term[i]], ...)
                if(isTRUE(polygon)) {
                        polygon(c(x$eval[,term[i]], rev(x$eval[,term[i]])),
                                c(upr, rev(lwr)), col = col, border = border)
                } else {
                        lines(x$eval[,term[i]], upr, lty = "dashed")
                        lines(x$eval[,term[i]], lwr, lty = "dashed")
                }
                abline(h = 0, ...)
                if(isTRUE(sizer)) {
                        lines(x$eval[,term[i]], x[[term[i]]]$deriv, lwd = 1)
                        S <- signifD(x[[term[i]]]$deriv, x[[term[i]]]$deriv, upr, lwr,
                                     eval = eval)
                        lines(x$eval[,term[i]], S$incr, lwd = lwd, col = "blue")
                        lines(x$eval[,term[i]], S$decr, lwd = lwd, col = "red")
                } else {
                        lines(x$eval[,term[i]], x[[term[i]]]$deriv, lwd = 2)
                }
        }
        layout(1)
        invisible(x)
}




## Model Checking function
tsDiagGamm <- function(x, timevar, observed, f = 0.3, type = "normalized") {
        resi <- resid(x$lme, type = type)
        fits <- fitted(x$lme)
        on.exit(layout(1))
        layout(matrix(1:6, ncol = 3, byrow = TRUE))
        plot(resi ~ fits, ylab = "Normalized Residuals",
             xlab = "Fitted Values", main = "Fitted vs. Residuals")
        lines(lowess(x = fits, y = resi, f = f), col = "blue",
              lwd = 2)
        plot(resi ~ timevar, ylab = "Normalized Residuals",
             xlab = "Time", main = "Time series of residuals")
        lines(lowess(x = timevar, y = resi, f = f), col = "blue", lwd = 2)
        plot(observed ~ fits, ylab = "Observed",
             xlab = "Fitted Values", main = "Fitted vs. Observed",
             type = "n")
        abline(a = 0, b = 1, col = "red")
        points(observed ~ fits)
        lines(lowess(x = fits, y = observed, f = f), col = "blue",
              lwd = 2)
        hist(resi, freq = FALSE, xlab = "Normalized Residuals")
        qqnorm(resi)
        qqline(resi)
        acf(resi, main = "ACF of Residuals")
}

gtemp <- read.table("http://www.cru.uea.ac.uk/cru/data/temperature/HadCRUT3-gl.dat",fill=T)

gtemp <- gtemp[-(seq(2,330,by=2)),]

gtemp <- gtemp[-(length(gtemp[,1])),]

rownames(gtemp) <- gtemp[,1]

colnames(gtemp) <- c("Year", month.abb, "Annual")

ylab <- expression(Temperature~Anomaly~(1961-1990)~degree*C)

plot(Annual ~ Year, data = gtemp, type = "o", ylab = ylab)

m1 <- gamm(Annual ~ s(Year, k = 20), data = gtemp)
summary(m1$gam)

acf(resid(m1$lme, type = "normalized"))
pacf(resid(m1$lme, type = "normalized"))


m2 <- gamm(Annual ~ s(Year, k = 30), data = gtemp,correlation= corARMA(form = ~ Year, p = 1))
m3 <- gamm(Annual ~ s(Year, k = 30), data = gtemp,correlation= corARMA(form = ~ Year, p = 2))

anova(m1$lme, m2$lme, m3$lme)

plot(m2$gam, residuals = T, pch = 19, cex = 0.75)

with(gtemp, tsDiagGamm(m2, timevar = Year, observed = Annual))


plot(Annual ~ Year, data = gtemp, type = "p", ylab = ylab)
pdat <- with(gtemp,data.frame(Year = seq(min(Year), max(Year),length = 200)))
p1 <- predict(m1$gam, newdata = pdat)
p2 <- predict(m2$gam, newdata = pdat)
lines(p1 ~ Year, data = pdat, col = "red")
lines(p2 ~ Year, data = pdat, col = "blue")
legend("topleft",
       legend = c("Uncorrelated Errors","AR(1) Errors"),
       bty = "n", col = c("red","blue"), lty = 1)


m2.d <- Deriv(m2, n = 200)

plot(m2.d, sizer = TRUE, alpha = 0.01)


plot(Annual ~ Year, data = gtemp, type = "p", ylab = ylab)
lines(p2 ~ Year, data = pdat)
CI <- confint(m2.d, alpha = 0.01)
S <- signifD(p2, m2.d$Year$deriv, CI$Year$upper, CI$Year$lower,
             eval = 0)
lines(S$incr ~ Year, data = pdat, lwd = 3, col = "blue")
lines(S$decr ~ Year, data = pdat, lwd = 3, col = "red")

library(MASS)
Rbeta <- mvrnorm(n = 1000, coef(m2$gam), vcov(m2$gam))
Xp <- predict(m2$gam, newdata = pdat, type = "lpmatrix")
sim1 <- Xp %*% t(Rbeta)


set.seed(321)
want <- sample(1000, 25)
ylim <- range(sim1[,want], gtemp$Annual)
plot(Annual ~ Year, data = gtemp, ylim = ylim, ylab = ylab)
matlines(pdat$Year, sim1[,want], col = "black", lty = 1, pch = NA)



set.seed(321)
want <- sample(1000, 50)
rwant <- with(pdat, which(Year >= 2000))
twant <- with(gtemp, which(Year >= 2000))
ylim <- range(sim1[rwant,want], gtemp$Annual[twant])
plot(Annual ~ Year, data = gtemp, ylim = ylim,
     xlim = c(1990, 2009), type = "n", ylab = ylab)
matlines(pdat$Year, sim1[,want], col = "black", lty = 1, pch = NA)
points(Annual ~ Year, data = gtemp, col = "red", bg = "yellow",
       pch = 21, cex = 1.5)
