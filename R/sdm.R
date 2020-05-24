#' @import grDevices
#' @import graphics
#' @import utils
#' @export

sdm <- function(A,
                B = diag(nrow(A)),
                n = nrow(B),
                m = ncol(B),
                S0Exg = matrix(NA, n, m),
                p0 = matrix(1, nrow = n, ncol = 1),
                z0 = matrix(100, nrow = m, ncol = 1),
                GRExg = NA,
                moneyOwnerIndex = NULL,
                moneyIndex = NULL,
                pExg = NULL,
                tolCond = 1e-5,
                maxIteration = 200,
                numberOfPeriods = 300,
                depreciationCoef = 0.8,
                thresholdForPriceAdjustment = 0.99,
                priceAdjustmentMethod = "variable",
                priceAdjustmentVelocity = 0.15,
                trace = TRUE,
                ts = FALSE,
                policy = NULL,
                exchangeFunction = F_Z) {
  ##### definition of economic transition function xNext
  xNext <- function(xt) {
    p_t <- xt$p
    S_t <- xt$S
    q_t <- xt$q
    z_t <- xt$z
    e_t <- xt$e

    if (all(is.na(e_t))) {
      e_tp1 <- NA
    }

    p_tp1 <- p_t
    switch(
      priceAdjustmentMethod,
      "fixed" = {
        for (ka in 1:n) {
          if (q_t[ka] <= thresholdForPriceAdjustment) {
            p_tp1[ka] <- p_t[ka] * (1 - priceAdjustmentVelocity)
          } else {
            p_tp1[ka] <- p_t[ka]
          }
        }
        p_tp1 <- p_tp1 / sum(p_tp1) # normalize
      },
      "variable" = {
        p_tp1 <- p_t * (1 - priceAdjustmentVelocity * (1 - q_t))

        if (any(!is.na(pExg))) {
          tmpIndex <- which(!is.na(pExg))
          tmpIndex <- tmpIndex[1]
          p_tp1 <- p_tp1 / p_tp1[tmpIndex] * pExg[tmpIndex]
          p_tp1[!is.na(pExg)] <- pExg[!is.na(pExg)]
        }
        else {
          p_tp1 <- p_tp1 / sum(p_tp1) # normalize
        }
      },
      "monetary" = {
        # monetary economy
        p_tp1 <- p_t * (1 - priceAdjustmentVelocity * (1 - q_t))
        e_tp1 <-
          e_t * (1 - priceAdjustmentVelocity * (1 - q_t[moneyIndex]))

        p_tp1 <- p_tp1 / e_tp1[1]
        e_tp1 <- e_tp1 / e_tp1[1]

        if (!is.null(pExg) && length(pExg) != 0) {
          p_tp1[!is.na(pExg)] <- pExg[!is.na(pExg)]
        }
      },

      stop("LI: Wrong priceAdjustmentMethod!,xnext")
    ) # switch

    if (is.numeric(B)) {
      B_t <- B
    } else {
      B_t <- B(list(p = p_t, z = z_t, t = time - 1))
    }

    S_tp1 <- sweep(B_t, 2, z_t, "*") + sweep(S_t, 1, depreciationCoef * (1 - q_t), "*")

    if (all(is.na(S0Exg))) {

    } # Set exogenous supply
    else {
      S_tp1[!is.na(S0Exg)] <- S_t[!is.na(S0Exg)] * (1 + GRExg)

      if (any(!is.na(e_t))) {
        for (i in 1:length(e_t)) {
          S_tp1[moneyIndex[i], moneyOwnerIndex[i]] <-
            S_tp1[moneyIndex[i], moneyOwnerIndex[i]] / e_t[i] * e_tp1[i]
        }
      }
    }

    if (!is.null(policy)) {
      tmp <- policy(
        time = time,
        state = list(p = p_tp1, S = S_tp1),
        state.history = list(
          p = t(p),
          S = S,
          q = t(q),
          z = t(z),
          e = t(e)
        )
      ) # 20181204
      p_tp1 <- tmp$p
      S_tp1 <- tmp$S

      if (!is.null(tmp$current.policy.data)) {
        policy.data <<- rbind(policy.data, tmp$current.policy.data)
      }
    } # 20181205

    if (is.numeric(A)) {
      A_tp1 <- A
    } else {
      A_tp1 <-
        A(list(
          p = p_tp1,
          z = z_t,
          w = t(p_tp1) %*% S_tp1,
          t = time,
          e = e_tp1
        ))
    } # 20140614,add e

    tmp <- exchangeFunction(A_tp1, p_tp1, S_tp1)
    q_tp1 <- tmp$q
    z_tp1 <- tmp$z

    if (any(z_tp1 < 0)) {
      if (any(z_tp1[z_tp1 < 0] > -0.01)) {
        z_tp1[z_tp1 < 0] <- 0

        warning("LI: negative_z,z_tp1<0")
      }
      else {
        message(z_tp1)
        stop("Li: negative_z")
      }
    }

    list(
      p = p_tp1,
      S = S_tp1,
      q = q_tp1,
      z = z_tp1,
      e = e_tp1
    )
  } # xNext
  ##### the end of definition of economic function xNext


  # beginning ---------------------------------------------------------------
  substitutionMethod <- "finalValue" # the substitution method for iterations.
  priceAdjustmentVelocityCoefficient <- 0.95 # the changing coefficient of the price adjustment velocity.

  result <- c()

  if (is.na(GRExg) && !all(is.na(S0Exg))) GRExg <- 0

  if (trace) {
    message(paste("tolCond: ", tolCond))
  }

  p <- matrix(0, n, numberOfPeriods)
  S <- array(0, dim = c(n, m, numberOfPeriods))
  q <- matrix(0, n, numberOfPeriods)
  z <- matrix(0, m, numberOfPeriods)
  if (length(moneyIndex) > 1) {
    e <- matrix(0, length(moneyIndex), numberOfPeriods)
  } else {
    e <- matrix(0, 1, numberOfPeriods)
  }
  if (!is.null(policy)) policy.data <- data.frame()

  if (all(is.na(S0Exg))) {
    S0 <- matrix(0, n, m)
  } else {
    S0 <- S0Exg
    S0[is.na(S0)] <- 0
  }

  firstExgSupplyIndex <-
    which(!is.na(c(S0Exg)))[1] # Here we may get NA, not empty!

  if (!is.null(moneyIndex) && !all(is.na(moneyIndex))) {
    priceAdjustmentMethod <- "monetary" # for monetary economy

    e0 <- matrix(1, length(moneyIndex), 1)
  }
  else {
    e0 <- NA
  }

  time <- 1

  xtp1 <- xNext(list(
    p = p0,
    S = S0,
    q = matrix(1, n, 1),
    z = z0,
    e = e0
  ))

  p[, 1] <- xtp1$p
  S[, , 1] <- xtp1$S
  q[, 1] <- xtp1$q
  z[, 1] <- xtp1$z
  e[, 1] <- xtp1$e


  toleranceRec <- matrix(1, maxIteration, 1)

  for (k.iteration in 1:maxIteration) {
    for (t in 2:numberOfPeriods) {
      time <- time + 1

      xt <- c()
      xt$p <- p[, t - 1]
      xt$S <- S[, , t - 1]
      dim(xt$S) <- c(n, m)
      xt$q <- q[, t - 1]
      xt$z <- z[, t - 1]
      xt$e <- e[, t - 1]

      xtp1 <- xNext(xt)


      p[, t] <- xtp1$p
      S[, , t] <- xtp1$S
      q[, t] <- xtp1$q
      z[, t] <- xtp1$z
      e[, t] <- xtp1$e
    } # for (t in 2:numberOfPeriods)
    ##### the end of an iteration

    if (ts) {
      result$ts.p <- t(p)
      result$ts.z <- t(z)
      result$ts.S <- S
      result$ts.q <- t(q)
      result$ts.e <- t(e)
    }

    tmp1 <- z[, ncol(z)] / max(z[, ncol(z)])
    tmp2 <- z[, ncol(z) - 1] / max(z[, ncol(z) - 1])
    toleranceZ <- max(abs(tmp1 - tmp2))

    tmpU <- apply(q[, (ncol(q) - 20):ncol(q)], 1, min)
    tmpU <- tmpU[p[, ncol(p)] > tolCond]
    toleranceU <- max(1 - tmpU)
    tolerance <- max(c(toleranceU, toleranceZ))
    toleranceRec[k.iteration] <- tolerance

    if ((maxIteration > 1 && tolerance > 0.99) ||
      (k.iteration >= 5 && (toleranceRec[k.iteration] / toleranceRec[k.iteration - 1] > 0.9))
    ) {
      # converge slowly
      if (!is.na(GRExg) && GRExg == 0) {
        substitutionMethod <- "meanValue"
      } else {
        substitutionMethod <- "pMeanValue"
      }

      message(paste("Iteration ", k.iteration, ", substitutionMethod: ", substitutionMethod))


      if (k.iteration > 10 && (toleranceRec[k.iteration] / toleranceRec[k.iteration - 1] > 0.95)) {
        priceAdjustmentVelocity <-
          priceAdjustmentVelocity * priceAdjustmentVelocityCoefficient
      }
    }


    if (maxIteration > 1 && !is.na(GRExg) && GRExg == 0 && toleranceU < tolCond && toleranceZ >= tolCond) {
      substitutionMethod <- "zMeanValue"
      message(paste("substitutionMethod: ", substitutionMethod))
    }

    S0 <- S[, , dim(S)[3]]
    dim(S0) <- c(n, m)
    switch(
      substitutionMethod,
      "pMeanValue" = {
        p0 <- apply(p, 1, mean)
        # message(paste("p0:",p0))
      },
      "zMeanValue" = {
        z0 <- apply(z, 1, mean)
      },
      "meanValue" = {
        p0 <- apply(p, 1, mean)
        z0 <- apply(z, 1, mean)
        # message(paste("p0:",p0))
        # message(paste("z0:",z0))
      },
      "finalValue" = {
        p0 <- p[, ncol(p)]
        z0 <- z[, ncol(z)]
      },
      stop("Li: wrong substitutionMethod!")
    )

    if (!is.na(firstExgSupplyIndex)) {
      # There are exogenous supplies.
      z0 <- z0 / S0[firstExgSupplyIndex] * S0Exg[firstExgSupplyIndex]
      S0 <- S0 / S0[firstExgSupplyIndex] * S0Exg[firstExgSupplyIndex]
    }
    else {
      S0 <- S0 / max(z0)
      z0 <- z0 / max(z0)
    }

    if (trace) {
      message(paste("Iteration number ", k.iteration, ": tolerance coefficient ", tolerance))
    }

    if (tolerance < tolCond) {
      break
    }

    if (k.iteration < maxIteration) {
      xtp1 <- xNext(list(
        p = p0,
        S = S0,
        q = matrix(1, n, 1),
        z = z0,
        e = t(tail(t(e), 1))
      ))
      p[, 1] <- xtp1$p
      S[, , 1] <- xtp1$S
      q[, 1] <- xtp1$q
      z[, 1] <- xtp1$z
      e[, 1] <- xtp1$e
    }
  } # for (k.iteration in 1:maxIteration)


  # result ------------------------------------------------------------------
  result$tolerance <- tolerance
  result$p <- p0
  result$z <- z0
  result$S <- S0

  if (any(!is.na(e0))) {
    result$e <- e[, ncol(e)]
  }
  if (all(is.na(S0Exg))) {
    result$growthRate <- max(z[, ncol(z)]) / max(z[, ncol(z) - 1]) - 1
  }

  if (is.numeric(A)) {
    result$A <- A
  } else {
    tmpS <- S0Exg
    tmpS[is.na(tmpS)] <- 0
    result$A <-
      A(list(
        p = result$p,
        z = result$z,
        w = result$p %*% tmpS,
        t = 1,
        e = result$e
      )) # 20140614,add e
  }

  if (is.function(B)) {
    result$B <- B(list(p = result$p, z = result$z, t = 1))
  } # 20190214

  if (!is.null(policy) && length(policy.data) != 0) {
    result$policy.data <- policy.data
  }

  return(result)
}
