# LI Wu (liwu@staff.shu.edu.cn). Shanghai University. Mathematical Economics.

#' @export

# Cobb-Douglas pure production
Example2.2 <- function() {
  sdm(
    A = function(state) {
      alpha <- rbind(5, 3, 1)
      Beta <- matrix(c(
        0.6, 0.4, 0.2,
        0.1, 0.4, 0.7,
        0.3, 0.2, 0.1
      ), 3, 3, TRUE)
      CD_A(alpha, Beta, state$p)
    },

    B = diag(3)
  )
}

#' @export

# von Neumann economy
Example2.3 <- function() {
  sdm(
    A = matrix(c(
      0.8, 0.5, 0.06,
      2, 2, 0.4
    ), 2, 3, T),
    B = matrix(c(
      1, 1, 0,
      0, 0, 1
    ), 2, 3, T)
  )
}


#' @export

# Setion 3.1.2, Leontief two-sector corn economy
Example.Section.3.1.2.corn <- function() {
  sdm(
    A = matrix(c(
      0.5, 1,
      1, 0
    ), 2, 2, TRUE),
    B = diag(2),
    S0Exg = matrix(c(
      NA, NA,
      NA, 100
    ), 2, 2, TRUE),
    GRExg = 0
  )
}

#' @export

# two-sector corn economy with non-homothetic utility function
Example3.1 <- function() {
  GRExg <- 0.2
  rho <- 1 / (1 + GRExg)

  sdm(
    GRExg = GRExg,
    A = function(state) {
      with(state, {
        matrix(c(
          0.5, (-1 / 2 * (p[2] - (p[2]^2 + 4 * p[1] * p[2] * rho)^(1 / 2)) / p[1])^2,
          1, -1 / 2 * (p[2] - (p[2]^2 + 4 * p[1] * p[2] * rho)^(1 / 2)) / p[1]
        ), 2, 2, T)
      })
    },
    B = diag(2),
    S0Exg = matrix(c(
      NA, NA,
      NA, 100
    ), 2, 2, TRUE)
  )
}


#' @export

# Cobb-Douglas two-sector corn economy
Example3.2 <- function() {
  sdm(
    A = function(state) {
      alpha <- rbind(1, 1)
      Beta <- matrix(c(
        0.5, 0.4,
        0.5, 0.6
      ), 2, 2, TRUE)
      CD_A(alpha, Beta, state$p)
    },
    B = diag(2),
    S0Exg = matrix(c(
      NA, NA,
      NA, 100
    ), 2, 2, TRUE),
    GRExg = 0
  )
}


#' @export

# Lontief three-sector economy with one primary factor
Example3.4 <- function() {
  sdm(
    A = matrix(c(
      0, 0.4, 1,
      0.5, 0, 0,
      0.3, 0.4, 0
    ), 3, 3, T),
    B = diag(3),
    S0Exg = {
      tmp <- matrix(NA, 3, 3)
      tmp[3, 3] <- 100
      tmp
    },
    GRExg = 0
  )
}

#' @export

# Cobb-Douglas three-sector economy with one primary factor
Example3.8 <- function() {
  sdm(
    A = function(state) {
      alpha <- rbind(5, 3, 1)
      Beta <- matrix(c(
        0.6, 0.4, 0.2,
        0.1, 0.4, 0.7,
        0.3, 0.2, 0.1
      ), 3, 3, TRUE)
      CD_A(alpha, Beta, state$p)
    },
    B = diag(3),
    S0Exg = {
      tmp <- matrix(NA, 3, 3)
      tmp[3, 3] <- 100
      # Iron is both product and primary factor.
      # (ii) tmp[2,3]<-100
      # (iii) tmp[2,3]<- -1
      tmp
    },

    GRExg = 0
  )
}

#' @export

# Cobb-Douglas three-sector economy with two primary factors
Example3.9 <- function() {
  sdm(
    A = function(state) {
      alpha <- rbind(5, 3, 1)
      Beta <- matrix(c(
        0.6, 0.4, 0.2,
        0.1, 0.4, 0.7,
        0.3, 0.2, 0.1
      ), 3, 3, TRUE)
      CD_A(alpha, Beta, state$p)
    },
    B = diag(3),
    S0Exg = {
      tmp <- matrix(NA, 3, 3)
      tmp[2, 2] <- 60
      tmp[3, 3] <- 100
      tmp
    },
    GRExg = 0
  )
}


#' @export

# Leontief corn economy with three primary factors
Example3.10 <- function() {
  sdm(
    A = matrix(c(
      0, 0, 1, 1, 1,
      0.5, 1, 0, 0, 0,
      0.5, 0, 0, 0, 0,
      0, 1, 0, 0, 0
    ), 4, 5, TRUE),
    B = matrix(c(
      1, 1, 0, 0, 0,
      0, 0, 1, 0, 0,
      0, 0, 0, 1, 0,
      0, 0, 0, 0, 1
    ), 4, 5, TRUE),
    S0Exg = {
      tmp <- matrix(NA, 4, 5)
      tmp[2, 3] <- 30
      tmp[3, 4] <- 20
      tmp[4, 5] <- 20
      tmp
    },
    GRExg = 0
  )
}

#' @export

# decreasing returns to scale
Example3.12 <- function() {
  sdm(
    A = function(state) {
      with(state, {
        matrix(c(
          0, 0, (w[3] / 10000 - 0.4 * p[3]) / (3 * p[1]),
          0, 0, (w[3] / 10000 - 0.4 * p[3]) / (3 * p[2]),
          (p[4] / p[3])^0.5, 0.5 * (p[5] / p[3])^0.5, 0.4 + (w[3] / 10000 - 0.4 * p[3]) / (3 * p[3]),
          (p[4] / p[3])^(-0.5), 0, 0,
          0, 0.5 * (p[5] / p[3])^(-0.5), 0
        ), 5, 3, T)
      })
    },
    B = matrix(c(
      1, 0, 0,
      0, 1, 0,
      0, 0, 1,
      0, 0, 1 / 10000,
      0, 0, 1 / 10000
    ), 5, 3, TRUE),
    S0Exg = {
      tmp <- matrix(NA, 5, 3)
      tmp[3, 3] <- 10000
      tmp[4, 3] <- 1
      tmp[5, 3] <- 1
      tmp
    },
    GRExg = 0
  )
}

#' @export

# regular economy and pure exchange economy
Example3.14 <- function() {
  sdm(
    A = function(state) {
      alpha <- rbind(1, 1)
      Beta <- matrix(c(
        0.5, 0.75,
        0.5, 0.25
      ), 2, 2, TRUE)
      CD_A(alpha, Beta, state$p)
    },
    B = diag(2),
    S0Exg = matrix(c(
      60, 0,
      0, 100
    ), 2, 2, T),
    GRExg = 0
  )
}

#' @export

# non-sufficient supply of the primary factor
Example4.2 <- function() {
  sdm(
    A = function(state) {
      sigma <- rbind(-1, -1, -1)
      alpha <- rbind(1, 1, 1)
      Beta <- matrix(c(
        0, 1, 1,
        1, 0, 0,
        1, 0, 0
      ), 3, 3, TRUE)
      CES_A(sigma, alpha, Beta, state$p)
    },
    B = diag(3),
    S0Exg = matrix(c(
      NA, NA, NA,
      NA, 100, NA,
      NA, NA, 100
    ), 3, 3, T),
    GRExg = 0
  )
}

#' @export

# increasing returns to scale
Example4.8 <- function() {
  sdm(
    A = function(state) {
      alpha <- rbind(1, 1)
      Beta <- matrix(c(
        0.5, 1,
        0.5, 0
      ), 2, 2, TRUE)
      CD_A(alpha, Beta, state$p) %*% diag(c(state$z[1]^(-1 / 4), 1))
    },
    B = diag(2),
    S0Exg = matrix(c(
      NA, NA,
      NA, 100
    ), 2, 2, TRUE),
    GRExg = 0
  )
}

#' @export

# price signal
Example4.9 <- function() {
  sdm(
    A = function(state) {
      alpha <- rbind(1, 1)
      Beta <- matrix(c(
        0.5, 0.4,
        0.5, 0.6
      ), 2, 2, TRUE)
      CD_A(alpha, Beta, state$p) %*% diag(c(state$z[1]^(-1 / 4), 1))
    },
    B = diag(2),
    S0Exg = matrix(c(
      NA, NA,
      NA, 100
    ), 2, 2, TRUE),
    GRExg = 0
  )
}

#' @export

# tax
Example4.10 <- function() {
  sdm(
    A = function(state) {
      alpha <- rbind(5, 3, 1)
      Beta <- matrix(c(
        0.6, 0.4, 0.2,
        0.1, 0.4, 0.7,
        0.3, 0.2, 0.1,
        0, 0, 0
      ), 4, 3, TRUE)

      tau <- 0.1
      Tax <- matrix(c(
        0, 0, 0,
        0, 0, 0,
        0, 0, 0,
        0, tau / (1 + tau), 0
      ), 4, 3, TRUE)

      CD_A(alpha, Beta, state$p) + state$p[2] * Tax / state$p[4]
    },
    B = matrix(c(
      1, 0, 0,
      0, 1, 0,
      0, 0, 1,
      1, 0, 0
    ), 4, 3, TRUE),
    S0Exg = {
      tmp <- matrix(NA, 4, 3)
      tmp[3, 3] <- 100
      tmp
    },
    GRExg = 0
  )
}

#' @export

Example4.11.1 <- function() {
  sdm(
    A = function(state) {
      tau <- 1
      result <- matrix(NA, 3, 3)
      result[1:2, 1] <- CD_A(1, rbind(0.5, 0.5), state$p[1:2])
      # result[3,1]<-state$p[1]*tau/(1+tau)/state$p[3]
      result[3, 1] <- tau * (state$p[1] * result[1, 1] +
        state$p[2] * result[2, 1]) / state$p[3]

      result[1:2, 2] <- CD_A(1, rbind(0.5, 0.5), state$p[1:2])
      result[3, 2] <- 0

      result[, 3] <- c(1, 0, 0)

      result
    },
    B = matrix(c(
      1, 0, 0,
      0, 1, 1,
      0, 0, 1
    ), 3, 3, TRUE),
    S0Exg = matrix(c(
      NA, NA, NA,
      NA, 100, 100,
      NA, NA, 100
    ), 3, 3, T),
    GRExg = 0
  )
}

#' @export

Example4.11.2 <- function() {
  sdm(
    A = function(state) {
      tau <- 1

      result <- matrix(NA, 3, 3)
      result[1:2, 1] <- CD_A(1, rbind(0.5, 0.5), state$p[1:2])
      result[3, 1] <- 0


      result[1:2, 2] <- CD_A(1, rbind(0.5, 0.5), state$p[1:2])
      result[3, 2] <- (state$p[1] * result[1, 1] + state$p[2] * result[2, 1]) * tau / state$p[3]

      result[, 3] <- c(1, 0, 0)

      result
    },
    B = matrix(c(
      1, 0, 0,
      0, 1, 1,
      0, 0, 1
    ), 3, 3, TRUE),
    S0Exg = matrix(c(
      NA, NA, NA,
      NA, 100, 100,
      NA, NA, 100
    ), 3, 3, T),
    GRExg = 0
  )
}

#' @export

Example4.12 <- function() {
  sdm(
    A = function(state) {
      tau <- 1

      result <- matrix(NA, 3, 3)
      result[1:2, 1] <- CD_A(1, rbind(0.5, 0.5), rbind(state$p[1], state$p[2] * (1 + tau)))
      result[3, 1] <- state$p[2] * result[2, 1] * tau / state$p[3]

      result[1:2, 2] <- CD_A(1, rbind(0.5, 0.5), state$p[1:2])
      result[3, 2] <- 0

      result[, 3] <- c(1, 0, 0)

      result
    },
    B = matrix(c(
      1, 0, 0,
      0, 1, 0,
      0, 0, 100
    ), 3, 3, TRUE),
    S0Exg = matrix(c(
      NA, NA, NA,
      NA, 200, NA,
      NA, NA, 100
    ), 3, 3, T),
    GRExg = 0
  )
}

#' @export

# divident
Example4.13 <- function() {
  sdm(
    A = function(state) {
      r <- 0.25

      result <- matrix(NA, 3, 3)
      result[1:2, 1] <- CD_A(1, rbind(0.5, 0.5), state$p[1:2])
      result[3, 1] <- r * (state$p[1] * result[1, 1] + state$p[2] * result[2, 1]) / state$p[3]

      result[, 2] <- c(1, 0, 0)
      result[, 3] <- c(1, 0, 0)

      result
    },
    B = diag(3),
    S0Exg = matrix(c(
      NA, NA, NA,
      NA, 100, NA,
      NA, NA, 100
    ), 3, 3, T),
    GRExg = 0
  )
}

#' @export

# over-investment
Example4.15 <- function() {
  sdm(
    A = function(state) {
      result <- matrix(NA, 2, 2)
      result[, 1] <- CD_A(1, rbind(0.5, 0.5), state$p)
      result[, 2] <- c(1, 0)

      result
    },
    B = diag(2),
    S0Exg = matrix(c(
      NA, NA,
      75, 25
    ), 2, 2, T),
    GRExg = 0
  )
}

#' @export

# technology monopoly
Example4.16 <- function() {
  sdm(
    A = function(state) {
      alpha <- rbind(1, 1.5)
      Beta <- matrix(c(
        0.5, 0.5,
        0.5, 0.5
      ), 2, 2, TRUE)

      result <- matrix(0, 3, 4)
      result[1:2, 1:2] <- CD_A(alpha, Beta, state$p[1:2])
      result[3, 2] <- 1
      result[1:3, 3] <- result[1:3, 4] <- c(1, 0, 0)
      result
    },
    B = matrix(c(
      1, 1, 0, 0,
      0, 0, 1, 0,
      0, 0, 0, 1
    ), 3, 4, TRUE),
    S0Exg = matrix(c(
      NA, NA, NA, NA,
      NA, NA, 100, NA,
      NA, NA, NA, 99.99
    ), 3, 4, TRUE),
    GRExg = 0
  )
}

#' @export

# fixed assets
Example5.1 <- function() {
  sdm(
    A = matrix(c(
      0.5, 0.9, 0.5, 0.5,
      0.6, 0, 0, 0,
      0, 0, 0.6, 0,
      0, 0, 0, 0.6
    ), 4, 4, TRUE),
    B = matrix(c(
      1, 0, 1, 1,
      0, 1, 0, 0,
      0.6, 0, 0, 0,
      0, 0, 0.6, 0
    ), 4, 4, TRUE),
    priceAdjustmentVelocity = 0.05
  )
}

#' @export

# fixed assets
Example5.2 <- function() {
  sdm(
    A = matrix(c(
      0.5, 0.9,
      0.6, 0
    ), 2, 2, TRUE),
    B = matrix(c(
      1, 0,
      0.4, 1
    ), 2, 2, TRUE)
  )
}

#' @export

# fixed assets
Example5.3.1 <- function() {
  sdm(
    A = matrix(c(
      0.6, 0.4, 1,
      0.1, 0.4, 0,
      0.3, 0.2, 0
    ), 3, 3, TRUE),
    B = diag(3),
    S0Exg = {
      tmp <- matrix(NA, 3, 3)
      tmp[3, 3] <- 100
      tmp
    },
    GRExg = 0
  )
}

#' @export

# fixed assets
Example5.3.2 <- function() {
  GRExg <- 0
  v <- 1 / (2 + GRExg)

  sdm(
    A = matrix(c(
      0.6, 0.4, 1,
      0.1, 0.4, 0,
      0.3, 0.2, 0
    ), 3, 3, TRUE),
    GRExg = GRExg,
    B = matrix(c(
      1, 0, 0,
      0.1 * (1 - v), 1 + 0.4 * (1 - v), 0,
      0, 0, 1
    ), 3, 3, T),
    S0Exg = {
      tmp <- matrix(NA, 3, 3)
      tmp[3, 3] <- 100
      tmp
    }
  )
}

#' @export

# fixed assets
Example5.4 <- function() {
  GRExg <- 0.1
  v <- 1 / (2 + GRExg)

  sdm(
    A = matrix(c(
      0.6, 0.4, 1, 0,
      0, 0, 0, 1 / (1 + GRExg),
      0.3, 0.2, 0, 0,
      0.1, 0.4, 0, 0
    ), 4, 4, T),
    B = matrix(c(
      1, 0, 0, 0,
      0, 1, 0, (1 - v) / (1 + GRExg),
      0, 0, 1, 0,
      0, 0, 0, 1
    ), 4, 4, TRUE),
    S0Exg = {
      tmp <- matrix(NA, 4, 4)
      tmp[3, 3] <- 100
      tmp
    },
    GRExg = GRExg,
    priceAdjustmentVelocity = 0.05
  )
}

#' @export

# fixed assets
Example5.5 <- function() {
  GRExg <- 0.1
  rdm <- 0.1
  rho <- 1 / (1 + GRExg)

  sdm(
    A = function(state) {
      result <- matrix(NA, 6, 5)
      result[1:5, ] <- matrix(c(
        0.6, 0.4, 1, 0, 0,
        0, 0, 0, rho, 0,
        0.3, 0.2, 0, 0, 0,
        0.1, 0.4, 0, 0, 0,
        0, 0, 0, 0, rho
      ), 5, 5, T)
      result[6, 1:2] <- rbind(state$p[1:5]) %*% result[1:5, 1:2] * rdm / state$p[6]
      result[6, 3] <- 0
      result[6, 4] <- (state$p[1:5] %*% result[1:5, 4] - state$p[4] * result[2, 4]) * rdm / state$p[6]
      result[6, 5] <- (state$p[1:5] %*% result[1:5, 5] - state$p[4] * result[5, 5]) * rdm / state$p[6]
      result
    },
    B = matrix(c(
      1, 0, 0, 0, 0,
      0, 1, 0, 0, 0,
      0, 0, 1, 0, 0,
      0, 0, 0, 1, 1,
      0, 0, 0, rho, 0,
      0, 0, 1, 0, 0
    ), 6, 5, TRUE),
    S0Exg = {
      tmp <- matrix(NA, 6, 5)
      tmp[3, 3] <- 100
      tmp[6, 3] <- 100
      tmp
    },
    GRExg = GRExg,
    priceAdjustmentVelocity = 0.05
  )
}

#' @export

# fixed assets
Example5.6 <- function() {
  sdm(
    A = matrix(c(
      0.5, 0.9, 1, 0.5, 0.5,
      0.6, 0, 0, 0, 0,
      0.2, 0.1, 0, 0.4, 0.8,
      0, 0, 0, 0.6, 0,
      0, 0, 0, 0, 0.6
    ), 5, 5, TRUE),
    B = matrix(c(
      1, 0, 0, 1, 1,
      0, 1, 0, 0, 0,
      0, 0, 1, 0, 0,
      0.6, 0, 0, 0, 0,
      0, 0, 0, 0.6, 0.6
    ), 5, 5, TRUE),
    S0Exg = {
      tmp <- matrix(NA, 5, 5)
      tmp[3, 3] <- 100
      tmp
    },
    GRExg = 0,
    priceAdjustmentVelocity = 0.05
  )
}

#' @export

# pollution
Example5.10 <- function() {
  sdm(
    A = function(state) {
      result <- matrix(NA, 3, 3)
      result[, 1] <- c(0.5, 0.5, 0.1)
      result[, 2] <- c(0.1, 0, 0.1)
      result[, 3] <- CD_A(1, rbind(0.5, 0.5, 0), state$p)
      result
    },
    B = diag(3),
    S0Exg = {
      tmp <- matrix(NA, 3, 3)
      tmp[2, 3] <- 30
      tmp[3, 3] <- 3
      tmp
    },
    GRExg = 0
  )
}

#' @export

Example5.11.1 <- function() {
  sdm(
    A = function(state) {
      result <- matrix(NA, 3, 2)
      result[, 1] <- CD_A(1, rbind(0, 0.5, 0.5), state$p)
      result[, 2] <- CD_A(1, rbind(0.5, 0.5, 0), state$p)
      result
    },
    B = matrix(c(
      1, 0,
      0, 10,
      0, 1
    ), 3, 2, TRUE),
    S0Exg = matrix(c(
      NA, NA,
      NA, 30,
      NA, 3
    ), 3, 2, TRUE),
    GRExg = 0
  )
}

#' @export

Example5.11.2 <- function() {
  sdm(
    A = function(state) {
      result <- matrix(NA, 4, 2)
      result[, 1] <- CD_A(1, rbind(0, 0.5, 0.5, 0), state$p)
      result[, 2] <- CD_A(1, rbind(0.5, 0, 0, 0.5), state$p)
      result
    },
    B = matrix(c(
      1, 0,
      0, 4,
      0, 1,
      0, 6
    ), 4, 2, TRUE),
    S0Exg = matrix(c(
      NA, NA,
      NA, 12,
      NA, 3,
      NA, 18
    ), 4, 2, TRUE),
    GRExg = 0
  )
}

#' @export

# two-country economy
Example6.2.1 <- function() { # see also Example6.8
  # column 1: wheat producer of country 1;
  # column 2: iron producer of country 1;
  # column 3: laborer of country 1;
  # column 4: wheat producer of country 2;
  # column 5: iron producer of country 2;
  # column 6: laborer of country 2;
  # row 1: wheat (of country 1 and 2);
  # row 2: iron (of country 1 and 2);
  # row 3: labor of country 1;
  # row 4: labor of country 2;

  sdm(
    A = matrix(c(
      0, 0, 1, 0, 0, 1,
      0, 0, 1, 0, 0, 1,
      0.1, 0.4, 0, 0, 0, 0,
      0, 0, 0, 0.5, 0.5, 0
    ), 4, 6, TRUE),
    B = matrix(c(
      1, 0, 0, 1, 0, 0,
      0, 1, 0, 0, 1, 0,
      0, 0, 1, 0, 0, 0,
      0, 0, 0, 0, 0, 1
    ), 4, 6, TRUE),
    S0Exg = matrix(c(
      NA, NA, NA, NA, NA, NA,
      NA, NA, NA, NA, NA, NA,
      NA, NA, 100, NA, NA, NA,
      NA, NA, NA, NA, NA, 100
    ), 4, 6, TRUE),
    GRExg = 0
  )
}

#' @export

Example6.2.2 <- function() {
  sdm(
    A = matrix(c(
      0, 0, 1,
      0, 0, 1,
      0.5, 0.5, 0
    ), 3, 3, TRUE),
    B = diag(3),
    S0Exg = matrix(c(
      NA, NA, NA,
      NA, NA, NA,
      NA, NA, 100
    ), 3, 3, TRUE),
    GRExg = 0
  )
}

#' @export

Example6.3 <- function() {
  sdm(
    A = matrix(c(
      0, 0, 1, 0, 0, 1,
      0, 0, 1, 0, 0, 1,
      0.1, 0.4, 0, 0, 0, 0,
      0, 0, 0, 0.8, 0.2, 0
    ), 4, 6, TRUE),
    B = matrix(c(
      1, 0, 0, 1, 0, 0,
      0, 1, 0, 0, 1, 0,
      0, 0, 1, 0, 0, 0,
      0, 0, 0, 0, 0, 1
    ), 4, 6, TRUE),
    S0Exg = {
      tmp <- matrix(NA, 4, 6, TRUE)
      tmp[3, 3] <- 100
      tmp[4, 6] <- 100
      tmp
    },
    GRExg = 0
  )
}


#' @export

Example6.4 <- function() {
  sdm(
    A = matrix(c(
      0, 0, 1, 0, 0, 1,
      0, 0, 1, 0, 0, 1,
      0.1, 0.4, 0, 0, 0, 0,
      0, 0, 0, 0.4, 0.1, 0
    ), 4, 6, TRUE),
    B = matrix(c(
      1, 0, 0, 1, 0, 0,
      0, 1, 0, 0, 1, 0,
      0, 0, 1, 0, 0, 0,
      0, 0, 0, 0, 0, 1
    ), 4, 6, TRUE),
    S0Exg = {
      tmp <- matrix(NA, 4, 6, TRUE)
      tmp[3, 3] <- 100
      tmp[4, 6] <- 100
      tmp
    },
    GRExg = 0,
    p0 = rbind(1, 1, 1, 1)
  )
}

#' @export

Example6.5 <- function() {
  sdm(
    A = matrix(c(
      0, 0, 1, 0, 0, 1,
      0, 0, 0.25, 0, 0, 1,
      0.1, 0.4, 0, 0, 0, 0,
      0, 0, 0, 0.8, 0.2, 0
    ), 4, 6, TRUE),
    B = matrix(c(
      1, 0, 0, 1, 0, 0,
      0, 1, 0, 0, 1, 0,
      0, 0, 1, 0, 0, 0,
      0, 0, 0, 0, 0, 1
    ), 4, 6, TRUE),
    S0Exg = {
      tmp <- matrix(NA, 4, 6, TRUE)
      tmp[3, 3] <- 100
      tmp[4, 6] <- 100
      tmp
    },
    GRExg = 0,
    p0 = rbind(1, 1, 1, 1),
    priceAdjustmentVelocity = 0.05
  )
}

#' @export

Example6.6.1 <- function() {
  sdm(
    A = function(state) {
      result <- matrix(NA, 6, 8)
      result[, 1] <- CD_A(1, rbind(0, 0, 0.5, 0.5, 0, 0), state$p)
      result[, 2] <- c(0, 0, 0, 0.5, 0, 0)
      result[, 3] <- c(1, 1, 0, 0, 0, 0)
      result[, 4] <- c(1, 1, 0, 0, 0, 0)
      result[, 5] <- CD_A(1, rbind(0, 0, 0, 0, 0.5, 0.5), state$p)
      result[, 6] <- c(0, 0, 0, 0, 0, 0.5)
      result[, 7] <- c(1, 0.5, 0, 0, 0, 0)
      result[, 8] <- c(1, 0.5, 0, 0, 0, 0)
      result
    },
    B = matrix(c(
      1, 0, 0, 0, 1, 0, 0, 0,
      0, 1, 0, 0, 0, 1, 0, 0,
      0, 0, 1, 0, 0, 0, 0, 0,
      0, 0, 0, 1, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 1, 0,
      0, 0, 0, 0, 0, 0, 0, 1
    ), 6, 8, TRUE),
    S0Exg = {
      tmp <- matrix(NA, 6, 8, TRUE)
      tmp[3, 3] <- 100
      tmp[4, 4] <- 100
      tmp[5, 7] <- 100
      tmp[6, 8] <- 100
      tmp
    },
    GRExg = 0
  )
}

#' @export

# country 1
Example6.6.2 <- function() {
  sdm(
    A = function(state) {
      result <- matrix(NA, 4, 4)
      result[, 1] <- CD_A(1, rbind(0, 0, 0.5, 0.5), state$p)
      result[, 2] <- c(0, 0, 0, 0.5)
      result[, 3] <- c(1, 1, 0, 0)
      result[, 4] <- c(1, 1, 0, 0)
      result
    },
    B = diag(4),
    S0Exg = {
      tmp <- matrix(NA, 4, 4, TRUE)
      tmp[3, 3] <- 100
      tmp[4, 4] <- 100
      tmp
    },
    GRExg = 0
  )
}

#' @export

# country 2
Example6.6.3 <- function() {
  sdm(
    A = function(state) {
      result <- matrix(NA, 4, 4)
      result[, 1] <- CD_A(1, rbind(0, 0, 0.5, 0.5), state$p)
      result[, 2] <- c(0, 0, 0, 0.5)
      result[, 3] <- c(1, 0.5, 0, 0)
      result[, 4] <- c(1, 0.5, 0, 0)
      result
    },
    B = diag(4),
    S0Exg = {
      tmp <- matrix(NA, 4, 4, TRUE)
      tmp[3, 3] <- 100
      tmp[4, 4] <- 100
      tmp
    },
    GRExg = 0
  )
}

#' @export

Example6.7 <- function() {
  sdm(
    A = matrix(c(
      0, 0, 1, 0, 0, 1,
      0.5, 0.5, 0, 0.5, 0.5, 0,
      0.4, 0.2, 0, 0, 0, 0,
      0, 0, 0, 0.2, 0.08, 0
    ), 4, 6, TRUE),
    B = matrix(c(
      1, 0, 0, 1, 0, 0,
      0, 1, 0, 0, 1, 0,
      0, 0, 1, 0, 0, 0,
      0, 0, 0, 0, 0, 1
    ), 4, 6, TRUE),
    S0Exg = {
      tmp <- matrix(NA, 4, 6, TRUE)
      tmp[3, 3] <- 100
      tmp[4, 6] <- 100 # 1e-6
      tmp
    },
    GRExg = 0,
    priceAdjustmentVelocity = 0.05
  )
}

#' @export

Example6.9 <- function() {
  sdm(
    A = function(state) {
      taxRate <- 0.05

      matrix(c(
        0, 0, 1, 0, 0, 0, 1,
        0, 0, 1, 0, 0, 1, 0,
        0.1, 0.4, 0, 0, 0, 0, 0,
        0, 0, 0, 0.5, 0.5, 0, 0,
        0, 0, 0, 0, 0, 1, 0,
        0, 0, 0, 0, 0, 0, state$p[1] * taxRate / state$p[6]
      ), 6, 7, TRUE)
    },
    B = matrix(c(
      1, 0, 0, 0, 0, 0, 0,
      0, 1, 0, 0, 1, 0, 0,
      0, 0, 1, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 1, 0,
      0, 0, 0, 1, 0, 0, 1,
      0, 0, 0, 1, 0, 0, 0
    ), 6, 7, TRUE),
    S0Exg = {
      tmp <- matrix(NA, 6, 7, TRUE)
      tmp[3, 3] <- 100
      tmp[4, 6] <- 100
      tmp
    },
    GRExg = 0,
    priceAdjustmentVelocity = 0.05
  )
}

#' @export

Example6.10 <- function() {
  sdm(
    A = function(state) {
      b <- 2 / 3
      sigma <- rbind(-1, -1, -1, -1)
      alpha <- rbind(1, 1, 1, 1)
      Beta <- matrix(c(
        0, 1, 0, 1,
        b, 0, 0, 0,
        1, 0, 0, 0,
        0, 0, 1, 0
      ), 4, 4, TRUE)
      CES_A(sigma, alpha, Beta, state$p)
    },
    B = diag(4),
    S0Exg = {
      tmp <- matrix(NA, 4, 4, T)
      tmp[2, 2] <- 100
      tmp[4, 4] <- 100
      tmp
    },
    GRExg = 0
  )
}

#' @export

Example6.11 <- function() {
  sdm(
    A = matrix(c(
      0, 0, 1, 0, 0, 1,
      0, 0, 1, 0, 0, 1,
      0.1, 0.4, 0, 0, 0, 0,
      0, 0, 0, 0.5, 0.5, 0
    ), 4, 6, TRUE),
    B = matrix(c(
      1, 0, 0, 1, 0, 0,
      0, 1, 0, 0, 1, 0,
      0, 0, 1, 0, 0, 0,
      0, 0, 0, 0, 0, 1
    ), 4, 6, TRUE),
    S0Exg = {
      tmp <- matrix(NA, 4, 6, TRUE)
      tmp[3, 3] <- 1000
      tmp[4, 6] <- 100
      tmp
    },
    GRExg = 0
  )
}

#' @export
Example6.13 <- function() {
  sdm(
    A = function(state) {
      alpha <- rep(1, 6)
      Beta <- matrix(c(
        0, 0, 1, 0, 0, 1,
        0.5, 0.5, 0, 0.5, 0.5, 0,
        0.5, 0.5, 0, 0, 0, 0,
        0, 0, 0, 0.5, 0.5, 0
      ), 4, 6, TRUE)
      tmpA <- CD_A(alpha, Beta, state$p)
      tmp.z <- ifelse(state$z < 1e-10, 1e-10, state$z)
      tmpA %*% dg(c(tmp.z[1:2]^-0.25, 1, tmp.z[4:5]^-0.25, 1))
    },
    B = matrix(c(
      1, 0, 0, 1, 0, 0,
      0, 1, 0, 0, 1, 0,
      0, 0, 1, 0, 0, 0,
      0, 0, 0, 0, 0, 1
    ), 4, 6, TRUE),
    S0Exg = matrix(c(
      NA, NA, NA, NA, NA, NA,
      NA, NA, NA, NA, NA, NA,
      NA, NA, 100, NA, NA, NA,
      NA, NA, NA, NA, NA, 100
    ), 4, 6, TRUE),
    GRExg = 0,
    maxIteration = 1,
    z0 = c(200, 100, 100, 100, 200, 100), # (ii)
    # z0=c(100,200,100,200,100,100), #(iii)
    priceAdjustmentVelocity = 0.05,
    ts = TRUE,
    policy = function(time, state, state.history) {
      state$S <- ifelse(state$S > 0 & state$S < 1e-10, 1e-10, state$S)
      state
    }
  )
}

#' @export

Example7.1 <- function() {
  sdm(
    A = function(state) {
      alpha <- rbind(1, 1, 1)
      Beta <- matrix(c(
        0.64, 0.4, 0.4,
        0.16, 0.4, 0.4,
        0.2, 0.2, 0.2
      ), 3, 3, TRUE)
      CD_A(alpha, Beta, state$p)
    },
    B = diag(3),
    S0Exg = {
      tmp <- matrix(NA, 3, 3)
      tmp[1, 1] <- 100
      tmp[2, 2] <- 100
      tmp[3, 3] <- 100
      tmp
    },
    GRExg = 0
  )
}

#' @export

Example7.2 <- function() {
  # column 1: wheat producer;
  # column 2: laborer;
  # column 3: money owner;
  # row 1: wheat;
  # row 2: labor;
  # row 3: money;

  sdm(
    A = function(state) {
      moneyIndex <- 3
      moneyOwnerIndex <- 3

      alpha <- rbind(1, 1, 1)
      Beta <- matrix(c(
        0.5, 0.5, 0.5,
        0.5, 0.5, 0.5,
        -1, -1, -1
      ), 3, 3, TRUE)
      CD_mA(alpha, Beta, state$p)
    },
    B = diag(3),

    S0Exg = {
      tmp <- matrix(NA, 3, 3)
      tmp[2, 2] <- 100
      tmp[3, 3] <- 100
      tmp
    },
    GRExg = 0,
    pExg = rbind(NA, NA, 0.25) # (i)
    # pExg=rbind(NA, NA, 1e-6) #(ii)
  )
}

#' @export

Example7.3 <- function() {
  # column 1: wheat producer;
  # column 2: laborer;
  # column 3: money owner;
  # row 1: wheat;
  # row 2: labor;
  # row 3: money;

  sdm(
    A = function(state) {
      moneyIndex <- 3
      moneyOwnerIndex <- 3
      tmpA <- matrix(c(
        0.5, 1, 1,
        0.1, 0, 0,
        -1, -1, -1
      ), 3, 3, TRUE)
      Leontief_mA(tmpA, state$p)
    },
    B = diag(3),
    S0Exg = {
      tmp <- matrix(NA, 3, 3)
      tmp[2, 2] <- 100
      tmp[3, 3] <- 100
      tmp
    },
    GRExg = 0,
    pExg = rbind(NA, NA, 0.25)
  )
}

#' @export

Example7.4 <- function() {
  sdm(
    moneyIndex = 3,
    moneyOwnerIndex = 3,
    A = function(state) {
      alpha <- c(1, 1, 1)
      Beta <- matrix(c(
        0.5, 0.5, 0.5,
        0.5, 0.5, 0.5,
        -1, -1, -1
      ), 3, 3, TRUE)
      CD_mA(alpha, Beta, state$p)
    },
    B = diag(3),
    S0Exg = {
      tmp <- matrix(NA, 3, 3)
      tmp[2, 2] <- 100
      tmp[3, 3] <- 100
      tmp
    },
    GRExg = 0.1,
    pExg = rbind(NA, NA, 0.25)
  )
}


#' @export

Example7.5.1 <- function() {
  r <- rs <- 0.25
  sdm(
    A = function(state) {
      alpha <- rbind(1, 1, 1, 1)
      Beta <- matrix(c(
        0.5, 0.5, 0.5, 0.5,
        0.5, 0.5, 0.5, 0.5,
        -1, -1, -1, -1
      ), 3, 4, TRUE)

      result <- matrix(0, 4, 4)
      result[1:3, ] <- CD_mA(alpha, Beta, state$p[1:3])
      result[4, 1] <- result[3, 1] * (1 + r) * rs / state$p[4]
      result
    },
    B = diag(4),
    S0Exg = {
      tmp <- matrix(NA, 4, 4)
      tmp[2, 2] <- tmp[3, 3] <-
        tmp[4, 4] <- 100
      tmp
    },
    GRExg = 0.1, # (i) 0 (ii) 0.1
    moneyIndex = 3,
    moneyOwnerIndex = 3,
    pExg = rbind(NA, NA, r, NA)
  )
}

#' @export

Example7.5.2 <- function() {
  r <- rs <- 0.1423
  sdm(
    A = function(state) {
      alpha <- rbind(1, 1, 1)
      Beta <- matrix(c(
        0.5, 0.5, 0.5,
        0.5, 0.5, 0.5,
        -1, -1, -1
      ), 3, 3, TRUE)

      result <- matrix(0, 4, 3)
      result[1:3, ] <- CD_mA(alpha, Beta, state$p[1:3])
      result[4, 1] <- result[3, 1] * (1 + r) * rs / state$p[4]
      result
    },
    B = matrix(c(
      1, 0, 0,
      0, 1, 1,
      0, 0, 10,
      0, 0, 10
    ), 4, 3, TRUE),
    S0Exg = matrix(c(
      NA, NA, NA,
      NA, 99, 1,
      NA, NA, 100,
      NA, NA, 100
    ), 4, 3, TRUE),
    GRExg = 0,
    moneyIndex = 3,
    moneyOwnerIndex = 3,
    pExg = rbind(NA, NA, r, NA)
  )
}

#' @export

# foreign exchange rate
Example7.6 <- function() {
  # column 1: wheat producer of country 1;
  # column 2: laborer of country 1;
  # column 3: money owner of country 1;
  # column 4: iron producer of country 2;
  # column 5: laborer of country 2;
  # column 6: money owner of country 2;
  # row 1: wheat (of country 1);
  # row 2: labor of country 1;
  # row 3: money of country 1;
  # row 4: iron (of country 2);
  # row 5: labor of country 2;
  # row 6: money of country 2;

  sdm(
    A = function(state) {
      alpha <- matrix(1, 6, 1)
      Beta <- matrix(c(
        0, 1, 1, 0, 1, 1,
        0.5, 0, 0, 0, 0, 0,
        -1, -1, -1, 0, 0, 0,
        0.5, 0, 0, 0.5, 0, 0,
        0, 0, 0, 0.5, 0, 0,
        0, 0, 0, -1, -1, -1
      ), 6, 6, TRUE)
      CD_mA(alpha, Beta, state$p)
    },
    # A=function(state){CES_mA(-1*ones(6,1),alpha,Beta,state$p)}
    B = diag(6),
    S0Exg = {
      tmp <- matrix(NA, 6, 6)
      tmp[2, 2] <- 100
      tmp[3, 3] <- 600
      tmp[5, 5] <- 100
      tmp[6, 6] <- 100
      tmp
    },
    GRExg = 0,
    moneyOwnerIndex = c(3, 6),
    moneyIndex = c(3, 6),
    pExg = c(NA, NA, 0.1, NA, NA, 0.1)
  )
}

#' @export

# foreign exchange rate
Example7.7 <- function() {
  sdm(
    A = function(state) {
      result <- matrix(NA, 8, 12)
      result[, 1] <- result[, 2] <- CES_mA(-1, 10, rbind(0, 1, 1, -1, 0, 0, 0, 0), state$p)
      result[, 3] <- result[, 4] <- Leontief_mA(rbind(1, 0, 0, -1, 0, 0, 0, 0), state$p)
      result[, 5] <- result[, 6] <- CES_mA(-1, 10, rbind(0, 1, 0, 0, 1, -1, 0, 0), state$p)
      result[, 7] <- result[, 8] <- Leontief_mA(rbind(1, 0, 0, 0, 0, -1, 0, 0), state$p)
      result[, 9] <- result[, 10] <- CES_mA(-1, 10, rbind(0, 1, 0, 0, 0, 0, 1, -1), state$p)
      result[, 11] <- result[, 12] <- Leontief_mA(rbind(1, 0, 0, 0, 0, 0, 0, -1), state$p)
      result
    },
    B = {
      B <- matrix(0, 8, 12)
      B[1, 1] <- B[2, 2] <- B[3, 3] <- B[4, 4] <-
        B[1, 5] <- B[2, 6] <- B[5, 7] <- B[6, 8] <-
        B[1, 9] <- B[2, 10] <- B[7, 11] <- B[8, 12] <- 1
      B
    },
    S0Exg = {
      S0Exg <- matrix(NA, 8, 12)
      S0Exg[3, 3] <- S0Exg[4, 4] <- S0Exg[5, 7] <-
        S0Exg[6, 8] <- S0Exg[7, 11] <- S0Exg[8, 12] <- 100
      S0Exg
    },
    GRExg = 0,
    moneyIndex = rbind(4, 6, 8),
    moneyOwnerIndex = rbind(4, 8, 12),
    # p0=rbind(10,0.08749,0.4031,0.01,0.2448,0.4,0.1611,0.8);
    pExg = rbind(NA, NA, NA, 0.01, NA, 0.4, NA, 0.8)
  )
}


#' @export

# commodity money
Example7.8 <- function() {
  dv <- 0.2
  # (i) rd<-1e-6; rr<-1; tau<-1e-6 #7.8.1
  # (ii) rd<-0; rr<-1;tau<-1; #7.8.2
  # (iii) rd<-0.1; rr<-0.5; tau<-0 #7.9
  rd <- 0.1
  rr <- 0.5
  tau <- 1e-6

  sdm(
    A = function(state) {
      p1 <- state$p[1] / state$p[2]
      p2 <- 1
      p3 <- state$p[3] / state$p[2]
      r <- state$p[4] / state$p[2]
      ps <- state$p[5] / state$p[2]
      pt <- state$p[6] / state$p[2]
      matrix(c(
        0, 0, 1, 0,
        0, 0, 0, 1,
        1, 1, 0, 0,
        p3, p3, p1, dv,
        rd * (1 + tau) * (1 + r) * p3 / ps, rd * (1 + tau) * (1 + r) * p3 / ps, 0, rd * (1 + tau) * (p2 + r * dv) / ps,
        0, tau * (1 + r) * p3 / pt, 0, 0
      ), 6, 4, T)
    },
    B = matrix(c(
      1, 0, 0, 0,
      0, 1, 0, 1 - dv,
      0, 0, 1, 0,
      0, 0, 0, 1 / rr,
      0, 0, 1, 0,
      0, 0, 1, 0
    ), 6, 4, TRUE),
    S0Exg = {
      S0Exg <- matrix(NA, 6, 4)
      S0Exg[3, 3] <- S0Exg[5, 3] <- 100
      S0Exg[6, 3] <- 100
      S0Exg
    },
    GRExg = 0,
    priceAdjustmentVelocity = 0.008,
    p0 = rbind(1, 1, 1, 0.2, 1, 1),
    tolCond = 1e-5
  )
}

#' @export

# positive growth
Example7.9X <- function() {
  GRExg <- 0.01

  dv <- 0.2 # !!!
  money.dv <- 1 - (1 - dv) / (1 + GRExg)
  # (i) rd<-0; rr<-1;tau<-0; #7.8.1
  # (ii) rd<-0; rr<-1;tau<-1; #7.8.2
  # (iii) rd<-0.1; rr<-0.5; tau<-0 #7.9
  rd <- 0.1
  rr <- 0.5
  mm <- 1 / rr

  sdm(
    A = function(state) {
      p1 <- state$p[1] / state$p[2]
      p2 <- 1
      p3 <- state$p[3] / state$p[2]
      r <- state$p[4] / state$p[2]
      ps <- state$p[5] / state$p[2]

      alpha <- rbind(1, 1, 1)
      Beta <- matrix(c(
        0.5, 0.5, 0.5,
        0, 0, 0,
        0.5, 0.5, 0.5,
        -1, -1, -1
      ), 4, 3, TRUE)
      result <- matrix(0, 5, 4)
      result[1:4, 1:3] <- CD_mA(alpha, Beta, c(p1, p2, p3, r))
      result[1:4, 4] <- c(0, 1, 0, money.dv)
      result[5, 1:4] <- c(
        rd * (1 + r) * p3 / ps,
        rd * (1 + r) * p3 / ps, 0, rd * (1 + r * money.dv) / ps
      )
      result
    },
    B = matrix(c(
      1, 0, 0, 0,
      0, 1, 0, 1 - dv,
      0, 0, 1, 0,
      0, 0, 0, mm,
      0, 0, 1, 0
    ), 5, 4, TRUE),
    S0Exg = {
      S0Exg <- matrix(NA, 5, 4)
      S0Exg[3, 3] <- S0Exg[5, 3] <- 100
      S0Exg
    },
    GRExg = GRExg,
    priceAdjustmentVelocity = 0.1,
    p0 = rbind(1, 1, 1, 0.2, 1),
    tolCond = 1e-5
  )
}

#' @export

Example7.10 <- function() {
  GRExg <- 0
  rd <- 0
  rr <- 0.5
  rfm <- 0.05

  sdm(
    A = function(state) {
      matrix(c(
        0, 1, 1, 0,
        0, 0, 0, rr,
        1, 0, 0, 0,
        state$p[3],
        state$p[1],
        state$p[1],
        0
      ), 4, 4, T)
    },
    B = matrix(c(
      1, 0, 0, 0,
      0, 1, 0, 0,
      0, 0, 1, 0,
      0, 0, 0, 1
    ), 4, 4, TRUE),
    S0Exg = {
      S0Exg <- matrix(NA, 4, 4)
      S0Exg[3, 3] <- 100
      S0Exg[2, 2] <- 100
      S0Exg
    },
    GRExg = GRExg,
    moneyIndex = 2,
    moneyOwnerIndex = 2,
    pExg = rbind(NA, rfm, NA, NA),
    priceAdjustmentVelocity = 0.01
  )
}

#' @export

Example7.10.2 <- function() {
  rd <- 0
  rr <- 0.5
  rfm <- 0.05
  mm <- 1 / rr

  sdm(
    A = function(state) {
      matrix(c(
        0, 1, 1,
        state$p[3] / mm, state$p[1] / mm, state$p[1] / mm,
        1, 0, 0
      ), 3, 3, T)
    },
    B = matrix(c(
      1, 0, 0,
      0, 1, 0,
      0, 0, 1
    ), 3, 3, TRUE),
    S0Exg = {
      S0Exg <- matrix(NA, 3, 3)
      S0Exg[3, 3] <- 100
      S0Exg[2, 2] <- 100
      S0Exg
    },
    GRExg = 0,
    moneyIndex = 2,
    moneyOwnerIndex = 2,
    pExg = rbind(NA, rfm, NA),
    priceAdjustmentVelocity = 0.01
  )
}

#' @export

# Bond
Example7.11 <- function() {
  r <- 0.1
  labor.input <- 0

  sdm(
    A = function(state) {
      matrix(c(
        0, 1, 1, 1, # wheat
        1, 0, 0, 0, # labor
        0, 0, (1 + r) * state$p[1] / state$p[3], 0,

        state$p[2], state$p[1], state$p[1], state$p[1]
      ), 4, 4, T)
    },
    B = matrix(c(
      1, 0, 0, 0,
      0, 1, 1, 0,
      0, 1, 0, 0,
      0, 0, 0, 100
    ), 4, 4, TRUE),
    S0Exg = {
      S0Exg <- matrix(NA, 4, 4)
      S0Exg[2, 2] <- S0Exg[2, 3] <- S0Exg[4, 4] <- 100
      S0Exg[3, 2] <- 100
      S0Exg
    },
    GRExg = 0,
    moneyIndex = 4,
    moneyOwnerIndex = 4,
    pExg = rbind(NA, NA, NA, r),
    priceAdjustmentVelocity = 0.01
  )
}

#' @export

# exchange rate and international credit
Example7.12 <- function() {
  sdm(
    A = function(state) {
      alpha <- matrix(1, 6, 1)
      Beta <- matrix(c(
        0, 1, 1, 0, 1, 0,
        0.5, 0, 0, 0, 0, 0,
        -1, -1, -1, 0, 0, 0,
        0.5, 0, 0, 0.5, 0, 0,
        0, 0, 0, 0.5, 0, 0,
        0, 0, 0, -1, -1, 0,
        0, 0, 0, 0, 0, 1
      ), 7, 6, TRUE)
      CD_mA(alpha, Beta, state$p)
    },
    B = {
      B <- diag(6)
      B <- rbind(B, c(0, 0, 1, 0, 0, 0))
      B
    },
    S0Exg = {
      S0Exg <- matrix(NA, 7, 6)
      S0Exg[2, 2] <- 100
      S0Exg[3, 3] <- 600
      S0Exg[5, 5] <- 100
      S0Exg[6, 6] <- 100
      S0Exg[7, 3] <- 1
      S0Exg
    },
    GRExg = 0,
    moneyOwnerIndex = rbind(3, 6),
    moneyIndex = rbind(3, 6),
    pExg = rbind(NA, NA, 0.1, NA, NA, 0.1) # !!!!!!!!
  )
}

#' @export

# bank
Example7.13 <- function() {
  r <- 0.1
  rr <- 0.2 # (i) 0 (ii) 0.2
  labor.input <- 0

  sdm(
    A = function(state) {
      matrix(c(
        0, 1, 1, 1, 0, # wheat
        1, 0, 0, 0, 0, # labor
        0, 0, 0, 0, 1, # deposit
        0, 0, (1 + r) * state$p[1] / state$p[4], 0, 0, # credit
        # money
        state$p[2], state$p[1], state$p[1], state$p[1], 0,
        # reserve
        0, 0, 0, 0, rr * state$p[3] / state$p[6]
      ), 6, 5, T)
    },
    B = matrix(c(
      1, 0, 0, 0, 0,
      0, 1, 1, 0, 0,
      0, 1, 0, 0, 0,
      0, 0, 0, 0, 1,
      0, 0, 0, 100, 0,
      0, 0, 0, 1, 0
    ), 6, 5, TRUE),
    S0Exg = {
      S0Exg <- matrix(NA, 6, 5)
      S0Exg[2, 2] <- S0Exg[2, 3] <- S0Exg[5, 4] <- 100
      S0Exg[3, 2] <- 100
      S0Exg[6, 4] <- 1
      S0Exg
    },
    GRExg = 0,
    moneyIndex = 5,
    moneyOwnerIndex = 4,
    pExg = rbind(NA, NA, NA, NA, r, NA),
    priceAdjustmentVelocity = 0.05
  )
}

#' @export

# shadow price
Example7.14 <- function() {
  sdm(
    A = function(state) {
      alpha <- rbind(1, 1)
      Beta <- matrix(c(
        0.5, 0.5,
        0.5, 0.5,
        -1, -1
      ), 3, 2, TRUE)
      CD_mA(alpha, Beta, state$p)
    },
    B = matrix(c(
      1, 0,
      0, 100,
      0, 100
    ), 3, 2, T),
    S0Exg = {
      S0Exg <- matrix(NA, 3, 2)
      S0Exg[2, 2] <- 100
      S0Exg[3, 2] <- 100
      # S0Exg[1,2]<- 10
      S0Exg
    },
    GRExg = 0,
    pExg = rbind(NA, NA, 0.25),
    moneyIndex = 3,
    moneyOwnerIndex = 2
  )
}

#' @export

# shadow price and international trade
Example7.15 <- function() {
  sdm(
    A = function(state) {
      alpha <- rbind(1, 1, 1, 1)
      Beta <- matrix(c(
        0.5, 0.5, 0.5, 0.5,
        0.5, 0.5, 0.5, 0.5,
        -1, -1, 0, 0,
        0, 0, -1, -1
      ), 4, 4, TRUE)
      CD_mA(alpha, Beta, state$p)
    },
    B = matrix(c(
      1, 0, 1, 0,
      0, 100, 0, 100,
      0, 100, 0, 0,
      0, 0, 0, 100
    ), 4, 4, T),
    S0Exg = matrix(c(
      NA, NA, NA, NA,
      NA, 100, NA, 100,
      NA, 100, NA, NA,
      NA, NA, NA, 100
    ), 4, 4, T),
    GRExg = 0,
    moneyIndex = rbind(3, 4),
    moneyOwnerIndex = rbind(2, 4),
    pExg = rbind(NA, NA, 0.25, 0.1)
  )
}

#' @export

# equilibrium coffee problem
Example8.1 <- function() {
  sdm(
    A = matrix(c(
      0.05, 0.05, 0.1,
      0.1, 0, 0.1,
      0, 0.15, 0.05
    ), 3, 3, TRUE),
    B = matrix(0, 3, 3),
    S0Exg = diag(3),
    GRExg = 0
  )
}

#' @export

Example8.2 <- function() {
  S0Exg <- diag(c(100, 60, 100))

  sdm(
    A = function(state) {
      alpha <- c(5, 3, 1)
      Beta <- matrix(c(
        0.6, 0.4, 0.2,
        0.1, 0.4, 0.7,
        0.3, 0.2, 0.1
      ), 3, 3, TRUE)
      CD_A(alpha, Beta, state$p)
    },
    B = S0Exg,
    S0Exg = S0Exg,
    GRExg = 0
  )
}

#' @export

Example8.7 <- function() {
  ge <- sdm(
    A = matrix(c(
      0.05, 0.05, 0.1,
      0.1, 0, 0.1,
      0, 0.15, 0.05
    ), 3, 3, TRUE),
    B = matrix(0, 3, 3),
    S0Exg = diag(3),
    GRExg = 0,
    p0 = rbind(1, 1, 1),
    numberOfPeriods = 300,
    maxIteration = 1,
    ts = TRUE
  )
}

#' @export

Example8.8 <- function() {
  ge <- sdm(
    A = function(state) {
      tmpA <- matrix(c(
        0.05, 0.05, 0.1, 0.1,
        0.1, 0, 0.1, 0.1,
        0, 0.15, 0.05, 0.1,
        -1, -1, -1, -1
      ), 4, 4, TRUE)
      Leontief_mA(tmpA, state$p)
    },
    B = matrix(0, 4, 4),
    S0Exg = diag(4),
    GRExg = 0,
    moneyOwnerIndex = 4,
    moneyIndex = 4,
    depreciationCoef = 0,
    maxIteration = 1,
    numberOfPeriods = 800,
    p0 = rbind(0.5, 0.5, 0.5, 0.25),
    pExg = rbind(NaN, NaN, NaN, 0.25),
    ts = TRUE
  )
}

#' @export

Example8.9 <- function() {
  ge <- sdm(
    A = function(state) {
      tmpA <- matrix(c(
        0.05, 0.05, 0.1, 0.1, 0.1,
        0.1, 0, 0.1, 0.1, 0.1,
        0, 0.15, 0.05, 0.1, 0.1,
        -1, -1, 0, -1, 0,
        0, 0, -1, 0, -1
      ), 5, 5, TRUE)
      Leontief_mA(tmpA, state$p)
    },
    B = matrix(0, 5, 5),
    S0Exg = {
      S0Exg <- diag(5)
      S0Exg[4, 4] <- 3
      S0Exg
    },
    GRExg = 0,
    moneyOwnerIndex = c(4, 5),
    moneyIndex = c(4, 5),
    depreciationCoef = 0,
    maxIteration = 1,
    numberOfPeriods = 800,
    p0 = rbind(0.5, 0.5, 0.5, 0.25, 0.25),
    pExg = rbind(NaN, NaN, NaN, 0.25, 0.25),
    ts = TRUE
  )
}

#' @export

Example9.3 <- function() {
  ge <- sdm(
    A = matrix(c(
      56 / 115, 6,
      12 / 575, 2 / 5
    ), 2, 2, TRUE),
    B = diag(2),
    maxIteration = 1,
    numberOfPeriods = 100,
    p0 = rbind(1 / 15, 1),
    z0 = rbind(575, 20),
    thresholdForPriceAdjustment = 0.99,
    priceAdjustmentMethod = "fixed",
    priceAdjustmentVelocity = 0.02,
    ts = TRUE
  )
}

#' @export

Example9.4 <- function() {
  sdm(
    A = matrix(c(
      56 / 115, 6,
      12 / 575, 2 / 5
    ), 2, 2, TRUE),
    B = diag(2),
    S0Exg = matrix(c(
      NA, NA,
      NA, 190
    ), 2, 2, T),
    GRExg = 0,
    maxIteration = 1,
    numberOfPeriods = 1000,
    p0 = rbind(12 / 295, 1),
    z0 = rbind(3400, 90),
    thresholdForPriceAdjustment = 0.99,
    priceAdjustmentMethod = "variable",
    priceAdjustmentVelocity = 0.2,
    ts = TRUE
  )
}

#' @export

Example9.5 <- function() {
  sdm(
    A = function(state) {
      alpha <- rbind(1, 1, 1)
      Beta <- matrix(c(
        0, 1, 1,
        0.5, 0, 0,
        0.5, 0, 0
      ), 3, 3, TRUE)
      CD_A(alpha, Beta, state$p)
    },
    B = diag(3),
    S0Exg = matrix(c(
      NA, NA, NA,
      NA, 100, NA,
      NA, NA, 100
    ), 3, 3, T),
    GRExg = 0,
    pExg = rbind(1, NA, 0.625), # (i)
    # pExg=rbind(1, 0.25, 0.25)#(ii)
    maxIteration = 1,
    numberOfPeriods = 200,
    depreciationCoef = 0,
    ts = TRUE
  )
}

#' @export

Example9.6 <- function() {
  sdm(
    A = function(state) {
      alpha <- rbind(1.2, 1)
      Beta <- matrix(c(
        0.5, 1,
        0.5, 0
      ), 2, 2, TRUE)
      CD_A(alpha, Beta, state$p)
    },
    B = diag(2),
    S0Exg = matrix(c(
      NA, NA,
      NA, 100
    ), 2, 2, T),
    GRExg = 0,
    numberOfPeriods = 100,
    z0 = rbind(50, 50),
    p0 = rbind(1, 0.25),
    depreciationCoef = 1,
    # depreciationCoef=0.5
    ts = TRUE
  )
}

#' @export

Example9.7 <- function() {
  sdm(
    A = matrix(c(
      0, 0, 1,
      0.4, 0, 0,
      0.4, 1, 0
    ), 3, 3, TRUE),
    B = matrix(c(
      1, 0, 0,
      0.36, 1, 0,
      0, 0, 1
    ), 3, 3, TRUE),
    S0Exg = {
      S0Exg <- matrix(NA, 3, 3, TRUE)
      S0Exg[3, 3] <- 100
      S0Exg
    },
    GRExg = 0,
    numberOfPeriods = 800,
    maxIteration = 1,
    priceAdjustmentVelocity = 0.05,
    ts = TRUE
  )
}


#' @export

Example9.10 <- function(policy = NULL, pExg = rbind(NA, NA, 0.25),
                        p0 = rbind(0.625, 0.375, 0.25), priceAdjustmentVelocity = 0.3,
                        ts = TRUE) {
  sdm(
    A = function(state) {
      tmpA <- matrix(c(
        0.5, 0.5, 0.5,
        0.5, 0.5, 0.5,
        -1, -1, -1
      ), 3, 3, TRUE) #-1 denotes the demand for money
      Leontief_mA(tmpA, state$p)
    },
    B = diag(3),
    S0Exg = {
      S0Exg <- matrix(NA, 3, 3)
      S0Exg[2, 2] <- 100
      S0Exg[3, 3] <- 100
      S0Exg
    },
    GRExg = 0,
    moneyIndex = 3,
    moneyOwnerIndex = 3,
    pExg = pExg,
    p0 = p0,
    z0 = rbind(95, 100, 100),
    thresholdForPriceAdjustment = 0.99,
    priceAdjustmentVelocity = priceAdjustmentVelocity,
    numberOfPeriods = 1000,
    maxIteration = 1,
    trace = FALSE,
    ts = ts,
    policy = policy
  )
}



#' @export

Example9.10.policy.interest.rate <- function(time, state, state.history) {
  if (time >= 600) {
    upsilon <- state.history$z[time - 1, 1] / mean(state.history$z[(time - 50):(time - 1), 1])
    state$p[3] <- max(0.25 + 0.5 * log(upsilon), 0)
  }
  state
}

#' @export

Example9.10.policy.money.supply <- function(time, state, state.history) {
  if (time >= 600) {
    upsilon <- state.history$z[time - 1, 1] / mean(state.history$z[(time - 50):(time - 1), 1])
    state$S[3, 3] <- state.history$S[3, 3, time - 1] * (1 - 0.5 * log(upsilon))
  }
  state
}

#' @export
Example9.10.policy.tax <- function(time, state, state.history) {
  if (time >= 600) {
    upsilon <- state.history$z[time - 1, 1] / mean(state.history$z[(time - 50):(time - 1), 1])

    tau <- 0
    if (upsilon > 1) {
      tau <- min((upsilon - 1) / 2, 0.2)
      state$S[1, 1] <- state$S[1, 1] * (1 - tau)
    }

    state$current.policy.data <- data.frame(time = time, tau.Example9.10 = tau)
  }

  state
}

#' @export

Example9.10.policy.deflation <- function(time, state, state.history) {
  if (time >= 600) {
    upsilon <- state.history$z[time - 1, 1] / mean(state.history$z[(time - 50):(time - 1), 1])
    zeta.mu <- ifelse(upsilon > 1, 0.5, 0)
    state$S[3, 3] <- state.history$S[3, 3, time - 1] * (1 - zeta.mu * log(upsilon))
  }
  state
}

#' @export

Example9.10.policy.quantitative.easing <- function(time, state, state.history) {
  if (time >= 600) {
    upsilon <- state.history$z[time - 1, 1] / mean(state.history$z[(time - 50):(time - 1), 1])
    zeta.mu <- ifelse(upsilon > 1, 0, 0.5)
    state$S[3, 3] <- state.history$S[3, 3, time - 1] * (1 - zeta.mu * log(upsilon))
  }
  state
}


#' @export

Example9.10.policy.deficit.fiscal <- function(time, state, state.history) {
  if (time >= 400) {
    current.deficit <- 0
    if (state.history$q[time - 1, 1] < 0.95) {
      state$S[1, 1] <- state$S[1, 1] * 0.96

      current.deficit <- state$S[1, 1] * 0.04 * state$p[1]
      state$S[3, 3] <- state$S[3, 3] + current.deficit
    }

    state$current.policy.data <-
      data.frame(time = time, deficit.Example9.10 = current.deficit)
  }
  state
}
