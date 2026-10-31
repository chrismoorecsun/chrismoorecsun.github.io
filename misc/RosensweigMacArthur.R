################################################################################
# Rozensweig-MacArthur consumer-resource model
################################################################################

# Load libraries
  library(package = "deSolve")
  library(package = "phaseR")

# Set up numerical solver ingredients
  # Function
  RM <- function(t, y, parms) {
    with(as.list(c(y, parms)), {
      TIIFR <- a*N/(1 + a*h*N)
      
      dN <- r*N - alpha*N*N - TIIFR*P
      dP <- e*TIIFR*P - m*P
      
      return(list(c(dN, dP)))
    })
  }
  
  # Vectors
  time_vec <- seq(from = 0, to = 500, length.out = 1e4)
  init_vec <- c(N = 2, P = 2)
  parm_vec <- c(r = 1,
                alpha = 0.1,
                a = 1,
                h = 0.5,
                e = 0.5,
                m = 0.84)

# Create phase planes and time series
  # Time series
  parm_vec_2 <- parm_vec
  parm_vec_3 <- parm_vec
  parm_vec_4 <- parm_vec
  parm_vec_2["m"] <- 0.8
  parm_vec_3["m"] <- 0.7
  parm_vec_4["m"] <- 0.6
    
  
  ts_1 <- ode(y = init_vec, times = time_vec, parms = parm_vec, func = RM)
  ts_2 <- ode(y = init_vec, times = time_vec, parms = parm_vec_2, func = RM)
  ts_3 <- ode(y = init_vec, times = time_vec, parms = parm_vec_3, func = RM)
  ts_4 <- ode(y = init_vec, times = time_vec, parms = parm_vec_4, func = RM)
  
  # Plots/phase planes
  lims <- c(0, 11)
  
  # Using par() to set the (mar)gin space and the number of rows and columns (mfrow)
  par(mfrow = c(4, 2), mar = c(4.5, 4.5, 0.5, 0.5))
  
  plot(x = ts_1[,1], y = ts_1[,2], type = "l", ylim = lims, xlab = "Time", ylab = "Density", col = "#30d5c8", lwd = 2, las = 1)
    lines(x = ts_1[,1], y = ts_1[,3], lwd = 2, col = "gold")
  
  plot(x = lims, y = lims, type = "n", las = 1, xlab = "Prey (N)", ylab = "Predator (P)")
  trash <- flowField(deriv = RM,
            xlim = lims,
            ylim = lims,
            parameters = parm_vec,
            state.names = c("N", "P"))
  trash <- nullclines(deriv = RM,
                     xlim = c(-0.1, lims[2]),
                     ylim = c(-0.1, lims[2]),
                     parameters = parm_vec,
                     state.names = c("N", "P"),
                     lwd = 2,
                     col = c("#30d5c8", "gold"),
                     add.legend = FALSE)
  lines(x = ts_1[,2], y = ts_1[,3], lwd = 2)
  points(x = init_vec[1], y = init_vec[2], pch = 16)

  ## Second m value
    plot(x = ts_2[,1], y = ts_2[,2], type = "l", ylim = lims, xlab = "Time", ylab = "Density", col = "#30d5c8", lwd = 2, las = 1)
    lines(x = ts_2[,1], y = ts_2[,3], lwd = 2, col = "gold")
    
    plot(x = lims, y = lims, type = "n", las = 1, xlab = "Prey (N)", ylab = "Predator (P)")
    trash <- flowField(deriv = RM,
                       xlim = lims,
                       ylim = lims,
                       parameters = parm_vec_2,
                       state.names = c("N", "P"))
    trash <- nullclines(deriv = RM,
                        xlim = c(-0.1, lims[2]),
                        ylim = c(-0.1, lims[2]),
                        parameters = parm_vec_2,
                        state.names = c("N", "P"),
                        lwd = 2,
                        col = c("#30d5c8", "gold"),
                        add.legend = FALSE)
    lines(x = ts_2[,2], y = ts_2[,3], lwd = 2)
    points(x = init_vec[1], y = init_vec[2], pch = 16)
  ## Third m value
    plot(x = ts_3[,1], y = ts_3[,2], type = "l", ylim = lims, xlab = "Time", ylab = "Density", col = "#30d5c8", lwd = 2, las = 1)
    lines(x = ts_3[,1], y = ts_3[,3], lwd = 2, col = "gold")
    
    plot(x = lims, y = lims, type = "n", las = 1, xlab = "Prey (N)", ylab = "Predator (P)")
    trash <- flowField(deriv = RM,
                       xlim = lims,
                       ylim = lims,
                       parameters = parm_vec_3,
                       state.names = c("N", "P"))
    trash <- nullclines(deriv = RM,
                        xlim = c(-0.1, lims[2]),
                        ylim = c(-0.1, lims[2]),
                        parameters = parm_vec_3,
                        state.names = c("N", "P"),
                        lwd = 2,
                        col = c("#30d5c8", "gold"),
                        add.legend = FALSE)
    lines(x = ts_3[,2], y = ts_3[,3], lwd = 2)
    points(x = init_vec[1], y = init_vec[2], pch = 16)
  ## Fourth m value
    plot(x = ts_4[,1], y = ts_4[,2], type = "l", ylim = lims, xlab = "Time", ylab = "Density", col = "#30d5c8", lwd = 2, las = 1)
    lines(x = ts_4[,1], y = ts_4[,3], lwd = 2, col = "gold")
    
    plot(x = lims, y = lims, type = "n", las = 1, xlab = "Prey (N)", ylab = "Predator (P)")
    trash <- flowField(deriv = RM,
                       xlim = lims,
                       ylim = lims,
                       parameters = parm_vec_4,
                       state.names = c("N", "P"))
    trash <- nullclines(deriv = RM,
                        xlim = c(-0.1, lims[2]),
                        ylim = c(-0.1, lims[2]),
                        parameters = parm_vec_4,
                        state.names = c("N", "P"),
                        lwd = 2,
                        col = c("#30d5c8", "gold"),
                        add.legend = FALSE)
    lines(x = ts_4[,2], y = ts_4[,3], lwd = 2)
    points(x = init_vec[1], y = init_vec[2], pch = 16)