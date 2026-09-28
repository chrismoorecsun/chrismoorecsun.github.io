# Simulate some time series
  # Set up in ode()
    dlogistic <- function(time, state, pars) {
      with(as.list(c(state, pars)), {
        dN <- N + rd*N*(1 - alpha*N)
        return(list(c(dN)))
      })
    }

  yini  <- c(N = 10)
  times <- 0:20

  rd_vec <- seq(from = 1.3, to = 2.8, by = 0.3)
  
  pars1  <- c(rd = rd_vec[1], alpha = 0.01)
  pars2  <- c(rd = rd_vec[2], alpha = 0.01)
  pars3  <- c(rd = rd_vec[3], alpha = 0.01)
  pars4  <- c(rd = rd_vec[4], alpha = 0.01)
  pars5  <- c(rd = rd_vec[5], alpha = 0.01)
  pars6  <- c(rd = rd_vec[6], alpha = 0.01)

out1   <- ode(y = yini, times = times, func = dlogistic, parms = pars1, method = "iteration")
out2   <- ode(y = yini, times = times, func = dlogistic, parms = pars2, method = "iteration")
out3   <- ode(y = yini, times = times, func = dlogistic, parms = pars3, method = "iteration")
out4   <- ode(y = yini, times = times, func = dlogistic, parms = pars4, method = "iteration")
out5   <- ode(y = yini, times = times, func = dlogistic, parms = pars5, method = "iteration")
out6   <- ode(y = yini, times = times, func = dlogistic, parms = pars6, method = "iteration")

par(mfrow = c(2, 3), mar = c(4.5, 4, 1, 0.5)) # mar changes the amount of space below, left, top, and right of each plot
plot(x = out1[,1], y = out1[,2], xlab = "Time", ylab = "Density", type = "l", las = 1, ylim = c(0, 125), lwd = 1.5)
plot(x = out2[,1], y = out2[,2], xlab = "Time", ylab = "Density", type = "l", las = 1, ylim = c(0, 125), lwd = 1.5)
plot(x = out3[,1], y = out3[,2], xlab = "Time", ylab = "Density", type = "l", las = 1, ylim = c(0, 125), lwd = 1.5)
plot(x = out4[,1], y = out4[,2], xlab = "Time", ylab = "Density", type = "l", las = 1, ylim = c(0, 125), lwd = 1.5)
plot(x = out5[,1], y = out5[,2], xlab = "Time", ylab = "Density", type = "l", las = 1, ylim = c(0, 125), lwd = 1.5)
plot(x = out6[,1], y = out6[,2], xlab = "Time", ylab = "Density", type = "l", las = 1, ylim = c(0, 125), lwd = 1.5)

# Simulate discrete logisic over many r_d values

  # Create a vector of r_d
  n_rd <- 10000
  rd_vec <- seq(from = 1.5, to = 3.0, length.out = n_rd)

  # Create a time vector from 0 to 75
  n_steps <- 75
  times <- 1:n_steps
  
  # Create a matrix into which we save out logistic output
  N_mat <- matrix(data = NA, nrow = n_steps, ncol = n_rd)
  
  # Create parameter vector
  par_vec  <- c(rd = NA, alpha = 0.01)

  # Find numerical solutions while varying r_d
  for (i in 1:n_rd) {
    # 1. Extract the ith r_d
      rd_i <- rd_vec[i]
    # 2. Put into parameter vec
      par_vec["rd"] <- rd_i
    # 3. Run ode()
      out <- ode(y = yini, times = times, func = dlogistic, parms = par_vec, method = "iteration")
    # 4. Save into out matrix
      N_mat[, i] <- out[,2]
    }
  image(N_mat, y = rd_vec, x = times, las = 1)
  abline(v = 65, lwd = 2)

# Birucation diagram
  # With only the LAST time step
    plot(x = NA, type = "n", xlim = c(0.3, 3), ylim = c(0, 135), las = 1, xlab = "r_d", ylab = "N (last 10 steps)")

  points(x = rd_vec, y = N_mat[75,], pch = 16)
  
  # With last 10 time steps
  plot(x = NA, type = "n", xlim = c(1.5, 3), ylim = c(0, 135), las = 1, xlab = "r_d", ylab = "N (last 10 steps)")
  
  for (i in 46:75) {
    points(x = rd_vec, y = N_mat[i,], pch = 16, cex = 0.1, col = "#00000033")
  }
  
  
  

  
  
  
  
  
    
  
  
  
  