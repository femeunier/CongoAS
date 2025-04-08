match.distribution <- function(data1 = rnorm(1000, mean = 2, sd = 1),
                               data2 = rnorm(1000, mean = 3, sd = 1),
                               plot = FALSE){

  density1 <- density(data1)
  density2 <- density(data2)

  # Interpolate density2 to match the x-values of density1
  density2_interp <- approx(density2$x, density2$y, xout = density1$x, rule = 2)$y

  # Compute weighting function
  weights_interp <- density1$y / (density2_interp + 1e-10)  # Avoid division by zero

  # Map weights to the actual data2 points
  weights <- approx(density1$x, weights_interp, xout = data2, rule = 2)$y

  # Ensure all weights are positive and normalize them
  weights[weights < 0] <- 0
  weights <- weights / sum(weights)

  if (plot){

    # Resample data2 using computed weights
    resampled_data2 <- sample(data2, size = length(data2), replace = TRUE, prob = weights)

    # Compute density of resampled data
    density_resampled <- density(resampled_data2)

    # Create dataframes for plotting
    df1 <- data.frame(x = density1$x, y = density1$y, group = "Target Density")
    df2 <- data.frame(x = density2$x, y = density2$y, group = "Original Density")
    df3 <- data.frame(x = density_resampled$x, y = density_resampled$y, group = "Weighted Density")

    df_plot <- rbind(df1, df2, df3)

    # Plot the original, target, and weighted densities
    g <- ggplot(df_plot, aes(x = x, y = y, color = group)) +
      geom_line(size = 1) +
      theme_minimal() +
      labs(title = "Density Matching using Weights", x = "Value", y = "Density")

    print(g)
  }

  return(weights)
}


