#' Drug Transport and Release Modeling
#'
#' Models for drug transport mechanisms and release kinetics including:
#' - Diffusion-controlled release
#' - Convection and electric field-driven transport
#' - Effective medium theory for porous materials
#' - Percolation theory for disordered systems
#' - Drug release kinetics from various formulations
#'
#' Course: Molecular Physical Pharmacy (3FC003)

#' @title DiffusionModel
#' @description Model for diffusion-controlled drug release
#' @param temperature Temperature in Kelvin (default 298.15)
#' @return A list with diffusion model methods
DiffusionModel <- function(temperature = 298.15) {
  kB <- 1.380649e-23  # J/K
  N_A <- 6.02214076e23  # mol^-1
  
  list(
    T = temperature,
    kB = kB,
    N_A = N_A,
    
    #' Calculate diffusion flux using Fick's first law
    #' @param diffusion_coefficient Diffusion coefficient in m^2/s
    #' @param concentration_gradient Concentration gradient in mol/(m^4)
    #' @return Flux in mol/(m^2*s)
    ficks_first_law = function(diffusion_coefficient, concentration_gradient) {
      return(-diffusion_coefficient * concentration_gradient)
    },
    
    #' Solve Fick's second law in 1D for a slab geometry
    #' @param diffusion_coefficient Diffusion coefficient in m^2/s
    #' @param initial_concentration Initial uniform concentration in mol/m^3
    #' @param length Length of the slab in meters
    #' @param time_points Array of time points in seconds
    #' @return List with time_points, concentration_profiles, and x_values
    ficks_second_law_1d = function(diffusion_coefficient, initial_concentration, 
                                  length, time_points) {
      # Analytical solution for 1D diffusion from a slab
      # C(x,t) = C_0 * sum_{n=0}^inf [4/(2n+1)pi * sin((2n+1)pi x/L) * exp(-D(2n+1)^2 pi^2 t/L^2)]
      
      x_vals <- seq(0, length, length.out = 50)
      concentration_profiles <- list()
      
      for (t in time_points) {
        C <- numeric(length(x_vals))
        for (n in 0:9) {  # Sum first 10 terms
          term <- (4 / ((2*n + 1) * pi)) * 
                  sin((2*n + 1) * pi * x_vals / length) * 
                  exp(-diffusion_coefficient * (2*n + 1)^2 * pi^2 * t / length^2)
          C <- C + term
        }
        concentration_profiles[[length(concentration_profiles) + 1]] <- initial_concentration * C
      }
      
      return(list(time_points = time_points, 
                  concentration_profiles = concentration_profiles, 
                  x_values = x_vals))
    },
    
    #' Calculate diffusion coefficient using Stokes-Einstein equation
    #' @param radius Particle radius in meters
    #' @param viscosity Viscosity in Pa*s (water at 25C = 0.00089 Pa*s)
    #' @param temperature Temperature in Kelvin
    #' @return Diffusion coefficient in m^2/s
    diffusion_coefficient_stokes_einstein = function(radius, viscosity, temperature = 298.15) {
      kB <- 1.380649e-23
      return(kB * temperature / (6 * pi * viscosity * radius))
    },
    
    #' Calculate mean squared displacement for Brownian motion
    #' @param diffusion_coefficient Diffusion coefficient in m^2/s
    #' @param time Time in seconds
    #' @return Mean squared displacement in m^2
    mean_squared_displacement = function(diffusion_coefficient, time) {
      return(6 * diffusion_coefficient * time)
    }
  )
}

#' @title DrugReleaseKinetics
#' @description Model for drug release kinetics from various formulations
#' @return A list with drug release kinetics methods
DrugReleaseKinetics <- function() {
  list(
    #' Zero-order release (constant release rate)
    #' @param release_rate Release rate in mol/s
    #' @param initial_amount Initial amount in mol
    #' @param time_points Array of time points in seconds
    #' @return Array of released amounts
    zero_order_release = function(release_rate, initial_amount, time_points) {
      released <- release_rate * time_points
      return(pmin(released, rep(initial_amount, length(time_points))))
    },
    
    #' First-order release (exponential decay)
    #' @param release_rate_constant Rate constant in s^-1
    #' @param initial_amount Initial amount in mol
    #' @param time_points Array of time points in seconds
    #' @return Array of released amounts
    first_order_release = function(release_rate_constant, initial_amount, time_points) {
      return(initial_amount * (1 - exp(-release_rate_constant * time_points)))
    },
    
    #' Higuchi model for diffusion-controlled release from a matrix
    #' @param diffusion_coefficient Diffusion coefficient in m^2/s
    #' @param drug_loading Initial drug loading in kg/m^3
    #' @param tortuosity Tortuosity factor (>= 1)
    #' @param porosity Porosity (0-1)
    #' @param time_points Array of time points in seconds
    #' @return Array of released amounts per unit area
    higuchi_model = function(diffusion_coefficient, drug_loading, tortuosity, 
                            porosity, time_points) {
      # Higuchi equation: Q = sqrt(D * C_s * (2A - C_s) * t * epsilon / tau)
      # For simplicity, assume C_s << 2A
      
      effective_D <- diffusion_coefficient * porosity / tortuosity
      C_s <- drug_loading * 0.1  # Assume solubility is 10% of loading
      
      released <- sqrt(effective_D * C_s * drug_loading * time_points)
      return(released)
    },
    
    #' Peppas (Power Law) model for drug release
    #' @param n Release exponent (indicates mechanism)
    #' @param k Kinetic constant
    #' @param initial_amount Initial amount in mol
    #' @param time_points Array of time points in seconds
    #' @return Array of released amounts
    peppas_model = function(n, k, initial_amount, time_points) {
      return(initial_amount * k * time_points^n)
    },
    
    #' Interpret the Peppas exponent for release mechanism
    #' @param n Release exponent
    #' @return String describing the release mechanism
    interpret_peppas_exponent = function(n) {
      if (n <= 0.45) {
        return("Fickian diffusion (Case I)")
      } else if (n < 0.89) {
        return("Anomalous (non-Fickian) transport")
      } else {
        return("Case II transport (zero-order, relaxation-controlled)")
      }
    }
  )
}

#' @title EffectiveMediumTheory
#' @description Effective medium theory for transport in porous materials
#' @return A list with effective medium theory methods
EffectiveMediumTheory <- function() {
  list(
    #' Calculate effective diffusivity in porous medium
    #' @param diffusion_coefficient_free Free diffusion coefficient in m^2/s
    #' @param porosity Porosity (0-1)
    #' @param tortuosity Tortuosity factor (>= 1)
    #' @param constrictivity Constrictivity factor (0-1)
    #' @return Effective diffusivity in m^2/s
    effective_diffusivity_porous = function(diffusion_coefficient_free, 
                                           porosity, tortuosity = 1.0, 
                                           constrictivity = 1.0) {
      return(diffusion_coefficient_free * porosity * constrictivity / tortuosity)
    },
    
    #' Calculate effective diffusivity using percolation theory
    #' @param diffusion_coefficient_free Free diffusion coefficient in m^2/s
    #' @param porosity Porosity (0-1)
    #' @param critical_porosity Critical porosity for percolation threshold
    #' @param exponent Critical exponent (typically 1.8-2.0)
    #' @return Effective diffusivity in m^2/s
    effective_diffusivity_percolation = function(diffusion_coefficient_free, 
                                                 porosity, critical_porosity = 0.2, 
                                                 exponent = 2.0) {
      if (porosity <= critical_porosity) {
        return(0.0)  # No percolation path
      }
      
      epsilon <- (porosity - critical_porosity) / (1 - critical_porosity)
      return(diffusion_coefficient_free * epsilon^exponent)
    },
    
    #' Calculate effective diffusivity using Mackie-Meares model
    #' @param diffusion_coefficient_free Free diffusion coefficient in m^2/s
    #' @param porosity Porosity (0-1)
    #' @param pore_radius_distribution Array of pore radii in meters
    #' @return Effective diffusivity in m^2/s
    effective_diffusivity_mackie_meares = function(diffusion_coefficient_free, 
                                                   porosity, pore_radius_distribution) {
      # For simplicity, use average constrictivity
      if (length(pore_radius_distribution) == 0) {
        return(0.0)
      }
      
      r_avg <- mean(pore_radius_distribution)
      r_max <- max(pore_radius_distribution)
      constrictivity <- (r_avg / r_max)^2
      
      # Tortuosity approximation
      tortuosity <- 1 / sqrt(porosity)
      
      return(diffusion_coefficient_free * porosity * constrictivity / tortuosity)
    }
  )
}

#' @title ConvectionModel
#' @description Model for convection-driven transport
#' @return A list with convection model methods
ConvectionModel <- function() {
  list(
    #' Solve advection-diffusion equation in 1D
    #' @param velocity Fluid velocity in m/s
    #' @param diffusion_coefficient Diffusion coefficient in m^2/s
    #' @param initial_concentration Initial concentration in mol/m^3
    #' @param length Domain length in meters
    #' @param time_points Array of time points in seconds
    #' @param dx Spatial step size in meters
    #' @return List with x_values and concentration_profiles
    advection_diffusion_1d = function(velocity, diffusion_coefficient, 
                                    initial_concentration, length, time_points, dx = 0.01) {
      x_vals <- seq(0, length, by = dx)
      n_x <- length(x_vals)
      
      # Initial condition
      C_0 <- numeric(n_x)
      C_0[1] <- initial_concentration
      
      concentration_profiles <- list(C_0)
      
      for (t_idx in 2:length(time_points)) {
        t <- time_points[t_idx]
        dt <- t - time_points[t_idx - 1]
        
        # Advection term
        dC_adv <- -velocity * diff(C_0) / dx
        
        # Diffusion term
        dC_diff <- diffusion_coefficient * diff(diff(C_0) / dx) / dx
        
        # Update
        C_new <- C_0 + dt * (c(0, dC_adv) + c(dC_diff, 0))
        C_new <- pmax(C_new, 0)  # No negative concentrations
        
        concentration_profiles[[length(concentration_profiles) + 1]] <- C_new
        C_0 <- C_new
      }
      
      return(list(x_values = x_vals, concentration_profiles = concentration_profiles))
    }
  )
}

#' @title ElectricFieldTransport
#' @description Model for electric field-driven transport (electrophoresis)
#' @param temperature Temperature in Kelvin (default 298.15)
#' @return A list with electric field transport methods
ElectricFieldTransport <- function(temperature = 298.15) {
  kB <- 1.380649e-23
  e <- 1.602176634e-19
  epsilon_r <- 78.5
  epsilon_0 <- 8.854e-12
  
  list(
    T = temperature,
    kB = kB,
    e = e,
    epsilon_r = epsilon_r,
    epsilon_0 = epsilon_0,
    
    #' Calculate electrophoretic mobility
    #' @param charge Particle charge in Coulombs
    #' @param radius Particle radius in meters
    #' @param viscosity Viscosity in Pa*s
    #' @return Electrophoretic mobility in m^2/(V*s)
    electrophoretic_mobility = function(charge, radius, viscosity) {
      # Huckel approximation (for small particles)
      return(charge / (6 * pi * viscosity * radius))
    },
    
    #' Calculate electrophoretic velocity
    #' @param mobility Electrophoretic mobility in m^2/(V*s)
    #' @param electric_field Electric field strength in V/m
    #' @return Velocity in m/s
    electrophoretic_velocity = function(mobility, electric_field) {
      return(mobility * electric_field)
    },
    
    #' Calculate drift velocity in electric field
    #' @param charge Particle charge in Coulombs
    #' @param electric_field Electric field strength in V/m
    #' @param viscosity Viscosity in Pa*s
    #' @param radius Particle radius in meters
    #' @return Drift velocity in m/s
    drift_velocity = function(charge, electric_field, viscosity, radius) {
      return(charge * electric_field / (6 * pi * viscosity * radius))
    }
  )
}

#' @title main
#' @description Example usage of drug transport and release models
#' @export
main <- function() {
  cat("============================================================\n")
  cat("Drug Transport and Release Modeling\n")
  cat("============================================================\n")
  
  # Diffusion model
  cat("\n1. Diffusion Models\n")
  cat("----------------------------------------\n")
  
  diffusion <- DiffusionModel()
  
  # Stokes-Einstein
  radius <- 1e-9  # 1 nm particle
  viscosity <- 0.00089  # Water at 25C
  D <- diffusion$diffusion_coefficient_stokes_einstein(radius, viscosity)
  cat(paste("Diffusion coefficient (1 nm particle in water):", format(D, scientific = 3), "m^2/s\n"))
  
  # Mean squared displacement
  t <- 1.0  # 1 second
  msd <- diffusion$mean_squared_displacement(D, t)
  cat(paste("Mean squared displacement in 1s:", format(msd, scientific = 3), "m^2\n"))
  cat(paste("RMS displacement:", format(sqrt(msd) * 1e9, digits = 2), "nm\n"))
  
  # Drug release kinetics
  cat("\n2. Drug Release Kinetics\n")
  cat("----------------------------------------\n")
  
  release <- DrugReleaseKinetics()
  time_points <- seq(0, 3600, length.out = 100)  # 1 hour
  initial_amount <- 1.0  # mol
  
  # Zero-order release
  rate <- 0.001  # mol/s
  released_zero <- release$zero_order_release(rate, initial_amount, time_points)
  cat(paste("Zero-order release after 1 hour:", format(tail(released_zero, 1), digits = 3), "mol\n"))
  
  # First-order release
  k <- 0.001  # s^-1
  released_first <- release$first_order_release(k, initial_amount, time_points)
  cat(paste("First-order release after 1 hour:", format(tail(released_first, 1), digits = 3), "mol\n"))
  
  # Higuchi model
  D_hig <- 1e-10  # m^2/s
  drug_loading <- 100  # kg/m^3
  tortuosity <- 2.0
  porosity <- 0.5
  released_higuchi <- release$higuchi_model(D_hig, drug_loading, tortuosity, porosity, time_points)
  cat(paste("Higuchi model release after 1 hour:", format(tail(released_higuchi, 1), digits = 3), "kg/m^2\n"))
  
  # Peppas model
  n <- 0.5  # Fickian diffusion
  k_pep <- 0.1
  released_peppas <- release$peppas_model(n, k_pep, initial_amount, time_points)
  cat(paste("Peppas model release after 1 hour:", format(tail(released_peppas, 1), digits = 3), "mol\n"))
  cat(paste("Release mechanism:", release$interpret_peppas_exponent(n), "\n"))
  
  # Effective medium theory
  cat("\n3. Effective Medium Theory\n")
  cat("----------------------------------------\n")
  
  emt <- EffectiveMediumTheory()
  D_free <- 1e-9  # m^2/s
  
  # Porous medium
  porosity <- 0.5
  tortuosity <- 2.0
  D_eff_porous <- emt$effective_diffusivity_porous(D_free, porosity, tortuosity)
  cat(paste("Effective diffusivity (porous, epsilon=", porosity, ", tau=", tortuosity, "):", 
            format(D_eff_porous, scientific = 3), "m^2/s\n"))
  
  # Percolation theory
  critical_porosity <- 0.2
  exponent <- 2.0
  porosity_values <- c(0.1, 0.2, 0.3, 0.4, 0.5)
  cat("\nPorosity | Effective Diffusivity\n")
  cat("----------------------------------------\n")
  for (p in porosity_values) {
    D_eff_perc <- emt$effective_diffusivity_percolation(D_free, p, critical_porosity, exponent)
    cat(sprintf("  %6.2f  | %6.2e m^2/s\n", p, D_eff_perc))
  }
  
  # Electric field transport
  cat("\n4. Electric Field Transport\n")
  cat("----------------------------------------\n")
  
  eft <- ElectricFieldTransport()
  
  # Electrophoretic mobility
  charge <- 1.602e-19  # Single electron charge
  radius_ef <- 1e-9  # 1 nm
  mobility <- eft$electrophoretic_mobility(charge, radius_ef, viscosity)
  cat(paste("Electrophoretic mobility:", format(mobility, scientific = 3), "m^2/(V*s)\n"))
  
  # Electrophoretic velocity
  electric_field <- 1000  # V/m
  velocity <- eft$electrophoretic_velocity(mobility, electric_field)
  cat(paste("Electrophoretic velocity in 1000 V/m:", format(velocity, scientific = 3), "m/s\n"))
  cat(paste("  =", format(velocity * 1e6, digits = 2), "um/s\n"))
  
  # Plot release kinetics
  if (requireNamespace("png", quietly = TRUE)) {
    png('transport/drug_release_kinetics.png', width = 1000, height = 800)
    par(mfrow = c(2, 2), mar = c(4, 4, 3, 2) + 0.1)
    
    # Plot 1: Release kinetics
    plot(time_points / 3600, released_zero, type = 'l', col = 'blue',
         xlab = 'Time (hours)', ylab = 'Released Amount (mol)',
         main = 'Drug Release Kinetics')
    lines(time_points / 3600, released_first, col = 'red')
    lines(time_points / 3600, released_peppas, col = 'green')
    legend(c('Zero-order', 'First-order', 'Peppas (n=0.5)'), 
           col = c('blue', 'red', 'green'), lty = 1, x = 'bottomright')
    grid()
    
    # Plot 2: Higuchi model
    plot(time_points / 3600, released_higuchi, type = 'l', col = 'magenta',
         xlab = 'Time (hours)', ylab = 'Released Amount (kg/m2)',
         main = 'Higuchi Model')
    grid()
    
    # Plot 3: Effective diffusivity vs porosity
    porosities <- seq(0.01, 0.6, length.out = 100)
    D_eff_values <- sapply(porosities, function(p) emt$effective_diffusivity_porous(D_free, p, tortuosity))
    plot(porosities, D_eff_values, type = 'l', col = 'cyan',
         xlab = 'Porosity', ylab = 'Effective Diffusivity (m2/s)',
         main = 'Effective Diffusivity vs Porosity')
    grid()
    
    # Plot 4: Electrophoretic velocity
    electric_fields <- seq(0, 5000, length.out = 50)
    velocities <- sapply(electric_fields, function(E) eft$electrophoretic_velocity(mobility, E))
    plot(electric_fields, velocities, type = 'l', col = 'yellow',
         xlab = 'Electric Field (V/m)', ylab = 'Electrophoretic Velocity (m/s)',
         main = 'Electrophoretic Velocity')
    grid()
    
    dev.off()
    cat("\nPlot saved as 'transport/drug_release_kinetics.png'\n")
  }
}

# Run main if executed directly
if (interactive() || !exists(".GlobalEnv")) {
  main()
}
