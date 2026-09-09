#' Poisson-Boltzmann Equation Solver for Pharmaceutical Systems
#'
#' Solves the linearized and non-linear Poisson-Boltzmann equation
#' for electrostatic potential in pharmaceutical and biological systems.
#'
#' Course: Molecular Physical Pharmacy (3FC003)

#' @title PoissonBoltzmannSolver
#' @description Solve Poisson-Boltzmann equation for planar geometry
#' @param epsilon_r Relative permittivity of the medium (water = 78.5)
#' @param temperature Temperature in Kelvin (default 298.15)
#' @return A list containing solver functions and parameters
PoissonBoltzmannSolver <- function(epsilon_r = 78.5, temperature = 298.15) {
  epsilon_0 <- 8.854e-12  # F/m, vacuum permittivity
  kB <- 1.380649e-23  # J/K, Boltzmann constant
  e <- 1.602176634e-19  # C, elementary charge
  N_A <- 6.02214076e23  # mol^-1, Avogadro's number
  
  epsilon <- epsilon_r * epsilon_0
  
  list(
    epsilon_r = epsilon_r,
    temperature = temperature,
    epsilon = epsilon,
    kB = kB,
    e = e,
    N_A = N_A,
    
    #' Calculate Debye screening length
    #' @param ionic_strength Ionic strength in M (mol/L)
    #' @return Debye length in meters
    debye_length = function(ionic_strength) {
      I <- ionic_strength
      kappa <- sqrt(2 * N_A * e^2 * I / (epsilon * kB * temperature))
      return(1 / kappa)
    },
    
    #' Solve linearized Poisson-Boltzmann equation for planar geometry
    #' @param sigma Surface charge density in C/m^2
    #' @param ionic_strength Ionic strength in M
    #' @param z_vals Distance values in meters (vector)
    #' @return List with z_values and potential_values
    linear_pb_planar = function(sigma, ionic_strength, z_vals = seq(0, 10e-9, length.out = 100)) {
      kappa <- 1 / debye_length(ionic_strength)
      potential <- (sigma / (epsilon * kappa)) * exp(-kappa * z_vals)
      return(list(z = z_vals, potential = potential))
    },
    
    #' Solve Gouy-Chapman theory for planar double layer
    #' @param sigma Surface charge density in C/m^2
    #' @param ionic_strength Ionic strength in M (for symmetric electrolyte)
    #' @param z_vals Distance values in meters (start > 0 to avoid singularity)
    #' @return List with z_values, potential_values, and surface_potential
    gouy_chapman_planar = function(sigma, ionic_strength, z_vals = seq(0.1e-9, 10e-9, length.out = 100)) {
      c0 <- ionic_strength * N_A * 1000  # Convert M to m^-3
      
      sigma_abs <- abs(sigma)
      sign_sigma <- sign(sigma)
      
      # Solve for surface potential using uniroot
      surface_potential_eq <- function(psi_0) {
        sqrt(8 * epsilon * c0 * kB * temperature) * sinh(e * psi_0 / (2 * kB * temperature)) - sigma_abs
      }
      
      # Initial guess
      psi_0_guess <- sigma_abs * 1e-9 / epsilon
      
      psi_0 <- tryCatch({
        uniroot(surface_potential_eq, interval = c(-0.1, 0.1))$root
      }, error = function(e) {
        psi_0_guess
      })
      psi_0 <- sign_sigma * psi_0
      
      # Potential profile
      kappa <- 1 / debye_length(ionic_strength)
      beta <- e / (kB * temperature)
      
      potential_func <- function(z) {
        (4 * kB * temperature / e) * atan(tanh(beta * psi_0 / 4) * exp(-kappa * z))
      }
      
      potential <- sapply(z_vals, potential_func)
      
      return(list(z = z_vals, potential = potential, surface_potential = psi_0))
    },
    
    #' Plot comparison of linear PB and Gouy-Chapman solutions
    #' @param sigma Surface charge density in C/m^2
    #' @param ionic_strength Ionic strength in M
    #' @param filename Output filename (default: 'electrostatics/pb_comparison.png')
    plot_comparison = function(sigma, ionic_strength, filename = 'electrostatics/pb_comparison.png') {
      z_vals <- seq(0.1e-9, 5e-9, length.out = 100)
      
      linear_result <- linear_pb_planar(sigma, ionic_strength, z_vals)
      gc_result <- gouy_chapman_planar(sigma, ionic_strength, z_vals)
      
      z_nm <- z_vals * 1e9
      
      plot(z_nm, linear_result$potential, type = 'l', col = 'blue',
           xlab = 'Distance from surface (nm)', ylab = 'Electrostatic Potential (V)',
           main = paste('Poisson-Boltzmann Solutions (sigma =', format(sigma, scientific = 3), 'C/m2, I =', ionic_strength, 'M)'),
           ylim = c(min(c(linear_result$potential, gc_result$potential)) * 0.9,
                    max(c(linear_result$potential, gc_result$potential)) * 1.1))
      lines(z_nm, gc_result$potential, col = 'red')
      legend(c('Linear PB', paste('Gouy-Chapman (psi0 =', format(gc_result$surface_potential, digits = 3), 'V)')),
             col = c('blue', 'red'), lty = 1, x = 'topright')
      grid()
      
      if (requireNamespace("png", quietly = TRUE)) {
        png(filename, width = 800, height = 600)
        par(mar = c(5, 4, 4, 2) + 0.1)
        plot(z_nm, linear_result$potential, type = 'l', col = 'blue',
             xlab = 'Distance from surface (nm)', ylab = 'Electrostatic Potential (V)',
             main = paste('Poisson-Boltzmann Solutions (sigma =', format(sigma, scientific = 3), 'C/m2, I =', ionic_strength, 'M)'))
        lines(z_nm, gc_result$potential, col = 'red')
        legend(c('Linear PB', paste('Gouy-Chapman (psi0 =', format(gc_result$surface_potential, digits = 3), 'V)')),
               col = c('blue', 'red'), lty = 1, x = 'topright')
        grid()
        dev.off()
      }
      
      invisible(list(linear = linear_result, gouy_chapman = gc_result))
    }
  )
}

#' @title main
#' @description Example usage of Poisson-Boltzmann solver
#' @export
main <- function() {
  cat("============================================================\n")
  cat("Poisson-Boltzmann Equation Solver\n")
  cat("============================================================\n")
  
  solver <- PoissonBoltzmannSolver()
  
  # Example: Lipid bilayer surface
  sigma <- 0.01  # C/m^2 (typical for lipid bilayer)
  ionic_strength <- 0.1  # M NaCl
  
  cat("Surface charge density:", format(sigma, scientific = 3), "C/m^2\n")
  cat("Ionic strength:", ionic_strength, "M\n")
  cat("Debye length:", solver$debye_length(ionic_strength) * 1e9, "nm\n")
  
  # Solve and plot
  solver$plot_comparison(sigma, ionic_strength)
  cat("\nPlot saved as 'electrostatics/pb_comparison.png'\n")
  
  # Calculate surface potential
  gc_result <- solver$gouy_chapman_planar(sigma, ionic_strength)
  cat(paste("Gouy-Chapman surface potential:", format(gc_result$surface_potential * 1000, digits = 2), "mV\n"))
}

# Run main if executed directly
if (interactive() || !exists(".GlobalEnv")) {
  main()
}
