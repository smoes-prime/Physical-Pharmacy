#' Polymer Thermodynamics for Pharmaceutical Applications
#'
#' Models for polymer behavior in pharmaceutical systems including:
#' - Polymer coil swelling
#' - Phase separation in polymer solutions
#' - Polyelectrolyte complex formation
#' - Gel swelling and elasticity
#'
#' Course: Molecular Physical Pharmacy (3FC003)

#' @title PolymerChain
#' @description Model for single polymer chain in solution
#' @param n_monomers Number of monomers in the chain
#' @param monomer_length Length per monomer in nm (default 0.35)
#' @param persistence_length Persistence length in nm (default 0.7)
#' @return A list with polymer chain properties and methods
PolymerChain <- function(n_monomers, monomer_length = 0.35, persistence_length = 0.7) {
  b <- monomer_length  # Kuhn length approximation
  l_p <- persistence_length
  
  list(
    n = n_monomers,
    b = b,
    l_p = l_p,
    
    #' Calculate radius of gyration for ideal chain
    #' @return Radius of gyration in nm
    radius_of_gyration = function() {
      # For ideal chain (Gaussian): R_g^2 = n * b^2 / 6
      return(sqrt(n_monomers * b^2 / 6))
    },
    
    #' Calculate end-to-end distance for ideal chain
    #' @return End-to-end distance in nm
    end_to_end_distance = function() {
      # For ideal chain: <R^2> = n * b^2
      return(sqrt(n_monomers * b^2))
    },
    
    #' Calculate radius of gyration for worm-like chain model
    #' @return Radius of gyration in nm
    radius_of_gyration_worm_like_chain = function() {
      L <- n_monomers * b
      if (l_p / L > 0.1) {
        # Rod-like limit
        return(L / sqrt(12))
      } else {
        # Coil limit
        return(sqrt(l_p * L / 3))
      }
    },
    
    #' Calculate radius of gyration with excluded volume effects
    #' @param chi_parameter Flory interaction parameter (default 0.5)
    #' @return Expanded radius of gyration in nm
    excluded_volume_radius = function(chi_parameter = 0.5) {
      # Flory exponent: nu ~ 3/(d+2) = 0.588 in good solvent
      # For theta conditions (chi = 0.5): nu = 0.5
      # For good solvent (chi < 0.5): nu ~ 0.588
      
      if (chi_parameter < 0.5) {
        nu <- 0.588  # Good solvent
      } else {
        nu <- 0.5  # Theta solvent
      }
      
      return(radius_of_gyration() * n_monomers^(nu - 0.5))
    }
  )
}

#' @title PolymerSolution
#' @description Model for polymer solutions and phase behavior
#' @param temperature Temperature in Kelvin (default 298.15)
#' @return A list with polymer solution methods
PolymerSolution <- function(temperature = 298.15) {
  kB <- 1.380649e-23  # J/K
  N_A <- 6.02214076e23  # mol^-1
  
  list(
    T = temperature,
    kB = kB,
    N_A = N_A,
    
    #' Calculate Flory-Huggins free energy of mixing
    #' @param phi_polymer Volume fraction of polymer
    #' @param n_polymer Degree of polymerization
    #' @param chi_parameter Flory interaction parameter
    #' @return Free energy density in kT per lattice site
    flory_huggins_free_energy = function(phi_polymer, n_polymer, chi_parameter) {
      phi_solvent <- 1 - phi_polymer
      
      # Entropy of mixing
      delta_f_entropy <- if (phi_polymer > 0 && phi_polymer < 1) {
        (phi_polymer / n_polymer) * log(phi_polymer) + phi_solvent * log(phi_solvent)
      } else {
        0
      }
      
      # Enthalpy of mixing
      delta_f_enthalpy <- chi_parameter * phi_polymer * phi_solvent
      
      return(delta_f_entropy + delta_f_enthalpy)
    },
    
    #' Calculate spinodal curve for phase separation
    #' @param n_polymer Degree of polymerization
    #' @param chi_parameter Flory interaction parameter
    #' @return List with critical volume fraction and critical chi
    spinodal_decomposition = function(n_polymer, chi_parameter) {
      # Spinodal condition: d^2F/dphi^2 = 0
      # For Flory-Huggins: chi_c = 1/(2*sqrt(n)) + 1/(2*n)
      chi_c <- 1 / (2 * sqrt(n_polymer)) + 1 / (2 * n_polymer)
      
      if (chi_parameter > chi_c) {
        # Phase separation occurs
        # Critical composition
        phi_c <- 1 / (1 + sqrt(n_polymer))
        return(list(phi_c = phi_c, chi_c = chi_c))
      } else {
        return(list(phi_c = NULL, chi_c = chi_c))
      }
    },
    
    #' Calculate binodal (coexistence) curve
    #' @param n_polymer Degree of polymerization
    #' @param chi_parameter Flory interaction parameter
    #' @return List with phi_1 and phi_2 coexistence compositions
    binodal_decomposition = function(n_polymer, chi_parameter) {
      # Solve for chemical potential equality
      # This is a simplified approximation
      
      if (chi_parameter <= 1 / (2 * sqrt(n_polymer))) {
        return(list(phi_1 = NULL, phi_2 = NULL))  # No phase separation
      }
      
      # Approximate binodal
      phi_1 <- 0.1
      phi_2 <- 0.9
      
      # Iterative refinement (simplified)
      for (i in 1:10) {
        # Chemical potential in phase 1
        mu_1 <- log(phi_1) + (1 - phi_1) + chi_parameter * (1 - phi_1)^2 + 
                (phi_1 / n_polymer) * (1 - 1/n_polymer)
        
        # Chemical potential in phase 2
        mu_2 <- log(phi_2) + (1 - phi_2) + chi_parameter * (1 - phi_2)^2 + 
                (phi_2 / n_polymer) * (1 - 1/n_polymer)
        
        if (abs(mu_1 - mu_2) < 0.01) {
          break
        }
        
        # Adjust compositions
        if (mu_1 > mu_2) {
          phi_1 <- min(phi_1 + 0.01, 0.99)
          phi_2 <- max(phi_2 - 0.01, 0.01)
        } else {
          phi_1 <- max(phi_1 - 0.01, 0.01)
          phi_2 <- min(phi_2 + 0.01, 0.99)
        }
      }
      
      return(list(phi_1 = phi_1, phi_2 = phi_2))
    },
    
    #' Calculate theta temperature for polymer solution
    #' @param n_polymer Degree of polymerization
    #' @return Theta temperature in Kelvin
    theta_temperature = function(n_polymer) {
      # Theta temperature when chi = 0.5
      return(temperature)
    }
  )
}

#' @title Polyelectrolyte
#' @description Model for polyelectrolyte solutions
#' @param n_monomers Number of monomers
#' @param charge_fraction Fraction of charged monomers (default 1.0)
#' @param temperature Temperature in Kelvin (default 298.15)
#' @return A list with polyelectrolyte properties and methods
Polyelectrolyte <- function(n_monomers, charge_fraction = 1.0, temperature = 298.15) {
  f <- charge_fraction
  T <- temperature
  kB <- 1.380649e-23
  e <- 1.602176634e-19
  epsilon_r <- 78.5
  epsilon_0 <- 8.854e-12
  N_A <- 6.02214076e23
  
  list(
    n = n_monomers,
    f = f,
    T = T,
    kB = kB,
    e = e,
    epsilon_r = epsilon_r,
    epsilon_0 = epsilon_0,
    N_A = N_A,
    
    #' Calculate radius of gyration for polyelectrolyte
    #' @return Radius of gyration in nm
    radius_of_gyration = function() {
      # For polyelectrolytes, the chain is expanded due to electrostatic repulsion
      # R_g ~ n^(3/5) * l * f^(2/5)  (approximate scaling)
      b <- 0.35  # nm, monomer length
      return(b * n_monomers^(3/5) * f^(2/5))
    },
    
    #' Calculate Debye screening length in solution
    #' @param ionic_strength Ionic strength in M
    #' @return Debye length in meters
    debye_length = function(ionic_strength) {
      I <- ionic_strength
      kappa <- sqrt(2 * N_A * e^2 * I / (epsilon_r * epsilon_0 * kB * T))
      return(1 / kappa)
    },
    
    #' Calculate radius of osmotically equivalent sphere
    #' @param concentration Polyelectrolyte concentration in M
    #' @return Radius in nm
    osmotically_equivalent_sphere = function(concentration) {
      c_molar <- concentration
      c_molecules <- c_molar * N_A * 1e-27  # molecules/nm^3
      
      # Ionic strength from polyelectrolyte
      I_poly <- f * n_monomers * c_molecules
      
      # Total ionic strength (including counterions)
      I_total <- I_poly
      
      return(debye_length(I_total) * 1e9)
    }
  )
}

#' @title GelModel
#' @description Model for polymer gels and their swelling behavior
#' @param polymer_volume_fraction_dry Volume fraction of polymer in dry state (default 0.1)
#' @param crosslink_density Crosslink density (default 0.01)
#' @param temperature Temperature in Kelvin (default 298.15)
#' @return A list with gel model methods
GelModel <- function(polymer_volume_fraction_dry = 0.1, 
                     crosslink_density = 0.01, temperature = 298.15) {
  phi_0 <- polymer_volume_fraction_dry
  n_crosslink <- crosslink_density
  T <- temperature
  kB <- 1.380649e-23
  N_A <- 6.02214076e23
  
  list(
    phi_0 = phi_0,
    n_crosslink = n_crosslink,
    T = T,
    kB = kB,
    N_A = N_A,
    
    #' Calculate gel swelling ratio using Flory-Rehner theory
    #' @param chi_parameter Flory interaction parameter
    #' @param ionic_strength Ionic strength for ionized gels (default 0)
    #' @return Swelling ratio (V/V_0)
    flory_rehner_swelling = function(chi_parameter, ionic_strength = 0) {
      # For non-ionized gels
      if (chi_parameter >= 0.5) {
        # Collapse or limited swelling
        return(1.0)
      }
      
      # Solve for equilibrium swelling
      swelling_eq <- function(phi) {
        # Chemical potential equality
        term1 <- phi / phi_0
        term2 <- if (phi < 1) log(1 - phi) + phi + chi_parameter * phi^2 else 0
        term3 <- (1 / n_crosslink) * (phi / phi_0 - 1)
        return(term1 + term2 + term3)
      }
      
      # Find root
      result <- tryCatch({
        uniroot(swelling_eq, interval = c(phi_0, 0.9))$root
      }, error = function(e) {
        1.0
      })
      
      if (is.numeric(result)) {
        phi_eq <- result
        return((phi_0 / phi_eq)^3)
      } else {
        return(1.0)
      }
    },
    
    #' Calculate swelling for ionized gel (polyelectrolyte gel)
    #' @param chi_parameter Flory interaction parameter
    #' @param ionization_fraction Fraction of ionizable groups that are ionized
    #' @param ionic_strength External ionic strength in M
    #' @return Swelling ratio
    swelling_with_ionization = function(chi_parameter, ionization_fraction, ionic_strength) {
      # For ionized gels, there's additional osmotic pressure from counterions
      base_swelling <- flory_rehner_swelling(chi_parameter)
      
      # Enhancement factor due to ionization
      if (ionization_fraction > 0) {
        enhancement <- 1 + 10 * ionization_fraction * sqrt(1 / (ionic_strength + 1e-6))
        return(base_swelling * enhancement)
      } else {
        return(base_swelling)
      }
    }
  )
}

#' @title main
#' @description Example usage of polymer thermodynamics models
#' @export
main <- function() {
  cat("============================================================\n")
  cat("Polymer Thermodynamics for Pharmaceutical Applications\n")
  cat("============================================================\n")
  
  # Single polymer chain
  cat("\n1. Single Polymer Chain Properties\n")
  cat("----------------------------------------\n")
  
  chain <- PolymerChain(n_monomers = 1000, monomer_length = 0.35, persistence_length = 0.7)
  cat(paste("Degree of polymerization:", chain$n, "\n"))
  cat(paste("Ideal chain R_g:", format(chain$radius_of_gyration(), digits = 2), "nm\n"))
  cat(paste("Ideal chain R_ee:", format(chain$end_to_end_distance(), digits = 2), "nm\n"))
  cat(paste("Worm-like chain R_g:", format(chain$radius_of_gyration_worm_like_chain(), digits = 2), "nm\n"))
  cat(paste("Good solvent R_g:", format(chain$excluded_volume_radius(chi_parameter = 0.4), digits = 2), "nm\n"))
  cat(paste("Theta solvent R_g:", format(chain$excluded_volume_radius(chi_parameter = 0.5), digits = 2), "nm\n"))
  
  # Polymer solution phase behavior
  cat("\n2. Polymer Solution Phase Behavior\n")
  cat("----------------------------------------\n")
  
  solution <- PolymerSolution()
  n_polymer <- 1000
  chi_values <- c(0.4, 0.45, 0.48, 0.5, 0.52)
  
  cat("Chi Parameter | Critical Chi | Phase Separation?\n")
  cat("----------------------------------------\n")
  for (chi in chi_values) {
    result <- solution$spinodal_decomposition(n_polymer, chi)
    separates <- chi > result$chi_c
    cat(sprintf("  %6.2f     | %8.4f    | %s\n", chi, result$chi_c, ifelse(separates, "Yes", "No")))
  }
  
  # Binodal calculation
  chi <- 0.55
  binodal <- solution$binodal_decomposition(n_polymer, chi)
  if (!is.null(binodal$phi_1)) {
    cat(paste("\nBinodal at chi=", chi, ": phi_1 =", format(binodal$phi_1, digits = 3),
               ", phi_2 =", format(binodal$phi_2, digits = 3), "\n"))
  }
  
  # Polyelectrolyte
  cat("\n3. Polyelectrolyte Properties\n")
  cat("----------------------------------------\n")
  
  pe <- Polyelectrolyte(n_monomers = 1000, charge_fraction = 0.5)
  cat(paste("Number of monomers:", pe$n, "\n"))
  cat(paste("Charge fraction:", pe$f, "\n"))
  cat(paste("Radius of gyration:", format(pe$radius_of_gyration(), digits = 2), "nm\n"))
  cat(paste("Debye length (0.1 M NaCl):", format(pe$debye_length(0.1) * 1e9, digits = 2), "nm\n"))
  
  concentrations <- c(0.001, 0.01, 0.1)
  cat("\nConcentration (M) | Osmotic radius (nm)\n")
  cat("----------------------------------------\n")
  for (c in concentrations) {
    r <- pe$osmotically_equivalent_sphere(c)
    cat(sprintf("  %8.3f      | %8.2f\n", c, r))
  }
  
  # Gel swelling
  cat("\n4. Gel Swelling\n")
  cat("----------------------------------------\n")
  
  gel <- GelModel(polymer_volume_fraction_dry = 0.1, crosslink_density = 0.01)
  chi_values <- c(0.4, 0.45, 0.49, 0.51)
  
  cat("Chi Parameter | Swelling Ratio (V/V_0)\n")
  cat("----------------------------------------\n")
  for (chi in chi_values) {
    swelling <- gel$flory_rehner_swelling(chi)
    cat(sprintf("  %6.2f     | %8.2f\n", chi, swelling))
  }
  
  # Ionized gel
  cat("\n5. Polyelectrolyte Gel Swelling\n")
  cat("----------------------------------------\n")
  
  ionization_fractions <- c(0, 0.25, 0.5, 0.75, 1.0)
  chi <- 0.45
  ionic_strength <- 0.01
  
  cat("Ionization Fraction | Swelling Ratio\n")
  cat("----------------------------------------\n")
  for (f in ionization_fractions) {
    swelling <- gel$swelling_with_ionization(chi, f, ionic_strength)
    cat(sprintf("  %6.2f            | %8.2f\n", f, swelling))
  }
  
  # Plot phase diagram
  if (requireNamespace("png", quietly = TRUE)) {
    png('polymers/polymer_phase_diagram.png', width = 800, height = 600)
    par(mar = c(5, 4, 4, 2) + 0.1)
    
    chi_range <- seq(0.3, 0.6, length.out = 100)
    n_values <- c(100, 500, 1000)
    
    for (n in n_values) {
      chi_c <- 1 / (2 * sqrt(n)) + 1 / (2 * n)
      abline(v = chi_c, col = 'black', lty = 2, lwd = 0.5)
      
      # Spinodal curve
      phi_c_values <- numeric(length(chi_range))
      for (i in seq_along(chi_range)) {
        result <- solution$spinodal_decomposition(n, chi_range[i])
        phi_c_values[i] <- if (!is.null(result$phi_c)) result$phi_c else NA
      }
      
      lines(chi_range, phi_c_values, col = i, lwd = 2)
    }
    
    legend(paste("n =", n_values), col = seq_along(n_values), lty = 1, lwd = 2,
           x = 'topright')
    xlab('Chi Parameter')
    ylab('Critical Volume Fraction')
    title('Spinodal Curve for Polymer Solutions')
    grid()
    dev.off()
    cat("\nPlot saved as 'polymers/polymer_phase_diagram.png'\n")
  }
}

# Run main if executed directly
if (interactive() || !exists(".GlobalEnv")) {
  main()
}
