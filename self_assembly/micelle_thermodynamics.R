#' Micelle Thermodynamics and Self-Assembly Modeling
#'
#' Models for the self-assembly of amphiphilic molecules including:
#' - Critical Micelle Concentration (CMC) calculations
#' - Micelle size and shape predictions
#' - Bending elasticity and spontaneous curvature models
#' - Solubilization modeling
#'
#' Course: Molecular Physical Pharmacy (3FC003)

#' @title Amphiphile
#' @description Represents an amphiphilic molecule with head and tail properties
#' @param head_area Head group area in nm^2
#' @param tail_volume Tail volume in nm^3
#' @param tail_length Tail length in nm
#' @param head_charge Head group charge (e.g., -1 for anionic)
#' @return A list with amphiphile properties and methods
Amphiphile <- function(head_area, tail_volume, tail_length, head_charge = 0) {
  list(
    head_area = head_area,
    tail_volume = tail_volume,
    tail_length = tail_length,
    head_charge = head_charge,
    
    #' Calculate packing parameter (v/a*l)
    packing_parameter = function() {
      head_area <<- head_area
      tail_volume <<- tail_volume
      tail_length <<- tail_length
      return(tail_volume / (head_area * tail_length))
    },
    
    #' Predict aggregate shape based on packing parameter
    #' @return String describing predicted shape
    predict_aggregate_shape = function() {
      pp <- packing_parameter()
      
      if (pp < 1/3) {
        return("Spherical micelles")
      } else if (pp < 1/2) {
        return("Cylindrical micelles")
      } else if (pp < 2/3) {
        return("Flexible bilayers / Vesicles")
      } else if (pp < 1) {
        return("Planar bilayers")
      } else {
        return("Inverted micelles")
      }
    }
  )
}

#' @title MicelleModel
#' @description Thermodynamic model for micelle formation
#' @param temperature Temperature in Kelvin (default 298.15)
#' @return A list with micelle model methods
MicelleModel <- function(temperature = 298.15) {
  kB <- 1.380649e-23  # J/K
  N_A <- 6.02214076e23  # mol^-1
  epsilon_r <- 78.5
  epsilon_0 <- 8.854e-12  # F/m
  e <- 1.602176634e-19
  
  list(
    temperature = temperature,
    kB = kB,
    N_A = N_A,
    epsilon_r = epsilon_r,
    epsilon_0 = epsilon_0,
    e = e,
    
    #' Calculate CMC using Tanford's thermodynamic model
    #' @param amphiphile Amphiphile object
    #' @param ionic_strength Ionic strength in M
    #' @return CMC in M
    cmc_tanford = function(amphiphile, ionic_strength = 0.1) {
      # Hydrophobic transfer free energy (approximate)
      # For alkyl chains: ~ -1.5 kT per CH2 group
      n_ch2 <- floor(amphiphile$tail_volume * 100)  # Rough estimate
      delta_g_hydrophobic <- -1.5 * n_ch2 * kB * temperature
      
      # Head group repulsion (simplified)
      a <- amphiphile$head_area * 1e-18  # Convert nm^2 to m^2
      sigma <- if (amphiphile$head_charge != 0) amphiphile$head_charge * e / a else 0
      
      if (sigma != 0) {
        # Electrostatic contribution
        kappa <- sqrt(2 * N_A * e^2 * ionic_strength / 
                     (epsilon_r * epsilon_0 * kB * temperature))
        delta_g_electrostatic <- (sigma^2) / (2 * epsilon_r * epsilon_0 * kappa)
      } else {
        delta_g_electrostatic <- 0
      }
      
      # Total free energy of micellization
      delta_g_mic <- delta_g_hydrophobic + delta_g_electrostatic
      
      # CMC from free energy
      # CMC ~ exp(-delta_g_mic / (kT)) / aggregation_number
      # For simplicity, assume aggregation number ~100
      n_agg <- 100
      cmc <- (1 / n_agg) * exp(-delta_g_mic / (kB * temperature * N_A * 1000))
      
      return(cmc)
    },
    
    #' Empirical CMC prediction based on alkyl chain length
    #' @param alkyl_chain_length Number of carbon atoms in alkyl chain
    #' @param head_group_type 'ionic' or 'nonionic'
    #' @return CMC in M
    cmc_empirical = function(alkyl_chain_length, head_group_type = 'ionic') {
      # Empirical constants
      if (head_group_type == 'ionic') {
        a <- 0.015
        b <- 0.29
      } else {  # nonionic
        a <- 0.0085
        b <- 0.30
      }
      
      cmc <- a * exp(-b * alkyl_chain_length)
      return(cmc)
    },
    
    #' Calculate micelle dimensions
    #' @param amphiphile Amphiphile object
    #' @param aggregation_number Number of molecules per micelle
    #' @return List with micelle dimensions
    micelle_size = function(amphiphile, aggregation_number) {
      pp <- amphiphile$packing_parameter()
      
      if (pp < 1/3) {
        # Spherical micelle
        total_tail_volume <- aggregation_number * amphiphile$tail_volume
        core_radius <- (3 * total_tail_volume / (4 * pi))^(1/3)
        shell_thickness <- amphiphile$tail_length * 0.5  # Approximate
        
        return(list(
          shape = 'spherical',
          core_radius_nm = core_radius,
          total_radius_nm = core_radius + shell_thickness,
          aggregation_number = aggregation_number
        ))
      } else if (pp < 1/2) {
        # Cylindrical micelle
        total_tail_volume <- aggregation_number * amphiphile$tail_volume
        # For cylinder: V = pi * r^2 * L
        # Assume L ~ 2 * r for cylindrical micelles
        radius <- (total_tail_volume / (2 * pi))^(1/3)
        length <- 2 * radius
        
        return(list(
          shape = 'cylindrical',
          radius_nm = radius,
          length_nm = length,
          aggregation_number = aggregation_number
        ))
      } else {
        # Bilayer structures
        total_tail_volume <- aggregation_number * amphiphile$tail_volume
        thickness <- 2 * amphiphile$tail_length
        area_per_molecule <- amphiphile$head_area
        total_area <- aggregation_number * area_per_molecule
        
        # For vesicle: radius from area
        radius <- sqrt(total_area / (4 * pi))
        
        return(list(
          shape = 'vesicle',
          radius_nm = radius,
          bilayer_thickness_nm = thickness,
          aggregation_number = aggregation_number
        ))
      }
    }
  )
}

#' @title BendingElasticityModel
#' @description Model for bending elasticity and spontaneous curvature
#' @param temperature Temperature in Kelvin (default 298.15)
#' @return A list with bending elasticity methods
BendingElasticityModel <- function(temperature = 298.15) {
  kB <- 1.380649e-23
  
  list(
    temperature = temperature,
    kB = kB,
    
    #' Calculate bending energy density
    #' @param curvature Mean curvature (1/radius) in nm^-1
    #' @param spontaneous_curvature Spontaneous curvature in nm^-1
    #' @param bending_rigidity Bending rigidity in kT
    #' @return Bending energy density in kT/nm^2
    bending_energy = function(curvature, spontaneous_curvature, bending_rigidity) {
      return(0.5 * bending_rigidity * (curvature - spontaneous_curvature)^2)
    },
    
    #' Calculate optimal aggregate radius
    #' @param spontaneous_curvature Spontaneous curvature in nm^-1
    #' @param bending_rigidity Bending rigidity in kT
    #' @return Optimal radius in nm
    optimal_radius = function(spontaneous_curvature, bending_rigidity) {
      # For minimal bending energy, curvature ~ spontaneous curvature
      if (spontaneous_curvature != 0) {
        return(1 / spontaneous_curvature)
      } else {
        # Planar is optimal
        return(Inf)
      }
    },
    
    #' Predict shape transitions as function of spontaneous curvature
    #' @param spontaneous_curvature_values Array of spontaneous curvature values
    #' @param bending_rigidity Bending rigidity in kT
    #' @return Array of predicted shapes
    predict_shape_transition = function(spontaneous_curvature_values, bending_rigidity) {
      shapes <- character(length(spontaneous_curvature_values))
      for (i in seq_along(spontaneous_curvature_values)) {
        H0 <- spontaneous_curvature_values[i]
        if (H0 < -0.1) {
          shapes[i] <- "Inverted micelles"
        } else if (H0 < 0) {
          shapes[i] <- "Planar bilayers"
        } else if (H0 < 0.05) {
          shapes[i] <- "Vesicles"
        } else if (H0 < 0.15) {
          shapes[i] <- "Cylindrical micelles"
        } else {
          shapes[i] <- "Spherical micelles"
        }
      }
      return(shapes)
    }
  )
}

#' @title main
#' @description Example usage of micelle thermodynamics models
#' @export
main <- function() {
  cat("============================================================\n")
  cat("Micelle Thermodynamics and Self-Assembly\n")
  cat("============================================================\n")
  
  # Create amphiphiles
  cat("\n1. Amphiphile Properties and Shape Prediction\n")
  cat("----------------------------------------\n")
  
  # SDS-like (anionic surfactant)
  sds <- Amphiphile(head_area = 0.5, tail_volume = 0.5, tail_length = 1.5, head_charge = -1)
  cat(paste("SDS-like: Packing parameter =", format(sds$packing_parameter(), digits = 3)))
  cat(paste("  Predicted shape:", sds$predict_aggregate_shape(), "\n"))
  
  # Phospholipid (PC)
  pc <- Amphiphile(head_area = 0.7, tail_volume = 1.2, tail_length = 2.0, head_charge = 0)
  cat(paste("Phosphatidylcholine: Packing parameter =", format(pc$packing_parameter(), digits = 3)))
  cat(paste("  Predicted shape:", pc$predict_aggregate_shape(), "\n"))
  
  # Bile salt
  bile_salt <- Amphiphile(head_area = 0.3, tail_volume = 0.8, tail_length = 1.0, head_charge = -1)
  cat(paste("Bile salt: Packing parameter =", format(bile_salt$packing_parameter(), digits = 3)))
  cat(paste("  Predicted shape:", bile_salt$predict_aggregate_shape(), "\n"))
  
  # CMC calculations
  cat("\n2. CMC Predictions\n")
  cat("----------------------------------------\n")
  
  model <- MicelleModel()
  
  # Empirical CMC
  chain_lengths <- c(8, 10, 12, 14, 16)
  cmcs_ionic <- sapply(chain_lengths, function(n) model$cmc_empirical(n, 'ionic'))
  cmcs_nonionic <- sapply(chain_lengths, function(n) model$cmc_empirical(n, 'nonionic'))
  
  cat("Chain length | CMC (ionic) | CMC (nonionic)\n")
  cat("----------------------------------------\n")
  for (i in seq_along(chain_lengths)) {
    n <- chain_lengths[i]
    cmc_i <- cmcs_ionic[i]
    cmc_n <- cmcs_nonionic[i]
    cat(sprintf("  C%2d      | %6.2e M    | %6.2e M\n", n, cmc_i, cmc_n))
  }
  
  # Micelle size
  cat("\n3. Micelle Size Predictions\n")
  cat("----------------------------------------\n")
  
  size_info <- model$micelle_size(sds, 100)
  cat(paste("SDS micelle (n=100):", toString(size_info), "\n"))
  
  # Bending elasticity
  cat("\n4. Bending Elasticity Model\n")
  cat("----------------------------------------\n")
  
  bending_model <- BendingElasticityModel()
  
  H0_values <- seq(-0.2, 0.2, length.out = 9)
  shapes <- bending_model$predict_shape_transition(H0_values, 10)
  
  cat("Spontaneous Curvature | Predicted Shape\n")
  cat("----------------------------------------\n")
  for (i in seq_along(H0_values)) {
    cat(sprintf("  %6.2f nm^-1         | %s\n", H0_values[i], shapes[i]))
  }
  
  # Plot CMC vs chain length
  if (requireNamespace("png", quietly = TRUE)) {
    png('self_assembly/cmc_chain_length.png', width = 800, height = 600)
    par(mar = c(5, 4, 4, 2) + 0.1)
    plot(chain_lengths, cmcs_ionic, type = 'o', col = 'blue', pch = 19,
         xlab = 'Alkyl Chain Length (C atoms)', ylab = 'CMC (M)',
         main = 'CMC vs Alkyl Chain Length', log = 'y')
    lines(chain_lengths, cmcs_ionic, col = 'blue')
    points(chain_lengths, cmcs_nonionic, col = 'red', pch = 19)
    lines(chain_lengths, cmcs_nonionic, col = 'red')
    legend(c('Ionic surfactants', 'Nonionic surfactants'), col = c('blue', 'red'),
           pch = c(19, 19), x = 'topright')
    grid()
    dev.off()
    cat("\nPlot saved as 'self_assembly/cmc_chain_length.png'\n")
  }
}

# Run main if executed directly
if (interactive() || !exists(".GlobalEnv")) {
  main()
}
