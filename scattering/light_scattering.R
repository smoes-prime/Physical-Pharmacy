#' Light Scattering and Small-Angle Scattering Analysis
#'
#' Tools for analyzing experimental scattering data from:
#' - Static Light Scattering (SLS)
#' - Dynamic Light Scattering (DLS)
#' - Small-Angle X-ray Scattering (SAXS)
#' - Small-Angle Neutron Scattering (SANS)
#'
#' Course: Molecular Physical Pharmacy (3FC003)

#' @title StaticLightScattering
#' @description Static Light Scattering (SLS) analysis
#' @param wavelength Laser wavelength in meters (default 632.8e-9, HeNe)
#' @param refractive_index Refractive index of medium (default water = 1.33)
#' @return A list with SLS analysis methods
StaticLightScattering <- function(wavelength = 632.8e-9, refractive_index = 1.33) {
  lambda_ <- wavelength
  n <- refractive_index
  kB <- 1.380649e-23  # J/K
  N_A <- 6.02214076e23  # mol^-1
  
  list(
    lambda_ = lambda_,
    n = n,
    kB = kB,
    N_A = N_A,
    
    #' Calculate wave vector magnitude in medium
    wave_vector = function() {
      return((2 * pi * n) / lambda_)
    },
    
    #' Calculate Rayleigh ratio for a solution
    #' @param concentration Concentration in kg/m^3
    #' @param molecular_weight Molecular weight in kg/mol
    #' @param dn_dc Refractive index increment in m^3/kg
    #' @param temperature Temperature in Kelvin
    #' @param angle Scattering angle in degrees
    #' @return Rayleigh ratio in m^-1
    rayleigh_ratio = function(concentration, molecular_weight, dn_dc, 
                              temperature = 298.15, angle = 90) {
      theta <- radians(angle)
      q <- 2 * wave_vector() * sin(theta / 2)
      
      # Rayleigh ratio: R = K * c * M * P(q)
      # where K = 4 * pi^2 * n^2 * (dn/dc)^2 / (lambda^4 * N_A)
      K <- (4 * pi^2 * n^2 * dn_dc^2) / (lambda_^4 * N_A)
      
      # For small particles (qR_g << 1), P(q) ~ 1
      P_q <- 1.0
      
      R <- K * concentration * molecular_weight * P_q
      return(R)
    },
    
    #' Calculate molecular weight from SLS measurements
    #' @param rayleigh_ratios Array of Rayleigh ratios
    #' @param concentrations Array of concentrations in kg/m^3
    #' @param dn_dc Refractive index increment in m^3/kg
    #' @param temperature Temperature in Kelvin
    #' @param angle Scattering angle in degrees
    #' @return List with molecular weight and second virial coefficient
    molecular_weight_from_sls = function(rayleigh_ratios, concentrations, 
                                       dn_dc, temperature = 298.15, angle = 90) {
      theta <- radians(angle)
      q <- 2 * wave_vector() * sin(theta / 2)
      
      K <- (4 * pi^2 * n^2 * dn_dc^2) / (lambda_^4 * N_A)
      
      # Debye plot: K * c / R = 1/M + 2 * A2 * c + ...
      # For dilute solutions, intercept gives 1/M
      
      y <- K * concentrations / rayleigh_ratios
      x <- concentrations
      
      # Linear fit
      fit <- lm(y ~ x)
      slope <- coef(fit)[2]
      intercept <- coef(fit)[1]
      
      M <- 1 / intercept
      A2 <- slope / 2  # Second virial coefficient
      
      return(list(M = M, A2 = A2))
    },
    
    #' Calculate radius of gyration from Guinier plot
    #' @param intensity Array of scattered intensities
    #' @param q_values Array of scattering vectors in m^-1
    #' @return Radius of gyration in meters
    radius_of_gyration_from_guinier = function(intensity, q_values) {
      # Guinier equation: ln(I(q)) = ln(I(0)) - (R_g^2 / 3) * q^2
      
      log_intensity <- log(intensity)
      q_squared <- q_values^2
      
      # Linear fit
      fit <- lm(log_intensity ~ q_squared)
      slope <- coef(fit)[2]
      
      R_g <- sqrt(-3 * slope)
      return(R_g)
    },
    
    #' Calculate form factor for a sphere
    #' @param q Scattering vector in m^-1
    #' @param radius Sphere radius in meters
    #' @return Form factor P(q)
    form_factor_sphere = function(q, radius) {
      qr <- q * radius
      if (qr == 0) {
        return(1.0)
      }
      return((3 * (sin(qr) - qr * cos(qr)) / qr^3)^2)
    },
    
    #' Calculate form factor for a cylindrical rod
    #' @param q Scattering vector in m^-1
    #' @param length Rod length in meters
    #' @param radius Rod radius in meters
    #' @return Form factor P(q)
    form_factor_rod = function(q, length, radius) {
      qL <- q * length / 2
      qr <- q * radius
      
      if (qL == 0 && qr == 0) {
        return(1.0)
      }
      
      # For a rod of length L and radius r
      # P(q) = (2 / (qL)) * j1(qL) * (2 * j1(qr) / (qr))^2
      # where j1 is the first-order spherical Bessel function
      # j1(x) = sin(x)/x^2 - cos(x)/x
      j1 <- function(x) {
        if (x == 0) return(1/3)
        return(sin(x)/x^2 - cos(x)/x)
      }
      
      if (qL > 0) {
        term1 <- (2 / qL) * j1(qL)
      } else {
        term1 <- 1.0
      }
      
      if (qr > 0) {
        term2 <- (2 * j1(qr) / qr)^2
      } else {
        term2 <- 1.0
      }
      
      return(term1 * term2)
    }
  )
}

#' @title DynamicLightScattering
#' @description Dynamic Light Scattering (DLS) analysis
#' @param wavelength Laser wavelength in meters
#' @param refractive_index Refractive index of medium
#' @return A list with DLS analysis methods
DynamicLightScattering <- function(wavelength = 632.8e-9, refractive_index = 1.33) {
  lambda_ <- wavelength
  n <- refractive_index
  kB <- 1.380649e-23  # J/K
  
  list(
    lambda_ = lambda_,
    n = n,
    kB = kB,
    
    #' Calculate wave vector magnitude in medium
    wave_vector = function() {
      return((2 * pi * n) / lambda_)
    },
    
    #' Calculate theoretical autocorrelation function
    #' @param time_points Array of time points in seconds
    #' @param diffusion_coefficient Diffusion coefficient in m^2/s
    #' @param angle Scattering angle in degrees
    #' @param baseline Baseline level
    #' @return Autocorrelation function values
    autocorrelation_function = function(time_points, diffusion_coefficient, 
                                      angle = 90, baseline = 0.1) {
      theta <- radians(angle)
      q <- 2 * wave_vector() * sin(theta / 2)
      
      # g1(t) = exp(-D * q^2 * t)
      g1 <- exp(-diffusion_coefficient * q^2 * time_points)
      
      # g2(t) = A * (1 + beta * g1(t)^2)
      # For simplicity, assume beta = 1 and A = 1 - baseline
      g2 <- (1 - baseline) * (1 + g1^2) + baseline
      
      return(g2)
    },
    
    #' Calculate diffusion coefficient from DLS autocorrelation
    #' @param time_points Array of time points in seconds
    #' @param autocorrelation Array of autocorrelation function values
    #' @param angle Scattering angle in degrees
    #' @param viscosity Viscosity in Pa*s
    #' @param temperature Temperature in Kelvin
    #' @return Diffusion coefficient in m^2/s
    diffusion_coefficient_from_dls = function(time_points, autocorrelation, 
                                            angle = 90, viscosity = 0.00089, temperature = 298.15) {
      theta <- radians(angle)
      q <- 2 * wave_vector() * sin(theta / 2)
      
      # Fit: g1(t) = exp(-D * q^2 * t)
      # g2(t) - baseline = A * exp(-2 * D * q^2 * t)
      
      # Subtract baseline (assume last 10% is baseline)
      n_baseline <- floor(0.1 * length(autocorrelation))
      baseline <- mean(tail(autocorrelation, n_baseline))
      g2_corrected <- autocorrelation - baseline
      
      # Take square root for g1
      g1 <- sqrt(g2_corrected / (1 - baseline))
      
      # Fit: ln(g1) = -D * q^2 * t
      log_g1 <- log(g1)
      
      # Linear fit
      fit <- lm(log_g1 ~ -q^2 * time_points)
      D <- coef(fit)[2]
      
      return(D)
    },
    
    #' Calculate hydrodynamic radius from diffusion coefficient
    #' @param diffusion_coefficient Diffusion coefficient in m^2/s
    #' @param viscosity Viscosity in Pa*s
    #' @param temperature Temperature in Kelvin
    #' @return Hydrodynamic radius in meters
    hydrodynamic_radius = function(diffusion_coefficient, viscosity, temperature = 298.15) {
      kB <- 1.380649e-23
      return(kB * temperature / (6 * pi * viscosity * diffusion_coefficient))
    },
    
    #' Calculate size distribution using cumulants analysis
    #' @param autocorrelation Array of autocorrelation function values
    #' @param time_points Array of time points in seconds
    #' @param angle Scattering angle in degrees
    #' @return List with mean diffusion coefficient and PDI
    size_distribution_from_cumulants = function(autocorrelation, time_points, angle = 90) {
      theta <- radians(angle)
      q <- 2 * wave_vector() * sin(theta / 2)
      
      # Fit: ln(g1) = -Gamma * t + (mu2 / 2) * t^2 - ...
      # where Gamma = D * q^2
      # and mu2 / Gamma^2 = PDI (polydispersity index)
      
      # Subtract baseline
      n_baseline <- floor(0.1 * length(autocorrelation))
      baseline <- mean(tail(autocorrelation, n_baseline))
      g2_corrected <- autocorrelation - baseline
      g1 <- sqrt(g2_corrected / (1 - baseline))
      
      # Fit second-order polynomial to ln(g1)
      log_g1 <- log(g1)
      
      fit <- lm(log_g1 ~ time_points + I(time_points^2))
      Gamma <- coef(fit)[2]
      mu2_div_2 <- coef(fit)[3]
      
      # Calculate PDI
      PDI <- if (Gamma != 0) mu2_div_2 / Gamma^2 else 0
      
      # Calculate mean diffusion coefficient
      D <- Gamma / q^2
      
      return(list(D = D, PDI = PDI))
    }
  )
}

#' @title SmallAngleScattering
#' @description Small-Angle X-ray and Neutron Scattering (SAXS/SANS) analysis
#' @return A list with SAXS/SANS analysis methods
SmallAngleScattering <- function() {
  kB <- 1.380649e-23
  N_A <- 6.02214076e23
  
  list(
    kB = kB,
    N_A = N_A,
    
    #' Calculate scattering intensity for spherical particles
    #' @param q_values Array of scattering vectors in m^-1
    #' @param radius Particle radius in meters
    #' @param contrast Contrast (difference in scattering length density) in m^-2
    #' @param volume_fraction Volume fraction of particles
    #' @return Array of scattering intensities
    scattering_intensity_sphere = function(q_values, radius, contrast, volume_fraction = 0.01) {
      form_factors <- sapply(q_values, function(q) form_factor_sphere(q, radius))
      
      # I(q) = N * V^2 * (Delta rho)^2 * P(q) * S(q)
      # For dilute systems, S(q) ~ 1
      
      N <- volume_fraction / ((4/3) * pi * radius^3)
      V <- (4/3) * pi * radius^3
      
      intensity <- N * V^2 * contrast^2 * form_factors
      
      return(intensity)
    },
    
    #' Calculate form factor for a sphere
    #' @param q Scattering vector in m^-1
    #' @param radius Sphere radius in meters
    #' @return Form factor P(q)
    form_factor_sphere = function(q, radius) {
      qr <- q * radius
      if (qr == 0) {
        return(1.0)
      }
      return((3 * (sin(qr) - qr * cos(qr)) / qr^3)^2)
    },
    
    #' Calculate form factor for a vesicle (hollow sphere)
    #' @param q_values Array of scattering vectors in m^-1
    #' @param outer_radius Outer radius in meters
    #' @param thickness Shell thickness in meters
    #' @return Array of form factors
    form_factor_vesicle = function(q_values, outer_radius, thickness) {
      inner_radius <- outer_radius - thickness
      
      # Form factor for hollow sphere
      # P(q) proportional to (R_outer^3 * F(q, R_outer) - R_inner^3 * F(q, R_inner))^2
      
      F <- function(q, R) {
        if (q * R == 0) {
          return(1.0)
        }
        return(3 * (sin(q * R) - q * R * cos(q * R)) / (q * R)^3)
      }
      
      form_factors <- numeric(length(q_values))
      for (i in seq_along(q_values)) {
        q <- q_values[i]
        if (q == 0) {
          form_factors[i] <- 0
          next
        }
        
        F_outer <- F(q, outer_radius)
        F_inner <- F(q, inner_radius)
        
        # P(q) proportional to (R_outer^3 * F_outer - R_inner^3 * F_inner)^2
        form_factors[i] <- (outer_radius^3 * F_outer - inner_radius^3 * F_inner)^2
      }
      
      return(form_factors)
    },
    
    #' Create Guinier plot and calculate R_g
    #' @param intensity Array of scattering intensities
    #' @param q_values Array of scattering vectors in m^-1
    #' @return List with q_values, ln_intensity, and R_g
    guinier_plot = function(intensity, q_values) {
      log_intensity <- log(intensity)
      q_squared <- q_values^2
      
      # Linear fit
      fit <- lm(log_intensity ~ q_squared)
      slope <- coef(fit)[2]
      
      R_g <- sqrt(-3 * slope)
      
      return(list(q_values = q_values, log_intensity = log_intensity, R_g = R_g))
    },
    
    #' Create Kratky plot
    #' @param intensity Array of scattering intensities
    #' @param q_values Array of scattering vectors in m^-1
    #' @return List with q_values and q^2 * I(q)
    kratky_plot = function(intensity, q_values) {
      return(list(q_values = q_values, kratky = q_values^2 * intensity))
    },
    
    #' Calculate pair distance distribution function P(r)
    #' @param intensity Array of scattering intensities
    #' @param q_values Array of scattering vectors in m^-1
    #' @param q_max Maximum q value for integration
    #' @return List with r_values and P_r_values
    pair_distance_distribution = function(intensity, q_values, q_max) {
      # P(r) = (2 / pi) * integral_0^q_max [q * r * sin(qr) * I(q) * dq]
      
      # Filter q values up to q_max
      mask <- q_values <= q_max
      q_filtered <- q_values[mask]
      I_filtered <- intensity[mask]
      
      r_values <- seq(0, 2 * pi / q_max, length.out = 100)
      P_r_values <- numeric(length(r_values))
      
      for (i in seq_along(r_values)) {
        r <- r_values[i]
        integrand <- q_filtered * r * sin(q_filtered * r) * I_filtered
        P_r_values[i] <- sum(integrand * diff(c(0, q_filtered)))
      }
      
      P_r_values <- P_r_values * 2 / pi
      
      return(list(r_values = r_values, P_r_values = P_r_values))
    }
  )
}

#' @title main
#' @description Example usage of scattering analysis tools
#' @export
main <- function() {
  cat("============================================================\n")
  cat("Light Scattering and Small-Angle Scattering Analysis\n")
  cat("============================================================\n")
  
  # Static Light Scattering
  cat("\n1. Static Light Scattering (SLS)\n")
  cat("----------------------------------------\n")
  
  sls <- StaticLightScattering()
  
  # Example: Protein solution
  concentration <- 1.0  # kg/m^3 = 1 g/L
  molecular_weight <- 50000 / N_A  # g/mol to kg/mol (50000 g/mol = 0.05 kg/mol)
  dn_dc <- 0.18 * 1e-6  # mL/g = 0.18 cm^3/g = 1.8e-7 m^3/kg
  
  R <- sls$rayleigh_ratio(concentration, molecular_weight, dn_dc)
  cat(paste("Rayleigh ratio:", format(R, scientific = 3), "m^-1\n"))
  
  # Calculate molecular weight from SLS
  concentrations <- c(0.5, 1.0, 1.5, 2.0) * 1e-3  # kg/m^3 (0.5-2 g/L)
  rayleigh_ratios <- sapply(concentrations, function(c) sls$rayleigh_ratio(c, molecular_weight, dn_dc))
  
  mw_result <- sls$molecular_weight_from_sls(rayleigh_ratios, concentrations, dn_dc)
  cat(paste("Calculated molecular weight:", format(mw_result$M * 1e3, digits = 2), "kg/mol =", 
            format(mw_result$M * N_A, digits = 0), "g/mol\n"))
  cat(paste("Second virial coefficient:", format(mw_result$A2, scientific = 3), "m^3 mol/kg^2\n"))
  
  # Radius of gyration from Guinier
  q_values <- seq(0.1, 3.0, length.out = 50)  # nm^-1
  q_values_m <- q_values * 1e9  # Convert to m^-1
  R_g <- 3.0e-9  # 3 nm
  
  # Form factor for sphere
  form_factors <- sapply(q_values_m, function(q) sls$form_factor_sphere(q, R_g))
  
  # Guinier plot
  R_g_calc <- sls$radius_of_gyration_from_guinier(form_factors, q_values_m)
  cat(paste("Radius of gyration from Guinier:", format(R_g_calc * 1e9, digits = 2), "nm\n"))
  
  # Dynamic Light Scattering
  cat("\n2. Dynamic Light Scattering (DLS)\n")
  cat("----------------------------------------\n")
  
  dls <- DynamicLightScattering()
  
  # Example: Particle with known diffusion coefficient
  D <- 1e-10  # m^2/s (typical for 10 nm particle)
  time_points <- seq(0, 1e-3, length.out = 100)  # 0 to 1 ms
  
  autocorr <- dls$autocorrelation_function(time_points, D)
  
  # Calculate diffusion coefficient from autocorrelation
  D_calc <- dls$diffusion_coefficient_from_dls(time_points, autocorr)
  cat(paste("Calculated diffusion coefficient:", format(D_calc, scientific = 3), "m^2/s\n"))
  
  # Calculate hydrodynamic radius
  viscosity <- 0.00089  # Pa*s (water at 25C)
  R_h <- dls$hydrodynamic_radius(D, viscosity)
  cat(paste("Hydrodynamic radius:", format(R_h * 1e9, digits = 2), "nm\n"))
  
  # Size distribution from cumulants
  cumulants <- dls$size_distribution_from_cumulants(autocorr, time_points)
  cat(paste("Mean diffusion coefficient (cumulants):", format(cumulants$D, scientific = 3), "m^2/s\n"))
  cat(paste("Polydispersity index:", format(cumulants$PDI, digits = 3), "\n"))
  
  # Small-Angle Scattering
  cat("\n3. Small-Angle X-ray/Neutron Scattering (SAXS/SANS)\n")
  cat("----------------------------------------\n")
  
  sas <- SmallAngleScattering()
  
  # Example: Spherical protein
  radius_sas <- 3e-9  # 3 nm
  contrast <- 1e-5  # m^-2 (typical for protein in water)
  volume_fraction <- 0.01
  
  q_values_sas <- seq(0.1, 10, length.out = 100)  # nm^-1
  q_values_sas_m <- q_values_sas * 1e9  # m^-1
  
  intensity <- sas$scattering_intensity_sphere(q_values_sas_m, radius_sas, contrast, volume_fraction)
  
  # Guinier plot
  guinier_result <- sas$guinier_plot(intensity, q_values_sas_m)
  cat(paste("Radius of gyration from SAXS Guinier:", format(guinier_result$R_g * 1e9, digits = 2), "nm\n"))
  
  # Kratky plot
  kratky_result <- sas$kratky_plot(intensity, q_values_sas_m)
  
  # Pair distance distribution
  pd_result <- sas$pair_distance_distribution(intensity, q_values_sas_m, q_max = 10e9)
  cat(paste("Maximum dimension from P(r):", format(max(pd_result$r_values) * 1e9, digits = 2), "nm\n"))
  
  # Vesicle form factor
  outer_radius <- 20e-9  # 20 nm
  thickness <- 4e-9  # 4 nm
  form_factors_vesicle <- sas$form_factor_vesicle(q_values_sas_m, outer_radius, thickness)
  
  # Plot results
  cat("\n4. Creating Visualizations\n")
  cat("----------------------------------------\n")
  
  if (requireNamespace("png", quietly = TRUE)) {
    png('scattering/scattering_analysis.png', width = 1000, height = 800)
    par(mfrow = c(2, 2), mar = c(4, 4, 3, 2) + 0.1)
    
    # SLS: Debye plot
    K <- (4 * pi^2 * sls$n^2 * dn_dc^2) / (sls$lambda_^4 * sls$N_A)
    y <- K * concentrations / rayleigh_ratios
    plot(concentrations * 1000, y, type = 'o', col = 'blue', pch = 19,
         xlab = 'Concentration (g/L)', ylab = 'K c / R',
         main = 'Debye Plot (SLS)')
    lines(concentrations * 1000, y, col = 'blue')
    grid()
    
    # SLS: Guinier plot
    q_guinier_nm <- guinier_result$q_values * 1e-9
    plot(q_guinier_nm^2, guinier_result$log_intensity, type = 'o', col = 'red', pch = 19,
         xlab = expression(q^2 ~ (nm^-2)), ylab = 'ln(I(q))',
         main = 'Guinier Plot')
    lines(q_guinier_nm^2, guinier_result$log_intensity, col = 'red')
    grid()
    
    # DLS: Autocorrelation function
    plot(time_points * 1e6, autocorr, type = 'l', col = 'green',
         xlab = expression(paste(Delta, t, " (", mu, "s)")),
         ylab = expression(g[2](t)),
         main = 'DLS Autocorrelation')
    grid()
    
    # SAXS: Scattering intensity
    q_nm <- q_values_sas_m * 1e-9
    plot(q_nm, intensity, type = 'l', col = 'blue', log = 'xy',
         xlab = 'q (nm^-1)', ylab = 'I(q)',
         main = 'SAXS Scattering Intensity')
    grid()
    
    dev.off()
    
    # Additional plots
    png('scattering/scattering_analysis_detailed.png', width = 1000, height = 800)
    par(mfrow = c(2, 2), mar = c(4, 4, 3, 2) + 0.1)
    
    # Kratky plot
    plot(kratky_result$q_values * 1e-9, kratky_result$kratky, type = 'l', col = 'red',
         xlab = 'q (nm^-1)', ylab = expression(q^2 * I(q)),
         main = 'Kratky Plot')
    grid()
    
    # Pair distance distribution
    r_nm <- pd_result$r_values * 1e9
    plot(r_nm, pd_result$P_r_values, type = 'l', col = 'magenta',
         xlab = 'Distance (nm)', ylab = 'P(r)',
         main = 'Pair Distance Distribution')
    grid()
    
    # Form factors
    plot(q_nm, form_factors, type = 'l', col = 'cyan',
         xlab = 'q (nm^-1)', ylab = 'P(q)',
         main = 'Form Factors')
    lines(q_nm, form_factors_vesicle, col = 'yellow')
    legend(c('Sphere', 'Vesicle'), col = c('cyan', 'yellow'), lty = 1, x = 'topright')
    grid()
    
    # SLS form factor
    plot(q_values, form_factors, type = 'l', col = 'blue',
         xlab = 'q (nm^-1)', ylab = 'P(q)',
         main = 'SLS Form Factor (Sphere)')
    grid()
    
    dev.off()
    cat("Plots saved as 'scattering/scattering_analysis.png' and 'scattering/scattering_analysis_detailed.png'\n")
  }
}

# Run main if executed directly
if (interactive() || !exists(".GlobalEnv")) {
  main()
}
