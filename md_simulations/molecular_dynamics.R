#' Molecular Dynamics Simulation Tools for Pharmaceutical Systems
#'
#' Tools for setting up and analyzing classical molecular dynamics simulations
#' for pharmaceutically relevant molecules, particularly peptides and their interactions
#' with bile salts, phospholipids, and micelles.
#'
#' Course: Molecular Physical Pharmacy (3FC003)

#' @title MDSystem
#' @description Represents a molecular dynamics simulation system
#' @param temperature Temperature in Kelvin (default 310.0 = 37 C)
#' @param pressure Pressure in bar (default 1.0)
#' @return A list with MD system properties and methods
MDSystem <- function(temperature = 310.0, pressure = 1.0) {
  atoms <- list()
  positions <- list()
  velocities <- list()
  forces <- list()
  masses <- list()
  charges <- list()
  atom_types <- list()
  box_dimensions <- c(0.0, 0.0, 0.0)
  time <- 0.0
  dt <- 0.002  # ps (typical MD timestep)
  
  list(
    T = temperature,
    P = pressure,
    atoms = atoms,
    positions = positions,
    velocities = velocities,
    forces = forces,
    masses = masses,
    charges = charges,
    atom_types = atom_types,
    box_dimensions = box_dimensions,
    time = time,
    dt = dt,
    
    #' Add an atom to the system
    #' @param position Position vector c(x, y, z) in nm
    #' @param velocity Velocity vector c(vx, vy, vz) in nm/ps
    #' @param mass Atomic mass in amu
    #' @param charge Partial charge in elementary charge units
    #' @param atom_type Atom type name
    add_atom = function(position, velocity, mass, charge = 0.0, atom_type = "") {
      atoms <<- c(atoms, length(atoms) + 1)
      positions <<- c(positions, list(position))
      velocities <<- c(velocities, list(velocity))
      masses <<- c(masses, mass)
      charges <<- c(charges, charge)
      atom_types <<- c(atom_types, atom_type)
      forces <<- c(forces, list(rep(0, 3)))
    },
    
    #' Set simulation box dimensions
    #' @param dimensions Box dimensions c(Lx, Ly, Lz) in nm
    set_box = function(dimensions) {
      box_dimensions <<- dimensions
    },
    
    #' Calculate center of mass
    #' @param atom_indices List of atom indices to include (NULL for all)
    #' @return Center of mass position c(x, y, z) in nm
    calculate_center_of_mass = function(atom_indices = NULL) {
      if (is.null(atom_indices)) {
        atom_indices <- 1:length(atoms)
      }
      
      total_mass <- 0.0
      com <- c(0, 0, 0)
      
      for (i in atom_indices) {
        mass <- masses[[i]]
        pos <- positions[[i]]
        total_mass <- total_mass + mass
        com <- com + mass * pos
      }
      
      if (total_mass > 0) {
        com <- com / total_mass
      }
      
      return(com)
    },
    
    #' Calculate radius of gyration
    #' @param atom_indices List of atom indices to include (NULL for all)
    #' @return Radius of gyration in nm
    calculate_radius_of_gyration = function(atom_indices = NULL) {
      if (is.null(atom_indices)) {
        atom_indices <- 1:length(atoms)
      }
      
      com <- calculate_center_of_mass(atom_indices)
      
      total_mass <- 0.0
      sum_squared <- 0.0
      
      for (i in atom_indices) {
        mass <- masses[[i]]
        pos <- positions[[i]]
        total_mass <- total_mass + mass
        sum_squared <- sum_squared + mass * sum((pos - com)^2)
      }
      
      if (total_mass > 0) {
        return(sqrt(sum_squared / total_mass))
      } else {
        return(0.0)
      }
    },
    
    #' Calculate pairwise distance matrix
    #' @param atom_indices List of atom indices to include (NULL for all)
    #' @return Distance matrix in nm
    calculate_distance_matrix = function(atom_indices = NULL) {
      if (is.null(atom_indices)) {
        atom_indices <- 1:length(atoms)
      }
      
      n <- length(atom_indices)
      dist_matrix <- matrix(0, nrow = n, ncol = n)
      
      for (i in 1:n) {
        for (j in 1:n) {
          if (i != j) {
            pos_i <- positions[[atom_indices[i]]]
            pos_j <- positions[[atom_indices[j]]]
            dist_matrix[i, j] <- sqrt(sum((pos_i - pos_j)^2))
          }
        }
      }
      
      return(dist_matrix)
    },
    
    #' Calculate radial distribution function
    #' @param atom_type_a First atom type
    #' @param atom_type_b Second atom type
    #' @param r_max Maximum distance in nm
    #' @param bins Number of bins
    #' @return List with r_values and g_r_values
    calculate_rdf = function(atom_type_a, atom_type_b, r_max = 5.0, bins = 100) {
      indices_a <- which(sapply(atom_types, function(x) x == atom_type_a))
      indices_b <- which(sapply(atom_types, function(x) x == atom_type_b))
      
      if (length(indices_a) == 0 || length(indices_b) == 0) {
        return(list(r_values = numeric(0), g_r_values = numeric(0)))
      }
      
      # Calculate all pairwise distances
      distances <- numeric(0)
      for (i in indices_a) {
        for (j in indices_b) {
          if (i != j) {
            dist <- sqrt(sum((positions[[i]] - positions[[j]])^2))
            distances <- c(distances, dist)
          }
        }
      }
      
      if (length(distances) == 0) {
        return(list(r_values = numeric(0), g_r_values = numeric(0)))
      }
      
      # Histogram
      hist_result <- hist(distances, breaks = seq(0, r_max, length.out = bins + 1), plot = FALSE)
      r_values <- (hist_result$breaks[-1] + hist_result$breaks[-length(hist_result$breaks)]) / 2
      dr <- hist_result$breaks[2] - hist_result$breaks[1]
      
      # Normalize
      n_a <- length(indices_a)
      n_b <- length(indices_b)
      box_volume <- prod(box_dimensions)
      rho_b <- n_b / box_volume
      
      g_r <- hist_result$counts / (n_a * n_b * 4 * pi * r_values^2 * dr * rho_b)
      
      return(list(r_values = r_values, g_r_values = g_r))
    },
    
    #' Calculate mean squared displacement from trajectory
    #' @param trajectory List of positions arrays over time
    #' @param atom_indices List of atom indices to include (NULL for all)
    #' @return Array of MSD values
    calculate_msd = function(trajectory, atom_indices = NULL) {
      if (is.null(atom_indices)) {
        atom_indices <- 1:length(atoms)
      }
      
      n_atoms <- length(atom_indices)
      n_frames <- length(trajectory)
      msd_values <- numeric(n_frames)
      
      for (t in 1:n_frames) {
        sum_sq <- 0.0
        for (i in atom_indices) {
          displacement <- trajectory[[t]][[i]] - trajectory[[1]][[i]]
          sum_sq <- sum_sq + sum(displacement^2)
        }
        msd_values[t] <- sum_sq / n_atoms
      }
      
      return(msd_values)
    },
    
    #' Calculate diffusion coefficient from MSD
    #' @param msd_values Array of MSD values
    #' @param dt Time step in ps
    #' @param dimensions Number of dimensions (default 3)
    #' @return Diffusion coefficient in nm^2/ps
    calculate_diffusion_coefficient = function(msd_values, dt, dimensions = 3) {
      # MSD = 2 * D * t * dimensions
      time_points <- (0:(length(msd_values) - 1)) * dt
      
      # Fit last 20% of data
      n_fit <- max(floor(0.2 * length(msd_values)), 2)
      
      t_fit <- tail(time_points, n_fit)
      msd_fit <- tail(msd_values, n_fit)
      
      # Linear fit: MSD = slope * t
      fit <- lm(msd_fit ~ t_fit)
      slope <- coef(fit)[2]
      
      return(slope / (2 * dimensions))
    }
  )
}

#' @title PeptideBuilder
#' @description Build simple peptide structures for MD simulations
#' @return A list with peptide builder methods
PeptideBuilder <- function() {
  amino_acid_data <- list(
    GLY = list(mass = 57.05, charge = 0.0),
    ALA = list(mass = 71.08, charge = 0.0),
    VAL = list(mass = 99.13, charge = 0.0),
    LEU = list(mass = 113.16, charge = 0.0),
    ILE = list(mass = 113.16, charge = 0.0),
    PHE = list(mass = 147.18, charge = 0.0),
    TRP = list(mass = 186.21, charge = 0.0),
    SER = list(mass = 87.08, charge = 0.0),
    THR = list(mass = 101.11, charge = 0.0),
    TYR = list(mass = 163.18, charge = 0.0),
    CYS = list(mass = 103.15, charge = 0.0),
    MET = list(mass = 131.19, charge = 0.0),
    PRO = list(mass = 97.12, charge = 0.0),
    ASN = list(mass = 114.10, charge = 0.0),
    GLN = list(mass = 128.13, charge = 0.0),
    ASP = list(mass = 115.09, charge = -1.0),
    GLU = list(mass = 129.12, charge = -1.0),
    LYS = list(mass = 128.17, charge = +1.0),
    ARG = list(mass = 156.19, charge = +1.0),
    HIS = list(mass = 137.14, charge = +1.0)
  )
  
  list(
    amino_acid_data = amino_acid_data,
    
    #' Build a simple peptide chain with random coil conformation
    #' @param sequence Amino acid sequence (e.g., 'ALA-GLY-VAL')
    #' @param start_position Starting position c(x, y, z) in nm
    #' @return MDSystem with the peptide
    build_peptide = function(sequence, start_position = c(0, 0, 0)) {
      system <- MDSystem()
      
      aa_list <- unlist(strsplit(sequence, "-"))
      current_pos <- start_position
      
      # Simple random walk to create peptide conformation
      for (aa in aa_list) {
        if (!(aa %in% names(amino_acid_data))) {
          next
        }
        
        aa_data <- amino_acid_data[[aa]]
        mass <- aa_data$mass
        charge <- aa_data$charge
        
        # Add backbone atoms (simplified: C-alpha only)
        system$add_atom(
          position = current_pos,
          velocity = rnorm(3, sd = 0.1),  # Random initial velocity
          mass = mass,
          charge = charge,
          atom_type = aa
        )
        
        # Random direction for next amino acid
        # Bond length ~0.38 nm (typical C-alpha to C-alpha)
        direction <- rnorm(3)
        direction <- direction / sqrt(sum(direction^2))
        current_pos <- current_pos + direction * 0.38
      }
      
      return(system)
    }
  )
}

#' @title BileSaltBuilder
#' @description Build bile salt molecules for MD simulations
#' @return A list with bile salt builder methods
BileSaltBuilder <- function() {
  list(
    #' Build a simplified cholate (bile salt) molecule
    #' @param position Starting position c(x, y, z) in nm
    #' @return MDSystem with the bile salt
    build_cholate = function(position = c(0, 0, 0)) {
      system <- MDSystem()
      
      # Simplified cholate: steroid nucleus + side chain + carboxylate
      # Mass ~408 g/mol for sodium cholate
      # Charge ~ -1 (carboxylate group)
      
      system$add_atom(
        position = position,
        velocity = rnorm(3, sd = 0.1),
        mass = 408.0,
        charge = -1.0,
        atom_type = "CHOLATE"
      )
      
      return(system)
    },
    
    #' Build a bile salt micelle
    #' @param n_bile_salts Number of bile salt molecules
    #' @param center Center position c(x, y, z) in nm
    #' @return MDSystem with the micelle
    build_micelle = function(n_bile_salts = 20, center = c(0, 0, 0)) {
      system <- MDSystem()
      
      # Micelle radius ~2-3 nm for bile salt micelles
      micelle_radius <- 2.5
      
      for (i in 1:n_bile_salts) {
        # Random position on sphere
        theta <- runif(1, 0, pi)
        phi <- runif(1, 0, 2 * pi)
        
        x <- center[1] + micelle_radius * sin(theta) * cos(phi)
        y <- center[2] + micelle_radius * sin(theta) * sin(phi)
        z <- center[3] + micelle_radius * cos(theta)
        
        # Add some randomness
        x <- x + rnorm(1, sd = 0.3)
        y <- y + rnorm(1, sd = 0.3)
        z <- z + rnorm(1, sd = 0.3)
        
        system$add_atom(
          position = c(x, y, z),
          velocity = rnorm(3, sd = 0.1),
          mass = 408.0,
          charge = -1.0,
          atom_type = "CHOLATE"
        )
      }
      
      return(system)
    }
  )
}

#' @title PhospholipidBuilder
#' @description Build phospholipid molecules for MD simulations
#' @return A list with phospholipid builder methods
PhospholipidBuilder <- function() {
  list(
    #' Build a simplified phosphatidylcholine (PC) molecule
    #' @param position Head group position c(x, y, z) in nm
    #' @param tail_length Tail length in nm
    #' @return MDSystem with the phospholipid
    build_pc = function(position = c(0, 0, 0), tail_length = 1.5) {
      system <- MDSystem()
      
      # Head group (choline + phosphate + glycerol)
      # Mass ~180 g/mol for head group
      head_mass <- 180.0
      head_charge <- 0.0  # PC is zwitterionic
      
      system$add_atom(
        position = position,
        velocity = rnorm(3, sd = 0.1),
        mass = head_mass,
        charge = head_charge,
        atom_type = "PC_HEAD"
      )
      
      # Two tails (simplified as single beads)
      tail_mass <- 250.0  # Approximate for two fatty acid chains
      
      # Tail 1
      tail1_pos <- position + c(tail_length, 0, 0)
      system$add_atom(
        position = tail1_pos,
        velocity = rnorm(3, sd = 0.1),
        mass = tail_mass / 2,
        charge = 0.0,
        atom_type = "PC_TAIL"
      )
      
      # Tail 2
      tail2_pos <- position + c(-tail_length/2, tail_length * sqrt(3)/2, 0)
      system$add_atom(
        position = tail2_pos,
        velocity = rnorm(3, sd = 0.1),
        mass = tail_mass / 2,
        charge = 0.0,
        atom_type = "PC_TAIL"
      )
      
      return(system)
    },
    
    #' Build a phospholipid bilayer
    #' @param n_lipids Number of lipids per leaflet
    #' @param box_size Simulation box size c(Lx, Ly, Lz) in nm
    #' @return MDSystem with the bilayer
    build_bilayer = function(n_lipids = 100, box_size = c(10, 10, 10)) {
      system <- MDSystem()
      system$set_box(box_size)
      
      # Create two leaflets
      for (leaflet in c(-1, 1)) {
        for (i in 1:n_lipids) {
          # Random position in xy plane
          x <- runif(1, 0, box_size[1])
          y <- runif(1, 0, box_size[2])
          z <- box_size[3] / 2 * leaflet
          
          # Add phospholipid
          pc_system <- build_pc(c(x, y, z))
          
          for (j in 1:length(pc_system$atoms)) {
            system$add_atom(
              position = pc_system$positions[[j]],
              velocity = pc_system$velocities[[j]],
              mass = pc_system$masses[[j]],
              charge = pc_system$charges[[j]],
              atom_type = pc_system$atom_types[[j]]
            )
          }
        }
      }
      
      return(system)
    }
  )
}

#' @title main
#' @description Example usage of molecular dynamics simulation tools
#' @export
main <- function() {
  cat("============================================================\n")
  cat("Molecular Dynamics Simulation Tools\n")
  cat("============================================================\n")
  
  # Build a peptide
  cat("\n1. Building a Peptide\n")
  cat("----------------------------------------\n")
  
  peptide_builder <- PeptideBuilder()
  peptide <- peptide_builder$build_peptide('ALA-GLY-VAL-LEU-ILE')
  
  cat(paste("Peptide has", length(peptide$atoms), "atoms\n"))
  cat(paste("Atom types:", paste(unique(peptide$atom_types), collapse = ", "), "\n"))
  
  # Calculate properties
  com <- peptide$calculate_center_of_mass()
  rg <- peptide$calculate_radius_of_gyration()
  cat(paste("Center of mass: [", paste(format(com, digits = 2), collapse = ", "), "]\n"))
  cat(paste("Radius of gyration:", format(rg, digits = 2), "nm\n"))
  
  # Build a bile salt micelle
  cat("\n2. Building a Bile Salt Micelle\n")
  cat("----------------------------------------\n")
  
  bile_builder <- BileSaltBuilder()
  micelle <- bile_builder$build_micelle(n_bile_salts = 20)
  
  cat(paste("Micelle has", length(micelle$atoms), "bile salt molecules\n"))
  com <- micelle$calculate_center_of_mass()
  rg <- micelle$calculate_radius_of_gyration()
  cat(paste("Micelle center of mass: [", paste(format(com, digits = 2), collapse = ", "), "]\n"))
  cat(paste("Micelle radius of gyration:", format(rg, digits = 2), "nm\n"))
  
  # Build a phospholipid bilayer
  cat("\n3. Building a Phospholipid Bilayer\n")
  cat("----------------------------------------\n")
  
  lipid_builder <- PhospholipidBuilder()
  bilayer <- lipid_builder$build_bilayer(n_lipids = 50, box_size = c(10, 10, 10))
  
  cat(paste("Bilayer has", length(bilayer$atoms), "atoms\n"))
  cat(paste("Box size: [", paste(format(bilayer$box_dimensions, digits = 1), collapse = ", "), "] nm\n"))
  
  # Create a mixed system (peptide + micelle)
  cat("\n4. Creating a Mixed System\n")
  cat("----------------------------------------\n")
  
  mixed_system <- MDSystem()
  mixed_system$set_box(c(10, 10, 10))
  
  # Add peptide
  for (i in 1:length(peptide$atoms)) {
    mixed_system$add_atom(
      position = peptide$positions[[i]] + c(0, 0, 2),
      velocity = peptide$velocities[[i]],
      mass = peptide$masses[[i]],
      charge = peptide$charges[[i]],
      atom_type = peptide$atom_types[[i]]
    )
  }
  
  # Add micelle
  for (i in 1:length(micelle$atoms)) {
    mixed_system$add_atom(
      position = micelle$positions[[i]] + c(0, 0, -2),
      velocity = micelle$velocities[[i]],
      mass = micelle$masses[[i]],
      charge = micelle$charges[[i]],
      atom_type = micelle$atom_types[[i]]
    )
  }
  
  cat(paste("Mixed system has", length(mixed_system$atoms), "atoms\n"))
  atom_type_counts <- table(mixed_system$atom_types)
  cat("Atom type distribution:\n")
  print(atom_type_counts)
  
  # Calculate RDF
  cat("\n5. Calculating Radial Distribution Function\n")
  cat("----------------------------------------\n")
  
  rdf_result <- mixed_system$calculate_rdf('ALA', 'CHOLATE')
  if (length(rdf_result$r_values) > 0) {
    cat(paste("RDF calculated with", length(rdf_result$r_values), "points\n"))
    peak_idx <- which.max(rdf_result$g_r_values)
    cat(paste("First peak at r =", format(rdf_result$r_values[peak_idx], digits = 2), "nm\n"))
  } else {
    cat("No pairs found for RDF calculation\n")
  }
  
  # Visualization
  cat("\n6. Creating Visualizations\n")
  cat("----------------------------------------\n")
  
  if (requireNamespace("png", quietly = TRUE)) {
    # Extract positions for plotting
    peptide_positions <- do.call(rbind, peptide$positions)
    micelle_positions <- do.call(rbind, micelle$positions)
    mixed_positions <- do.call(rbind, mixed_system$positions)
    
    png('md_simulations/md_structures.png', width = 1000, height = 800)
    par(mfrow = c(2, 2), mar = c(4, 4, 3, 2) + 0.1)
    
    # Plot 1: Peptide structure
    plot3d <- function(pos, col, main_title, xlab = "X (nm)", ylab = "Y (nm)", zlab = "Z (nm)") {
      if (requireNamespace("rgl", quietly = TRUE)) {
        rgl::setupKnitr()
        rgl::plot3d(pos, col = col, size = 5, xlab = xlab, ylab = ylab, zlab = zlab)
        rgl::title3d(main = main_title)
      } else {
        # Fallback 2D plot
        plot(pos[, 1], pos[, 2], col = col, pch = 19, xlab = xlab, ylab = ylab,
             main = main_title)
      }
    }
    
    # Simple 2D projections for base R
    plot(peptide_positions[, 1], peptide_positions[, 2], col = 'blue', pch = 19,
         xlab = 'X (nm)', ylab = 'Y (nm)', main = 'Peptide Structure (XY projection)')
    grid()
    
    plot(micelle_positions[, 1], micelle_positions[, 2], col = 'red', pch = 19,
         xlab = 'X (nm)', ylab = 'Y (nm)', main = 'Bile Salt Micelle (XY projection)')
    grid()
    
    # Plot RDF
    if (length(rdf_result$r_values) > 0) {
      plot(rdf_result$r_values, rdf_result$g_r_values, type = 'l', col = 'green',
           xlab = 'Distance (nm)', ylab = 'g(r)', main = 'RDF: Peptide - Bile Salt')
      grid()
    }
    
    # Plot mixed system
    colors <- c(ALA = 'blue', GLY = 'cyan', VAL = 'green', LEU = 'orange', 
                ILE = 'purple', CHOLATE = 'red')
    
    # Plot each atom type
    for (at in unique(mixed_system$atom_types)) {
      indices <- which(mixed_system$atom_types == at)
      if (at %in% names(colors)) {
        points(mixed_positions[indices, 1], mixed_positions[indices, 2], 
               col = colors[[at]], pch = 19, cex = 0.7)
      }
    }
    title('Mixed System (XY projection)')
    legend('topright', legend = names(colors), col = as.vector(colors), pch = 19, cex = 0.7)
    
    dev.off()
    cat("\nPlot saved as 'md_simulations/md_structures.png'\n")
  }
}

# Run main if executed directly
if (interactive() || !exists(".GlobalEnv")) {
  main()
}
