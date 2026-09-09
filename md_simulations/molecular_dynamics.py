#!/usr/bin/env python3
"""
Molecular Dynamics Simulation Tools for Pharmaceutical Systems

Tools for setting up and analyzing classical molecular dynamics simulations
for pharmaceutically relevant molecules, particularly peptides and their interactions
with bile salts, phospholipids, and micelles.

Course: Molecular Physical Pharmacy (3FC003)
"""

import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from scipy.spatial.distance import pdist, squareform
from scipy.stats import gaussian_kde


class MDSystem:
    """Represents a molecular dynamics simulation system."""
    
    def __init__(self, temperature=310.0, pressure=1.0):
        """
        Initialize MD system.
        
        Args:
            temperature: Temperature in Kelvin (default 310 K = 37 C)
            pressure: Pressure in bar (default 1.0 bar)
        """
        self.T = temperature
        self.P = pressure
        self.atoms = []
        self.positions = []
        self.velocities = []
        self.forces = []
        self.masses = []
        self.charges = []
        self.atom_types = []
        self.box_dimensions = np.array([0.0, 0.0, 0.0])
        self.time = 0.0
        self.dt = 0.002  # ps (typical MD timestep)
        
    def add_atom(self, position, velocity, mass, charge=0.0, atom_type=""):
        """
        Add an atom to the system.
        
        Args:
            position: Position vector [x, y, z] in nm
            velocity: Velocity vector [vx, vy, vz] in nm/ps
            mass: Atomic mass in amu
            charge: Partial charge in elementary charge units
            atom_type: Atom type name
        """
        self.atoms.append(len(self.atoms))
        self.positions.append(np.array(position))
        self.velocities.append(np.array(velocity))
        self.masses.append(mass)
        self.charges.append(charge)
        self.atom_types.append(atom_type)
        self.forces.append(np.zeros(3))
        
    def set_box(self, dimensions):
        """
        Set simulation box dimensions.
        
        Args:
            dimensions: Box dimensions [Lx, Ly, Lz] in nm
        """
        self.box_dimensions = np.array(dimensions)
        
    def calculate_center_of_mass(self, atom_indices=None):
        """
        Calculate center of mass.
        
        Args:
            atom_indices: List of atom indices to include (None for all)
            
        Returns:
            Center of mass position [x, y, z] in nm
        """
        if atom_indices is None:
            atom_indices = range(len(self.atoms))
        
        total_mass = 0.0
        com = np.zeros(3)
        
        for i in atom_indices:
            mass = self.masses[i]
            pos = self.positions[i]
            total_mass += mass
            com += mass * pos
        
        if total_mass > 0:
            com /= total_mass
        
        return com
    
    def calculate_radius_of_gyration(self, atom_indices=None):
        """
        Calculate radius of gyration.
        
        Args:
            atom_indices: List of atom indices to include (None for all)
            
        Returns:
            Radius of gyration in nm
        """
        if atom_indices is None:
            atom_indices = range(len(self.atoms))
        
        com = self.calculate_center_of_mass(atom_indices)
        
        total_mass = 0.0
        sum_squared = 0.0
        
        for i in atom_indices:
            mass = self.masses[i]
            pos = self.positions[i]
            total_mass += mass
            sum_squared += mass * np.sum((pos - com)**2)
        
        if total_mass > 0:
            return np.sqrt(sum_squared / total_mass)
        else:
            return 0.0
    
    def calculate_distance_matrix(self, atom_indices=None):
        """
        Calculate pairwise distance matrix.
        
        Args:
            atom_indices: List of atom indices to include (None for all)
            
        Returns:
            Distance matrix in nm
        """
        if atom_indices is None:
            atom_indices = range(len(self.atoms))
        
        positions = [self.positions[i] for i in atom_indices]
        return squareform(pdist(positions))
    
    def calculate_rdf(self, atom_type_a, atom_type_b, r_max=5.0, bins=100):
        """
        Calculate radial distribution function.
        
        Args:
            atom_type_a: First atom type
            atom_type_b: Second atom type
            r_max: Maximum distance in nm
            bins: Number of bins
            
        Returns:
            Tuple of (r_values, g_r_values)
        """
        indices_a = [i for i, at in enumerate(self.atom_types) if at == atom_type_a]
        indices_b = [i for i, at in enumerate(self.atom_types) if at == atom_type_b]
        
        if len(indices_a) == 0 or len(indices_b) == 0:
            return np.array([]), np.array([])
        
        # Calculate all pairwise distances
        distances = []
        for i in indices_a:
            for j in indices_b:
                if i != j:
                    dist = np.linalg.norm(self.positions[i] - self.positions[j])
                    distances.append(dist)
        
        if len(distances) == 0:
            return np.array([]), np.array([])
        
        # Histogram
        hist, edges = np.histogram(distances, bins=bins, range=(0, r_max))
        r_values = (edges[:-1] + edges[1:]) / 2
        dr = edges[1] - edges[0]
        
        # Normalize
        n_a = len(indices_a)
        n_b = len(indices_b)
        box_volume = np.prod(self.box_dimensions)
        rho_b = n_b / box_volume
        
        g_r = hist / (n_a * n_b * 4 * np.pi * r_values**2 * dr * rho_b)
        
        return r_values, g_r
    
    def calculate_msd(self, trajectory, atom_indices=None):
        """
        Calculate mean squared displacement from trajectory.
        
        Args:
            trajectory: List of positions arrays over time
            atom_indices: List of atom indices to include (None for all)
            
        Returns:
            Array of MSD values
        """
        if atom_indices is None:
            atom_indices = range(len(self.atoms))
        
        n_atoms = len(atom_indices)
        n_frames = len(trajectory)
        msd_values = np.zeros(n_frames)
        
        for t in range(n_frames):
            sum_sq = 0.0
            for i in atom_indices:
                displacement = trajectory[t][i] - trajectory[0][i]
                sum_sq += np.sum(displacement**2)
            msd_values[t] = sum_sq / n_atoms
        
        return msd_values
    
    def calculate_diffusion_coefficient(self, msd_values, dt, dimensions=3):
        """
        Calculate diffusion coefficient from MSD.
        
        Args:
            msd_values: Array of MSD values
            dt: Time step in ps
            dimensions: Number of dimensions (default 3)
            
        Returns:
            Diffusion coefficient in nm^2/ps
        """
        # MSD = 2 * D * t * dimensions
        # For long times, MSD ~ 6 * D * t (in 3D)
        
        time_points = np.arange(len(msd_values)) * dt
        
        # Fit last 20% of data
        n_fit = int(0.2 * len(msd_values))
        if n_fit < 2:
            n_fit = len(msd_values)
        
        t_fit = time_points[-n_fit:]
        msd_fit = msd_values[-n_fit:]
        
        # Linear fit: MSD = slope * t
        A = np.vstack([t_fit, np.ones(len(t_fit))]).T
        slope, _ = np.linalg.lstsq(A, msd_fit, rcond=None)[0]
        
        return slope / (2 * dimensions)


class PeptideBuilder:
    """Build simple peptide structures for MD simulations."""
    
    def __init__(self):
        """Initialize peptide builder."""
        self.amino_acid_data = {
            'GLY': {'mass': 57.05, 'charge': 0.0},
            'ALA': {'mass': 71.08, 'charge': 0.0},
            'VAL': {'mass': 99.13, 'charge': 0.0},
            'LEU': {'mass': 113.16, 'charge': 0.0},
            'ILE': {'mass': 113.16, 'charge': 0.0},
            'PHE': {'mass': 147.18, 'charge': 0.0},
            'TRP': {'mass': 186.21, 'charge': 0.0},
            'SER': {'mass': 87.08, 'charge': 0.0},
            'THR': {'mass': 101.11, 'charge': 0.0},
            'TYR': {'mass': 163.18, 'charge': 0.0},
            'CYS': {'mass': 103.15, 'charge': 0.0},
            'MET': {'mass': 131.19, 'charge': 0.0},
            'PRO': {'mass': 97.12, 'charge': 0.0},
            'ASN': {'mass': 114.10, 'charge': 0.0},
            'GLN': {'mass': 128.13, 'charge': 0.0},
            'ASP': {'mass': 115.09, 'charge': -1.0},
            'GLU': {'mass': 129.12, 'charge': -1.0},
            'LYS': {'mass': 128.17, 'charge': +1.0},
            'ARG': {'mass': 156.19, 'charge': +1.0},
            'HIS': {'mass': 137.14, 'charge': +1.0},
        }
        
    def build_peptide(self, sequence, start_position=[0, 0, 0]):
        """
        Build a simple peptide chain with random coil conformation.
        
        Args:
            sequence: Amino acid sequence (e.g., 'ALA-GLY-VAL')
            start_position: Starting position [x, y, z] in nm
            
        Returns:
            MDSystem with the peptide
        """
        system = MDSystem()
        
        aa_list = sequence.split('-')
        current_pos = np.array(start_position)
        
        # Simple random walk to create peptide conformation
        for i, aa in enumerate(aa_list):
            if aa not in self.amino_acid_data:
                continue
            
            aa_data = self.amino_acid_data[aa]
            mass = aa_data['mass']
            charge = aa_data['charge']
            
            # Add backbone atoms (simplified: C-alpha only)
            system.add_atom(
                position=current_pos,
                velocity=np.random.randn(3) * 0.1,  # Random initial velocity
                mass=mass,
                charge=charge,
                atom_type=aa
            )
            
            # Random direction for next amino acid
            # Bond length ~0.38 nm (typical C-alpha to C-alpha)
            direction = np.random.randn(3)
            direction /= np.linalg.norm(direction)
            current_pos += direction * 0.38
        
        return system


class BileSaltBuilder:
    """Build bile salt molecules for MD simulations."""
    
    def __init__(self):
        """Initialize bile salt builder."""
        pass
    
    def build_cholate(self, position=[0, 0, 0]):
        """
        Build a simplified cholate (bile salt) molecule.
        
        Args:
            position: Starting position [x, y, z] in nm
            
        Returns:
            MDSystem with the bile salt
        """
        system = MDSystem()
        
        # Simplified cholate: steroid nucleus + side chain + carboxylate
        # Mass ~408 g/mol for sodium cholate
        # Charge ~ -1 (carboxylate group)
        
        system.add_atom(
            position=np.array(position),
            velocity=np.random.randn(3) * 0.1,
            mass=408.0,
            charge=-1.0,
            atom_type="CHOLATE"
        )
        
        return system
    
    def build_micelle(self, n_bile_salts=20, center=[0, 0, 0]):
        """
        Build a bile salt micelle.
        
        Args:
            n_bile_salts: Number of bile salt molecules
            center: Center position [x, y, z] in nm
            
        Returns:
            MDSystem with the micelle
        """
        system = MDSystem()
        
        # Micelle radius ~2-3 nm for bile salt micelles
        micelle_radius = 2.5
        
        for i in range(n_bile_salts):
            # Random position on sphere
            theta = np.random.uniform(0, np.pi)
            phi = np.random.uniform(0, 2 * np.pi)
            
            x = center[0] + micelle_radius * np.sin(theta) * np.cos(phi)
            y = center[1] + micelle_radius * np.sin(theta) * np.sin(phi)
            z = center[2] + micelle_radius * np.cos(theta)
            
            # Add some randomness
            x += np.random.randn() * 0.3
            y += np.random.randn() * 0.3
            z += np.random.randn() * 0.3
            
            system.add_atom(
                position=[x, y, z],
                velocity=np.random.randn(3) * 0.1,
                mass=408.0,
                charge=-1.0,
                atom_type="CHOLATE"
            )
        
        return system


class PhospholipidBuilder:
    """Build phospholipid molecules for MD simulations."""
    
    def __init__(self):
        """Initialize phospholipid builder."""
        pass
    
    def build_pc(self, position=[0, 0, 0], tail_length=1.5):
        """
        Build a simplified phosphatidylcholine (PC) molecule.
        
        Args:
            position: Head group position [x, y, z] in nm
            tail_length: Tail length in nm
            
        Returns:
            MDSystem with the phospholipid
        """
        system = MDSystem()
        
        # Head group (choline + phosphate + glycerol)
        # Mass ~180 g/mol for head group
        head_mass = 180.0
        head_charge = 0.0  # PC is zwitterionic
        
        system.add_atom(
            position=np.array(position),
            velocity=np.random.randn(3) * 0.1,
            mass=head_mass,
            charge=head_charge,
            atom_type="PC_HEAD"
        )
        
        # Two tails (simplified as single beads)
        tail_mass = 250.0  # Approximate for two fatty acid chains
        
        # Tail 1
        tail1_pos = position + np.array([tail_length, 0, 0])
        system.add_atom(
            position=tail1_pos,
            velocity=np.random.randn(3) * 0.1,
            mass=tail_mass / 2,
            charge=0.0,
            atom_type="PC_TAIL"
        )
        
        # Tail 2
        tail2_pos = position + np.array([-tail_length/2, tail_length * np.sqrt(3)/2, 0])
        system.add_atom(
            position=tail2_pos,
            velocity=np.random.randn(3) * 0.1,
            mass=tail_mass / 2,
            charge=0.0,
            atom_type="PC_TAIL"
        )
        
        return system
    
    def build_bilayer(self, n_lipids=100, box_size=[10, 10, 10]):
        """
        Build a phospholipid bilayer.
        
        Args:
            n_lipids: Number of lipids per leaflet
            box_size: Simulation box size [Lx, Ly, Lz] in nm
            
        Returns:
            MDSystem with the bilayer
        """
        system = MDSystem()
        system.set_box(box_size)
        
        # Create two leaflets
        for leaflet in [-1, 1]:
            for i in range(n_lipids):
                # Random position in xy plane
                x = np.random.uniform(0, box_size[0])
                y = np.random.uniform(0, box_size[1])
                z = box_size[2] / 2 * leaflet
                
                # Add phospholipid
                pc_system = self.build_pc(position=[x, y, z])
                
                for j in range(len(pc_system.atoms)):
                    system.add_atom(
                        position=pc_system.positions[j],
                        velocity=pc_system.velocities[j],
                        mass=pc_system.masses[j],
                        charge=pc_system.charges[j],
                        atom_type=pc_system.atom_types[j]
                    )
        
        return system


class AnalysisTools:
    """Analysis tools for MD simulation data."""
    
    @staticmethod
    def calculate_structure_factor(positions, box_size, k_max=10):
        """
        Calculate static structure factor S(k).
        
        Args:
            positions: Array of atom positions (N x 3)
            box_size: Box dimensions [Lx, Ly, Lz]
            k_max: Maximum k vector magnitude
            
        Returns:
            Tuple of (k_values, S_k_values)
        """
        n_atoms = len(positions)
        
        # Generate k vectors
        kx = np.linspace(-k_max, k_max, 20)
        ky = np.linspace(-k_max, k_max, 20)
        kz = np.linspace(-k_max, k_max, 20)
        
        k_values = []
        S_k_values = []
        
        for kx_val in kx:
            for ky_val in ky:
                for kz_val in kz:
                    k_mag = np.sqrt(kx_val**2 + ky_val**2 + kz_val**2)
                    if k_mag > k_max or k_mag < 0.1:
                        continue
                    
                    # Calculate S(k)
                    sum_real = 0.0
                    sum_imag = 0.0
                    for pos in positions:
                        k_dot_r = kx_val * pos[0] + ky_val * pos[1] + kz_val * pos[2]
                        sum_real += np.cos(k_dot_r)
                        sum_imag += np.sin(k_dot_r)
                    
                    S_k = (sum_real**2 + sum_imag**2) / n_atoms
                    
                    k_values.append(k_mag)
                    S_k_values.append(S_k)
        
        return np.array(k_values), np.array(S_k_values)
    
    @staticmethod
    def analyze_trajectory(trajectory, dt):
        """
        Perform comprehensive trajectory analysis.
        
        Args:
            trajectory: List of positions arrays over time
            dt: Time step in ps
            
        Returns:
            Dictionary with analysis results
        """
        n_frames = len(trajectory)
        n_atoms = len(trajectory[0])
        
        results = {
            'n_frames': n_frames,
            'n_atoms': n_atoms,
            'time_points': np.arange(n_frames) * dt,
        }
        
        # Calculate MSD
        msd = np.zeros(n_frames)
        for t in range(n_frames):
            displacements = trajectory[t] - trajectory[0]
            squared_displacements = np.sum(displacements**2, axis=1)
            msd[t] = np.mean(squared_displacements)
        results['msd'] = msd
        
        # Calculate Rg over time
        rg = np.zeros(n_frames)
        for t in range(n_frames):
            com = np.mean(trajectory[t], axis=0)
            squared_distances = np.sum((trajectory[t] - com)**2, axis=1)
            rg[t] = np.sqrt(np.mean(squared_distances))
        results['radius_of_gyration'] = rg
        
        return results


def main():
    """Example usage of molecular dynamics simulation tools."""
    print("=" * 60)
    print("Molecular Dynamics Simulation Tools")
    print("=" * 60)
    
    # Build a peptide
    print("\n1. Building a Peptide")
    print("-" * 40)
    
    peptide_builder = PeptideBuilder()
    peptide = peptide_builder.build_peptide('ALA-GLY-VAL-LEU-ILE')
    
    print(f"Peptide has {len(peptide.atoms)} atoms")
    print(f"Atom types: {set(peptide.atom_types)}")
    
    # Calculate properties
    com = peptide.calculate_center_of_mass()
    rg = peptide.calculate_radius_of_gyration()
    print(f"Center of mass: {com}")
    print(f"Radius of gyration: {rg:.2f} nm")
    
    # Build a bile salt micelle
    print("\n2. Building a Bile Salt Micelle")
    print("-" * 40)
    
    bile_builder = BileSaltBuilder()
    micelle = bile_builder.build_micelle(n_bile_salts=20)
    
    print(f"Micelle has {len(micelle.atoms)} bile salt molecules")
    com = micelle.calculate_center_of_mass()
    rg = micelle.calculate_radius_of_gyration()
    print(f"Micelle center of mass: {com}")
    print(f"Micelle radius of gyration: {rg:.2f} nm")
    
    # Build a phospholipid bilayer
    print("\n3. Building a Phospholipid Bilayer")
    print("-" * 40)
    
    lipid_builder = PhospholipidBuilder()
    bilayer = lipid_builder.build_bilayer(n_lipids=50, box_size=[10, 10, 10])
    
    print(f"Bilayer has {len(bilayer.atoms)} atoms")
    print(f"Box size: {bilayer.box_dimensions} nm")
    
    # Create a mixed system (peptide + micelle)
    print("\n4. Creating a Mixed System")
    print("-" * 40)
    
    mixed_system = MDSystem()
    mixed_system.set_box([10, 10, 10])
    
    # Add peptide
    for i in range(len(peptide.atoms)):
        mixed_system.add_atom(
            position=peptide.positions[i] + np.array([0, 0, 2]),
            velocity=peptide.velocities[i],
            mass=peptide.masses[i],
            charge=peptide.charges[i],
            atom_type=peptide.atom_types[i]
        )
    
    # Add micelle
    for i in range(len(micelle.atoms)):
        mixed_system.add_atom(
            position=micelle.positions[i] + np.array([0, 0, -2]),
            velocity=micelle.velocities[i],
            mass=micelle.masses[i],
            charge=micelle.charges[i],
            atom_type=micelle.atom_types[i]
        )
    
    print(f"Mixed system has {len(mixed_system.atoms)} atoms")
    print(f"Atom type distribution: {dict(zip(*np.unique(mixed_system.atom_types, return_counts=True)))}")
    
    # Calculate RDF
    print("\n5. Calculating Radial Distribution Function")
    print("-" * 40)
    
    r_values, g_r = mixed_system.calculate_rdf('ALA', 'CHOLATE')
    if len(r_values) > 0:
        print(f"RDF calculated with {len(r_values)} points")
        print(f"First peak at r = {r_values[np.argmax(g_r)]:.2f} nm")
    else:
        print("No pairs found for RDF calculation")
    
    # Visualization
    print("\n6. Creating Visualizations")
    print("-" * 40)
    
    # Plot peptide structure
    fig = plt.figure(figsize=(12, 10))
    
    ax1 = fig.add_subplot(221, projection='3d')
    peptide_positions = np.array(peptide.positions)
    ax1.scatter(peptide_positions[:, 0], peptide_positions[:, 1], peptide_positions[:, 2],
                c='blue', s=50, alpha=0.7)
    ax1.set_title('Peptide Structure')
    ax1.set_xlabel('X (nm)')
    ax1.set_ylabel('Y (nm)')
    ax1.set_zlabel('Z (nm)')
    
    # Plot micelle structure
    ax2 = fig.add_subplot(222, projection='3d')
    micelle_positions = np.array(micelle.positions)
    ax2.scatter(micelle_positions[:, 0], micelle_positions[:, 1], micelle_positions[:, 2],
                c='red', s=50, alpha=0.7)
    ax2.set_title('Bile Salt Micelle')
    ax2.set_xlabel('X (nm)')
    ax2.set_ylabel('Y (nm)')
    ax2.set_zlabel('Z (nm)')
    
    # Plot RDF
    ax3 = fig.add_subplot(223)
    if len(r_values) > 0:
        ax3.plot(r_values, g_r, 'g-')
        ax3.set_xlabel('Distance (nm)')
        ax3.set_ylabel('g(r)')
        ax3.set_title('RDF: Peptide - Bile Salt')
        ax3.grid(True)
    
    # Plot mixed system
    ax4 = fig.add_subplot(224, projection='3d')
    mixed_positions = np.array(mixed_system.positions)
    atom_types = mixed_system.atom_types
    
    colors = {'ALA': 'blue', 'GLY': 'cyan', 'VAL': 'green', 'LEU': 'orange', 
              'ILE': 'purple', 'CHOLATE': 'red'}
    
    for at in set(atom_types):
        indices = [i for i, t in enumerate(atom_types) if t == at]
        if at in colors:
            ax4.scatter(mixed_positions[indices, 0], mixed_positions[indices, 1], mixed_positions[indices, 2],
                        c=colors[at], s=30, alpha=0.7, label=at)
    
    ax4.set_title('Mixed System')
    ax4.set_xlabel('X (nm)')
    ax4.set_ylabel('Y (nm)')
    ax4.set_zlabel('Z (nm)')
    ax4.legend()
    
    plt.tight_layout()
    plt.savefig('md_simulations/md_structures.png', dpi=150)
    plt.close()
    
    print("Plots saved as 'md_simulations/md_structures.png'")


if __name__ == "__main__":
    main()
