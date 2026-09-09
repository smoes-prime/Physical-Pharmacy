#!/usr/bin/env python3
"""
Micelle Thermodynamics and Self-Assembly Modeling

Models for the self-assembly of amphiphilic molecules including:
- Critical Micelle Concentration (CMC) calculations
- Micelle size and shape predictions
- Bending elasticity and spontaneous curvature models
- Solubilization modeling

Course: Molecular Physical Pharmacy (3FC003)
"""

import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import minimize_scalar


class Amphiphile:
    """Represents an amphiphilic molecule with head and tail properties."""
    
    def __init__(self, head_area, tail_volume, tail_length, head_charge=0):
        """
        Initialize amphiphile properties.
        
        Args:
            head_area: Head group area in nm^2
            tail_volume: Tail volume in nm^3
            tail_length: Tail length in nm
            head_charge: Head group charge (e.g., -1 for anionic)
        """
        self.head_area = head_area
        self.tail_volume = tail_volume
        self.tail_length = tail_length
        self.head_charge = head_charge
        
    @property
    def packing_parameter(self):
        """Calculate packing parameter (v/a*l)."""
        return self.tail_volume / (self.head_area * self.tail_length)
    
    def predict_aggregate_shape(self):
        """
        Predict aggregate shape based on packing parameter.
        
        Returns:
            String describing predicted shape
        """
        pp = self.packing_parameter
        
        if pp < 1/3:
            return "Spherical micelles"
        elif pp < 1/2:
            return "Cylindrical micelles"
        elif pp < 2/3:
            return "Flexible bilayers / Vesicles"
        elif pp < 1:
            return "Planar bilayers"
        else:
            return "Inverted micelles"


class MicelleModel:
    """Thermodynamic model for micelle formation."""
    
    def __init__(self, temperature=298.15):
        """
        Initialize micelle model.
        
        Args:
            temperature: Temperature in Kelvin
        """
        self.T = temperature
        self.kB = 1.380649e-23  # J/K
        self.N_A = 6.02214076e23  # mol^-1
        self.epsilon_r = 78.5
        self.epsilon_0 = 8.854e-12  # F/m
        
    def cmc_tanford(self, amphiphile, ionic_strength=0.1):
        """
        Calculate CMC using Tanford's thermodynamic model.
        
        Args:
            amphiphile: Amphiphile object
            ionic_strength: Ionic strength in M
            
        Returns:
            CMC in M
        """
        # Hydrophobic transfer free energy (approximate)
        # For alkyl chains: ~ -1.5 kT per CH2 group
        n_ch2 = int(amphiphile.tail_volume * 100)  # Rough estimate
        delta_g_hydrophobic = -1.5 * n_ch2 * self.kB * self.T
        
        # Head group repulsion (simplified)
        a = amphiphile.head_area * 1e-18  # Convert nm^2 to m^2
        sigma = amphiphile.head_charge * self.e / a if amphiphile.head_charge != 0 else 0
        
        if sigma != 0:
            # Electrostatic contribution
            kappa = np.sqrt(2 * self.N_A * self.e**2 * ionic_strength / 
                           (self.epsilon_r * self.epsilon_0 * self.kB * self.T))
            delta_g_electrostatic = (sigma**2) / (2 * self.epsilon_r * self.epsilon_0 * kappa)
        else:
            delta_g_electrostatic = 0
        
        # Total free energy of micellization
        delta_g_mic = delta_g_hydrophobic + delta_g_electrostatic
        
        # CMC from free energy
        # CMC ≈ exp(-delta_g_mic / (kT)) / aggregation_number
        # For simplicity, assume aggregation number ~100
        n_agg = 100
        cmc = (1 / n_agg) * np.exp(-delta_g_mic / (self.kB * self.T * self.N_A * 1000))
        
        return cmc
    
    def cmc_empirical(self, alkyl_chain_length, head_group_type='ionic'):
        """
        Empirical CMC prediction based on alkyl chain length.
        
        Args:
            alkyl_chain_length: Number of carbon atoms in alkyl chain
            head_group_type: 'ionic' or 'nonionic'
            
        Returns:
            CMC in M
        """
        # Empirical constants
        if head_group_type == 'ionic':
            a = 0.015
            b = 0.29
        else:  # nonionic
            a = 0.0085
            b = 0.30
        
        cmc = a * np.exp(-b * alkyl_chain_length)
        return cmc
    
    def micelle_size(self, amphiphile, aggregation_number):
        """
        Calculate micelle dimensions.
        
        Args:
            amphiphile: Amphiphile object
            aggregation_number: Number of molecules per micelle
            
        Returns:
            Dictionary with micelle dimensions
        """
        # Spherical micelle
        pp = amphiphile.packing_parameter
        
        if pp < 1/3:
            # Spherical micelle
            total_tail_volume = aggregation_number * amphiphile.tail_volume
            core_radius = (3 * total_tail_volume / (4 * np.pi)) ** (1/3)
            
            # Shell thickness (head group region)
            shell_thickness = amphiphile.tail_length * 0.5  # Approximate
            
            return {
                'shape': 'spherical',
                'core_radius_nm': core_radius,
                'total_radius_nm': core_radius + shell_thickness,
                'aggregation_number': aggregation_number
            }
        elif pp < 1/2:
            # Cylindrical micelle
            total_tail_volume = aggregation_number * amphiphile.tail_volume
            # For cylinder: V = πr^2 * L
            # Assume L ≈ 2 * r for cylindrical micelles
            radius = (total_tail_volume / (2 * np.pi)) ** (1/3)
            length = 2 * radius
            
            return {
                'shape': 'cylindrical',
                'radius_nm': radius,
                'length_nm': length,
                'aggregation_number': aggregation_number
            }
        else:
            # Bilayer structures
            total_tail_volume = aggregation_number * amphiphile.tail_volume
            # For bilayer: thickness ≈ 2 * tail_length
            thickness = 2 * amphiphile.tail_length
            area_per_molecule = amphiphile.head_area
            total_area = aggregation_number * area_per_molecule
            
            # For vesicle: radius from area
            radius = np.sqrt(total_area / (4 * np.pi))
            
            return {
                'shape': 'vesicle',
                'radius_nm': radius,
                'bilayer_thickness_nm': thickness,
                'aggregation_number': aggregation_number
            }


class BendingElasticityModel:
    """Model for bending elasticity and spontaneous curvature."""
    
    def __init__(self, temperature=298.15):
        """Initialize bending elasticity model."""
        self.T = temperature
        self.kB = 1.380649e-23
        
    def bending_energy(self, curvature, spontaneous_curvature, bending_rigidity):
        """
        Calculate bending energy density.
        
        Args:
            curvature: Mean curvature (1/radius) in nm^-1
            spontaneous_curvature: Spontaneous curvature in nm^-1
            bending_rigidity: Bending rigidity in kT
            
        Returns:
            Bending energy density in kT/nm^2
        """
        return 0.5 * bending_rigidity * (curvature - spontaneous_curvature)**2
    
    def optimal_radius(self, spontaneous_curvature, bending_rigidity):
        """
        Calculate optimal aggregate radius.
        
        Args:
            spontaneous_curvature: Spontaneous curvature in nm^-1
            bending_rigidity: Bending rigidity in kT
            
        Returns:
            Optimal radius in nm
        """
        # For minimal bending energy, curvature ≈ spontaneous curvature
        if spontaneous_curvature != 0:
            return 1 / spontaneous_curvature
        else:
            # Planar is optimal
            return np.inf
    
    def predict_shape_transition(self, spontaneous_curvature_values, bending_rigidity):
        """
        Predict shape transitions as function of spontaneous curvature.
        
        Args:
            spontaneous_curvature_values: Array of spontaneous curvature values
            bending_rigidity: Bending rigidity in kT
            
        Returns:
            Array of predicted shapes
        """
        shapes = []
        for H0 in spontaneous_curvature_values:
            if H0 < -0.1:
                shapes.append("Inverted micelles")
            elif H0 < 0:
                shapes.append("Planar bilayers")
            elif H0 < 0.05:
                shapes.append("Vesicles")
            elif H0 < 0.15:
                shapes.append("Cylindrical micelles")
            else:
                shapes.append("Spherical micelles")
        return np.array(shapes)


def main():
    """Example usage of micelle thermodynamics models."""
    print("=" * 60)
    print("Micelle Thermodynamics and Self-Assembly")
    print("=" * 60)
    
    # Create amphiphiles
    print("\n1. Amphiphile Properties and Shape Prediction")
    print("-" * 40)
    
    # SDS-like (anionic surfactant)
    sds = Amphiphile(head_area=0.5, tail_volume=0.5, tail_length=1.5, head_charge=-1)
    print(f"SDS-like: Packing parameter = {sds.packing_parameter:.3f}")
    print(f"  Predicted shape: {sds.predict_aggregate_shape()}")
    
    # Phospholipid (PC)
    pc = Amphiphile(head_area=0.7, tail_volume=1.2, tail_length=2.0, head_charge=0)
    print(f"Phosphatidylcholine: Packing parameter = {pc.packing_parameter:.3f}")
    print(f"  Predicted shape: {pc.predict_aggregate_shape()}")
    
    # Bile salt
    bile_salt = Amphiphile(head_area=0.3, tail_volume=0.8, tail_length=1.0, head_charge=-1)
    print(f"Bile salt: Packing parameter = {bile_salt.packing_parameter:.3f}")
    print(f"  Predicted shape: {bile_salt.predict_aggregate_shape()}")
    
    # CMC calculations
    print("\n2. CMC Predictions")
    print("-" * 40)
    
    model = MicelleModel()
    
    # Empirical CMC
    chain_lengths = [8, 10, 12, 14, 16]
    cmcs_ionic = [model.cmc_empirical(n, 'ionic') for n in chain_lengths]
    cmcs_nonionic = [model.cmc_empirical(n, 'nonionic') for n in chain_lengths]
    
    print("Chain length | CMC (ionic) | CMC (nonionic)")
    print("-" * 40)
    for n, cmc_i, cmc_n in zip(chain_lengths, cmcs_ionic, cmcs_nonionic):
        print(f"  C{n:2d}      | {cmc_i:.2e} M    | {cmc_n:.2e} M")
    
    # Micelle size
    print("\n3. Micelle Size Predictions")
    print("-" * 40)
    
    size_info = model.micelle_size(sds, 100)
    print(f"SDS micelle (n=100): {size_info}")
    
    # Bending elasticity
    print("\n4. Bending Elasticity Model")
    print("-" * 40)
    
    bending_model = BendingElasticityModel()
    
    H0_values = np.linspace(-0.2, 0.2, 9)
    shapes = bending_model.predict_shape_transition(H0_values, 10)
    
    print("Spontaneous Curvature | Predicted Shape")
    print("-" * 40)
    for H0, shape in zip(H0_values, shapes):
        print(f"  {H0:6.2f} nm⁻¹         | {shape}")
    
    # Plot CMC vs chain length
    plt.figure(figsize=(10, 6))
    plt.semilogy(chain_lengths, cmcs_ionic, 'bo-', label='Ionic surfactants')
    plt.semilogy(chain_lengths, cmcs_nonionic, 'ro-', label='Nonionic surfactants')
    plt.xlabel('Alkyl Chain Length (C atoms)')
    plt.ylabel('CMC (M)')
    plt.title('CMC vs Alkyl Chain Length')
    plt.legend()
    plt.grid(True, which='both', linestyle='--')
    plt.tight_layout()
    plt.savefig('self_assembly/cmc_chain_length.png', dpi=150)
    plt.close()
    
    print("\nPlots saved as 'self_assembly/cmc_chain_length.png'")


if __name__ == "__main__":
    main()
