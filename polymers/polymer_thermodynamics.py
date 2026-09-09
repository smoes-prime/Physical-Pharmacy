#!/usr/bin/env python3
"""
Polymer Thermodynamics for Pharmaceutical Applications

Models for polymer behavior in pharmaceutical systems including:
- Polymer coil swelling
- Phase separation in polymer solutions
- Polyelectrolyte complex formation
- Gel swelling and elasticity

Course: Molecular Physical Pharmacy (3FC003)
"""

import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import root_scalar


class PolymerChain:
    """Model for single polymer chain in solution."""
    
    def __init__(self, n_monomers, monomer_length=0.35, persistence_length=0.7):
        """
        Initialize polymer chain.
        
        Args:
            n_monomers: Number of monomers in the chain
            monomer_length: Length per monomer in nm
            persistence_length: Persistence length in nm
        """
        self.n = n_monomers
        self.b = monomer_length  # Kuhn length approximation
        self.l_p = persistence_length
        
    @property
    def radius_of_gyration(self):
        """Calculate radius of gyration for ideal chain."""
        # For ideal chain (Gaussian): R_g^2 = n * b^2 / 6
        return np.sqrt(self.n * self.b**2 / 6)
    
    @property
    def end_to_end_distance(self):
        """Calculate end-to-end distance for ideal chain."""
        # For ideal chain: <R^2> = n * b^2
        return np.sqrt(self.n * self.b**2)
    
    def radius_of_gyration_worm_like_chain(self):
        """
        Calculate radius of gyration for worm-like chain model.
        
        Returns:
            Radius of gyration in nm
        """
        L = self.n * self.b
        if self.l_p / L > 0.1:  # Rod-like limit
            return L / np.sqrt(12)
        else:  # Coil limit
            return np.sqrt(self.l_p * L / 3)
    
    def excluded_volume_radius(self, chi_parameter=0.5):
        """
        Calculate radius of gyration with excluded volume effects.
        
        Args:
            chi_parameter: Flory interaction parameter
            
        Returns:
            Expanded radius of gyration in nm
        """
        # Flory exponent: nu ≈ 3/(d+2) = 0.588 in good solvent
        # For theta conditions (chi = 0.5): nu = 0.5
        # For good solvent (chi < 0.5): nu ≈ 0.588
        
        if chi_parameter < 0.5:
            nu = 0.588  # Good solvent
        else:
            nu = 0.5  # Theta solvent
        
        return self.radius_of_gyration * self.n**(nu - 0.5)


class PolymerSolution:
    """Model for polymer solutions and phase behavior."""
    
    def __init__(self, temperature=298.15):
        """
        Initialize polymer solution model.
        
        Args:
            temperature: Temperature in Kelvin
        """
        self.T = temperature
        self.kB = 1.380649e-23  # J/K
        self.N_A = 6.02214076e23  # mol^-1
        
    def flory_huggins_free_energy(self, phi_polymer, n_polymer, chi_parameter):
        """
        Calculate Flory-Huggins free energy of mixing.
        
        Args:
            phi_polymer: Volume fraction of polymer
            n_polymer: Degree of polymerization
            chi_parameter: Flory interaction parameter
            
        Returns:
            Free energy density in kT per lattice site
        """
        phi_solvent = 1 - phi_polymer
        
        # Entropy of mixing
        delta_f_entropy = (phi_polymer / n_polymer) * np.log(phi_polymer) + \
                         phi_solvent * np.log(phi_solvent)
        
        # Enthalpy of mixing
        delta_f_enthalpy = chi_parameter * phi_polymer * phi_solvent
        
        return delta_f_entropy + delta_f_enthalpy
    
    def spinodal_decomposition(self, n_polymer, chi_parameter):
        """
        Calculate spinodal curve for phase separation.
        
        Args:
            n_polymer: Degree of polymerization
            chi_parameter: Flory interaction parameter
            
        Returns:
            Critical volume fraction for spinodal decomposition
        """
        # Spinodal condition: d^2F/dphi^2 = 0
        # For Flory-Huggins: chi_c = 1/(2*sqrt(n)) + 1/(2*n)
        chi_c = 1 / (2 * np.sqrt(n_polymer)) + 1 / (2 * n_polymer)
        
        if chi_parameter > chi_c:
            # Phase separation occurs
            # Critical composition
            phi_c = 1 / (1 + np.sqrt(n_polymer))
            return phi_c, chi_c
        else:
            return None, chi_c
    
    def binodal_decomposition(self, n_polymer, chi_parameter):
        """
        Calculate binodal (coexistence) curve.
        
        Args:
            n_polymer: Degree of polymerization
            chi_parameter: Flory interaction parameter
            
        Returns:
            Tuple of (phi_1, phi_2) coexistence compositions
        """
        # Solve for chemical potential equality
        # This is a simplified approximation
        
        if chi_parameter <= 1 / (2 * np.sqrt(n_polymer)):
            return None, None  # No phase separation
        
        # Approximate binodal
        phi_1 = 0.1
        phi_2 = 0.9
        
        # Iterative refinement (simplified)
        for _ in range(10):
            # Chemical potential in phase 1
            mu_1 = np.log(phi_1) + (1 - phi_1) + \
                   chi_parameter * (1 - phi_1)**2 + \
                   (phi_1 / n_polymer) * (1 - 1/n_polymer)
            
            # Chemical potential in phase 2
            mu_2 = np.log(phi_2) + (1 - phi_2) + \
                   chi_parameter * (1 - phi_2)**2 + \
                   (phi_2 / n_polymer) * (1 - 1/n_polymer)
            
            if abs(mu_1 - mu_2) < 0.01:
                break
            
            # Adjust compositions
            if mu_1 > mu_2:
                phi_1 += 0.01
                phi_2 -= 0.01
            else:
                phi_1 -= 0.01
                phi_2 += 0.01
            
            phi_1 = np.clip(phi_1, 0.01, 0.99)
            phi_2 = np.clip(phi_2, 0.01, 0.99)
        
        return phi_1, phi_2
    
    def theta_temperature(self, n_polymer):
        """
        Calculate theta temperature for polymer solution.
        
        Args:
            n_polymer: Degree of polymerization
            
        Returns:
            Theta temperature in Kelvin (relative to reference)
        """
        # Theta temperature when chi = 0.5
        # This is a reference value; actual theta depends on polymer-solvent pair
        return self.T  # Return current temperature as reference


class Polyelectrolyte:
    """Model for polyelectrolyte solutions."""
    
    def __init__(self, n_monomers, charge_fraction=1.0, temperature=298.15):
        """
        Initialize polyelectrolyte model.
        
        Args:
            n_monomers: Number of monomers
            charge_fraction: Fraction of charged monomers
            temperature: Temperature in Kelvin
        """
        self.n = n_monomers
        self.f = charge_fraction
        self.T = temperature
        self.kB = 1.380649e-23
        self.e = 1.602176634e-19
        self.epsilon_r = 78.5
        self.epsilon_0 = 8.854e-12
        self.N_A = 6.02214076e23
        
    @property
    def radius_of_gyration(self):
        """Calculate radius of gyration for polyelectrolyte."""
        # For polyelectrolytes, the chain is expanded due to electrostatic repulsion
        # R_g ~ n^(3/5) * l * f^(2/5)  (approximate scaling)
        b = 0.35  # nm, monomer length
        return b * self.n**(3/5) * self.f**(2/5)
    
    def debye_length(self, ionic_strength):
        """Calculate Debye screening length in solution."""
        I = ionic_strength
        kappa = np.sqrt(2 * self.N_A * self.e**2 * I / 
                       (self.epsilon_r * self.epsilon_0 * self.kB * self.T))
        return 1 / kappa
    
    def osmotically_equivalent_sphere(self, concentration):
        """
        Calculate radius of osmotically equivalent sphere.
        
        Args:
            concentration: Polyelectrolyte concentration in M
            
        Returns:
            Radius in nm
        """
        # For dilute solutions, the polyelectrolyte behaves like
        # a sphere with radius equal to the Debye length
        c_molar = concentration
        c_molecules = c_molar * self.N_A * 1e-27  # molecules/nm^3
        
        # Ionic strength from polyelectrolyte
        I_poly = self.f * self.n * c_molecules
        
        # Total ionic strength (including counterions)
        I_total = I_poly
        
        return self.debye_length(I_total) * 1e9


class GelModel:
    """Model for polymer gels and their swelling behavior."""
    
    def __init__(self, polymer_volume_fraction_dry=0.1, 
                 crosslink_density=0.01, temperature=298.15):
        """
        Initialize gel model.
        
        Args:
            polymer_volume_fraction_dry: Volume fraction of polymer in dry state
            crosslink_density: Crosslink density (moles of crosslinks per mole of monomers)
            temperature: Temperature in Kelvin
        """
        self.phi_0 = polymer_volume_fraction_dry
        self.n_crosslink = crosslink_density
        self.T = temperature
        self.kB = 1.380649e-23
        self.N_A = 6.02214076e23
        
    def flory_rehner_swelling(self, chi_parameter, ionic_strength=0):
        """
        Calculate gel swelling ratio using Flory-Rehner theory.
        
        Args:
            chi_parameter: Flory interaction parameter
            ionic_strength: Ionic strength for ionized gels
            
        Returns:
            Swelling ratio (V/V_0)
        """
        # For non-ionized gels
        # Free energy has elastic and mixing contributions
        
        # Elastic free energy (Gaussian network)
        # F_elastic = (3/2) * (phi/phi_0)^(2/3) * (1 - 2/(f*n)) * kT * nu
        # where nu is number of polymer chains
        
        # Mixing free energy (Flory-Huggins)
        # F_mixing = kT * n * [phi * ln(phi) + (1-phi) * ln(1-phi) + chi * phi * (1-phi)]
        
        # At equilibrium: dF/dphi = 0
        # This gives the swelling equilibrium
        
        # Simplified solution for non-ionized gel
        # phi^(1/3) = (1/2 - chi) / (1/2 - chi + phi_0^(1/3))
        
        if chi_parameter >= 0.5:
            # Collapse or limited swelling
            return 1.0
        
        # Solve for equilibrium swelling
        def swelling_eq(phi):
            # Chemical potential equality
            term1 = phi / self.phi_0
            term2 = np.log(1 - phi) + phi + chi_parameter * phi**2
            term3 = (1 / self.n_crosslink) * (phi / self.phi_0 - 1)
            return term1 + term2 + term3
        
        # Find root
        try:
            sol = root_scalar(swelling_eq, bracket=[self.phi_0, 0.9])
            phi_eq = sol.root
            swelling_ratio = (self.phi_0 / phi_eq)**3
            return swelling_ratio
        except:
            return 1.0
    
    def swelling_with_ionization(self, chi_parameter, ionization_fraction, ionic_strength):
        """
        Calculate swelling for ionized gel (polyelectrolyte gel).
        
        Args:
            chi_parameter: Flory interaction parameter
            ionization_fraction: Fraction of ionizable groups that are ionized
            ionic_strength: External ionic strength in M
            
        Returns:
            Swelling ratio
        """
        # For ionized gels, there's additional osmotic pressure from counterions
        # This significantly increases swelling
        
        # Donnan equilibrium contribution
        # Pi_ion = c_ion * kT
        # where c_ion is the concentration of mobile ions inside the gel
        
        # Simplified: swelling ratio increases with ionization
        base_swelling = self.flory_rehner_swelling(chi_parameter)
        
        # Enhancement factor due to ionization
        # For polyelectrolyte gels, swelling can be much higher
        if ionization_fraction > 0:
            # Approximate enhancement
            enhancement = 1 + 10 * ionization_fraction * np.sqrt(1 / (ionic_strength + 1e-6))
            return base_swelling * enhancement
        else:
            return base_swelling


def main():
    """Example usage of polymer thermodynamics models."""
    print("=" * 60)
    print("Polymer Thermodynamics for Pharmaceutical Applications")
    print("=" * 60)
    
    # Single polymer chain
    print("\n1. Single Polymer Chain Properties")
    print("-" * 40)
    
    chain = PolymerChain(n_monomers=1000, monomer_length=0.35, persistence_length=0.7)
    print(f"Degree of polymerization: {chain.n}")
    print(f"Ideal chain R_g: {chain.radius_of_gyration:.2f} nm")
    print(f"Ideal chain R_ee: {chain.end_to_end_distance:.2f} nm")
    print(f"Worm-like chain R_g: {chain.radius_of_gyration_worm_like_chain():.2f} nm")
    print(f"Good solvent R_g: {chain.excluded_volume_radius(chi_parameter=0.4):.2f} nm")
    print(f"Theta solvent R_g: {chain.excluded_volume_radius(chi_parameter=0.5):.2f} nm")
    
    # Polymer solution phase behavior
    print("\n2. Polymer Solution Phase Behavior")
    print("-" * 40)
    
    solution = PolymerSolution()
    n_polymer = 1000
    chi_values = [0.4, 0.45, 0.48, 0.5, 0.52]
    
    print("Chi Parameter | Critical Chi | Phase Separation?")
    print("-" * 40)
    for chi in chi_values:
        phi_c, chi_c = solution.spinodal_decomposition(n_polymer, chi)
        separates = chi > chi_c
        print(f"  {chi:6.2f}     | {chi_c:8.4f}    | {'Yes' if separates else 'No'}")
    
    # Binodal calculation
    chi = 0.55
    phi_1, phi_2 = solution.binodal_decomposition(n_polymer, chi)
    if phi_1:
        print(f"\nBinodal at chi={chi}: phi_1 = {phi_1:.3f}, phi_2 = {phi_2:.3f}")
    
    # Polyelectrolyte
    print("\n3. Polyelectrolyte Properties")
    print("-" * 40)
    
    pe = Polyelectrolyte(n_monomers=1000, charge_fraction=0.5)
    print(f"Number of monomers: {pe.n}")
    print(f"Charge fraction: {pe.f}")
    print(f"Radius of gyration: {pe.radius_of_gyration:.2f} nm")
    print(f"Debye length (0.1 M NaCl): {pe.debye_length(0.1) * 1e9:.2f} nm")
    
    concentrations = [0.001, 0.01, 0.1]
    print("\nConcentration (M) | Osmotic radius (nm)")
    print("-" * 40)
    for c in concentrations:
        r = pe.osmotically_equivalent_sphere(c)
        print(f"  {c:8.3f}      | {r:8.2f}")
    
    # Gel swelling
    print("\n4. Gel Swelling")
    print("-" * 40)
    
    gel = GelModel(polymer_volume_fraction_dry=0.1, crosslink_density=0.01)
    chi_values = [0.4, 0.45, 0.49, 0.51]
    
    print("Chi Parameter | Swelling Ratio (V/V_0)")
    print("-" * 40)
    for chi in chi_values:
        swelling = gel.flory_rehner_swelling(chi)
        print(f"  {chi:6.2f}     | {swelling:8.2f}")
    
    # Ionized gel
    print("\n5. Polyelectrolyte Gel Swelling")
    print("-" * 40)
    
    ionization_fractions = [0, 0.25, 0.5, 0.75, 1.0]
    chi = 0.45
    ionic_strength = 0.01
    
    print("Ionization Fraction | Swelling Ratio")
    print("-" * 40)
    for f in ionization_fractions:
        swelling = gel.swelling_with_ionization(chi, f, ionic_strength)
        print(f"  {f:6.2f}            | {swelling:8.2f}")
    
    # Plot phase diagram
    chi_range = np.linspace(0.3, 0.6, 100)
    n_values = [100, 500, 1000]
    
    plt.figure(figsize=(10, 6))
    for n in n_values:
        chi_c = 1 / (2 * np.sqrt(n)) + 1 / (2 * n)
        plt.axvline(x=chi_c, color='k', linestyle='--', alpha=0.5)
        
        # Spinodal curve
        phi_c_values = []
        for chi in chi_range:
            phi_c, _ = solution.spinodal_decomposition(n, chi)
            if phi_c:
                phi_c_values.append(phi_c)
            else:
                phi_c_values.append(np.nan)
        
        plt.plot(chi_range, phi_c_values, label=f'n = {n}')
    
    plt.xlabel('Chi Parameter')
    plt.ylabel('Critical Volume Fraction')
    plt.title('Spinodal Curve for Polymer Solutions')
    plt.legend()
    plt.grid(True)
    plt.tight_layout()
    plt.savefig('polymers/polymer_phase_diagram.png', dpi=150)
    plt.close()
    
    print("\nPlots saved as 'polymers/polymer_phase_diagram.png'")


if __name__ == "__main__":
    main()
