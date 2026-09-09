#!/usr/bin/env python3
"""
Poisson-Boltzmann Equation Solver for Pharmaceutical Systems

Solves the linearized and non-linear Poisson-Boltzmann equation
for electrostatic potential in pharmaceutical and biological systems.

Course: Molecular Physical Pharmacy (3FC003)
"""

import numpy as np
from scipy.integrate import solve_ivp
from scipy.optimize import root_scalar
import matplotlib.pyplot as plt


class PoissonBoltzmannSolver:
    """Solve Poisson-Boltzmann equation for planar, cylindrical, and spherical geometries."""
    
    def __init__(self, epsilon_r=78.5, temperature=298.15):
        """
        Initialize solver with medium properties.
        
        Args:
            epsilon_r: Relative permittivity of the medium (water = 78.5)
            temperature: Temperature in Kelvin
        """
        self.epsilon_r = epsilon_r
        self.epsilon_0 = 8.854e-12  # F/m, vacuum permittivity
        self.T = temperature
        self.kB = 1.380649e-23  # J/K, Boltzmann constant
        self.e = 1.602176634e-19  # C, elementary charge
        self.N_A = 6.02214076e23  # mol^-1, Avogadro's number
        
    @property
    def epsilon(self):
        """Absolute permittivity of the medium."""
        return self.epsilon_r * self.epsilon_0
    
    def debye_length(self, ionic_strength):
        """
        Calculate Debye screening length.
        
        Args:
            ionic_strength: Ionic strength in M (mol/L)
            
        Returns:
            Debye length in meters
        """
        I = ionic_strength
        kappa = np.sqrt(2 * self.N_A * self.e**2 * I / 
                       (self.epsilon * self.kB * self.T))
        return 1 / kappa
    
    def linear_pb_planar(self, sigma, ionic_strength, z_vals=np.linspace(0, 10e-9, 100)):
        """
        Solve linearized Poisson-Boltzmann equation for planar geometry.
        
        Args:
            sigma: Surface charge density in C/m^2
            ionic_strength: Ionic strength in M
            z_vals: Distance values in meters
            
        Returns:
            Tuple of (z_values, potential_values)
        """
        kappa = 1 / self.debye_length(ionic_strength)
        
        # Linear PB solution for planar geometry
        potential = (sigma / (self.epsilon * kappa)) * np.exp(-kappa * z_vals)
        
        return z_vals, potential
    
    def gouy_chapman_planar(self, sigma, ionic_strength, z_vals=np.linspace(0.1e-9, 10e-9, 100)):
        """
        Solve Gouy-Chapman theory for planar double layer.
        
        Args:
            sigma: Surface charge density in C/m^2
            ionic_strength: Ionic strength in M (for symmetric electrolyte)
            z_vals: Distance values in meters (start > 0 to avoid singularity)
            
        Returns:
            Tuple of (z_values, potential_values)
        """
        # For symmetric 1:1 electrolyte
        z_valency = 1
        c0 = ionic_strength * self.N_A * 1000  # Convert M to m^-3
        
        # Surface potential calculation
        sigma_abs = abs(sigma)
        sign_sigma = np.sign(sigma)
        
        # Solve for surface potential
        def surface_potential_eq(psi_0):
            return sigma_abs - np.sqrt(8 * self.epsilon * c0 * self.kB * self.T) * \
                   np.sinh(self.e * psi_0 / (2 * self.kB * self.T))
        
        # Initial guess
        psi_0_guess = sigma_abs * 1e-9 / self.epsilon  # Simple guess
        
        sol = root_scalar(surface_potential_eq, bracket=[-0.1, 0.1])
        psi_0 = sign_sigma * sol.root
        
        # Potential profile
        kappa = 1 / self.debye_length(ionic_strength)
        beta = self.e / (self.kB * self.T)
        
        def potential_func(z):
            return (4 * self.kB * self.T / self.e) * \
                   np.arctanh(np.tanh(beta * psi_0 / 4) * np.exp(-kappa * z))
        
        potential = np.array([potential_func(z) for z in z_vals])
        
        return z_vals, potential, psi_0
    
    def plot_comparison(self, sigma, ionic_strength):
        """Plot comparison of linear PB and Gouy-Chapman solutions."""
        z_vals = np.linspace(0.1e-9, 5e-9, 100)
        
        _, linear_pot = self.linear_pb_planar(sigma, ionic_strength, z_vals)
        _, gc_pot, psi_0 = self.gouy_chapman_planar(sigma, ionic_strength, z_vals)
        
        plt.figure(figsize=(10, 6))
        plt.plot(z_vals * 1e9, linear_pot, 'b-', label='Linear PB')
        plt.plot(z_vals * 1e9, gc_pot, 'r-', label=f'Gouy-Chapman (ψ₀ = {psi_0:.3f} V)')
        plt.xlabel('Distance from surface (nm)')
        plt.ylabel('Electrostatic Potential (V)')
        plt.title(f'Poisson-Boltzmann Solutions (σ = {sigma:.2e} C/m², I = {ionic_strength} M)')
        plt.legend()
        plt.grid(True)
        plt.tight_layout()
        plt.savefig('electrostatics/pb_comparison.png', dpi=150)
        plt.close()


def main():
    """Example usage of Poisson-Boltzmann solver."""
    solver = PoissonBoltzmannSolver()
    
    # Example: Lipid bilayer surface
    sigma = 0.01  # C/m^2 (typical for lipid bilayer)
    ionic_strength = 0.1  # M NaCl
    
    print("=" * 60)
    print("Poisson-Boltzmann Equation Solver")
    print("=" * 60)
    print(f"Surface charge density: {sigma:.2e} C/m²")
    print(f"Ionic strength: {ionic_strength} M")
    print(f"Debye length: {solver.debye_length(ionic_strength) * 1e9:.2f} nm")
    
    # Solve and plot
    solver.plot_comparison(sigma, ionic_strength)
    print("\nPlot saved as 'electrostatics/pb_comparison.png'")
    
    # Calculate surface potential
    _, _, psi_0 = solver.gouy_chapman_planar(sigma, ionic_strength)
    print(f"Gouy-Chapman surface potential: {psi_0 * 1000:.2f} mV")


if __name__ == "__main__":
    main()
