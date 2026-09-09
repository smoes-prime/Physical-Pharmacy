#!/usr/bin/env python3
"""
Drug Transport and Release Modeling

Models for drug transport mechanisms and release kinetics including:
- Diffusion-controlled release
- Convection and electric field-driven transport
- Effective medium theory for porous materials
- Percolation theory for disordered systems
- Drug release kinetics from various formulations

Course: Molecular Physical Pharmacy (3FC003)
"""

import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import solve_ivp
from scipy.special import erfc


class DiffusionModel:
    """Model for diffusion-controlled drug release."""
    
    def __init__(self, temperature=298.15):
        """
        Initialize diffusion model.
        
        Args:
            temperature: Temperature in Kelvin
        """
        self.T = temperature
        self.kB = 1.380649e-23  # J/K
        self.N_A = 6.02214076e23  # mol^-1
        
    def ficks_first_law(self, diffusion_coefficient, concentration_gradient):
        """
        Calculate diffusion flux using Fick's first law.
        
        Args:
            diffusion_coefficient: Diffusion coefficient in m^2/s
            concentration_gradient: Concentration gradient in mol/(m^4)
            
        Returns:
            Flux in mol/(m^2*s)
        """
        return -diffusion_coefficient * concentration_gradient
    
    def ficks_second_law_1d(self, diffusion_coefficient, initial_concentration, 
                           length, time_points):
        """
        Solve Fick's second law in 1D for a slab geometry.
        
        Args:
            diffusion_coefficient: Diffusion coefficient in m^2/s
            initial_concentration: Initial uniform concentration in mol/m^3
            length: Length of the slab in meters
            time_points: Array of time points in seconds
            
        Returns:
            Tuple of (time_points, concentration_profiles)
        """
        # Analytical solution for 1D diffusion from a slab
        # C(x,t) = C_0 * sum_{n=0}^inf [4/(2n+1)pi * sin((2n+1)pi x/L) * exp(-D(2n+1)^2 pi^2 t/L^2)]
        
        x_vals = np.linspace(0, length, 50)
        concentration_profiles = []
        
        for t in time_points:
            C = np.zeros_like(x_vals)
            for n in range(10):  # Sum first 10 terms
                term = (4 / ((2*n + 1) * np.pi)) * \
                       np.sin((2*n + 1) * np.pi * x_vals / length) * \
                       np.exp(-diffusion_coefficient * (2*n + 1)**2 * np.pi**2 * t / length**2)
                C += term
            concentration_profiles.append(initial_concentration * C)
        
        return time_points, np.array(concentration_profiles), x_vals
    
    def diffusion_coefficient_stokes_einstein(self, radius, viscosity, temperature=298.15):
        """
        Calculate diffusion coefficient using Stokes-Einstein equation.
        
        Args:
            radius: Particle radius in meters
            viscosity: Viscosity in Pa*s (water at 25C = 0.00089 Pa*s)
            temperature: Temperature in Kelvin
            
        Returns:
            Diffusion coefficient in m^2/s
        """
        kB = 1.380649e-23
        return kB * temperature / (6 * np.pi * viscosity * radius)
    
    def mean_squared_displacement(self, diffusion_coefficient, time):
        """
        Calculate mean squared displacement for Brownian motion.
        
        Args:
            diffusion_coefficient: Diffusion coefficient in m^2/s
            time: Time in seconds
            
        Returns:
            Mean squared displacement in m^2
        """
        return 6 * diffusion_coefficient * time


class DrugReleaseKinetics:
    """Model for drug release kinetics from various formulations."""
    
    def __init__(self):
        """Initialize drug release kinetics model."""
        pass
    
    def zero_order_release(self, release_rate, initial_amount, time_points):
        """
        Zero-order release (constant release rate).
        
        Args:
            release_rate: Release rate in mol/s
            initial_amount: Initial amount in mol
            time_points: Array of time points in seconds
            
        Returns:
            Array of released amounts
        """
        released = release_rate * time_points
        return np.minimum(released, initial_amount)
    
    def first_order_release(self, release_rate_constant, initial_amount, time_points):
        """
        First-order release (exponential decay).
        
        Args:
            release_rate_constant: Rate constant in s^-1
            initial_amount: Initial amount in mol
            time_points: Array of time points in seconds
            
        Returns:
            Array of released amounts
        """
        return initial_amount * (1 - np.exp(-release_rate_constant * time_points))
    
    def higuchi_model(self, diffusion_coefficient, drug_loading, tortuosity, 
                      porosity, time_points):
        """
        Higuchi model for diffusion-controlled release from a matrix.
        
        Args:
            diffusion_coefficient: Diffusion coefficient in m^2/s
            drug_loading: Initial drug loading in kg/m^3
            tortuosity: Tortuosity factor (>= 1)
            porosity: Porosity (0-1)
            time_points: Array of time points in seconds
            
        Returns:
            Array of released amounts per unit area
        """
        # Higuchi equation: Q = sqrt(D * C_s * (2A - C_s) * t * epsilon / tau)
        # where A is initial loading, C_s is solubility
        # For simplicity, assume C_s << 2A
        
        effective_D = diffusion_coefficient * porosity / tortuosity
        C_s = drug_loading * 0.1  # Assume solubility is 10% of loading
        
        released = np.sqrt(effective_D * C_s * drug_loading * time_points)
        return released
    
    def peppas_model(self, n, k, initial_amount, time_points):
        """
        Peppas (Power Law) model for drug release.
        
        Args:
            n: Release exponent (indicates mechanism)
            k: Kinetic constant
            initial_amount: Initial amount in mol
            time_points: Array of time points in seconds
            
        Returns:
            Array of released amounts
        """
        return initial_amount * k * time_points**n
    
    def interpret_peppas_exponent(self, n):
        """
        Interpret the Peppas exponent for release mechanism.
        
        Args:
            n: Release exponent
            
        Returns:
            String describing the release mechanism
        """
        if n <= 0.45:
            return "Fickian diffusion (Case I)"
        elif n < 0.89:
            return "Anomalous (non-Fickian) transport"
        else:
            return "Case II transport (zero-order, relaxation-controlled)"


class EffectiveMediumTheory:
    """Effective medium theory for transport in porous materials."""
    
    def __init__(self):
        """Initialize effective medium theory model."""
        pass
    
    def effective_diffusivity_porous(self, diffusion_coefficient_free, 
                                     porosity, tortuosity=1.0, 
                                     constrictivity=1.0):
        """
        Calculate effective diffusivity in porous medium.
        
        Args:
            diffusion_coefficient_free: Free diffusion coefficient in m^2/s
            porosity: Porosity (0-1)
            tortuosity: Tortuosity factor (>= 1)
            constrictivity: Constrictivity factor (0-1)
            
        Returns:
            Effective diffusivity in m^2/s
        """
        return diffusion_coefficient_free * porosity * constrictivity / tortuosity
    
    def effective_diffusivity_percolation(self, diffusion_coefficient_free, 
                                          porosity, critical_porosity=0.2, 
                                          exponent=2.0):
        """
        Calculate effective diffusivity using percolation theory.
        
        Args:
            diffusion_coefficient_free: Free diffusion coefficient in m^2/s
            porosity: Porosity (0-1)
            critical_porosity: Critical porosity for percolation threshold
            exponent: Critical exponent (typically 1.8-2.0)
            
        Returns:
            Effective diffusivity in m^2/s
        """
        if porosity <= critical_porosity:
            return 0.0  # No percolation path
        
        epsilon = (porosity - critical_porosity) / (1 - critical_porosity)
        return diffusion_coefficient_free * epsilon**exponent
    
    def effective_diffusivity_mackie_meares(self, diffusion_coefficient_free, 
                                           porosity, pore_radius_distribution):
        """
        Calculate effective diffusivity using Mackie-Meares model.
        
        Args:
            diffusion_coefficient_free: Free diffusion coefficient in m^2/s
            porosity: Porosity (0-1)
            pore_radius_distribution: Array of pore radii in meters
            
        Returns:
            Effective diffusivity in m^2/s
        """
        # Mackie-Meares: D_eff = D_0 * epsilon * delta / tau
        # where delta is constrictivity and tau is tortuosity
        
        # For simplicity, use average constrictivity
        # delta ≈ (r_avg / r_max)^2 where r_max is the largest pore
        if len(pore_radius_distribution) == 0:
            return 0.0
        
        r_avg = np.mean(pore_radius_distribution)
        r_max = np.max(pore_radius_distribution)
        constrictivity = (r_avg / r_max)**2
        
        # Tortuosity approximation: tau ≈ 1 / sqrt(epsilon)
        tortuosity = 1 / np.sqrt(porosity)
        
        return diffusion_coefficient_free * porosity * constrictivity / tortuosity


class ConvectionModel:
    """Model for convection-driven transport."""
    
    def __init__(self):
        """Initialize convection model."""
        pass
    
    def advection_diffusion_1d(self, velocity, diffusion_coefficient, 
                              initial_concentration, length, time_points, dx=0.01):
        """
        Solve advection-diffusion equation in 1D.
        
        Args:
            velocity: Fluid velocity in m/s
            diffusion_coefficient: Diffusion coefficient in m^2/s
            initial_concentration: Initial concentration in mol/m^3
            length: Domain length in meters
            time_points: Array of time points in seconds
            dx: Spatial step size in meters
            
        Returns:
            Tuple of (x_values, concentration_profiles)
        """
        x_vals = np.arange(0, length, dx)
        n_x = len(x_vals)
        
        # Initial condition: delta function at x=0
        C_0 = np.zeros(n_x)
        C_0[0] = initial_concentration
        
        concentration_profiles = [C_0]
        
        for t in time_points[1:]:
            # Simple explicit scheme (for demonstration)
            dt = t - time_points[np.searchsorted(time_points, t) - 1]
            
            # Advection term
            dC_adv = -velocity * np.gradient(C_0, dx)
            
            # Diffusion term
            dC_diff = diffusion_coefficient * np.gradient(np.gradient(C_0, dx), dx)
            
            # Update
            C_new = C_0 + dt * (dC_adv + dC_diff)
            C_new = np.maximum(C_new, 0)  # No negative concentrations
            
            concentration_profiles.append(C_new)
            C_0 = C_new
        
        return x_vals, np.array(concentration_profiles)


class ElectricFieldTransport:
    """Model for electric field-driven transport (electrophoresis)."""
    
    def __init__(self, temperature=298.15):
        """
        Initialize electric field transport model.
        
        Args:
            temperature: Temperature in Kelvin
        """
        self.T = temperature
        self.kB = 1.380649e-23
        self.e = 1.602176634e-19
        self.epsilon_r = 78.5
        self.epsilon_0 = 8.854e-12
        
    def electrophoretic_mobility(self, charge, radius, viscosity):
        """
        Calculate electrophoretic mobility.
        
        Args:
            charge: Particle charge in Coulombs
            radius: Particle radius in meters
            viscosity: Viscosity in Pa*s
            
        Returns:
            Electrophoretic mobility in m^2/(V*s)
        """
        # Huckel approximation (for small particles)
        return charge / (6 * np.pi * viscosity * radius)
    
    def electrophoretic_velocity(self, mobility, electric_field):
        """
        Calculate electrophoretic velocity.
        
        Args:
            mobility: Electrophoretic mobility in m^2/(V*s)
            electric_field: Electric field strength in V/m
            
        Returns:
            Velocity in m/s
        """
        return mobility * electric_field
    
    def drift_velocity(self, charge, electric_field, viscosity, radius):
        """
        Calculate drift velocity in electric field.
        
        Args:
            charge: Particle charge in Coulombs
            electric_field: Electric field strength in V/m
            viscosity: Viscosity in Pa*s
            radius: Particle radius in meters
            
        Returns:
            Drift velocity in m/s
        """
        return charge * electric_field / (6 * np.pi * viscosity * radius)


def main():
    """Example usage of drug transport and release models."""
    print("=" * 60)
    print("Drug Transport and Release Modeling")
    print("=" * 60)
    
    # Diffusion model
    print("\n1. Diffusion Models")
    print("-" * 40)
    
    diffusion = DiffusionModel()
    
    # Stokes-Einstein
    radius = 1e-9  # 1 nm particle
    viscosity = 0.00089  # Water at 25C
    D = diffusion.diffusion_coefficient_stokes_einstein(radius, viscosity)
    print(f"Diffusion coefficient (1 nm particle in water): {D:.2e} m^2/s")
    
    # Mean squared displacement
    t = 1.0  # 1 second
    msd = diffusion.mean_squared_displacement(D, t)
    print(f"Mean squared displacement in 1s: {msd:.2e} m^2")
    print(f"RMS displacement: {np.sqrt(msd) * 1e9:.2f} nm")
    
    # Drug release kinetics
    print("\n2. Drug Release Kinetics")
    print("-" * 40)
    
    release = DrugReleaseKinetics()
    time_points = np.linspace(0, 3600, 100)  # 1 hour
    initial_amount = 1.0  # mol
    
    # Zero-order release
    rate = 0.001  # mol/s
    released_zero = release.zero_order_release(rate, initial_amount, time_points)
    print(f"Zero-order release after 1 hour: {released_zero[-1]:.3f} mol")
    
    # First-order release
    k = 0.001  # s^-1
    released_first = release.first_order_release(k, initial_amount, time_points)
    print(f"First-order release after 1 hour: {released_first[-1]:.3f} mol")
    
    # Higuchi model
    D = 1e-10  # m^2/s
    drug_loading = 100  # kg/m^3
    tortuosity = 2.0
    porosity = 0.5
    released_higuchi = release.higuchi_model(D, drug_loading, tortuosity, porosity, time_points)
    print(f"Higuchi model release after 1 hour: {released_higuchi[-1]:.3f} kg/m^2")
    
    # Peppas model
    n = 0.5  # Fickian diffusion
    k = 0.1
    released_peppas = release.peppas_model(n, k, initial_amount, time_points)
    print(f"Peppas model release after 1 hour: {released_peppas[-1]:.3f} mol")
    print(f"Release mechanism: {release.interpret_peppas_exponent(n)}")
    
    # Effective medium theory
    print("\n3. Effective Medium Theory")
    print("-" * 40)
    
    emt = EffectiveMediumTheory()
    D_free = 1e-9  # m^2/s
    
    # Porous medium
    porosity = 0.5
    tortuosity = 2.0
    D_eff_porous = emt.effective_diffusivity_porous(D_free, porosity, tortuosity)
    print(f"Effective diffusivity (porous, ε={porosity}, τ={tortuosity}): {D_eff_porous:.2e} m^2/s")
    
    # Percolation theory
    critical_porosity = 0.2
    exponent = 2.0
    porosity_values = [0.1, 0.2, 0.3, 0.4, 0.5]
    print("\nPorosity | Effective Diffusivity")
    print("-" * 40)
    for p in porosity_values:
        D_eff_perc = emt.effective_diffusivity_percolation(D_free, p, critical_porosity, exponent)
        print(f"  {p:6.2f}  | {D_eff_perc:.2e} m^2/s")
    
    # Electric field transport
    print("\n4. Electric Field Transport")
    print("-" * 40)
    
    eft = ElectricFieldTransport()
    
    # Electrophoretic mobility
    charge = 1.602e-19  # Single electron charge
    radius = 1e-9  # 1 nm
    mobility = eft.electrophoretic_mobility(charge, radius, viscosity)
    print(f"Electrophoretic mobility: {mobility:.2e} m^2/(V*s)")
    
    # Electrophoretic velocity
    electric_field = 1000  # V/m
    velocity = eft.electrophoretic_velocity(mobility, electric_field)
    print(f"Electrophoretic velocity in 1000 V/m: {velocity:.2e} m/s")
    print(f"  = {velocity * 1e6:.2f} μm/s")
    
    # Plot release kinetics
    plt.figure(figsize=(12, 8))
    
    plt.subplot(2, 2, 1)
    plt.plot(time_points / 3600, released_zero, 'b-', label='Zero-order')
    plt.plot(time_points / 3600, released_first, 'r-', label='First-order')
    plt.plot(time_points / 3600, released_peppas, 'g-', label='Peppas (n=0.5)')
    plt.xlabel('Time (hours)')
    plt.ylabel('Released Amount (mol)')
    plt.title('Drug Release Kinetics')
    plt.legend()
    plt.grid(True)
    
    plt.subplot(2, 2, 2)
    plt.plot(time_points / 3600, released_higuchi, 'm-')
    plt.xlabel('Time (hours)')
    plt.ylabel('Released Amount (kg/m²)')
    plt.title('Higuchi Model')
    plt.grid(True)
    
    plt.subplot(2, 2, 3)
    porosities = np.linspace(0.01, 0.6, 100)
    D_eff_values = [emt.effective_diffusivity_porous(D_free, p, tortuosity) for p in porosities]
    plt.plot(porosities, D_eff_values, 'c-')
    plt.xlabel('Porosity')
    plt.ylabel('Effective Diffusivity (m²/s)')
    plt.title('Effective Diffusivity vs Porosity')
    plt.grid(True)
    
    plt.subplot(2, 2, 4)
    electric_fields = np.linspace(0, 5000, 50)
    velocities = [eft.electrophoretic_velocity(mobility, E) for E in electric_fields]
    plt.plot(electric_fields, velocities, 'y-')
    plt.xlabel('Electric Field (V/m)')
    plt.ylabel('Electrophoretic Velocity (m/s)')
    plt.title('Electrophoretic Velocity')
    plt.grid(True)
    
    plt.tight_layout()
    plt.savefig('transport/drug_release_kinetics.png', dpi=150)
    plt.close()
    
    print("\nPlots saved as 'transport/drug_release_kinetics.png'")


if __name__ == "__main__":
    main()
