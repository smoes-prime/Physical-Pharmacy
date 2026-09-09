#!/usr/bin/env python3
"""
Light Scattering and Small-Angle Scattering Analysis

Tools for analyzing experimental scattering data from:
- Static Light Scattering (SLS)
- Dynamic Light Scattering (DLS)
- Small-Angle X-ray Scattering (SAXS)
- Small-Angle Neutron Scattering (SANS)

Course: Molecular Physical Pharmacy (3FC003)
"""

import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit
from scipy.special import j0, j1


class StaticLightScattering:
    """Static Light Scattering (SLS) analysis."""
    
    def __init__(self, wavelength=632.8e-9, refractive_index=1.33):
        """
        Initialize SLS analyzer.
        
        Args:
            wavelength: Laser wavelength in meters (default 632.8 nm HeNe)
            refractive_index: Refractive index of medium (default water = 1.33)
        """
        self.lambda_ = wavelength
        self.n = refractive_index
        self.kB = 1.380649e-23  # J/K
        self.N_A = 6.02214076e23  # mol^-1
        
    @property
    def wave_vector(self):
        """Calculate wave vector magnitude in medium."""
        return (2 * np.pi * self.n) / self.lambda_
    
    def rayleigh_ratio(self, concentration, molecular_weight, dn_dc, 
                      temperature=298.15, angle=90):
        """
        Calculate Rayleigh ratio for a solution.
        
        Args:
            concentration: Concentration in kg/m^3
            molecular_weight: Molecular weight in kg/mol
            dn_dc: Refractive index increment in m^3/kg
            temperature: Temperature in Kelvin
            angle: Scattering angle in degrees
            
        Returns:
            Rayleigh ratio in m^-1
        """
        theta = np.radians(angle)
        q = 2 * self.wave_vector * np.sin(theta / 2)
        
        # Rayleigh ratio: R = K * c * M * P(q)
        # where K = 4 * pi^2 * n^2 * (dn/dc)^2 / (lambda^4 * N_A)
        K = (4 * np.pi**2 * self.n**2 * dn_dc**2) / \
            (self.lambda_**4 * self.N_A)
        
        # For small particles (qR_g << 1), P(q) ≈ 1
        P_q = 1.0
        
        R = K * concentration * molecular_weight * P_q
        return R
    
    def molecular_weight_from_sls(self, rayleigh_ratios, concentrations, 
                                   dn_dc, temperature=298.15, angle=90):
        """
        Calculate molecular weight from SLS measurements.
        
        Args:
            rayleigh_ratios: Array of Rayleigh ratios
            concentrations: Array of concentrations in kg/m^3
            dn_dc: Refractive index increment in m^3/kg
            temperature: Temperature in Kelvin
            angle: Scattering angle in degrees
            
        Returns:
            Molecular weight in kg/mol
        """
        theta = np.radians(angle)
        q = 2 * self.wave_vector * np.sin(theta / 2)
        
        K = (4 * np.pi**2 * self.n**2 * dn_dc**2) / \
            (self.lambda_**4 * self.N_A)
        
        # Debye plot: K * c / R = 1/M + 2 * A2 * c + ...
        # For dilute solutions, intercept gives 1/M
        
        y = K * concentrations / rayleigh_ratios
        x = concentrations
        
        # Linear fit
        A = np.vstack([x, np.ones(len(x))]).T
        slope, intercept = np.linalg.lstsq(A, y, rcond=None)[0]
        
        M = 1 / intercept
        A2 = slope / 2  # Second virial coefficient
        
        return M, A2
    
    def radius_of_gyration_from_guinier(self, intensity, q_values):
        """
        Calculate radius of gyration from Guinier plot.
        
        Args:
            intensity: Array of scattered intensities
            q_values: Array of scattering vectors in m^-1
            
        Returns:
            Radius of gyration in meters
        """
        # Guinier equation: ln(I(q)) = ln(I(0)) - (R_g^2 / 3) * q^2
        
        log_intensity = np.log(intensity)
        q_squared = q_values**2
        
        # Linear fit
        A = np.vstack([q_squared, np.ones(len(q_squared))]).T
        slope, intercept = np.linalg.lstsq(A, log_intensity, rcond=None)[0]
        
        R_g = np.sqrt(-3 * slope)
        return R_g
    
    def form_factor_sphere(self, q, radius):
        """
        Calculate form factor for a sphere.
        
        Args:
            q: Scattering vector in m^-1
            radius: Sphere radius in meters
            
        Returns:
            Form factor P(q)
        """
        qr = q * radius
        if qr == 0:
            return 1.0
        return (3 * (np.sin(qr) - qr * np.cos(qr)) / qr**3)**2
    
    def form_factor_rod(self, q, length, radius):
        """
        Calculate form factor for a cylindrical rod.
        
        Args:
            q: Scattering vector in m^-1
            length: Rod length in meters
            radius: Rod radius in meters
            
        Returns:
            Form factor P(q)
        """
        qL = q * length / 2
        qr = q * radius
        
        if qL == 0 and qr == 0:
            return 1.0
        
        # For a rod of length L and radius r
        # P(q) = (2 / (qL)) * j1(qL) * (2 * j1(qr) / (qr))^2
        # where j1 is the first-order spherical Bessel function
        
        if qL > 0:
            term1 = (2 / qL) * j1(qL)
        else:
            term1 = 1.0
        
        if qr > 0:
            term2 = (2 * j1(qr) / qr)**2
        else:
            term2 = 1.0
        
        return term1 * term2


class DynamicLightScattering:
    """Dynamic Light Scattering (DLS) analysis."""
    
    def __init__(self, wavelength=632.8e-9, refractive_index=1.33):
        """
        Initialize DLS analyzer.
        
        Args:
            wavelength: Laser wavelength in meters
            refractive_index: Refractive index of medium
        """
        self.lambda_ = wavelength
        self.n = refractive_index
        self.kB = 1.380649e-23  # J/K
        
    @property
    def wave_vector(self):
        """Calculate wave vector magnitude in medium."""
        return (2 * np.pi * self.n) / self.lambda_
    
    def autocorrelation_function(self, time_points, diffusion_coefficient, 
                                angle=90, baseline=0.1):
        """
        Calculate theoretical autocorrelation function.
        
        Args:
            time_points: Array of time points in seconds
            diffusion_coefficient: Diffusion coefficient in m^2/s
            angle: Scattering angle in degrees
            baseline: Baseline level
            
        Returns:
            Autocorrelation function values
        """
        theta = np.radians(angle)
        q = 2 * self.wave_vector * np.sin(theta / 2)
        
        # g1(t) = exp(-D * q^2 * t)
        g1 = np.exp(-diffusion_coefficient * q**2 * time_points)
        
        # g2(t) = A * (1 + beta * g1(t)^2)
        # For simplicity, assume beta = 1 and A = 1 - baseline
        g2 = (1 - baseline) * (1 + g1**2) + baseline
        
        return g2
    
    def diffusion_coefficient_from_dls(self, time_points, autocorrelation, 
                                       angle=90, viscosity=0.00089, temperature=298.15):
        """
        Calculate diffusion coefficient from DLS autocorrelation.
        
        Args:
            time_points: Array of time points in seconds
            autocorrelation: Array of autocorrelation function values
            angle: Scattering angle in degrees
            viscosity: Viscosity in Pa*s
            temperature: Temperature in Kelvin
            
        Returns:
            Diffusion coefficient in m^2/s
        """
        theta = np.radians(angle)
        q = 2 * self.wave_vector * np.sin(theta / 2)
        
        # Fit: g1(t) = exp(-D * q^2 * t)
        # g2(t) - baseline = A * exp(-2 * D * q^2 * t)
        
        # Subtract baseline (assume last 10% is baseline)
        n_baseline = int(0.1 * len(autocorrelation))
        baseline = np.mean(autocorrelation[-n_baseline:])
        g2_corrected = autocorrelation - baseline
        
        # Take square root for g1
        g1 = np.sqrt(g2_corrected / (1 - baseline))
        
        # Fit: ln(g1) = -D * q^2 * t
        log_g1 = np.log(g1)
        
        # Linear fit
        A = np.vstack([-q**2 * time_points, np.ones(len(time_points))]).T
        D, _ = np.linalg.lstsq(A, log_g1, rcond=None)[0]
        
        return D
    
    def hydrodynamic_radius(self, diffusion_coefficient, viscosity, temperature=298.15):
        """
        Calculate hydrodynamic radius from diffusion coefficient.
        
        Args:
            diffusion_coefficient: Diffusion coefficient in m^2/s
            viscosity: Viscosity in Pa*s
            temperature: Temperature in Kelvin
            
        Returns:
            Hydrodynamic radius in meters
        """
        kB = 1.380649e-23
        return kB * temperature / (6 * np.pi * viscosity * diffusion_coefficient)
    
    def size_distribution_from_cumulants(self, autocorrelation, time_points, angle=90):
        """
        Calculate size distribution using cumulants analysis.
        
        Args:
            autocorrelation: Array of autocorrelation function values
            time_points: Array of time points in seconds
            angle: Scattering angle in degrees
            
        Returns:
            Dictionary with mean radius and polydispersity index
        """
        theta = np.radians(angle)
        q = 2 * self.wave_vector * np.sin(theta / 2)
        
        # Fit: ln(g1) = -Gamma * t + (mu2 / 2) * t^2 - ...
        # where Gamma = D * q^2
        # and mu2 / Gamma^2 = PDI (polydispersity index)
        
        # Subtract baseline
        n_baseline = int(0.1 * len(autocorrelation))
        baseline = np.mean(autocorrelation[-n_baseline:])
        g2_corrected = autocorrelation - baseline
        g1 = np.sqrt(g2_corrected / (1 - baseline))
        
        # Fit second-order polynomial to ln(g1)
        log_g1 = np.log(g1)
        
        A = np.vstack([time_points, time_points**2, np.ones(len(time_points))]).T
        Gamma, mu2_div_2, _ = np.linalg.lstsq(A, log_g1, rcond=None)[0]
        
        # Calculate PDI
        PDI = mu2_div_2 / Gamma**2 if Gamma != 0 else 0
        
        # Calculate mean diffusion coefficient
        D = Gamma / q**2
        
        return {'D': D, 'PDI': PDI}


class SmallAngleScattering:
    """Small-Angle X-ray and Neutron Scattering (SAXS/SANS) analysis."""
    
    def __init__(self):
        """Initialize SAXS/SANS analyzer."""
        self.kB = 1.380649e-23
        self.N_A = 6.02214076e23
        
    def scattering_intensity_sphere(self, q_values, radius, contrast, volume_fraction=0.01):
        """
        Calculate scattering intensity for spherical particles.
        
        Args:
            q_values: Array of scattering vectors in m^-1
            radius: Particle radius in meters
            contrast: Contrast (difference in scattering length density) in m^-2
            volume_fraction: Volume fraction of particles
            
        Returns:
            Array of scattering intensities
        """
        form_factors = np.array([self.form_factor_sphere(q, radius) for q in q_values])
        
        # I(q) = N * V^2 * (Delta rho)^2 * P(q) * S(q)
        # For dilute systems, S(q) ≈ 1
        
        N = volume_fraction / ((4/3) * np.pi * radius**3)
        V = (4/3) * np.pi * radius**3
        
        intensity = N * V**2 * contrast**2 * form_factors
        
        return intensity
    
    def form_factor_sphere(self, q, radius):
        """
        Calculate form factor for a sphere.
        
        Args:
            q: Scattering vector in m^-1
            radius: Sphere radius in meters
            
        Returns:
            Form factor P(q)
        """
        qr = q * radius
        if qr == 0:
            return 1.0
        return (3 * (np.sin(qr) - qr * np.cos(qr)) / qr**3)**2
    
    def form_factor_vesicle(self, q_values, outer_radius, thickness):
        """
        Calculate form factor for a vesicle (hollow sphere).
        
        Args:
            q_values: Array of scattering vectors in m^-1
            outer_radius: Outer radius in meters
            thickness: Shell thickness in meters
            
        Returns:
            Array of form factors
        """
        inner_radius = outer_radius - thickness
        
        # Form factor for hollow sphere
        # P(q) = (4/3) * pi * [rho_outer * R_outer^3 * F(q, R_outer) - 
        #                 rho_inner * R_inner^3 * F(q, R_inner)]^2
        # where F(q, R) = 3 * (sin(qR) - qR cos(qR)) / (qR)^3
        
        def F(q, R):
            if q * R == 0:
                return 1.0
            return 3 * (np.sin(q * R) - q * R * np.cos(q * R)) / (q * R)**3
        
        form_factors = []
        for q in q_values:
            if q == 0:
                form_factors.append(0)
                continue
            
            F_outer = F(q, outer_radius)
            F_inner = F(q, inner_radius)
            
            # For simplicity, assume contrast is the same
            # P(q) proportional to (R_outer^3 * F_outer - R_inner^3 * F_inner)^2
            P = (outer_radius**3 * F_outer - inner_radius**3 * F_inner)**2
            form_factors.append(P)
        
        return np.array(form_factors)
    
    def guinier_plot(self, intensity, q_values):
        """
        Create Guinier plot and calculate R_g.
        
        Args:
            intensity: Array of scattering intensities
            q_values: Array of scattering vectors in m^-1
            
        Returns:
            Tuple of (q_values, ln_intensity, R_g)
        """
        log_intensity = np.log(intensity)
        q_squared = q_values**2
        
        # Linear fit
        A = np.vstack([q_squared, np.ones(len(q_squared))]).T
        slope, intercept = np.linalg.lstsq(A, log_intensity, rcond=None)[0]
        
        R_g = np.sqrt(-3 * slope)
        
        return q_values, log_intensity, R_g
    
    def kratky_plot(self, intensity, q_values):
        """
        Create Kratky plot.
        
        Args:
            intensity: Array of scattering intensities
            q_values: Array of scattering vectors in m^-1
            
        Returns:
            Tuple of (q_values, q^2 * I(q))
        """
        return q_values, q_values**2 * intensity
    
    def pair_distance_distribution(self, intensity, q_values, q_max):
        """
        Calculate pair distance distribution function P(r).
        
        Args:
            intensity: Array of scattering intensities
            q_values: Array of scattering vectors in m^-1
            q_max: Maximum q value for integration
            
        Returns:
            Tuple of (r_values, P_r_values)
        """
        # P(r) = (2 / pi) * integral_0^q_max [q * r * sin(qr) * I(q) * dq]
        
        # For simplicity, use numerical integration
        r_values = np.linspace(0, 2 * np.pi / q_max, 100)
        P_r_values = np.zeros_like(r_values)
        
        # Filter q values up to q_max
        mask = q_values <= q_max
        q_filtered = q_values[mask]
        I_filtered = intensity[mask]
        
        for i, r in enumerate(r_values):
            integrand = q_filtered * r * np.sin(q_filtered * r) * I_filtered
            P_r_values[i] = np.trapz(integrand, q_filtered)
        
        P_r_values *= 2 / np.pi
        
        return r_values, P_r_values


def main():
    """Example usage of scattering analysis tools."""
    print("=" * 60)
    print("Light Scattering and Small-Angle Scattering Analysis")
    print("=" * 60)
    
    # Static Light Scattering
    print("\n1. Static Light Scattering (SLS)")
    print("-" * 40)
    
    sls = StaticLightScattering()
    
    # Example: Protein solution
    concentration = 1.0  # kg/m^3 = 1 g/L
    molecular_weight = 50000  # g/mol = 0.05 kg/mol
    dn_dc = 0.18  # mL/g = 0.18 cm^3/g = 1.8e-4 m^3/kg
    
    R = sls.rayleigh_ratio(concentration, molecular_weight, dn_dc)
    print(f"Rayleigh ratio: {R:.2e} m^-1")
    
    # Calculate molecular weight from SLS
    concentrations = np.array([0.5, 1.0, 1.5, 2.0]) * 1e-3  # kg/m^3 (0.5-2 g/L)
    rayleigh_ratios = np.array([sls.rayleigh_ratio(c, molecular_weight, dn_dc) for c in concentrations])
    
    M_calc, A2 = sls.molecular_weight_from_sls(rayleigh_ratios, concentrations, dn_dc)
    print(f"Calculated molecular weight: {M_calc * 1e3:.2f} kg/mol = {M_calc * self.N_A:.0f} g/mol")
    print(f"Second virial coefficient: {A2:.2e} m^3 mol/kg^2")
    
    # Radius of gyration from Guinier
    q_values = np.linspace(0.1, 3.0, 50)  # nm^-1, convert to m^-1
    q_values_m = q_values * 1e9
    R_g = 3.0  # nm = 3e-9 m
    
    # Form factor for sphere
    form_factors = np.array([sls.form_factor_sphere(q, R_g * 1e-9) for q in q_values_m])
    
    # Guinier plot
    R_g_calc = sls.radius_of_gyration_from_guinier(form_factors, q_values_m)
    print(f"Radius of gyration from Guinier: {R_g_calc * 1e9:.2f} nm")
    
    # Dynamic Light Scattering
    print("\n2. Dynamic Light Scattering (DLS)")
    print("-" * 40)
    
    dls = DynamicLightScattering()
    
    # Example: Particle with known diffusion coefficient
    D = 1e-10  # m^2/s (typical for 10 nm particle)
    time_points = np.linspace(0, 1e-3, 100)  # 0 to 1 ms
    
    autocorr = dls.autocorrelation_function(time_points, D)
    
    # Calculate diffusion coefficient from autocorrelation
    D_calc = dls.diffusion_coefficient_from_dls(time_points, autocorr)
    print(f"Calculated diffusion coefficient: {D_calc:.2e} m^2/s")
    
    # Calculate hydrodynamic radius
    viscosity = 0.00089  # Pa*s (water at 25C)
    R_h = dls.hydrodynamic_radius(D, viscosity)
    print(f"Hydrodynamic radius: {R_h * 1e9:.2f} nm")
    
    # Size distribution from cumulants
    cumulants = dls.size_distribution_from_cumulants(autocorr, time_points)
    print(f"Mean diffusion coefficient (cumulants): {cumulants['D']:.2e} m^2/s")
    print(f"Polydispersity index: {cumulants['PDI']:.3f}")
    
    # Small-Angle Scattering
    print("\n3. Small-Angle X-ray/Neutron Scattering (SAXS/SANS)")
    print("-" * 40)
    
    sas = SmallAngleScattering()
    
    # Example: Spherical protein
    radius = 3e-9  # 3 nm
    contrast = 1e-5  # m^-2 (typical for protein in water)
    volume_fraction = 0.01
    
    q_values_sas = np.linspace(0.1, 10, 100)  # nm^-1
    q_values_sas_m = q_values_sas * 1e9  # m^-1
    
    intensity = sas.scattering_intensity_sphere(q_values_sas_m, radius, contrast, volume_fraction)
    
    # Guinier plot
    q_guinier, ln_I, R_g_sas = sas.guinier_plot(intensity, q_values_sas_m)
    print(f"Radius of gyration from SAXS Guinier: {R_g_sas * 1e9:.2f} nm")
    
    # Kratky plot
    q_kratky, kratky = sas.kratky_plot(intensity, q_values_sas_m)
    
    # Pair distance distribution
    r_values, P_r = sas.pair_distance_distribution(intensity, q_values_sas_m, q_max=10e9)
    print(f"Maximum dimension from P(r): {r_values[-1] * 1e9:.2f} nm")
    
    # Vesicle form factor
    outer_radius = 20e-9  # 20 nm
    thickness = 4e-9  # 4 nm
    form_factors_vesicle = sas.form_factor_vesicle(q_values_sas_m, outer_radius, thickness)
    
    # Plot results
    print("\n4. Creating Visualizations")
    print("-" * 40)
    
    fig = plt.figure(figsize=(12, 10))
    
    # SLS: Debye plot
    ax1 = fig.add_subplot(221)
    K = (4 * np.pi**2 * sls.n**2 * dn_dc**2) / (sls.lambda_**4 * sls.N_A)
    y = K * concentrations / rayleigh_ratios
    ax1.plot(concentrations * 1000, y, 'bo-')  # Convert kg/m^3 to g/L
    ax1.set_xlabel('Concentration (g/L)')
    ax1.set_ylabel('K c / R')
    ax1.set_title('Debye Plot (SLS)')
    ax1.grid(True)
    
    # SLS: Guinier plot
    ax2 = fig.add_subplot(222)
    q_guinier_nm = q_guinier * 1e-9  # Convert to nm^-1
    ax2.plot(q_guinier_nm**2, ln_I, 'ro-')
    ax2.set_xlabel('q² (nm⁻²)')
    ax2.set_ylabel('ln(I(q))')
    ax2.set_title('Guinier Plot')
    ax2.grid(True)
    
    # DLS: Autocorrelation function
    ax3 = fig.add_subplot(223)
    ax3.plot(time_points * 1e6, autocorr, 'g-')  # Convert to microseconds
    ax3.set_xlabel('Time (μs)')
    ax3.set_ylabel('g₂(t)')
    ax3.set_title('DLS Autocorrelation')
    ax3.grid(True)
    
    # SAXS: Scattering intensity
    ax4 = fig.add_subplot(224)
    q_nm = q_values_sas_m * 1e-9  # Convert to nm^-1
    ax4.loglog(q_nm, intensity, 'b-')
    ax4.set_xlabel('q (nm⁻¹)')
    ax4.set_ylabel('I(q)')
    ax4.set_title('SAXS Scattering Intensity')
    ax4.grid(True, which='both')
    
    plt.tight_layout()
    plt.savefig('scattering/scattering_analysis.png', dpi=150)
    plt.close()
    
    # Additional plots
    fig2 = plt.figure(figsize=(12, 8))
    
    # Kratky plot
    ax1 = fig2.add_subplot(221)
    ax1.plot(q_nm, kratky, 'r-')
    ax1.set_xlabel('q (nm⁻¹)')
    ax1.set_ylabel('q² I(q)')
    ax1.set_title('Kratky Plot')
    ax1.grid(True)
    
    # Pair distance distribution
    ax2 = fig2.add_subplot(222)
    r_nm = r_values * 1e9
    ax2.plot(r_nm, P_r, 'm-')
    ax2.set_xlabel('Distance (nm)')
    ax2.set_ylabel('P(r)')
    ax2.set_title('Pair Distance Distribution')
    ax2.grid(True)
    
    # Form factors
    ax3 = fig2.add_subplot(223)
    ax3.plot(q_nm, form_factors, 'c-', label='Sphere')
    ax3.plot(q_nm, form_factors_vesicle, 'y-', label='Vesicle')
    ax3.set_xlabel('q (nm⁻¹)')
    ax3.set_ylabel('P(q)')
    ax3.set_title('Form Factors')
    ax3.legend()
    ax3.grid(True)
    
    # SLS form factor
    ax4 = fig2.add_subplot(224)
    ax4.plot(q_values, form_factors, 'b-')
    ax4.set_xlabel('q (nm⁻¹)')
    ax4.set_ylabel('P(q)')
    ax4.set_title('SLS Form Factor (Sphere)')
    ax4.grid(True)
    
    plt.tight_layout()
    plt.savefig('scattering/scattering_analysis_detailed.png', dpi=150)
    plt.close()
    
    print("Plots saved as 'scattering/scattering_analysis.png' and 'scattering/scattering_analysis_detailed.png'")


if __name__ == "__main__":
    main()
