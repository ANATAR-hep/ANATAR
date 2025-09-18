# ANATAR
ANATAR: AN Automated Tool for higher-order Amplitude geneRation

# Authors
Claude Duhr[cduhr@uni-bonn.de]  
Pooja Mukherjee [pooja.mukherjee@desy.de]  
Andres Vasquez [avasquez@uni-bonn.de]

# Introduction
ANATAR, a Mathematica-based package for computing scattering amplitudes in  QFT. For a detailed description of the code please see arXiv:2509.13951. 

# Features

1. Generation Amplitudes, amplitude conjugate and amplitude square for a given process in a given model using FORM.
2. Generation of projected amplitudes for a given process and given projector using FORM.
3. Generation of topologies/integral families from the set of Feynman diagrams in Mathematica.
4. Reduction of the topologies in terms of master integrals using LiteRed or Kira.
5. Rewriting all the amplitudes in terms of the master integrals in Mathematica.

# Requirements
Mathematica (>=13.2)  
FORM (>=4.2)  
QGRAF 3.6.5  

(Optionals)

Kira 2.3  
LiteRed

# Examples

There are some examples provided in the ANATAR folder : 
1. Two-loop virtual correction to Higgs production through bottom quark annihilation.
2. Two-loop double-real correction to Higgs production through bottom quark annihilation.


