# SA-Edward
Spalart-Allmaras model is a one equation model which solves a transport equation for a viscosity-like variable <img src="https://render.githubusercontent.com/render/math?math=\tilde{\nu}"> . This may be referred to as the Spalart-Allmaras variable. An in-house code for SA Edward's model of turbulence is written with all its subroutines. This code may be a base for future improvements. It May be Copied, Modified and Redistributed for Non-Commercial Use. For more information about different types of turbulence models, please visit https://turbmodels.larc.nasa.gov/spalart.html.

## Features
-  2D 
- Using Unstructured Mesh
- Edge Based Data Structured
- Cell Center Conrol Volume
- Turbulent Flow
- Convection Terms is Discritized by AUSM Scheme
- Transient Term is Discritized by Runge-Kutta Explicit
- Spalart-Allmaras Edward Tulbulence Model  


 <img src="https://github.com/Vaezi92/SA-Edward/blob/main/Figs/mesh1.png" width="300">   <img src="https://github.com/Vaezi92/SA-Edward/blob/main/Figs/mesh2.png" width="300"> 


<img src="https://github.com/Vaezi92/SA-Edward/blob/main/Figs/CP.png" width="400">   <img src="https://github.com/Vaezi92/SA-Edward/blob/main/Figs/P.png" width="400">

## Citation

@article{edwards1996comparison,
  title={Comparison of eddy viscosity-transport turbulence models for three-dimensional, shock-separated flowfields},
  author={Edwards, Jack R and Chandra, Suresh},
  journal={AIAA journal},
  volume={34},
  number={4},
  pages={756--763},
  year={1996}
}
