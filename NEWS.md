# NEWS.md

# smfa v1.0.0: Imitation is the sincerest form of flattery

The definitive, original implementation of stochastic metafrontier analysis for R is officially stable. 

After rigorous development, extensive methodological testing, and refinement for CRAN submission, `smfa` v1.0.0 provides a robust, production-ready environment for productivity and performance benchmarking across firms operating under different technologies. This release establishes the standard for stochastic metafrontier analysis in the R ecosystem.

## Initial CRAN release

* First public release of `smfa`.
* Implements stochastic metafrontier analysis for productivity and performance benchmarking across firms operating under different technologies.
* Supports three group-frontier types via 'sfaR':
  - Standard SFA (`sfacross`)
  - Latent class SFA (`sfalcmcross`)
  - Sample-selection-corrected SFA (`sfaselectioncross`)
* Three metafrontier estimation methods:
  - Linear programming (LP) deterministic envelope
  - Quadratic programming (QP) deterministic envelope
  - Second-stage stochastic frontier (Huang et al. 2014; O'Donnell et al. 2008)
* Full efficiency outputs via `efficiencies()`: group TE (JLMS and BC),
  metafrontier TE, and metatechnology ratios (MTR).
* Five vignettes illustrating each major use case.
