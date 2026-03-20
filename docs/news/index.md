# Changelog

## metafrontieR 1.0.0

### Initial CRAN release

- First public release of `metafrontieR`.
- Implements stochastic and deterministic metafrontier analysis for
  productivity and performance benchmarking across firms operating under
  different technologies.
- Supports three group-frontier types via ‘sfaR’:
  - Standard SFA (`sfacross`)
  - Latent class SFA (`sfalcmcross`)
  - Sample-selection-corrected SFA (`sfaselectioncross`)
- Three metafrontier estimation methods:
  - Linear programming (LP) deterministic envelope
  - Quadratic programming (QP) deterministic envelope
  - Second-stage stochastic frontier (Huang et al. 2014; O’Donnell et
    al. 2008)
- Full efficiency outputs via
  [`efficiencies()`](https://SulmanOlieko.github.io/metafrontieR/reference/efficiencies.md):
  group TE (JLMS and BC), metafrontier TE, and metatechnology ratios
  (MTR).
- Five vignettes illustrating each major use case.
