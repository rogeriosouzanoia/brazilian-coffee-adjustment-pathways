# Private repository and eventual public release

This working repository is private at
`https://github.com/rogeriosouzanoia/brazilian-coffee-adjustment-pathways`.
It contains selected research code, complete audited municipal coffee statistics,
derived municipal annual climate indices, inputs for the submitted figures,
independent audit scripts and figure exports. It does not contain the original
daily NASA POWER downloads, raster tiles or serialized model objects.

During coauthor review, keep the repository private. Before a public release:

1. Run the full selected R workflow with recorded R package versions and frozen
   source extracts, and resolve any discrepancies in the remaining water,
   raster and productivity outputs.
2. Have all authors approve the code, data redistribution, repository license,
   authorship and data/code availability statements.
3. Tag a reviewed release and archive that exact version with a persistent DOI.
   Insert the DOI and public URL into the manuscript before submission.

A private GitHub URL alone does not satisfy the manuscript's proposed public
code/data deposit. Keep `.Renviron`, `.env`, credentials, unreviewed downloads,
model caches and restricted files out of the public release.
