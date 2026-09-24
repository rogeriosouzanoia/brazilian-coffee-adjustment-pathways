# Publish this repository on GitHub

Keep the repository **private** during coauthor review. The archive includes code, annual municipal climate indices and the audited coffee production series in compressed form, compact figure inputs and figure files. Original daily NASA POWER downloads and raster/model intermediates are not bundled.

From a computer with Git and the GitHub CLI (`gh`) installed and authenticated:

```bash
unzip Brazilian_coffee_reproducibility_verified.zip
cd brazilian-coffee-study-repro
git init
git add .
git commit -m "Archive Brazilian coffee manuscript code and figure inputs"
gh repo create brazilian-coffee-adjustment-pathways --private --source=. --remote=origin --push
```

If `gh repo create` reports that the name is taken, choose another name. Verify the GitHub account and the private visibility before publishing. Review authorship, the data availability statement, licensing and a persistent archive DOI before switching to public visibility. Do not add `.Renviron`, `.env`, API keys, unreviewed raw downloads, model caches or restricted files. A private GitHub repository by itself is not the public code/data deposit described in the manuscript.
