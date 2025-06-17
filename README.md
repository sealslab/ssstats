# ssstats
An R package for applied statistics courses at the University of West Florida.

This is joint work between Ihsan E. Buker and Samantha R. Seals.

You can install the package using the following code:

```
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
devtools::install_github("sealslab/ssstats")
```

Then, you will call the package into R with `library(ssstats)`.

Please only install the package *once*. By repeatedly running the installation code above, you will repeatedly download and install the package onto your computer, wasting resources. Further, Quarto does not allow for package installation in .qmd files.
