## Resubmission changes

Thank you for considering our package and for the feedback provided. Here is the justification of the two issues detected by the CRAN team:

* **\dontrun{} should only be used if the example really cannot be executed (e.g. because of missing additional software, missing API keys, ...) by the user.** The only instance of `\dontrun` we have is in the `c4h_get()` data, which is a function used to download climate data using an API that and API key. According to the provided guideline, we think that the use of `\dontrun` is correct in this instance.
* **If there are references describing the methods in your package, please add these in the description field of your DESCRIPTION file.**

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
