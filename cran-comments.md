# Version 0.10.3

This is a resubmission of v0.10.3. Previous submission failed to pass CRAN
pretest due to a NOTE caused by using more than 2 cores in examples. In this
submission, I explicitly skip all examples that cause this NOTE.

This version also eliminates the checking notes of `"for non-standard things in
the check directory"` caused by examples that download files from EnergyPlus
website.

## Resubmission

### Test environments

* local Manjaro Linux 18.0.4 install, R 3.6.0
* Windows (on appveyor-ci), R oldrel, release and devel
* Ubuntu 14.04 (on travis-ci), R oldrel, release and devel
* OS X (on travis-ci), R oldrel and release

### R CMD check results

0 errors | 0 warnings | 0 notes

### Reverse dependencies

eplusr currently has no reverse dependencies.
