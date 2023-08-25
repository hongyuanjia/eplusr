# Version 0.16.2

## Re-submission

### Test environments

* Windows 10 22H2 (on local PC), R release
* Windows Server 2022 (on GitHub Actions), R release
* Ubuntu 22.04 (on GitHub Actions), R devel
* macOS 12.6.5 (on GitHub Actions), R release

### R CMD check results

In this submission, I explicitly call `data.table::setDTthreads(2)` on package
load on CRAN machines.

0 errors | 0 warnings | 0 note

### Reverse dependencies

0 errors | 0 warnings | 0 notes
