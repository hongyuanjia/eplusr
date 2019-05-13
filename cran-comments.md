# Version 0.10.2

This version completely fixes checking errors of this package in a *strict
Latin-1* locale, as kindly stated by Kurt Hornik in email.

```
Dear maintainer,

Pls see
<https://cran.r-project.org/web/checks/check_results_eplusr.html>.

Your package fails its checks in a *strict Latin-1* locale, e.g. under
Linux using LC_CTYPE=en_US.

Please fix before 2019-05-19 to safely retain your package on CRAN.

Best
-k
```

## Round 1

### Test environments

* local Manjaro Linux 17.1.11.1 install, R 3.5.3
* Windows (on appveyor-ci), R oldrel, release and devel
* Ubuntu 14.04 (on travis-ci), R oldrel, release and devel
* OS X (on travis-ci), R oldrel and release

### R CMD check results

0 errors | 0 warnings | 1 notes

```
* checking CRAN incoming feasibility ... NOTE
   Maintainer: 'Hongyuan Jia <hongyuan.jia@bears-berkeley.sg>'

   Days since last update: 4
```

### Reverse dependencies

eplusr currently has no reverse dependencies.
