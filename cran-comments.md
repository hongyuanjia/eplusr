# Version 0.9.1

## Round 1

### Test environments

* local Manjaro Linux 17.1.11.1 install, R 3.5.1
* Windows (on appveyor-ci), R oldrel, release and devel
* Ubuntu 14.04 (on travis-ci), R oldrel, release and devel
* OS X (on travis-ci), R oldrel and release

### R CMD check results

0 errors | 0 warnings | 1 notes

```
Days since last update: 2
```

### Reverse dependencies

eplusr currently has no reverse dependencies.

### Submission comments

There was a problem generating the vignettes because that EnergyPlus only
has releases for Windows, macOS and Linux, but not for Solaris platform. Also,
there is a typo that causes EnergyPlus installation failed on macOS.

I modified the vignette to skip some chunks that call EnergyPlus on Solaris
platform but keep all chunks that show the model modification functionalities,
and also fixed the installation on macOS.
