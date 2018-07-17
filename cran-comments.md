# Version 0.8.1

## Round 1
### Test environments
* local Windows 10 install, R 3.5.0
* Windows (on appveyor-ci), R oldrel, patched, stable, and release
* ubuntu 14.04 (on travis-ci), R oldrel, release, devel
* OS X (on travis-ci), R oldrel and release

### R CMD check results
0 errors | 0 warnings | 1 notes

There is one note which indicates this is the first submission:

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Hongyuan Jia <hongyuanjia@cqu.edu.cn>'
```

### Reverse dependencies
eplusr currently has no reverse dependencies.

### Reviewer comments
2018-07-17 Uwe Ligges:

```
   Possibly mis-spelled words in DESCRIPTION:
     EnergyPlus (2:28, 10:60, 11:66)

   The Description field should not start with the package name,
     'This package' or similar.

Also, file LCIENSE should  be omitted, if it is just a copy of the GPL-3.

Please fix and resubmit.
```

## Round 2
### Submission comments
2018-07-17

Addressed all privious comments.
