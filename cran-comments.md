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

# Version 0.8.2

## Round 1
### Reviewer comments
2018-07-22 Swetlana Herbrandt

```
Thanks, please omit the redundant "in R" in your title.

Please write package names, software names and API names in single 
quotes (e.g. 'EnergyPlus') in Title and Description.

Please fix and resubmit.

Best,
Swetlana Herbrandt
```

# Version 0.8.3
## Round 1
### Test environments
* local Windows 10 install, R 3.5.0
* local Manjaro Linux 17.1.11.1 install, R 3.5.1
* Windows (on appveyor-ci), R oldrel, patched, stable, and release
* Ubuntu 14.04 (on travis-ci), R oldrel, release, devel
* OS X (on travis-ci), R oldrel and release

### R CMD check results
0 errors | 0 warnings | 0 notes

### Reverse dependencies
eplusr currently has no reverse dependencies.

### Reviewer comments
2018-08-07 Uwe Ligges

```
This hangs on Windows.

Please verify that this works on Windows before submitting to CRAN, or 
if not possible, declare an OS_type.

Please fix and resubmit.

Best,
Uwe Ligges
```

### Submission comments
2018-08-07

Fix the function that install EnergyPlus which hangs on the tests when there is
no powershell installed on Windows machine.

# Version 0.8.4

## Round 1

### Test environments

* local Manjaro Linux 17.1.11.1 install, R 3.5.1
* Windows (on appveyor-ci), R oldrel, release and devel
* Ubuntu 14.04 (on travis-ci), R oldrel, release and devel
* OS X (on travis-ci), R oldrel and release

### CRAN comments

2018-08-08

The package was archived by CRAN due to leaving 'EnergyPlus' installer file and
installation in home.

### Submission comments

2018-08-09

Skip tests which download and install 'EnergyPlus' in home folder on CRAN and
only run those tests locally.
