## Test environments
* local Windows 8.1, R-release
* Ubuntu 16.04 (on travis-ci), R-oldrel
* Ubuntu 16.04 (on travis-ci), R-release
* Ubuntu 16.04 (on travis-ci), R-devel
* Windows Server 2012 R2, R-release (AppVeyor CI)
* win-builder (devel and release), R 3.6.0
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit (rhub)
* Ubuntu Linux 16.04 LTS, R-release, GCC (rhub)
* Debian Linux, R-patched, GCC (rhub)
* Fedora Linux, R-devel, clang, gfortran (rhub)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release
* We kept a \dontrun{} block in the `rr_auth()` help file because we don't want the user to execute it as an example as it changes its environmental variables
