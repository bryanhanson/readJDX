### readJDX
#### Import data in the JCAMP-DX format.

[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)

Import data written in the JCAMP-DX format. This is an instrument-independent format used in the field of spectroscopy. Examples include IR, NMR, and Raman spectroscopy. See the vignette for background and supported formats.

`readJDX` is tested against a wide variety of JCAMP-DX formatted files provided by collaborators and available in various places on the internet. Details about supported formats, known issues and literature references are available in the vignette.

`readJDX` is distributed under the GPL-3 license, as stated in the DESCRIPTION file.  For more info, see the [GPL site.](https://gnu.org/licenses/gpl.html)

### To install from Github using R:

````r
install.packages("devtools")
library("devtools")
install_github(repo = "bryanhanson/readJDX@master")
library("readJDX")
````
If you use `@devel` you can get the development branch if it is available (and there may be other branches out there too).  Development branches have new, possibly incompletely tested features.  They may may also not be ready to pass checks and thus may not install automatically using the method above.  Check the news file to see what's up.

Questions?  hanson@depauw.edu
