# crater-db-reader

[![Built with Lisp](https://img.shields.io/badge/built%20with-Lisp-blueviolet)](https://lisp-lang.org)
[![License](https://img.shields.io/github/license/crater-space/db-reader.svg)](https://opensource.org/licenses/MIT)  
[![ko-fi](https://ko-fi.com/img/githubbutton_sm.svg)](https://ko-fi.com/Y8Y5E5GL7)

An adapter to read [Crater DB](https://github.com/crater-space/db) and generate shell scripts for appropriate actions

## Commands

1. `get_sources` generates a Bash script to determine the package sources (among the configured ones in the [DB](https://github.com/crater-space/db)) available on the current system
2. `search` generates a Bash script to search for the specified package across the available known sources
3. `list` generates a Bash script to list the installed packages across the available known sources
4. `install` generates a Bash script to install the specified packages using the most appropriate option from among the available known sources
5. `uninstall` generates a Bash script to uninstall the specified packages from the system
6. `update` generates a Bash script to update all (or specified) packages using the appropriate available known sources

## Executing

The DB reader script can be executed using [SBCL](https://www.sbcl.org), as shown below:

    sbcl --script ./main.lisp get_sources

More documentation coming soon...
