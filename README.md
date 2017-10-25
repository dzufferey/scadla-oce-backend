# Scadla Rendering Backend based on Open CASCADE

**WORK IN PROGRESS**

A rendering backend based on [Open CASCADE Community Edition](https://github.com/tpaviot/oce) and using the Open CASCADE Java wrapper from [jCAE](https://github.com/jeromerobert/jCAE) for [Scadla](https://github.com/dzufferey/scadla).

## Setup

1. Install `oce`: see https://github.com/tpaviot/oce
2. run the `jCAE_setup.sh` script
3. clone `scadla` (https://github.com/dzufferey/scadla) into a separate folder and run `sbt publishLocal`
4. run `sbt compile` in this folder
