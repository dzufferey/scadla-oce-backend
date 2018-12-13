# Scadla Rendering Backend based on Open CASCADE

**WORK IN PROGRESS**

A rendering backend based on [Open CASCADE Community Edition](https://github.com/tpaviot/oce) and using the Open CASCADE Java wrapper from [jCAE](https://github.com/jeromerobert/jCAE) for [Scadla](https://github.com/dzufferey/scadla).

## Setup

1. Install `oce`: see https://github.com/tpaviot/oce
2. run the `jCAE_setup.sh` script
3. clone `scadla` (https://github.com/dzufferey/scadla) into a separate folder and run `sbt publishLocal`
4. run `sbt compile` in this folder

## OCE Specific Operations

Compared to [Scadla](https://github.com/dzufferey/scadla) which was mostly inspired by [OpenSCAD](http://www.openscad.org/), the supported operations are a bit different.

We get the basic Boolean operations: `Intersection`, `Union`, and `Difference`.

What we do *not* have is the `Minkowski` sum and convex `Hull`.

Then we have some new operations.
Open CASCADE is based on [B-rep](https://en.wikipedia.org/wiki/Boundary_representation) which decompose 3D objects along the topological features and each feature has an associated geometry.
Therefore, the operations are structured a bit differently.
A operation selects features and modify them is a particular way.

### Fillet

TODO ...

### Chamfer

TODO ...

### Offset

TODO ...

### Sweep

TODO could and should also be done directly in scadla ...

## Related Project

There are a few projects which are similar in their goals and approach.
The closest is [CadQuery](https://github.com/CadQuery/cadquery) and, then,[FeatureScript](https://cad.onshape.com/FsDoc/).

TODO ...
