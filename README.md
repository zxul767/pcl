# Practical Common Lisp
![status](https://github.com/zxul767/pcl/actions/workflows/build.yml/badge.svg)

Examples and experiments from the [Practical Common Lisp](http://www.gigamonkeys.com/book/) book.

<img src="https://user-images.githubusercontent.com/442314/107993544-49e30f80-6fa0-11eb-9ee5-537a3ff1b7ec.jpg" width="512" />

## Project checks

Run the same compilation and test checks used by CI:

```sh
src/check.sh
```

This "quickloads" `mp3-browser` and all its dependencies (including subsystems in the same project and third-party libraries), compiling and loading its local dependency graph, and then runs the test suites.

The check runner discovers local primary systems from their `.asd` filenames,
following ASDF's convention that a primary system has the same name as its
definition file. For example, `src/prelude/prelude.asd` defines the `prelude`
system. Secondary systems such as `functools/tests` are exercised through the
primary system's `test-op`.

Routine compiler output and warning details are suppressed by default and the
warnings are written to `warnings.log`. To print the full output instead:

```sh
CHECK_VERBOSE=1 src/check.sh
```
