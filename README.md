# Practical Common Lisp
![status](https://github.com/zxul767/pcl/actions/workflows/build.yml/badge.svg)

Examples and experiments from the [Practical Common Lisp](http://www.gigamonkeys.com/book/) book.

<img src="https://user-images.githubusercontent.com/442314/107993544-49e30f80-6fa0-11eb-9ee5-537a3ff1b7ec.jpg" width="512" />

## Compilation, Loading and Testing
You can run the same compilation and test checks used in CI with:

```sh
src/check.sh
```

This check ensures that all `.asd` systems are compiled, loaded, and tested.
This "quickloads" the main project (`mp3-browser`) and all its dependencies (including subsystems in the same project and third-party libraries), compiling and loading its local dependency graph, and then runs their corresponding test suites.

### Warnings
Routine compiler output and warning details are suppressed by default in the terminal, but the latter are written to `warnings.log`. These are the warnings that are not shown on the terminal at lower verbosity levels. However, style warnings are treated specially: they are counted during project compilation and cause the check to fail, even when the compiler output itself is redirected to the log. 

Test failures and their output are handled separately and are written to `test-output.log` when they are not printed directly.

### Verbosity levels:

- `CHECK_VERBOSE=1` shows the high-level check steps and the systems being
compiled and loaded.
- `CHECK_VERBOSE=2` also shows the test suites being run.
- `CHECK_VERBOSE=3` prints all compiler, loader, and test output directly.

To print the full output instead:

```sh
CHECK_VERBOSE=3 src/check.sh
```
