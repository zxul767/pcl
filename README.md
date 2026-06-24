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
