# cmple - A minimal project and package manager for C

> ⚠️ This is a work in progress.

## What is `cmple`?

`cmple` is a project and package managed for C, and it is designed
to provide an easy and uniform structure for C projects that enables
easy usage of other C projects.

## Why `cmple`?

C projects are notorious for being difficult to handle and using libraries written
in C is such a fragile experience, that most C developers just avoid it and write
the code themselves. `cmple` aims to provide an interface similar to that of
`cargo` for Rust, `npm` for JavaScript, `pip` for Python, etc.

## How does `cmple` work?

First, let's create a library project, that we are next going to publish to `cmple`, and show how another user can use it in their own project.

```bash
cmple new greet --libstatic
```

This will create a new project called `greet` in the the subdirectory `greet`. The `--libstatic` flag tells `cmple` to create a static library. Other options are `--libshared` and `--binary`.

Let's see what `cmple` created.

```bash
$ cd greet
$ tree .
.
├── build.toml
├── CMakeLists.txt
├── header
│   ├── private
│   └── public
└── source
```

The `build.toml` file contains the build configuration for the project. The `CMakeLists.txt` file is the main CMake file that is used to build the project. The `header` directory contains the header files for the project. The `source` directory contains the source files for the project.

```bash
$ cat build.toml
user = ""
project = "greet"
type = "library static"
dependencies = []
```

We see that the user field is empty. If we don't plan on publishing the project, it's ok to leave it as is. But, since we are going to publish it, let's give us a name.

```bash
$ cat build.toml
user = "cmple"
...
```

We are then going to create the file `header/public/greet.h` and add the following code to it.

```c
#ifndef GREET_H
#define GREET_H

void greet(const char *name);

#endif // GREET_H
```

Notice how we put this file inside the `public` header subdirectory. We did that because we want to make this header file available to other projects that use our library. The `private` header subdirectory is for header files that are only used internally by the library.

Next, we are going to create the file `source/greet.c` and add the following code to it.

```c
#include "public/greet.h"
#include <stdio.h>

void greet(const char *name) {
    printf("Hello, %s!\n", name);
}
```

Now, let's build our library

```bash
cmple build
```

This will build our library and place the output in the `target` directory.
Notice that we didn't have to do anything after adding new files to our project. `cmple` automatically detects new files and adds them to the build.
Also notice how `cmple` generate the `CMakeLists.txt` file for us, and calls `cmake` with the option to generate a `compile_commands.json` file by default. This ensures that we can open this project with any editor such as `VSCode`, `CLion` or `Vim` and get full support for code completion and navigation.

Now, let's publish our library to `cmple`.

```bash
cmple publish
```

This will publish our library to `cmple` and make it available for other users to use. Currently, this only publishes the `build.toml`, `CMakeLists.txt`, `header` and `source` directories to `cmple`. In the future, I plan to generate a default `.gitignore` file with `cmple new` and publish everything except the files and directories that are listed in the `.gitignore` file.

> ⚠️ At the moment, `cmple` just takes the username and the project name and uploads them without any verification. This means that anyone can publish a project with the same name as yours, and they will overwrite your project. The first thing I plan to do is add a simple signup and login system to `cmple` so that users can claim their username and publish their projects under their username.

Now, let's create a new project that uses our library.

```bash
cmple new hello --binary
```

The file structure of a binary project is the same as that of a library project. Now, let's add the dependency to our library.

```bash
$ cat build.toml
user = ""
project = "hello"
type = "binary"
dependencies = ["greet"]
```

and let's write a simple program that uses our library. Create the file `source/main.c` and add the following code to it.

```c
#include "greet/greet.h"

int main() {
    greet("World");
    return 0;
}
```

Now, let's run our project.

```bash
$ cmple run
...
Hello, World!
```

This command, automatically downloaded the dependency, and placed it in the `depends` directory. It then built the project and ran it. Notice how we included the header file as `greet/greet.h`. Generally, when you download a dependecy, you can use its header files by including them as `<dependency-name>/<header-file-name>`.

## Future work

- The first and most important thing is adding a login and signup system to `cmple`, so each user can upload their projects under their username securely.
- A versioning system for the projects would be nice. Currently, if you publish a new version of your project, it will overwrite the old version. This is not ideal.
- A convenient way to search for packages is very importat. I first plan to add a simple search command to `cmple` that will search for packages on `cmple` and print the results. Then, I plan to add a web interface for `cmple` that will allow users to search for packages and view their details.

## Installation

### Dependencies

- [Git](https://git-scm.com/) to downloading the source code
- [The Haskell Platform](https://www.haskell.org/platform/) to build `cmple`
- [CMake](https://cmake.org/) to use `cmple`

```bash
git clone https://github.com/nikos-alexandris/cmple.git
cd cmple
stack install
```
