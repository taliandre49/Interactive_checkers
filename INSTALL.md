# Installation Instructions
This program requires the usage of OCaml with its standard library and additional package(s): `ANSITerminal`.
We are using OCaml compiler `ocaml-base-compiler.4.14.0`.

## Installing Packages
Currently our project requires 1 additional package: `ANSITerminal`.
If necessary, update OPAM before installing any new packages.

`$ opam update`

If it prompts you to upgrade any pre-existing packages then do so.

`$ opam upgrade`

If it tells you that everything is as up to date as possible then that is fine and you can proceed to the next steps. Next, install the necessary packages.

`$ opam install ANSITerminal`

If you already have a copy of the package it will tell you something like: `[NOTE] Package ANSITerminal is already installed (current version is 0.8.5).`. If you get this message, you do not need to worry about the installation as it already exists.

## Downloading Files
Identify the location that you'd like to store these files. For example, it could be within a file for your home directory for cs3110. Within the terminal go to that location with `cd`. For example, `$ cd ~/cs3110`.

Download the zip file containing the files and place it into the directory. Extract the zip file.
`$ unzip ms2_code.zip`

That will unzip the files and place them in a directory called  `md2_code`. Enter this directory.

`$ cd ms2_code`

To check if your OCaml environment is good, you can run `$ make check`.

## Running the Game
Ensure that you have built the project before you run it while you're in the project root directory.

`$ dune build`

To launch the game, simply use the makefile command `$ make play` to launch the user interface. From there, you can follow the prompts within the terminal to interact with the game.