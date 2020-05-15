Make sure to use `git clone --recurse-submodules` to clone this repo. (so you can get art assets from original tutorial).

# YourFirstGame in Haskell

Example of converting the YourFirstGame godot tutorial to Haskell with godot-haskell library.

# Quick Start

1. Build the Haskell library
`stack build`

2. Copy the Haskell library to the game folder (change .so to .dynlib in Mac or .dll in Windows)
`cp $(shell stack path --local-install-root)/lib/libgodot3-dodge-haskell.so $(shell stack path --project-root)/game/lib`

3. Copy over art assets from the original tutorial repo
`cp -rf Godot3_dodge/art game/art`

4. Get a copy of Godot engine from Godot's website: https://godotengine.org/. This was tested with Godot version 3.2.1

5. Run with Godot
`Godot --path ./game`

* NOTE you'll need to add and make sure .dynlib or .dll is loaded correctly in Godot project if not running on Windows. Only tested on Linux so far.


