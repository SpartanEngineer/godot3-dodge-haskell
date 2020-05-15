Make sure to use `git clone --recurse-submodules` to clone this repo. (so you can get art assets from original tutorial).

# YourFirstGame in Haskell

Example of converting the YourFirstGame godot tutorial to Haskell with godot-haskell library.

You can find the original tutorial here: https://docs.godotengine.org/en/3.2/getting_started/step_by_step/your_first_game.html

# Quick Start

1. Build the Haskell library
`stack build`

2. Copy the Haskell library to the game folder (change .so to .dynlib in Mac or .dll in Windows)
`cp $(shell stack path --local-install-root)/lib/libgodot3-dodge-haskell.so $(shell stack path --project-root)/game/lib`

3. Copy over art assets from the original tutorial repo
`cp -rf Godot3_dodge/art game/art`

4. Get a copy of Godot engine from Godot's website: https://godotengine.org/. This was tested with Godot version 3.2.1

5. Add and make sure the generated .so/.dynlib/.dll is loaded correctly in Godot project. NOTE Godot may remove the signals / baseclasses of the GDNativeScript classes in project.godot during this step.  Make sure to add them back or it won't work.

6. Run with Godot
`Godot --path ./game`

* NOTE Only tested in Linux so far.

# Structure
- Haskell source is in src. 
  - src/Lib.hs contains the Godot class exports.
  - src/Util.hs contains Godot utility functions.
  - files in src/NSClass contain the different NativeScript classes implemented in Haskell.
- Godot files and assets are in game.


