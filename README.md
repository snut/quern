# Quern / Slime Game

This is a little rendering framework (`Quern`), and a game using it (thus far imaginatively called `slime`).

The rendering framework uses OpenGL 4.5 with bindless texture extensions to support rendering many meshes with very few draw calls. Whether or not this is necessary or practical is debatable.

The game is intended to be a simple turn-based tactics game. Right now there's not much game, due to still lacking a lot of essential things.

## Implementation notes

The game is implemented in Haskell. Currently it uses [Stack](https://docs.haskellstack.org/en/stable/README/) to resolve dependencies and wrap build tools.

The `Quern` library (found under the `src/` directory) provides a thin wrapper around [SDL 2](http://hackage.haskell.org/package/sdl2-2.4.1.0) and [OpenGL](https://hackage.haskell.org/package/gl-0.8.0), provides facilities for loading assets from files (meshes, shaders, textures, sounds), and some useful utilities for simple transform animation, rudimentary line-trace checks for selecting units, etc.

The `Slime` executable project (found under the `slime/` directory) uses `Quern` to render the WIP game. It handles board generation, configures the assets used to represent slimes, items and obstacles, mediates communication between the turn-based game thread and the realtime render/audio thread, and has some utilities for hex grids.


![A screenshot of the slime game](/images/screenshot.jpg)

## Building and running
To build the game with a working installation of `stack`:

```
$ stack build
```

To invoke the game after a successful build:

```
$ stack exec slime [-- OPTIONS]
```

The options flags are provided after a leading double-dash (disambiguating them from options to `stack` itself). If the binary is run without using `stack exec` they can be provided normally.

### Options

1. `-m` / `--msaa` : start the game with multi-sample antialiasing (2x)
2. `-f` / `--fullscreen` : start the game in fullscreen rather than windowed mode
3. `-a` / `--audio` : start the audio subsystem, this defaults to off as it is underdeveloped

### Debug keybinds in game
- `P` - toggle debug camera, click and drag mouse to look around, WASDQE to move
- `R` - randomise and reset test board
- `LMB` - select a unit (friendly or otherwise), or order a friendly unit to move to a hex/attack an enemy
- `RMB` - clone the selected unit into hovered adjacent space

![An animation showing a slime attacking](/images/attack_anim.gif)
