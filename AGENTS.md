# AGENTS.md

## Overview

Personal xmonad configuration. Entrypoint is `xmonad.hs`, custom modules live in `lib/`. Uses `cabal` with a package environment (not stack). `xmonad` and `xmonad-contrib` are git submodules.

## Structure

- `xmonad.hs` — main config, wires together modules from `lib/`
- `lib/` — custom Haskell modules:
  - `ShortCuts.hs` — keybindings (mod4/super key), workspaces 1-9,0,-,=
  - `Layout.hs` — layouts, scratchpads, manageHook rules
  - `StatusBar.hs` — polybar/lemonbar log hook via DBus
  - `Colors.hs` — Gruvbox color scheme constants
  - `Prompt.hs` — XMonad prompt configs (normal, warn, danger)
  - `Projects.hs` — dynamic project definitions (currently not wired into `xmonad.hs`)
- `xmonad/`, `xmonad-contrib/` — upstream git submodules
- Shell scripts at root — dmenu helpers, screen switching, wallpaper, bookmarks

## Commands

```sh
# Install dependencies into package environment
bash install.sh

# Compile and restart (run from within X session)
xmonad --recompile && xmonad --restart
```

## Key conventions

- Modules expose a `modify` function that transforms `XConfig` — these chain in `xmonad.hs`
- `Colors` module is imported by most other modules; changing color names is a cross-cutting change
- Screen order mapping (`wer = [1,0,2]` in `ShortCuts.hs:160`) is hardware-specific
- StatusBar formatting uses polybar/lemonbar `%{...}` syntax, not plain text

## Dependencies

Key packages beyond xmonad/xmonad-contrib: `xmonad-dbus`, `dbus`, `utf8-string`

## Gotchas

- `cabal.project` sets `hs-source-dirs: lib` — this is non-standard; Haskell modules in `lib/` are found via package environment, not a cabal file in this directory
- Build artifacts (`.hi`, `.o`) in `lib/` are gitignored but present on disk
- The compiled binary `xmonad-x86_64-linux` is gitignored
- `Projects.hs` defines a `modify` function but it is **not** used in `xmonad.hs` currently
