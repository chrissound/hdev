# hdev

A convenient Haskell development tool that wraps `ghcid` so you don't have to remember the exact commands. It automatically finds executables from hpack's `package.yaml`.

## What problem does this solve?

I got really annoyed trying to remember exact commands specific to cabal or ghcid. This is mostly a convenience wrapper.

## Benefits

- **No more remembering ghcid commands** - just run `hdev --run` and it handles the rest
- **Automatically finds executables from `package.yaml`** - no need to specify paths
- **Automatically creates a ghcid output file** at `/tmp/<project>.ghcid`

## Usage

```bash
# List executables from package.yaml
hdev --list

# Run with ghcid (default)
hdev --run

# Run specific executable
hdev --run my-executable

# Lint mode
hdev --lint

# Execute with cabal run
hdev --exec-with-cmd my-executable -- --arg1 --arg2
```

## Example
```
nix-shell --run "hdev --lint"
Select an executable:
1) example
2) hdev
2
Raw command: ghcid --command "cabal v2-repl exe:hdev" -o /tmp2/hdev.ghcid
...
```

## Requirements

- GHC, cabal, ghcid, hpack

**Note:** Currently developed for executables only (libraries untested).
