# Personal macOS Development Setup (Mogan)

This document is for a local personal development workflow where the app in Applications always points to the latest local build.

## Goal

- Build from the dev worktree frequently.
- Launch from Finder like a normal app.
- Avoid repeated copy-to-Applications after every build.

## One-time Setup

### 1) Build the app once

From the repository root:

```bash
xmake build stem
```

This produces the app bundle at:

```text
build/macosx/arm64/release/MoganSTEM.app
```

### 2) Link Applications entry to your build output

```bash
rm -rf /Applications/MoganSTEM.app
ln -sfn /Users/anvar/repos/mogan-dev/build/macosx/arm64/release/MoganSTEM.app /Applications/MoganSTEM.app
```

After this, Finder shows MoganSTEM in Applications, but it points to your local build folder.

## Daily Workflow

### How to see code changes locally (normal case)

If your setup is already correct, this is all you need after any code change:

```bash
xmake build stem
```

Then quit and reopen MoganSTEM from Applications.

- You do not need to copy the app again.
- You do not need to recreate the symlink each time.

### Optional one-liner (build + open)

```bash
cd /Users/anvar/repos/mogan-dev && xmake build stem && open /Applications/MoganSTEM.app
```

Use this only for convenience.

### Launch the app

- Finder -> Applications -> MoganSTEM
- Or use Spotlight and search for MoganSTEM

## Verification

Check the symlink:

```bash
ls -ld /Applications/MoganSTEM.app
readlink /Applications/MoganSTEM.app
```

Expected target:

```text
/Users/anvar/repos/mogan-dev/build/macosx/arm64/release/MoganSTEM.app
```

## Fallback Steps (only if something is not working)

### 1) App bundle missing after clean

If you cleaned build artifacts, the symlink remains but target may be missing.

Fix:

```bash
xmake build stem
```

### 2) Recreate the Applications symlink

```bash
rm -rf /Applications/MoganSTEM.app
ln -sfn /Users/anvar/repos/mogan-dev/build/macosx/arm64/release/MoganSTEM.app /Applications/MoganSTEM.app
```

### 3) Refresh build configuration (if needed)

```bash
xmake config -vD --yes
xmake build stem
```

## Notes

- This setup is intended for personal local development only.
- If you want a standalone app copy (not linked to your build folder), copy the .app bundle into Applications instead of using a symlink.
