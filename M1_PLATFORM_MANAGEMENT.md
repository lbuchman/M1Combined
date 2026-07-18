# M1Combined Platform Management Guide

## Purpose

This document is about managing the full M1Combined SW/FW platform.

The goal is to turn a difficult-to-manage combined development tree into a controlled platform structure with:

- one root repository for system documentation and management
- one repository per SW/FW component
- clear links from the root repo to each component repo
- PC setup guides, scripts, and operating instructions in one place
- release manifests that define which component versions belong together
- placeholders for future CAD, electrical, mechanical, and assembly drawings

## Current Problem

`M1Combined` is not one project. It is a complete embedded manufacturing test platform made from many related components.

The current single-tree structure helped during creation because everything was under one roof. Now it creates management pain:

- hard to know which component versions belong together
- hard to clone only one component
- hard to reproduce a full production fixture state
- hard to separate fixture PC setup from component source code
- hard to distinguish active release files from experiments and history
- hard to preserve system-level knowledge outside one person
- future CAD/drawing control has no defined home

The main issue is not internal refactoring inside one component. The main issue is managing the whole platform.

## Platform Components

| Component | Current Folder | Role |
| --- | --- | --- |
| M1TFC | `m1tfc` | Fixture CLI, ICT, functional tests, hardware orchestration |
| M1 fixture firmware | `M1TestFixtureTeensy41` | Teensy firmware for M1 fixture boards |
| Mercury test board firmware | `TestBoardMercury` | Teensy firmware for Mercury test board |
| STM32MP1 bare metal | `stm32mp1-baremetal` | STM32MP1 bare-metal firmware/work |
| REST server | `restServer` | REST API around fixture commands and status |
| Operator UI | `gui-react` | React production/debug operator interface |
| Fixture PC cloud client | `tfcroncli` | Fixture PC cloud communication, logs, secrets, nightly FW/SW updates |
| General cloud client | `m1CloudClient` | General Ubuntu cloud log/update client |
| PC setup assets | root/scripts/docs | Setup guide, installation scripts, operating instructions |
| Future drawings | future controlled location | CAD, electrical, mechanical, assembly, exported PDFs, release packages |

## Proposed Model

Create a private root GitHub repository, for example:

```text
m1-platform
```

The root repo is the platform control point. It owns documentation, setup guides, scripts, manifests, and links to component repositories.

Each SW/FW component lives in its own repository.

This gives two workflows:

- If only one component is needed, clone that component repo.
- If the full system is needed, clone the root repo and pull all linked components.

## Root Repository Layout

```text
m1-platform/
  README.md
  ARCHITECTURE.md
  COMPONENTS.md
  RELEASE_PROCESS.md
  VERSION_POLICY.md
  repos.json

  docs/
    pc-setup-guide.md
    fixture-setup.md
    networking.md
    calibration-config.md
    cloud-update.md
    troubleshooting.md
    known-issues.md
    drawings-placeholder.md

  scripts/
    clone-all.sh
    status-all.sh
    update-all.sh
    setup-fixture-pc.sh
    verify-platform.sh
    manifest-status.sh

  manifests/
    current-production.json
    mnp-s5-2026.07.18.json

  components/
    m1tfc/
    m1-fixture-teensy-fw/
    mercury-testboard-fw/
    stm32mp1-baremetal/
    m1-rest-server/
    m1-operator-ui/
    tfcroncli/
    m1-cloud-client/
```

The `components/` entries can be Git submodules or cloned by scripts from `repos.json`.

## Component Repositories

Recommended component repositories:

| Repo | Source Folder | Purpose |
| --- | --- | --- |
| `m1tfc` | `m1tfc` | Manufacturing test fixture control software |
| `m1-fixture-teensy-fw` | `M1TestFixtureTeensy41` | Fixture Teensy firmware |
| `mercury-testboard-fw` | `TestBoardMercury` | Mercury test board firmware |
| `stm32mp1-baremetal` | `stm32mp1-baremetal` | STM32MP1 bare-metal work |
| `m1-rest-server` | `restServer` | Fixture REST API |
| `m1-operator-ui` | `gui-react` | React operator/debug UI |
| `tfcroncli` | `tfcroncli` | Fixture PC cloud/update/log client |
| `m1-cloud-client` | `m1CloudClient` | General Ubuntu cloud/update client |

Each component repo should own its own source code, build instructions, tests, and component-level history.

The root repo should not contain active source code for these components. It should contain links, documentation, scripts, and release manifests.

## Component Link Strategy

There are two useful ways to link component repos from the root.

### Option A: Git Submodules

Use submodules when exact reproducible checkout matters.

```bash
git clone --recurse-submodules git@github.com:OWNER/m1-platform.git
```

Advantages:

- root repo records exact component commits
- good release traceability
- full platform checkout can be reproduced

Disadvantages:

- submodules add workflow friction
- component changes must be committed in the component repo, then the root repo must update the submodule pointer

### Option B: `repos.json` Plus Scripts

Use a repo registry and scripts for easier daily management.

Example `repos.json`:

```json
{
  "components": {
    "m1tfc": "git@github.com:OWNER/m1tfc.git",
    "m1-fixture-teensy-fw": "git@github.com:OWNER/m1-fixture-teensy-fw.git",
    "mercury-testboard-fw": "git@github.com:OWNER/mercury-testboard-fw.git",
    "stm32mp1-baremetal": "git@github.com:OWNER/stm32mp1-baremetal.git",
    "m1-rest-server": "git@github.com:OWNER/m1-rest-server.git",
    "m1-operator-ui": "git@github.com:OWNER/m1-operator-ui.git",
    "tfcroncli": "git@github.com:OWNER/tfcroncli.git",
    "m1-cloud-client": "git@github.com:OWNER/m1-cloud-client.git"
  }
}
```

Useful scripts:

```bash
./scripts/clone-all.sh
./scripts/status-all.sh
./scripts/update-all.sh
./scripts/manifest-status.sh
```

Advantages:

- easier day-to-day work
- simple to clone/update all repos
- less submodule confusion

Disadvantages:

- exact release state must be captured by manifest files
- root checkout alone does not automatically pin component commits

## Recommendation

Start with `repos.json` plus scripts. Add submodules later only where exact pinned checkout is useful.

Always use release manifests either way.

This avoids getting blocked by submodule workflow while still creating a path toward reproducible platform releases.

## Platform Scripts

The root repo should contain platform-level scripts. Component-specific build scripts should stay inside component repos.

Minimum useful platform scripts:

```text
clone-all.sh          -> clone missing component repos into components/
status-all.sh         -> show branch, commit, and dirty state for every component
update-all.sh         -> pull/update all component repos
setup-fixture-pc.sh   -> install platform prerequisites on a fixture PC
verify-platform.sh    -> check required files, tools, versions, and services
manifest-status.sh    -> compare checked-out commits against a release manifest
```

The most valuable script is `status-all.sh`, because managing many repositories becomes painful when platform state is not visible.

## PC Setup Guide

The root repo should include the PC setup guide because fixture PC setup spans multiple components.

The guide should cover:

- Ubuntu version
- required packages
- Node version
- snap setup
- serial/USB permissions
- network settings
- fixture IP assumptions
- cloud credential provisioning
- config file location
- calibration file location
- log locations
- update schedule
- service start/stop instructions
- recovery steps for a broken fixture PC

This turns setup from personal memory into a repeatable platform process.

## Release Manifest

A platform release should be defined by exact component versions, not by branch names.

Example:

```json
{
  "release": "mnp-s5-2026.07.18",
  "fixturePc": {
    "m1tfc": "commit-hash",
    "m1-rest-server": "commit-hash",
    "m1-operator-ui": "commit-hash",
    "tfcroncli": "commit-hash"
  },
  "fixtureFirmware": {
    "m1-fixture-teensy-fw": "commit-hash",
    "mercury-testboard-fw": "commit-hash"
  },
  "targetFirmware": {
    "stm32mp1-baremetal": "commit-hash"
  },
  "supportTools": {
    "m1-cloud-client": "commit-hash"
  },
  "drawings": {
    "package": "future-drawing-package-id"
  }
}
```

This answers:

- what version was installed on the fixture PC
- what firmware was flashed into each board
- what support tools matched the release
- what drawing package belonged to the fixture
- how to reconstruct the system later

There should also be a simple pointer to the active production release:

```text
manifests/current-production.json
```

## CAD and Drawing Placeholder

CAD and drawing control can come later, but the root repo should reserve a place for it now.

Suggested placeholder:

```text
docs/drawings-placeholder.md
```

Future drawing package structure:

```text
drawings/
  fixture-mechanical/
  pcb-assembly/
  wiring/
  exported-pdf/
  release-packages/
```

Eventually, drawing packages should be referenced by release manifests just like firmware and software commits.

## Migration Path

1. Create private root repo: `m1-platform`.
2. Add root documentation, `repos.json`, PC setup guide, and basic scripts.
3. Create private component repos.
4. Move each current component folder into its own repo, preserving history if practical.
5. Use `clone-all.sh` to populate `components/` from `repos.json`.
6. Create the first real platform release manifest.
7. Add submodules later if exact pinned checkout becomes useful.
8. Keep current `M1Combined` as a historical archive until the new structure is trusted.

## First Practical Step

Do not split everything first. Start by creating the root repo and management layer.

Minimum first commit in `m1-platform`:

```text
README.md
ARCHITECTURE.md
COMPONENTS.md
docs/pc-setup-guide.md
repos.json
scripts/clone-all.sh
scripts/status-all.sh
manifests/current-production.json
```

After that, component repos can be split one by one without losing the system map.

## Final Model

```text
root platform repo
  documentation
  PC setup guide
  platform scripts
  release manifests
  component links
  future drawing placeholders

component repos
  source code
  firmware
  build instructions
  tests
  component history
```

This structure matches the real system: one platform made from multiple independent SW/FW components, with one root place to understand, clone, set up, release, and recover the whole thing.
