# M1 Platform Split Log

2026-07-18

- Left `M1Combined` intact as the current working archive and transition source.
- Drafted `M1_PLATFORM_MANAGEMENT.md` as the platform management plan.
- Defined target model: root platform repo plus one repo per SW/FW component.
- Created local staging workspace: `/home/lenel/myGitHub/m1-platform-work`.
- Imported local component repos under `/home/lenel/myGitHub/m1-platform-work/components`.
- Imported components: `m1tfc`, `m1-fixture-teensy-fw`, `mercury-testboard-fw`, `stm32mp1-baremetal`, `m1-rest-server`, `m1-operator-ui`, `tfcroncli`, `m1-cloud-client`.
- Excluded generated folders during import: `.git`, `node_modules`, `dist`, `build`.
- Branch policy set locally: new component repos use `main`. Existing `stm32mp1-baremetal` repo may be used, but do not touch Makefiles or C/C++ source files there; only platform docs/metadata/integration files should be changed unless explicitly approved.
- GitHub remotes were not created yet. `gh` is now installed and authenticated as `lbuchman`.
- Next step: create private GitHub repos, add remotes to local component repos, push each component, then create the root `m1-platform` repo with `repos.json`, scripts, docs, and manifests.
