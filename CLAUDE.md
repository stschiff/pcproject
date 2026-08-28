# pcproject

A browser-only tool for projecting user-uploaded PLINK genotype data onto a
precomputed PCA (principal component analysis) of reference populations, and
plotting the result. No backend — everything (PLINK parsing, SNP overlap
matching, projection math) runs client-side in the browser.

Deployed via GitHub Pages at http://www.stephanschiffels.de/pcproject/
(a project page under the user's custom domain, so all app-internal
references — script imports, `fetch()` calls — must be relative paths, never
absolute (`/app.js` resolves to the domain root, not `/pcproject/`)).

## Stack

- **PureScript** (Halogen) compiled to a single JS module via Spago/esbuild.
- **Spago multi-package workspace** (`spago.yaml` at repo root holds only the
  `workspace:`/package-set config; each real package lives under `packages/*`
  with its own `spago.yaml`). This is groundwork for eventually publishing
  pieces of this app as standalone npm packages (a `@popgen-toolbox` scope) —
  see git history around the `refactor/monorepo-packages` branch for the
  rationale.
- `docs/` (repo root, **not** under `packages/`) is both the GitHub Pages
  source folder and the `npm run serve` target — deliberately left outside
  the package split because GitHub Pages' "source folder" setting only
  supports repo-root or `/docs`, not arbitrary nested paths.
- Charts via `chartjs` / `chartjs-halogen` (Chart.js wrapped for Halogen).

## Commands

- `npm run build` — `spago bundle -p webapp --outfile ../../docs/app.js
  --bundle-type module` (the `--outfile` path is relative to the *selected
  package's* directory, `packages/webapp`, hence `../../`)
- `npm run serve` — `http-server docs -c -1` (serves the same folder GitHub
  Pages serves, so this is a reliable local preview of the deployed site)
- `npm test` — `spago test` (runs the (currently placeholder) test suite for
  every package in the workspace)
- `npm run repl` — `spago repl`
- `spago build` (no `-p`) builds all three workspace packages; `spago build
  -p <name>` / `spago test -p <name>` scope to one.

After any change under `packages/*/src`, run `npm run build` before `npm run
serve` — `docs/app.js` is a committed build artifact, not generated on the fly.

## Package layout (`packages/`)

Three Spago packages, each environment-agnostic unless noted. Folder names
under `packages/` stay short (`genotype-io`, `pca`); the `spago.yaml`
`package: name:` field for the two publishable library packages carries the
`popgen-` prefix instead (`popgen-genotype-io`, `popgen-pca`) — Pursuit has
no npm-style `@scope/name` mechanism, so the shared-namespace signal has to
live in the package name itself. `webapp` isn't published, so it keeps a
plain name. Use the prefixed name with `spago build -p`/`spago test -p` and
in other packages' `dependencies:` lists.

- **`genotype-io`** (published as `popgen-genotype-io`, npm package
  `@popgen-toolbox/genotype-io`) — single module `GenotypeIO` (+ `.js`):
  binary parsers for `.bed`/`.bim`/`.fam`. Pure functions over
  strings/`ArrayBuffer`s, no browser/DOM/network dependency — usable from
  Node or the browser alike.
- **`pca`** (published as `popgen-pca`, npm package `@popgen-toolbox/pca`) —
  depends on `popgen-genotype-io`. Single module `PCA` (+ `.js`): SNP-weights
  and reference-position parsing (per-SNP PC weights/frequencies, reference
  sample coordinates) plus the actual projection math — `getOverlapMasks`
  matches SNPs between user data and reference weights, handling strand
  ambiguity and allele flips; `projectSamples` projects genotypes onto the
  PCs, using `@rreusser/blapack` (LAPACK `dgels`) for the underlying
  least-squares solve. Also has no browser-specific dependency — this is the
  intended reusable "core."
  - Each npm package's own `package.json` has a `build` script:
    `spago bundle -p <spago-name> --module <ModuleName> --outfile dist/index.js
    --bundle-type module`. The `--module` flag is required here (unlike
    `webapp`'s build, which finds its `Main` entry point automatically) —
    without it, `spago bundle` doesn't scope to the selected package at all
    and can silently pull in unrelated or even stale compiled modules from
    elsewhere in the workspace's shared `output/` directory. If a module gets
    renamed, double check `--module` still matches exactly: on a
    case-insensitive filesystem (default on macOS) a stale differently-cased
    `output/` directory from before the rename can silently satisfy a
    now-wrong `--module` argument instead of failing to resolve.
- **`webapp`** — the Halogen UI (only package allowed to depend on
  `halogen`/`chartjs`/DOM). Depends on both `genotype-io` and `pca`.
  - `src/Main.purs` — entry point, mounts `App.Interface.component` into the
    page body.
  - `src/App/Interface.purs` — root Halogen component. Loads the reference
    PCA bundle on init (`LoadRefData`), holds uploaded user data, and
    triggers `RunProjection` whenever both are present. Owns the
    three-column layout (reference data box / projection monitor / user
    upload) and the two chart boxes below it.
  - `src/App/UserInputComponent.purs` — file upload widget (PLINK
    `.fam`/`.bim`/`.bed` triplet) and the "Load Example Data" button, which
    fetches a bundled example triplet from `docs/assets/` instead of
    requiring a user upload.
  - `src/App/RefChart.purs` / `RefChart.js` — scatter plot of reference
    population samples (Chart.js), grouped/colored by `popGroup`.
  - `src/App/ProjChart.purs` — scatter plot overlaying projected user
    samples (black) on top of a grayed-out reference layer; filters out
    samples with fewer than 20000 overlapping SNPs.
  - `src/App/Utils.purs` — `RemoteData e a` (`NotAsked | Loading | Failure e
    | Success a`), used throughout to drive loading/error UI state.

PureScript modules with FFI pair a `.purs` file with a same-named `.js` file
holding the JS implementation (binary parsing, typed-array math) — check the
`.js` file when a `.purs` file only has `foreign import` declarations. Module
names follow each package's own namespace (`GenotypeIO.*`, `Pca.*`) rather
than the old flat `PCproject.*` namespace from before the package split; the
app's own UI modules keep the `App.*` namespace since they aren't a published
library.

## Data flow

1. On load, `App.Interface` fetches the reference bundle from
   `docs/assets/`: SNP weights+frequencies (`.txt`), reference sample
   PCA coordinates (`.tsv`), and PCA parameters (`.json`, includes
   eigenvalues and default X/Y PC axes for plotting).
2. User either uploads a `.fam`/`.bim`/`.bed` triplet or clicks "Load Example
   Data" (fetches a bundled example triplet from `docs/assets/`).
3. Once both the reference bundle and user data are present,
   `RunProjection` overlaps SNPs, reduces weights to the overlap, projects
   genotypes onto PCs, and renders results in the projection chart.

## Known constraints / gotchas

- **Git LFS breaks GitHub Pages.** `docs/assets/*.bed` and `*.bim` are
  currently tracked by Git LFS (`.gitattributes`). GitHub Pages serves the
  *raw git blob*, not the LFS-resolved content — for LFS-tracked files that
  blob is just a small pointer stub (`version https://git-lfs.github.com/...`),
  so the deployed "Load Example Data" button fetches ~100 bytes of pointer
  text instead of the real multi-MB file and fails to parse. This is not a
  path bug — `git cat-file -p HEAD:docs/assets/<file>` on a clean checkout
  will show a pointer file if this is still true. Not yet fixed as of this
  writing; options are to de-LFS these files (commit real bytes, grows repo
  size) or host them externally with CORS enabled and fetch by absolute URL.
- Local working-tree files *are* the real, full-size data (LFS smudges them
  on checkout) — the mismatch only shows up in what's actually pushed/served,
  so `ls -la docs/assets` locally looks fine even when this bug is present.
- The example-data assets in `docs/assets/` are large (tens of MB); avoid
  reading them directly into context — use `ls -la`, `wc -l`, or targeted
  `grep`/`head` instead.
- `nextAnimationFrame` in `App/Interface.purs` is a deliberate hack: the
  actual projection math (`RunProjection`) runs synchronously on the JS
  thread, so two `requestAnimationFrame` round-trips are forced first to let
  Halogen flush the "Loading…" spinner to the DOM before the blocking
  computation starts. Fetch-based loading doesn't need this since `fetch` is
  naturally async and yields control on its own.
