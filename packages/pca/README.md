# @popgen-toolbox/pca

Project PLINK genotype data onto a precomputed PCA of reference
populations, and load hosted reference panels to project against. Built on
[@popgen-toolbox/genotype-io](https://www.npmjs.com/package/@popgen-toolbox/genotype-io).

Compiled from PureScript ([source](https://github.com/stschiff/pcproject/tree/main/packages/pca));
full generated API reference is on [Pursuit](https://pursuit.purescript.org)
once published there.

## Install

```bash
npm install @popgen-toolbox/pca
```

## Loading a reference panel

```js
import { panels, loadReferenceBundle } from "@popgen-toolbox/pca";

console.log(Object.keys(panels)); // e.g. ["WestEurasia_HiRes"]

const ref = await loadReferenceBundle(panels["WestEurasia_HiRes"]);
// ref = { snpWeights, refPosData, pcaParams }
```

`loadReferenceBundle` returns a real `Promise` directly, awaitable as
normal - no extra `()` needed. A failed load rejects the promise like any
other JS async call, so wrap it in `try`/`catch` (or `.catch()`) if you want
to handle that case instead of letting it propagate.

## Full pipeline: projecting your own data

```js
import { readFamData, readBimData, readBedData } from "@popgen-toolbox/genotype-io";
import {
  panels, loadReferenceBundle,
  getOverlapMasks, reducePcWeights, extractAndTransposeGenotypes, projectSamples
} from "@popgen-toolbox/pca";

const fam = readFamData(famText);
const bim = readBimData(bimText);
const numSNPs = bim.snpIDs.length;
const numInds = fam.indNames.length;
const bed = readBedData(bedBuffer, numSNPs, numInds);

const ref = await loadReferenceBundle(panels["WestEurasia_HiRes"]);

const overlap = getOverlapMasks(bim, ref.snpWeights);
const reduced = reducePcWeights(ref.snpWeights, overlap);
const genotypes = extractAndTransposeGenotypes(bed, numSNPs, numInds, overlap);
const projected = projectSamples(genotypes, reduced.pcWeights, reduced.frequencies, numInds, reduced.numPCs, ref.pcaParams);

// projected[i] = { pcCoordinates: number[], nonMissingCount: number }
// corresponds to fam.indNames[i] / fam.popNames[i]
```

Every function above is a plain, directly-callable JS function - no
currying, no trailing `()` (except `await` on the one Promise-returning
call). Verified end-to-end against the published `WestEurasia_HiRes` panel
and a 35-sample test dataset: 413,151 overlapping SNPs, 10 PCs, all 35
samples projected.

## License

MIT
