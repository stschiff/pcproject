import dgels from '@rreusser/blapack/lapack/base/dgels';

function getOverlapMasksImpl(sampleBimData, snpWeights) {
    const snpWeightMask = new Uint8Array(snpWeights.snpIDs.length);
    const plinkMask = new Uint8Array(sampleBimData.snpIDs.length);
    const flipMask = new Uint8Array(sampleBimData.snpIDs.length);
    let plinkIndex = 0;
    let removedStrandAmbiguous = 0;
    let removedInconsistent = 0;
    let nrIncluded = 0;
    let nrToBeFlipped = 0;

    for (let i = 0; i < snpWeights.snpIDs.length; i++) {
        while (sampleBimData.chromosomes[plinkIndex] < snpWeights.chromosomes[i] ||
                (sampleBimData.chromosomes[plinkIndex] == snpWeights.chromosomes[i] &&
                sampleBimData.positions[plinkIndex] < snpWeights.positions[i])) {
            plinkIndex++;
        }
        if (sampleBimData.chromosomes[plinkIndex] === snpWeights.chromosomes[i] &&
            sampleBimData.positions[plinkIndex] === snpWeights.positions[i]) {
            const pa1 = String.fromCharCode(sampleBimData.alleles1[plinkIndex]);
            const pa2 = String.fromCharCode(sampleBimData.alleles2[plinkIndex]);
            const sa1 = String.fromCharCode(snpWeights.alleles1[i]);
            const sa2 = String.fromCharCode(snpWeights.alleles2[i]);
            if (!strandAmbiguous(sa1, sa2)) {
                if (isConsistent(sa1, pa1) && isConsistent(sa2, pa2) ||
                    isConsistent(sa1, complement(pa1)) && isConsistent(sa2, complement(pa2))) {
                    snpWeightMask[i] = 1;
                    plinkMask[plinkIndex] = 1;
                    nrIncluded++;
                } else if (isConsistent(sa1, pa2) && isConsistent(sa2, pa1) ||
                            isConsistent(sa1, complement(pa2)) && isConsistent(sa2, complement(pa1))) {
                    snpWeightMask[i] = 1;
                    plinkMask[plinkIndex] = 1;
                    flipMask[plinkIndex] = 1;
                    nrIncluded++;
                    nrToBeFlipped++;
                } else {
                    removedInconsistent++;
                }
            }
            else {
                removedStrandAmbiguous++;
            }
        }
        if(plinkIndex >= sampleBimData.snpIDs.length) {
            break;
        }
    }
    return { snpWeightMask, plinkMask, flipMask, removedStrandAmbiguous,
                removedInconsistent, nrIncluded, nrToBeFlipped };
}

function strandAmbiguous(a1, a2) {
    // Bug 1 fix: returns true when the pair IS ambiguous (A/T or C/G), false otherwise
    return (a1 + a2 === 'AT' ||
            a1 + a2 === 'TA' ||
            a1 + a2 === 'CG' ||
            a1 + a2 === 'GC');
}

function isConsistent(a1, a2) {
    return (  a1 === a2
//           || (a1 !== 'N' && a2 === 'N')
//           || (a1 === 'N' && a2 !== 'N')
            );
}

function complement(a) {
    switch (a) {
        case 'A':
            return 'T';
        case 'T':
            return 'A';
        case 'C':
            return 'G';
        case 'G':
            return 'C';
        default:
            return a; // Return the same character for non-ACGT bases
    }
}

function reducePcWeightsImpl(snpWeights, overlap) {
    if (snpWeights.snpIDs.length == overlap.nrIncluded) {
        return snpWeights; // no reduction needed
    } else {
        let reducedIndex = 0;
        const pcWeights = new Float32Array(overlap.nrIncluded * snpWeights.numPCs);
        const frequencies = new Float32Array(overlap.nrIncluded);
        const snpIDs = new Array(overlap.nrIncluded);
        const chromosomes = new Uint8Array(overlap.nrIncluded);
        const positions = new Uint32Array(overlap.nrIncluded);
        for(let i = 0; i < snpWeights.snpIDs.length; i++) {
            if(overlap.snpWeightMask[i]) {
                for(let j = 0; j < snpWeights.numPCs; j++)
                    pcWeights[reducedIndex * snpWeights.numPCs + j] = snpWeights.pcWeights[i * snpWeights.numPCs + j];
                frequencies[reducedIndex] = snpWeights.frequencies[i];
                chromosomes[reducedIndex] = snpWeights.chromosomes[i];
                positions[reducedIndex] = snpWeights.positions[i];
                snpIDs[reducedIndex] = snpWeights.snpIDs[i];
                reducedIndex++;
            }
        }
        return {pcWeights, frequencies, snpIDs, chromosomes, positions, numPCs: snpWeights.numPCs};
    }
}

function extractAndTransposeGenotypesImpl(plinkBedDat, numSNPs, numInds, overlap) {
    const newGenotypeMatrix = new Uint8Array(numInds * overlap.nrIncluded); // we transpose the output
    let reducedIndex = 0;
    for(let i = 0; i < numSNPs; i++) {
        if(overlap.plinkMask[i]) {
            for(let j = 0; j < numInds; j++) {
                const srcGeno = plinkBedDat[i * numInds + j];
                const targetGeno = overlap.flipMask[i] ? flip(srcGeno) : srcGeno;
                newGenotypeMatrix[j * overlap.nrIncluded + reducedIndex] = targetGeno; //transpose
            }
            reducedIndex++;
        }
    }
    return newGenotypeMatrix;
}

function flip(geno) {
    if(geno == 3) // missing
        return 3;
    else
        return 2 - geno;
}

function projectSamplesImpl(transposedGenotypeMatrix, pcWeights, frequencies,
                        numInds, numPCs, nScale, yScale, eigenValues) {
    let ret = [];
    const numSNPs = frequencies.length;
    const aBuf = new Float64Array(pcWeights.length);
    const bBuf = new Float64Array(numSNPs);
    for (let i = 0; i < numInds; i++) {
        let reducedIndex = 0;
        for (let j = 0; j < numSNPs; j++) {
            const g = transposedGenotypeMatrix[i * numSNPs + j];
            const f = frequencies[j];
            if (g !== 3) { // not missing
                const gRef = 2 - g; //all allele frequencies in smartPCA are for the reference allele
                const fRef = 1 - f;
                const centeredGenoRef = gRef - 2 * fRef;
                bBuf[reducedIndex] = centeredGenoRef / Math.sqrt(fRef * (1 - fRef));
                for (let k = 0; k < numPCs; k++) {
                    aBuf[reducedIndex * numPCs + k] =
                        pcWeights[j * numPCs + k] /
                            Math.sqrt(nScale * eigenValues[k] * yScale);
                }
                reducedIndex++;
            }
        }
        const nonMissing = reducedIndex;
        const M = nonMissing;
        dgels('row-major', 'no-transpose', M, numPCs, 1, aBuf, numPCs, bBuf, 1);
        ret.push({
            pcCoordinates: Array.from(
                bBuf.subarray(0, numPCs).map((x, k) =>
                    x / (yScale * eigenValues[k]))),
            nonMissingCount: nonMissing
        });
    }
    return ret;
}

export { getOverlapMasksImpl, reducePcWeightsImpl, extractAndTransposeGenotypesImpl, projectSamplesImpl };