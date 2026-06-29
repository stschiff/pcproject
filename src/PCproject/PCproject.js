import BooleanArray from '@stdlib/array-bool';
import mskfilter from '@stdlib/array-mskfilter';
import strided2array from '@stdlib/array-base-from-strided';

export function getOverlapMasks(plinkData, snpWeights) {
    let snpWeightMask = new Uint8Array(snpWeights.numSNPs);
    let plinkMask = new Uint8Array(plinkData.numSNPs);
    let flipMask = new Uint8Array(plinkData.numSNPs);
    let plinkIndex = 0;
    let removedStrandAmbiguous = 0;
    let removedInconsistent = 0;

    for (let i = 0; i < snpWeights.numSNPs; i++) {
        while (plinkData.bimData.chromosomes[plinkIndex] < snpWeights.chromosomes[i] ||
               (plinkData.bimData.chromosomes[plinkIndex] == snpWeights.chromosomes[i] &&
                plinkData.bimData.positions[plinkIndex] < snpWeights.positions[i])) {
            plinkIndex++;
        }
        if (plinkData.bimData.chromosomes[plinkIndex] === snpWeights.chromosomes[i] &&
            plinkData.bimData.positions[plinkIndex] === snpWeights.positions[i]) {
            const pa1 = plinkData.bimData.alleles1[plinkIndex];
            const pa2 = plinkData.bimData.alleles2[plinkIndex];
            const sa1 = String.fromCharCode(snpWeights.alleles1[i]);
            const sa2 = String.fromCharCode(snpWeights.alleles2[i]);
            if (!strandAmbiguous(sa1, sa2)) {
                if (isConsistent(sa1, pa1) && isConsistent(sa2, pa2) ||
                    isConsistent(sa1, complement(pa1)) && isConsistent(sa2, complement(pa2))) {
                    snpWeightMask.set(1, i);
                    plinkMask.set(1, plinkIndex);
                } else if (isConsistent(sa1, pa2) && isConsistent(sa2, pa1) ||
                           isConsistent(sa1, complement(pa2)) && isConsistent(sa2, complement(pa1))) {
                    snpWeightMask.set(1, i);
                    plinkMask.set(1, plinkIndex);
                    flipMask.set(1, plinkIndex);
                } else {
                    removedInconsistent++;
                }
            }
            else {
                removedStrandAmbiguous++;
            }
        }
        if(plinkIndex >= plinkData.numSNPs) {
            break;
        }
    }
    const snpWeightOverlap = mskfilter(snpWeights, snpWeightMask);
    const flipMaskReduced = mskfilter(flipMask, plinkMask);
    return { snpWeightOverlap, plinkMask, flipMaskReduced, removedStrandAmbiguous, removedInconsistent };
}

function strandAmbiguous(a1, a2) {
    return !(a1 + a2 === 'AT' ||
           a1 + a2 === 'TA' ||
           a1 + a2 === 'CG' ||
           a1 + a2 === 'GC');
}

function isConsistent(a1, a2) {
    return (  a1 === a2
           || (a1 !== 'N' && a2 === 'N')
           || (a1 === 'N' && a2 !== 'N')
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

export function projectImpl(snpWeightOverlap, plinkMask, flipMask, genoVec) {
    const nrSnps = snpWeightOverlap.length;
    const genoVecOverlap = mskfilter(genoVec, plinkMask);

    // would like to use Float32Array, but stdlib's dgels expects Float64 for now.
    const freqVec = Float64Array.from(genoVecOverlap, (g, i) => {
        if (flipMask[i])
            return makeFreqFlipped(g);
        else
            return makeFreq(g);
    });

    for (let i = 0; i < nrSnps; i++) {
        if(plinkMask[i]) {
    }
    for (let i = 0; i < plinkData.numIndividuals; i++) {
        const individualGenotypes =
            strided2array(plinkData.numSnps, plinkData.bedData, plinkData.numSnps, i);
        const individualGenotypeOverlap = mskfilter(individualGenotypes, plinkMask);
    }
}

function flipFreq(g) {
    switch (g) {
        case 0:
            return 0.0;
        case 1:
            return 0.5;
        case 2:
            return 1.0;
}