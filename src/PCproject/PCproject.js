import mskfilter from '@stdlib/array-mskfilter';
import strided2array from '@stdlib/array-base-from-strided';

export function getOverlapMasksImpl(plinkBimData, snpWeights) {
    let snpWeightMask = new Uint8Array(snpWeights.numSNPs);
    let plinkMask = new Uint8Array(plinkBimData.length);
    let flipMask = new Uint8Array(plinkBimData.length);
    let plinkIndex = 0;
    let removedStrandAmbiguous = 0;
    let removedInconsistent = 0;

    for (let i = 0; i < snpWeights.numSNPs; i++) {
        while (plinkBimData.chromosomes[plinkIndex] < snpWeights.chromosomes[i] ||
               (plinkBimData.chromosomes[plinkIndex] == snpWeights.chromosomes[i] &&
                plinkBimData.positions[plinkIndex] < snpWeights.positions[i])) {
            plinkIndex++;
        }
        if (plinkBimData.chromosomes[plinkIndex] === snpWeights.chromosomes[i] &&
            plinkBimData.positions[plinkIndex] === snpWeights.positions[i]) {
            const pa1 = plinkBimData.alleles1[plinkIndex];
            const pa2 = plinkBimData.alleles2[plinkIndex];
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
    return { snpWeightMask, plinkMask, flipMask, removedStrandAmbiguous, removedInconsistent };
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

export function projectImpl(overlapResult, snpWeights, genoVec) {
    const nrSnps = snpWeightOverlap.length;
    const genoVecOverlap = mskfilter(genoVec, plinkMask);

    const freqVecRaw = genoVecOverlap.map((g, i) => {
        if (flipMask[i])
            return 2 - g;
        else
            return g;
    });
    
    const nonMissingMask = genoVecOverlap.map(isNaN);
    
    // would like to use Float32Array, but stdlib's dgels expects Float64 for now.
    const freqVecNonMissing = Float64Array.from(mskfilter(freqVecRaw, nonMissingMask));
    const snpWeightsNonMissing = Float64Array.from(mskfilter(snpWeightOverlap, nonMissingMask));

}

