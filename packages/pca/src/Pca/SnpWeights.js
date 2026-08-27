export function readSnpWeights(snpWeightText) {
    const lines = snpWeightText.trim().split('\n');
    const numSNPs = lines.length;
    let snpIDs = new Array(numSNPs);
    const chromosomes = new Uint8Array(numSNPs);
    const positions = new Uint32Array(numSNPs);
    const alleles1 = new Uint8Array(numSNPs);
    const alleles2 = new Uint8Array(numSNPs);
    const firstLineFields = lines[0].trim().split(/\s+/);
    if (firstLineFields.length < 7) {
        throw new Error(`For SnpWeights expected at least 7 columns per line (snpIDs, chrom, pos, allele1, allele2, and at least one PC and one frequency), but found ${firstLineFields.length} in the first line.`);
    }
    const numPCs = firstLineFields.length - 6;
    const pcWeights = new Float32Array(numSNPs * numPCs);
    const frequencies = new Float32Array(numSNPs);
    for (let i = 0; i < numSNPs; i++) {
        const fields = lines[i].trim().split(/\s+/);
        snpIDs[i] = fields[0];
        chromosomes[i] = parseInt(fields[1]);
        positions[i] = parseInt(fields[2]);
        alleles1[i] = fields[3].charCodeAt(0);
        alleles2[i] = fields[4].charCodeAt(0);
        if (fields.length !== numPCs + 6) {
            throw new Error(`Inconsistent number of columns in line ${i + 1}:
                                expected ${numPCs + 6}, found ${fields.length}`);
        }
        for (let j = 0; j < numPCs; j++) {
            pcWeights[i * numPCs + j] = parseFloat(fields[5 + j]);
            if (isNaN(pcWeights[i * numPCs + j])) {
                throw new Error(`Invalid weight for SNP ${snpIDs[i]} PC${j + 1}: ${fields[5 + j]}`);    
            }
        }
        frequencies[i] = parseFloat(fields[fields.length - 1]);
    }
    console.log(`Loaded ${numSNPs} SNPs with ${numPCs} PCs from weight file.`);
    return { snpIDs, chromosomes, positions, alleles1, alleles2, pcWeights, frequencies, numSNPs, numPCs };
}