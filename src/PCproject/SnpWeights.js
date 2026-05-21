export function readSnpWeights(snpWeightText) {
    const lines = snpWeightText.trim().split('\n');
    const numSNPs = lines.length;
    let snpIDs = new Array(numSNPs);
    let chromosomes = new Array(numSNPs);
    let positions = new Uint32Array(numSNPs);
    let alleles1 = new Array(numSNPs);
    let alleles2 = new Array(numSNPs);
    let firstLineFields = lines[0].trim().split(/\s+/);
    if (firstLineFields.length < 7) {
        throw new Error(`For SnpWeights expected at least 7 columns per line (snpIDs, chrom, pos, allele1, allele2, and at least one PC and one frequency), but found ${firstLineFields.length} in the first line.`);
    }
    let numPCs = firstLineFields.length - 6;
    let pcWeights = new Float32Array(numSNPs * numPCs);
    let frequencies = new Float32Array(numSNPs);
    for (let i = 0; i < numSNPs; i++) {
        const fields = lines[i].trim().split(/\s+/);
        snpIDs[i] = fields[0];
        chromosomes[i] = parseInt(fields[1]);
        if (isNaN(chromosomes[i]) || chromosomes[i] < 1 || chromosomes[i] > 25) {
            throw new Error(`Invalid chromosome for SNP ${snpIDs[i]}: ${fields[1]}`);
        }
        positions[i] = parseInt(fields[2]);
        if (isNaN(positions[i])) {
            throw new Error(`Invalid position for SNP ${snpIDs[i]}: ${fields[2]}`);
        }
        alleles1[i] = fields[3];
        alleles2[i] = fields[4];
        if (fields.length !== numPCs + 6) {
            throw new Error(`Inconsistent number of columns in line ${i + 1}: expected ${numPCs + 6}, found ${fields.length}`);
        }
        for (let j = 0; j < numPCs; j++) {
            pcWeights[i * numPCs + j] = parseFloat(fields[5 + j]);
            if (isNaN(pcWeights[i * numPCs + j])) {
                throw new Error(`Invalid weight for SNP ${snpIDs[i]} PC${j + 1}: ${fields[5 + j]}`);    
            }
        }
        frequencies[i] = parseFloat(fields[5 + numPCs]);
        if (isNaN(frequencies[i])) {
            throw new Error(`Invalid frequency for SNP ${snpIDs[i]}: ${fields[5 + numPCs]}`);
        }
    }
    console.log(`Loaded ${numSNPs} SNPs with ${numPCs} PCs from weight file.`);
    return { snpIDs, chromosomes, positions, alleles1, alleles2, pcWeights, frequencies, numSNPs, numPCs };
}