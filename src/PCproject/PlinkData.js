export function readBimData(bimText) {
    const lines = bimText.trim().split('\n');
    const nrSNPs = lines.length;
    let chromosomes = new Array(nrSNPs);
    let positions = new Uint16Array(nrSNPs);
    let snpIDs = new Array(nrSNPs);
    let alleles1 = new Array(nrSNPs);
    let alleles2 = new Array(nrSNPs);
    for (let i = 0; i < nrSNPs; i++) {
        const fields = lines[i].trim().split(/\s+/);
        chromosomes[i] = fields[0];
        snpIDs[i] = fields[1];
        positions[i] = parseInt(fields[3]);
        if (isNaN(positions[i])) {
            throw new Error(`Invalid position for SNP ${snpIDs[i]}: ${fields[3]}`);
        }
        alleles1[i] = fields[4];
        alleles2[i] = fields[5];
    }
    console.log(`Loaded ${nrSNPs} SNPs from BIM file.`);
    return { snpIDs, chromosomes, positions, alleles1, alleles2 };
}

export function readFamData(famText) {
    const lines = famText.trim().split('\n');
    const nrSamples = lines.length;
    let popNames = new Array(nrSamples);
    let indNames = new Array(nrSamples);
    for (let i = 0; i < nrSamples; i++) {
        const fields = lines[i].trim().split(/\s+/);
        popNames[i] = fields[0];
        indNames[i] = fields[1];
    }
    console.log(`Loaded ${nrSamples} individuals from FAM file.`);
    return { indNames, popNames };
}
