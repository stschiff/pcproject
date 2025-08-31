export function readBimData(bimText) {
    const lines = bimText.trim().split('\n');
    const nrSnps = lines.length;
    let chromosomes = new Array(nrSnps);
    let positions = new Uint16Array(nrSnps);
    let snpIds = new Array(nrSnps);
    let alleles1 = new Array(nrSnps);
    let alleles2 = new Array(nrSnps);
    for (let i = 0; i < nrSnps; i++) {
        const fields = lines[i].trim().split(/\s+/);
        chromosomes[i] = fields[0];
        snpIds[i] = fields[1];
        positions[i] = parseInt(fields[3]);
        if (isNaN(positions[i])) {
            throw new Error(`Invalid position for SNP ${snpIds[i]}: ${fields[3]}`);
        }
        alleles1[i] = fields[4];
        alleles2[i] = fields[5];
    }
    return { snpIds, chromosomes, positions, alleles1, alleles2 };
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
    return { indNames, popNames };
}
