export function readRefPosData(content) {
    const lines = content.trim().split('\n');
    const numSamples = lines.length - 1;
    let samples = new Array(numSamples);
    const numFields = lines[1].trim().split(/\s+/).length;
    const numPCs = numFields - 3;
    if (numPCs < 1) {
        throw new Error(`Expected at least 4 columns per line (sampleID, PCs, popName and popGroup), but found ${numFields} in the first line.`);
    }
    for (let i = 0; i < numSamples; i++) {
        const fields = lines[i + 1].trim().split(/\s+/);
        if (fields.length !== numFields) {
            throw new Error(`Inconsistent number of columns in line ${i + 2}: expected ${numFields}, found ${fields.length}`);
        }
        samples[i] = {
            sampleID: fields[0],
            popName: fields[1],
            popGroup: fields[numFields - 1],
            pcValues: new Array(numPCs)
        };
        if (i < 10) {
            console.log(samples[i]);
        }
        for (let j = 0; j < numPCs; j++) {
            samples[i].pcValues[j] = parseFloat(fields[2 + j]);
            if (isNaN(samples[i].pcValues[j])) {
                throw new Error(`Invalid PC value for sample ${samples[i].sampleID} PC${j + 1}: ${fields[1 + j]}`);
            }
        }
    }
    console.log(`First sample: ${samples[0]}`);
    console.log(`First sample: ${samples[0].sampleID}, PCs: ${samples[0].pcValues.join(', ')}, popName: ${samples[0].popName}, popGroup: ${samples[0].popGroup}`);
    console.log(`Loaded ${numSamples} samples with ${numPCs} PCs from reference position file.`);
    return { samples, numSamples, numPCs };
}
    