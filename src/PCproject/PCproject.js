function getAlleleFreq(plinkData, snpIndex, individualIndex) {
    let blockSize = Math.ceil(plinkData.numIndividuals / 4);
    let byteIndex = 3 + snpIndex * blockSize + Math.floor(individualIndex / 4);
    let bitOffset = (individualIndex % 4) * 2;
    let byteValue = plinkData.bedData[byteIndex];
    let genotypeBits = (byteValue >> bitOffset) & 0b11;
    switch (genotypeBits) {
        case 0b00:
            return 0; // Homozygous reference
        case 0b10:
            return 0.5; // Heterozygous
        case 0b11:
            return 1; // Homozygous alternate
        case 0b01:
            return null; // Missing genotype
    }
}

export function projectPlinkOnWeights(plinkData) { return function(snpWeights) {

    let analysedPositions = new Array(plinkData.numIndividuals).fill(0);
    let overlappingPositions = 0;
    let plinkIndex = 0;
    let result = new Array(plinkData.numIndividuals * snpWeights.numPCs).fill(0);
    for (let i = 0; i < snpWeights.snpIDs.length; i++) {
        // console.log(`Processing SNP ${i+1}: ${snpWeights.snpIDs[i]} at ${snpWeights.chromosomes[i]}:${snpWeights.positions[i]}`);
        // console.log(`Current bim SNP: ${plinkData.bimData.snpIDs[plinkIndex]} at ${plinkData.bimData.chromosomes[plinkIndex]}:${plinkData.bimData.positions[plinkIndex]}`);
        while (plinkData.bimData.chromosomes[plinkIndex] < snpWeights.chromosomes[i] ||
               (plinkData.bimData.chromosomes[plinkIndex] == snpWeights.chromosomes[i] && plinkData.bimData.positions[plinkIndex] < snpWeights.positions[i])) {
            plinkIndex++;
            // console.log(`Advancing plinkIndex to ${plinkIndex}`);   
        }
        if (plinkData.bimData.chromosomes[plinkIndex] === snpWeights.chromosomes[i] && plinkData.bimData.positions[plinkIndex] === snpWeights.positions[i]) {
            overlappingPositions++;
            console.log(`Matching SNP at plinkIndex ${plinkIndex} for snpWeights index ${i}`);
            for (let j = 0; j < plinkData.numIndividuals; j++) {
                let alleleFreq = getAlleleFreq(plinkData, plinkIndex, j);
                if (alleleFreq !== null) {
                    for (let pc = 0; pc < snpWeights.numPCs; pc++) {
                        result[j * snpWeights.numPCs + pc] += alleleFreq * snpWeights.pcWeights[i * snpWeights.numPCs + pc];
                    }
                    analysedPositions[j]++;
                }
            }
        }
        if(plinkIndex >= plinkData.numSNPs) {
            break;
        }
    }
    for (let j = 0; j < 10; j++) { //plinkData.numIndividuals; j++) {
        for (let pc = 0; pc < snpWeights.numPCs; pc++) {
            if (analysedPositions[j] > 0) {
                result[j * snpWeights.numPCs + pc] /= analysedPositions[j];
            }
        }
    }
    return {
        pcPositions: result,
        numIndividuals: plinkData.numIndividuals,
        numPCs: snpWeights.numPCs,
        analysedPositions: analysedPositions,
        overlappingPositions: overlappingPositions
    }
} }
