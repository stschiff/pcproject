export function tooltipLabelImpl(labels) {
    return function(t) {
        return labels[t.datasetIndex][t.dataIndex];
    };
}