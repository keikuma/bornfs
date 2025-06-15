import argparse
import math
import re
from collections import Counter


def parse_log(path):
    features = []
    collecting = False
    with open(path) as f:
        for line in f:
            line = line.strip()
            if line.startswith('# Selected features'):
                collecting = True
                continue
            if collecting:
                if line.startswith('# Statistics'):
                    break
                if line.startswith('#') or not line:
                    continue
                features.extend(line.split())
    return features


def parse_header(path):
    attrs = []
    with open(path) as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith('%'):
                continue
            if line.lower().startswith('@attribute'):
                parts = re.split(r"\s+", line, 2)
                if len(parts) >= 2:
                    attrs.append(parts[1])
            elif line.lower() == '@data':
                break
    return attrs


def stream_data(path, class_idx):
    data_section = False
    with open(path) as f:
        for line in f:
            line = line.strip()
            if not data_section:
                if line.lower() == '@data':
                    data_section = True
                continue
            if not line or line.startswith('%'):
                continue
            row = {}
            for pair in line.strip('{}').split(','):
                pair = pair.strip()
                if not pair:
                    continue
                idx, val = pair.split()
                row[int(idx)] = int(val)
            label = row.pop(class_idx, 0)
            yield row, label


def entropy_from_counts(counter, total):
    return -sum((c / total) * math.log2(c / total) for c in counter.values() if c)


def compute_statistics(arff_file, log_file):
    attrs = parse_header(arff_file)
    attr2idx = {a: i for i, a in enumerate(attrs)}
    class_idx = attr2idx.get('class', len(attrs) - 1)
    selected_features = parse_log(log_file)
    sel_indices = [attr2idx[f] for f in selected_features]

    label_counts = Counter()
    entire = Counter()
    entire_label = Counter()
    selected = Counter()
    selected_label = Counter()

    for features, label in stream_data(arff_file, class_idx):
        label_counts[label] += 1
        pat_entire = tuple(sorted(features.items()))
        entire[pat_entire] += 1
        entire_label[(pat_entire, label)] += 1

        pat_sel = tuple(sorted((i, v) for i, v in features.items() if i in sel_indices))
        selected[pat_sel] += 1
        selected_label[(pat_sel, label)] += 1

    n_instances = sum(label_counts.values())
    n_features = len(attrs)

    hc = entropy_from_counts(label_counts, n_instances)
    h_entire = entropy_from_counts(entire, n_instances)
    h_entire_c = entropy_from_counts(entire_label, n_instances)
    i_entire = h_entire + hc - h_entire_c

    h_sel = entropy_from_counts(selected, n_instances)
    h_sel_c = entropy_from_counts(selected_label, n_instances)
    i_sel = h_sel + hc - h_sel_c
    h_sel_given_c = h_sel - i_sel
    mu_h = 0.0
    mu_g = 0.0
    if i_entire + h_sel:
        mu_h = 2 * i_sel / (i_entire + h_sel)
    if i_entire > 0 and h_sel > 0:
        mu_g = i_sel / math.sqrt(i_entire * h_sel)

    stats = {
        'Number of instances': n_instances,
        'Number of features': n_features,
        'H(C)': hc,
        'H(Entire)': h_entire,
        'H(Entire, C)': h_entire_c,
        'I(Entire; C)': i_entire,
        'H(Selected)': h_sel,
        'H(Selected, C)': h_sel_c,
        'I(Selected; C)': i_sel,
        'H(Selected | C)': h_sel_given_c,
        'mu_H': mu_h,
        'mu_G': mu_g,
    }

    return stats


def main():
    parser = argparse.ArgumentParser(description='Compute BornFS statistics from ARFF and log file.')
    parser.add_argument('arff', help='Input ARFF file')
    parser.add_argument('log', help='BornFS log file with selected features')
    args = parser.parse_args()

    stats = compute_statistics(args.arff, args.log)
    for k, v in stats.items():
        if isinstance(v, float):
            print(f'{k} = {v:.4f}')
        else:
            print(f'{k} = {v}')


if __name__ == '__main__':
    main()
