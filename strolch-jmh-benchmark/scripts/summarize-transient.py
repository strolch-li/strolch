#!/usr/bin/env python3
# Copyright (c) 2026 Robert von Burg <eitch@eitchnet.ch>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
"""Summarize paired JMH results without counting conflicts as useful transactions."""
import argparse
import csv
import gzip
import json
from pathlib import Path


def metric(result, key):
    return result['secondaryMetrics'].get(key, {}).get('score', 0.0)


def summarize(directory, partial=False):
    environment = json.loads((directory / 'environment.json').read_text())
    if not partial and 'finishedUtc' not in environment:
        raise RuntimeError('Comparison is incomplete; use --partial only for progress inspection')
    rows = []
    for name, method, roots, types, threads, tokens in environment['cases']:
        pair = {}
        for side in ('baseline', 'candidate'):
            path = directory / f'{name}-{side}.json'
            compressed = path.with_suffix(path.suffix + '.gz')
            if not path.exists() and compressed.exists():
                data = gzip.decompress(compressed.read_bytes()).decode()
            elif path.exists():
                data = path.read_text()
            else:
                data = ''
            if not data:
                if partial:
                    continue
                raise RuntimeError(f'Missing benchmark result: {path}')
            result = json.loads(data)[0]
            primary = result['primaryMetric']
            attempts = primary['score']
            commits = metric(result, 'commits')
            conflicts = metric(result, 'conflicts')
            timeouts = metric(result, 'lockTimeouts')
            writes = commits + conflicts + timeouts
            allocation = metric(result, 'gc.alloc.rate.norm')
            values = {'case': name, 'side': side, 'roots': roots, 'types': types, 'threads': threads,
                      'workTokens': tokens, 'dataset': result['params'].get('dataset', 'uniform'),
                      'rootKind': result['params'].get('rootKind', 'Resource'),
                      'targetType': result['params'].get('targetType', 'BenchType0'), 'mode': result['mode'], 'primary': attempts,
                      'primaryError99_9': primary['scoreError'], 'unit': primary['scoreUnit'],
                      'reads_per_s': metric(result, 'reads'), 'commits_per_s': commits,
                      'conflicts_per_s': conflicts, 'lock_timeouts_per_s': timeouts,
                      'write_rejection_pct': 100 * (conflicts + timeouts) / writes if writes else 0,
                      'allocated_bytes_per_op': allocation, 'gc_ms_total': metric(result, 'gc.time'),
                      'p50_us': primary.get('scorePercentiles', {}).get('50.0', 0),
                      'p95_us': primary.get('scorePercentiles', {}).get('95.0', 0),
                      'p99_us': primary.get('scorePercentiles', {}).get('99.0', 0)}
            if result['mode'] == 'thrpt':
                values['useful_per_s'] = values['reads_per_s'] + commits
                values['bytes_per_commit'] = allocation * attempts / commits if commits and not values['reads_per_s'] else 0
            else:
                values['useful_per_s'] = 0
                values['bytes_per_commit'] = allocation if method == 'successfulUpdateLatency' else 0
            pair[side] = values
            rows.append(values)
        if set(pair) == {'baseline', 'candidate'}:
            a, b = pair['baseline'], pair['candidate']
            if a['mode'] == 'thrpt':
                ratio = b['useful_per_s'] / a['useful_per_s']
                print(f"{name:28} useful/s {a['useful_per_s']:12,.0f} -> {b['useful_per_s']:12,.0f} ({ratio:.4f}x); "
                      f"reject {b['write_rejection_pct']:.1f}%; B/op {a['allocated_bytes_per_op']:,.0f} -> {b['allocated_bytes_per_op']:,.0f}")
            else:
                print(f"{name:28} p50 us {a['p50_us']:.2f} -> {b['p50_us']:.2f}; "
                      f"p99 us {a['p99_us']:.2f} -> {b['p99_us']:.2f}")
    if rows:
        with (directory / 'summary.csv').open('w', newline='') as file:
            writer = csv.DictWriter(file, fieldnames=list(rows[0]), lineterminator='\n')
            writer.writeheader()
            writer.writerows(rows)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('directory', type=Path)
    parser.add_argument('--partial', action='store_true')
    parser.add_argument('--compress', action='store_true', help='Losslessly archive raw JMH JSON/logs after summarizing')
    args = parser.parse_args()
    summarize(args.directory, args.partial)
    if args.compress:
        for path in args.directory.iterdir():
            if path.suffix in ('.json', '.log') and path.name != 'environment.json':
                path.with_suffix(path.suffix + '.gz').write_bytes(gzip.compress(path.read_bytes(), mtime=0))
                path.unlink()
