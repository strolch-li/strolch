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
"""Run one compiled JMH harness with each checkout's production classes, sequentially."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import platform
import subprocess
import time

MODULES = ('strolch-utils', 'strolch-privilege', 'strolch-model', 'strolch-agent',
           'strolch-test-base', 'strolch-xmlpers', 'strolch-persistence-postgresql',
           'strolch-persistence-xml', 'strolch-service')
CASES = [
    ('read-small', 'readOne', 1000, 1, 1, 0),
    ('read-large', 'readOne', 100000, 1, 1, 0),
    ('read-million', 'readOne', 1000000, 1, 1, 0),
    ('write-small', 'updateIndependent', 1000, 1, 1, 0),
    ('write-large', 'updateIndependent', 100000, 1, 1, 0),
    ('write-million', 'updateIndependent', 1000000, 1, 1, 0),
    ('write-concurrent', 'updateIndependent', 100000, 1, 8, 0),
    ('mixed-concurrent', 'mixedNineReadsOneWrite', 100000, 1, 8, 0),
    ('search-large', 'searchType', 100000, 1, 1, 0),
    ('read-many-types', 'readOne', 100000, 100, 1, 0),
    ('write-many-types', 'updateIndependent', 100000, 100, 8, 0),
    ('write-service-work', 'updateIndependent', 100000, 1, 8, 10000),
    ('latency-read', 'readLatency', 100000, 1, 8, 0),
    ('latency-successful-write', 'successfulUpdateLatency', 100000, 1, 8, 0),
]


CUSTOMER_CASES = [
    ('customer-product-single', 'updateIndependent', 290203, 32, 1, 0),
    ('customer-product-eight', 'updateIndependent', 290203, 32, 8, 0),
    ('customer-person-eight', 'updateIndependent', 290203, 32, 8, 0),
    ('customer-picking-eight', 'updateIndependent', 290203, 32, 8, 0),
    ('customer-fromstock-single', 'updateIndependent', 290203, 32, 1, 0),
    ('customer-fromstock-eight', 'updateIndependent', 290203, 32, 8, 0),
    ('customer-mixed-fromstock', 'mixedNineReadsOneWrite', 290203, 32, 8, 0),
    ('customer-read-fromstock', 'readOne', 290203, 32, 8, 0),
    ('customer-double-fromstock', 'updateIndependent', 580406, 32, 8, 0),
    ('customer-latency-fromstock', 'successfulUpdateLatency', 290203, 32, 8, 0),
]
CUSTOMER_TARGETS = {
    'customer-product-single': ('Resource', 'Product'),
    'customer-product-eight': ('Resource', 'Product'),
    'customer-person-eight': ('Resource', 'Person'),
    'customer-picking-eight': ('Resource', 'PickingItem'),
}


def output(command, cwd=None):
    return subprocess.check_output(command, cwd=cwd, text=True).strip()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--baseline', required=True, type=Path)
    parser.add_argument('--candidate', required=True, type=Path)
    parser.add_argument('--external-classpath', required=True, type=Path)
    parser.add_argument('--output', required=True, type=Path)
    parser.add_argument('--java', default='java')
    parser.add_argument('--quick', action='store_true', help='2 forks, 2x1s warmup, 3x1s measurement')
    parser.add_argument('--customer', action='store_true', help='Use the supplied customer type counts and a doubled dataset')
    parser.add_argument('--smoke', action='store_true', help='Tiny harness check; not performance evidence')
    parser.add_argument('--cases', nargs='*', help='Selected case names; default is the full matrix')
    args = parser.parse_args()
    args.baseline, args.candidate, args.output = (p.resolve() for p in (args.baseline, args.candidate, args.output))
    args.output.mkdir(parents=True, exist_ok=True)
    harness = args.candidate / 'strolch-jmh-benchmark/target/classes'
    external = args.external_classpath.read_text().strip()
    if not (harness / 'META-INF/BenchmarkList').exists():
        parser.error('Compile the candidate benchmark module first (including annotation processing)')
    if any('/li/strolch/' in entry for entry in external.split(os.pathsep)):
        parser.error('External classpath must exclude li.strolch; local production classes must win')
    for checkout in (args.baseline, args.candidate):
        for module in MODULES:
            if not (checkout / module / 'target/classes').is_dir():
                parser.error(f'Compile {checkout / module} before running')
    available = CASES + CUSTOMER_CASES if args.cases else (CUSTOMER_CASES if args.customer else CASES)
    cases = [case for case in available if not args.cases or case[0] in args.cases]
    if args.cases and set(args.cases) - {case[0] for case in available}:
        parser.error('Unknown case name')
    if args.smoke:
        cases = [CASES[0], ('smoke-writes', 'updateIndependent', 1000, 1, 8, 0)]
        timing = ['-f', '1', '-wi', '1', '-w', '200ms', '-i', '1', '-r', '200ms']
    elif args.quick:
        timing = ['-f', '2', '-wi', '2', '-w', '1s', '-i', '3', '-r', '1s']
    else:
        timing = ['-f', '3', '-wi', '5', '-w', '2s', '-i', '5', '-r', '2s']
    meta = {'startedUtc': time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime()),
            'java': subprocess.run([args.java, '-version'], capture_output=True, text=True).stderr,
            'platform': platform.platform(), 'cpu': output(['lscpu']),
            'load': os.getloadavg(), 'memory': output(['free', '-m']),
            'harnessSha256': hashlib.sha256((harness / 'li/strolch/test/benchmark/TransientTransactionBenchmark.class').read_bytes()).hexdigest(),
            'timing': timing, 'cases': cases, 'customerTargets': CUSTOMER_TARGETS, 'smoke': args.smoke, 'commands': [], 'checkouts': {}}
    for label, checkout in [('baseline', args.baseline), ('candidate', args.candidate)]:
        meta['checkouts'][label] = {'path': str(checkout), 'revision': output(['git', 'rev-parse', 'HEAD'], checkout),
                                    'status': output(['git', 'status', '--short'], checkout)}
    metadata_path = args.output / 'environment.json'
    for index, (name, method, roots, types, threads, tokens) in enumerate(cases):
        # Alternate which implementation runs first to reduce time/thermal ordering bias.
        sides = [('baseline', args.baseline), ('candidate', args.candidate)]
        if index % 2:
            sides.reverse()
        for label, checkout in sides:
            cp = os.pathsep.join([str(harness)] + [str(checkout / module / 'target/classes') for module in MODULES] + [external])
            result_file = args.output / f'{name}-{label}.json'
            log_file = args.output / f'{name}-{label}.log'
            command = [args.java, '-cp', cp, 'org.openjdk.jmh.Main',
                       f'li.strolch.test.benchmark.TransientTransactionBenchmark.{method}$',
                       '-p', f'roots={roots}', '-p', f'types={types}', '-p', f'workTokens={tokens}',
                       '-t', str(threads), '-foe', 'true', '-prof', 'gc', '-rf', 'json', '-rff', str(result_file),
                       '-jvmArgs', '-Xms2g -Xmx2g -XX:+UseG1GC -XX:ActiveProcessorCount=8 -Dlogback.configurationFile=isolation-logback.xml'] + timing
            if name.startswith('customer-'):
                kind, target_type = CUSTOMER_TARGETS.get(name, ('Order', 'FromStock'))
                dataset = 'customer-double' if name == 'customer-double-fromstock' else 'customer'
                command += ['-p', f'dataset={dataset}', '-p', f'rootKind={kind}', '-p', f'targetType={target_type}']
            meta['commands'].append({'case': name, 'side': label, 'cwd': str(args.candidate / 'strolch-jmh-benchmark'),
                                     'argv': command})
            metadata_path.write_text(json.dumps(meta, indent=2) + '\n')
            print(f'{time.strftime("%H:%M:%S")} {name} {label}: roots={roots}, types={types}, threads={threads}', flush=True)
            with log_file.open('w') as log:
                completed = subprocess.run(command, cwd=args.candidate / 'strolch-jmh-benchmark', stdout=log, stderr=subprocess.STDOUT)
            if completed.returncode or not result_file.exists() or not json.loads(result_file.read_text()):
                raise RuntimeError(f'Benchmark failed; inspect {log_file}')
    meta['finishedUtc'] = time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())
    metadata_path.write_text(json.dumps(meta, indent=2) + '\n')
    print('Complete. Use successful commit counters, not attempted throughput, to compare writes.', flush=True)


if __name__ == '__main__':
    main()
