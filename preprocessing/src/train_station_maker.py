#!/usr/bin/env python
#-*-coding:utf-8-*-

import argparse
import numpy as np

# args
parser = argparse.ArgumentParser()
parser.add_argument('in_path')
parser.add_argument('out_path')


if __name__ == '__main__':
    # setup
    args = parser.parse_args()
    in_file = open(args.in_path, 'r')

    # create station-train matrix
    st = np.zeros((133, 23))
    is_header = True
    for line in in_file:
        if is_header is True:
            is_header = False
            continue
        tokens = line.split(',')
        station, train = int(tokens[11])-1, int(tokens[12])-1
        st[station][train] = 1

    # write out
    np.savetxt(args.out_path, st, fmt="%.0f", delimiter=',')

    # tear down
    in_file.close()
