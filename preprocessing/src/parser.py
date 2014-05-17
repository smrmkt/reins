#!/usr/bin/env python
#-*-coding:utf-8-*-

import argparse
import os
from bs4 import BeautifulSoup

# args
parser = argparse.ArgumentParser()
parser.add_argument('in_path')
parser.add_argument('out_path', nargs='?')

def write_out(out, match):
    skip = True
    for m in match:
        if skip:
            if str(m).find('rownum') > 0:
                skip = False
            else:
                continue
        if str(m).find('rownum') > 0:
            out.write('\n')
        out.write(m.contents[0].encode('utf-8') + '\t')

if __name__ == '__main__':
    # setup
    args = parser.parse_args()
    in_path = args.in_path
    in_files = os.listdir(in_path)
    out_file = open(args.out_path, 'w')

    # parse
    for in_file in in_files:
        html = open(in_path+'/'+in_file, 'r').read()
        soup = BeautifulSoup(html)
        match = soup.findAll('td')
        write_out(out_file, match)
    out_file.close()
