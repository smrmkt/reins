#!/usr/bin/env python
#-*-coding:utf-8-*-

import argparse
from bs4 import BeautifulSoup
import time
import urllib2

# args
parser = argparse.ArgumentParser()
parser.add_argument('out_path')

# api base url
api_base_url = 'http://www.geocoding.jp/api/?v=1.1&q=%s'

# station list
stations = ['久地',
            '六浦',
            '反町',
            '大口',
            '子安',
            '小机',
            '尻手',
            '山手',
            '川崎',
            '平間',
            '幸浦',
            '戸部',
            '新羽',
            '日吉',
            '杉田',
            '柿生',
            '栗平',
            '根岸',
            '横浜',
            '浅野',
            '港町',
            '生田',
            '生麦',
            '登戸',
            '白楽',
            '矢向',
            '磯子',
            '綱島',
            '菊名',
            '蒔田',
            '追浜',
            '関内',
            '高津',
            '高田',
            '鳥浜',
            '鴨居',
            '鶴川',
            '鶴見',
            '鷺沼',
            '上大岡',
            '上星川',
            '上永谷',
            '下永谷',
            '並木北',
            '中野島',
            '五月台',
            '元住吉',
            '八丁畷',
            '八景島',
            '南太田',
            '吉野町',
            '向河原',
            '大倉山',
            '天王町',
            '妙蓮寺',
            '宮前平',
            '宮崎台',
            '宿河原',
            '屏風浦',
            '平沼橋',
            '弘明寺',
            '新丸子',
            '新子安',
            '新川崎',
            '新杉田',
            '新横浜',
            '東戸塚',
            '東白楽',
            '東門前',
            '桜木町',
            '梶が谷',
            '洋光台',
            '津田山',
            '浜川崎',
            '港南台',
            '溝の口',
            '片倉町',
            '矢野口',
            '石川町',
            '神奈川',
            '稲田堤',
            '能見台',
            '若葉台',
            '西横浜',
            '鈴木町',
            '阪東橋',
            '馬車道',
            '高島町',
            '鹿島田',
            '黄金町',
            'あざみ野',
            'はるひ野',
            '並木中央',
            '二子新地',
            '井土ヶ谷',
            '京急富岡',
            '京急川崎',
            '京急鶴見',
            '保土ヶ谷',
            '北新横浜',
            '南部市場',
            '小島新田',
            '岸根公園',
            '川崎大師',
            '日ノ出町',
            '日吉本町',
            '東神奈川',
            '武蔵中原',
            '武蔵小杉',
            '武蔵新城',
            '港南中央',
            '産業道路',
            '百合ヶ丘',
            '花月園前',
            '金沢八景',
            '金沢文庫',
            '鶴見小野',
            '鶴見市場',
            '三ッ沢上町',
            '三ッ沢下町',
            '京急新子安',
            '京王稲田堤',
            '向ヶ丘遊園',
            '新百合ヶ丘',
            '日本大通り',
            '神奈川新町',
            'たまプラーザ',
            'みなとみらい',
            '元町・中華街',
            '海の公園柴口',
            '読売ランド前',
            '伊勢佐木長者町',
            '京王よみうりランド']


def add_param(tokens, num, list):
    target = tokens[num]
    if list.count(target) > 0:
        tokens.append(str(list.index(target) + 1))
    else:
        tokens.append('NA')
    return tokens


def get_geo(station):
    url = api_base_url % urllib2.quote(station)
    bs = BeautifulSoup(urllib2.urlopen(url))
    lng = bs.find('lng')
    lat = bs.find('lat')
    if lng is None:
        return None, None
    else:
        return lng.renderContents(), lat.renderContents()


if __name__ == '__main__':
    get_geo('綱島')
    args = parser.parse_args()
    out_file = open(args.out_path, 'w')
    out_file.write('station, station_raw, long, lat\n')
    for station in stations:
        station_id = str(stations.index(station) + 1)
        lng, lat = get_geo(station)
        print lng, lat
        out_file.write('%s, %s, %s, %s\n' % (station_id, station, lng, lat))
        time.sleep(3)
    out_file.close()