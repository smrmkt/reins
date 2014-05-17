#!/usr/bin/env python
#-*-coding:utf-8-*-

import argparse

# args
parser = argparse.ArgumentParser()
parser.add_argument('in_path')
parser.add_argument('out_path', nargs='?')

# define list
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

trains = ['湘南新宿ライン宇須',
          '東海道本線',
          '南武線',
          '鶴見線',
          '横浜線',
          '根岸線',
          '横須賀線',
          '京浜東北・根岸線',
          '東急東横線',
          '京浜急行電鉄本線',
          '京浜急行電鉄逗子線',
          '相模鉄道本線',
          '横浜市ブルーライン',
          '金沢シーサイドＬ',
          '横浜高速鉄道ＭＭ線',
          '横浜市グリーンＬ',
          '東海道・山陽新幹線',
          '東急目黒線',
          '東急田園都市線',
          '京王電鉄相模原線',
          '小田急電鉄多摩線',
          '京浜急行電鉄大師線',
          '小田急電鉄小田原線']

distances = ['徒歩5分以内',
             '徒歩10分以内',
             '徒歩15分以内',
             '徒歩15分超',
             'バス']

spaces = ['0〜20m²',
          '20〜40m²',
          '40〜60m²',
          '60〜80m²',
          '80〜100m²',
          '100〜120m²',
          '120〜140m²',
          '140〜160m²',
          '160〜180m²',
          '180〜200m²']

def add_param(tokens, num, list):
    target = tokens[num]
    if list.count(target) > 0:
        tokens.append(str(list.index(target) + 1))
    else:
        tokens.append('NA')
    return tokens

def add_price(tokens):
    price = tokens[5].replace('万円', '')
    tokens.append(price)
    return tokens

def add_room(tokens):
    room = tokens[7]
    if room == 'ワンルーム':
        tokens.append('1')
    else:
        rooms = int(room[0])
        other = len(room[0]) - 1
        tokens.append(str(rooms + other))
    return tokens

def add_from(tokens):
    year_from = tokens[8].replace('年から', '')
    tokens.append(year_from)
    return tokens

def get_header():
    return 'id,' \
           'train_raw,' \
           'station_raw,' \
           'distance_raw,' \
           'address_raw,' \
           'price_raw,' \
           'space_raw,' \
           'room_raw,' \
           'from_raw,' \
           'contract_taw,' \
           'type_raw,' \
           'train,' \
           'station,' \
           'distance,' \
           'price,' \
           'space,' \
           'room,' \
           'from';

if __name__ == '__main__':
    # setup
    args = parser.parse_args()
    in_file = open(args.in_path, 'r')
    out_file = open(args.out_path, 'w')
    header = get_header()
    out_file.write(header + '\n')

    # add cleansed columns
    for line in in_file:
        # prepare
        tokens = line.split('\t')
        if len(tokens) != 12 or tokens[1] == '-':
            continue
        tokens.pop(-1)
        # add columns
        tokens = add_param(tokens, 1, trains)
        tokens = add_param(tokens, 2, stations)
        tokens = add_param(tokens, 3, distances)
        tokens = add_price(tokens)
        tokens = add_param(tokens, 6, spaces)
        tokens = add_room(tokens)
        tokens = add_from(tokens)
        out_file.write(','.join(tokens) + '\n')

    # tear down
    in_file.close()
    out_file.close()