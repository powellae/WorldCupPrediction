import requests
from bs4 import BeautifulSoup as bs
import pandas as pd
import re

pages = 913
for page in range(0, pages):
    p = 50*page
    url = "http://www.soccer-db.info/index.php?option=com_php&views=php&team1=0&team2=0&tour=all&limitstart=" + str(p)

    response = requests.get(url)
    html = response.content
    soup = bs(html, 'lxml')

    table = soup.findAll('tr', style="background-color:#DADEE0; text-align:center;")[1]

    df_headers = []
    for item in table.findAll('td'):
        df_headers.append(item.text.strip())

    #print(df_headers)
    df_dict = {}
    df_idx_ref = {}
    idx = 0

    for name in df_headers:
        df_dict[name] = []
        df_idx_ref[idx] = name
        idx += 1

    rows = soup.findAll('tr')[4:]

    for row in rows:
        data = row.findAll('td')[:6]
        idx = 0
        for d in data:
            df_dict[df_idx_ref[idx]].append(d.text.strip())
            #print(d.text.strip())
            #print("next")
            idx += 1

    #Print first five entries of df_dict
    #for key in df_dict:
    #    print('{}: {}\n'.format(key, df_dict[key][0:5]))

    df_dict.pop('Link', None)

    #Be sure number of entries in each dictionary is the same
    #for key in df_dict:
    #    print("{}: {}".format(key,len(df_dict[key])))

    df = pd.DataFrame(df_dict, columns=df_dict.keys())

    df2 = pd.DataFrame(columns=df_dict.keys())

    if page == 0:
        frames = [df, df2]
        df3 = pd.concat(frames)
    if page > 0:
        frames = [df, df3]
        df3 = pd.concat(frames)


df3.to_csv("soccer_db.csv")
