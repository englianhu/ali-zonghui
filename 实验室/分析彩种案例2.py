!pip install -q xlrd==2.0.1 openpyxl tqdm beautifulsoup4

import requests
import pandas as pd
import sqlite3
import re
import json

from io import BytesIO
from bs4 import BeautifulSoup
from tqdm.auto import tqdm
from urllib.parse import unquote

BASE = "https://github.com/scibrokes1022/ali-zhonghui/tree/..."
hdr = {"User-Agent": "Mozilla/5.0"}

# 1. 抓取
html = requests.get(BASE, headers=hdr, timeout=30).text
soup = BeautifulSoup(html, "html.parser")

links = []

for a in soup.select('a[href*="/blob/"]'):
    href = a["href"]

    if href.lower().endswith((".xls", ".xlsx")):
        name = unquote(href.split("/")[-1])

        raw = (
            "https://raw.githubusercontent.com"
            + href.replace("/blob/", "/")
        )

        links.append((name, raw))

print(f"🔍 网页解析到 {len(links)} 个链接")

if not links:
    print("⚠️ 没抓到！可能是 GitHub 需要登录或仓库私有了")
    print(html[:500])


# 2. 下载

def extract_date(fn):

    m = (
        re.search(
            r'(\d{4})[-_年]?(\d{1,2})[-_月]?(\d{1,2})',
            fn
        )
        or re.search(r'(\d{4})', fn)
    )

    if m:

        y, m, d = map(int, m.groups())

        try:
            return f"{y:04d}-{m:02d}-{d:02d}"
        except:
            return None

    return None


dfs = []

for name, url in tqdm(links, desc="下载"):

    try:

        r = requests.get(
            url,
            headers=hdr,
            timeout=30
        )

        if r.status_code != 200:

            tqdm.write(
                f"❌ {name} HTTP {r.status_code}"
            )

            continue

        eng = (
            "openpyxl"
            if name.endswith(".xlsx")
            else "xlrd"
        )

        df = pd.read_excel(
            BytesIO(r.content),
            engine=eng,
            dtype=str
        )

        df.insert(
            0,
            "date",
            extract_date(name)
        )

        df["source_file"] = name

        dfs.append(df)

        tqdm.write(
            f"✅ {name} {df.shape}"
        )

    except Exception as e:

        tqdm.write(
            f"⚠️ {name} 失败: {e}"
        )


# 3. 合并

if not dfs:

    raise SystemExit(
        "❌ 没有任何文件成功读取"
    )

combined = pd.concat(
    dfs,
    ignore_index=True
)

combined = combined.drop_duplicates()

combined["date"] = pd.to_datetime(
    combined["date"],
    errors="coerce"
)

combined = (
    combined
    .sort_values("date")
    .reset_index(drop=True)
)

print(
    f"\n✅ 成功合并 {len(dfs)} 个文件，共 {combined.shape[0]} 行"
)

display(combined.head())


# 4. 保存

# CSV

combined.to_csv(
    "/诸子百家学府/彩种_合并.csv",
    index=False,
    encoding="utf-8-sig"
)

# SQLite

with sqlite3.connect(
    "/诸子百家学府/mango.db"
) as con:

    combined.to_sql(
        "caizhong",
        con,
        if_exists="replace",
        index=False
    )

# MongoDB 风格 JSON

mongo_df = combined.copy()

mongo_df["date"] = (
    pd.to_datetime(
        mongo_df["date"],
        errors="coerce"
    )
    .dt.strftime("%Y-%m-%d")
)

records = (
    mongo_df
    .where(pd.notnull(mongo_df), None)
    .to_dict("records")
)

with sqlite3.connect(
    "/诸子百家学府/mongo.db"
) as con:

    con.execute(
        "DROP TABLE IF EXISTS caizhong"
    )

    con.execute(
        "CREATE TABLE caizhong (doc TEXT)"
    )

