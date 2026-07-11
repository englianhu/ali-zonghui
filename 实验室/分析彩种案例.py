import pandas as pd
import numpy as np
from scipy import stats
from sklearn.preprocessing import StandardScaler
from sklearn.cluster import KMeans
import os

# 读取原始 CSV，不假设表头稳定
raw = pd.read_csv("Cai-Chong-_He-Bing.csv", header=None)

# 只保留前 8 列，并设定标准字段名
raw = raw.iloc[:, :8].copy()
raw.columns = ["彩种名称", "投注人数", "投注金额", "中奖金额", "撤单金额", "返点金额", "盈利", "盈率"]

# 将数值列转换为数值类型
for c in ["投注人数", "投注金额", "中奖金额", "撤单金额", "返点金额", "盈利"]:
    raw[c] = pd.to_numeric(raw[c], errors="coerce")

# 将盈率中的百分号清除并转为数值
raw["盈率"] = pd.to_numeric(raw["盈率"].astype(str).str.replace("%", "", regex=False), errors="coerce")

# 区分明细行与“小计”行
subtotal_mask = raw["彩种名称"].astype(str).str.contains("小计", na=False)
df = raw[~subtotal_mask].copy()
subtotals = raw[subtotal_mask].copy()

# 构造每人投注与每人盈利
df["每人投注"] = df["投注金额"] / df["投注人数"]
df["每人盈利"] = df["盈利"] / df["投注人数"]

# 构造盈利复核指标
df["盈利率复核"] = df["盈利"] / df["投注金额"] * 100

# 构造返奖比指标
df["毛返奖比"] = df["中奖金额"] / df["投注金额"]
df["净返奖比"] = (df["中奖金额"] + df["撤单金额"] + df["返点金额"]) / df["投注金额"]

# 选取关键数值列
num_cols = ["投注人数", "投注金额", "中奖金额", "撤单金额", "返点金额", "盈利", "盈率", "每人投注", "每人盈利"]

# 生成描述统计表
summary = pd.DataFrame({
    "count": df[num_cols].count(),
    "mean": df[num_cols].mean(),
    "std": df[num_cols].std(),
    "min": df[num_cols].min(),
    "median": df[num_cols].median(),
    "max": df[num_cols].max(),
    "skew": df[num_cols].skew(),
    "kurtosis": df[num_cols].kurtosis(),
}).round(4)

# 计算投注金额集中度指标
shares = df["投注金额"] / df["投注金额"].sum()
hhi = float((shares ** 2).sum())
cr5 = float(df["投注金额"].sort_values(ascending=False).head(5).sum() / df["投注金额"].sum())
cr10 = float(df["投注金额"].sort_values(ascending=False).head(10).sum() / df["投注金额"].sum())

# 生成盈利排行榜
top_profit = df.sort_values("盈利", ascending=False).head(15)[
    ["彩种名称", "投注人数", "投注金额", "中奖金额", "撤单金额", "返点金额", "盈利", "盈率", "每人投注", "每人盈利"]
]

# 生成亏损排行榜
top_loss = df.sort_values("盈利", ascending=True).head(15)[
    ["彩种名称", "投注人数", "投注金额", "中奖金额", "撤单金额", "返点金额", "盈利", "盈率", "每人投注", "每人盈利"]
]

# 生成投注规模排行榜
top_stake = df.sort_values("投注金额", ascending=False).head(15)[
    ["彩种名称", "投注人数", "投注金额", "盈利", "盈率", "每人投注", "每人盈利"]
]

# 生成单人盈利效率排行榜
top_eff = df.sort_values("每人盈利", ascending=False).head(15)[
    ["彩种名称", "投注人数", "每人投注", "每人盈利", "投注金额", "盈利", "盈率"]
]

# 生成变量相关性矩阵
corr = df[num_cols].corr(numeric_only=True).round(4)

# 标准化后做聚类
cluster_cols = ["投注人数", "投注金额", "中奖金额", "撤单金额", "返点金额", "盈利", "盈率", "每人投注", "每人盈利"]
X = StandardScaler().fit_transform(df[cluster_cols].fillna(0))

# 4 类聚类
km = KMeans(n_clusters=4, random_state=42, n_init=20)
df["cluster"] = km.fit_predict(X)

# 聚类画像
cluster_profile = df.groupby("cluster")[cluster_cols].mean().round(2)
cluster_counts = df["cluster"].value_counts().sort_index().to_frame("count")

# 按规模与利润做四象限分群
conditions = [
    (df["投注金额"] >= df["投注金额"].median()) & (df["盈利"] >= df["盈利"].median()),
    (df["投注金额"] >= df["投注金额"].median()) & (df["盈利"] < df["盈利"].median()),
    (df["投注金额"] < df["投注金额"].median()) & (df["盈利"] >= df["盈利"].median()),
]
choices = ["高规模高利润", "高规模低利润", "低规模高利润"]
df["type"] = np.select(conditions, choices, default="低规模低利润")

# 四象限画像
seg_profile = df.groupby("type")[["投注金额", "盈利", "每人投注", "每人盈利", "盈率"]].mean().round(2)
seg_counts = df["type"].value_counts().to_frame("count")

# 生成一些诊断指标
neg_rate = float((df["盈利"] < 0).mean())
zero_rate = float((df["投注人数"] == 0).mean())

# 创建输出目录
os.makedirs("output", exist_ok=True)

# 输出各类结果文件
summary.to_csv("output/summary_stats.csv", encoding="utf-8-sig")
top_profit.to_csv("output/top_profit.csv", index=False, encoding="utf-8-sig")
top_loss.to_csv("output/top_loss.csv", index=False, encoding="utf-8-sig")
top_stake.to_csv("output/top_stake.csv", index=False, encoding="utf-8-sig")
top_eff.to_csv("output/top_efficiency.csv", index=False, encoding="utf-8-sig")
corr.to_csv("output/correlation.csv", encoding="utf-8-sig")
cluster_profile.to_csv("output/cluster_profile.csv", encoding="utf-8-sig")
cluster_counts.to_csv("output/cluster_counts.csv", encoding="utf-8-sig")
seg_profile.to_csv("output/segment_profile.csv", encoding="utf-8-sig")
seg_counts.to_csv("output/segment_counts.csv", encoding="utf-8-sig")
df.to_csv("output/enriched_main.csv", index=False, encoding="utf-8-sig")
subtotals.to_csv("output/subtotals.csv", index=False, encoding="utf-8-sig")

# 打印核心报告
print("rows_main =", len(df))
print("rows_subtotal =", len(subtotals))
print("total_bet =", round(df["投注金额"].sum(), 2))
print("total_profit =", round(df["盈利"].sum(), 2))
print("mean_rate =", round(df["盈率"].mean(), 4))
print("neg_profit_rate =", round(neg_rate, 4))
print("zero_bettor_rate =", round(zero_rate, 4))
print("HHI =", round(hhi, 6))
print("CR5 =", round(cr5, 4))
print("CR10 =", round(cr10, 4))
print("
TOP PROFIT")
print(top_profit.head(10).to_string(index=False))
print("
TOP LOSS")
print(top_loss.head(10).to_string(index=False))
print("
SEGMENT PROFILE")
print(seg_profile.to_string())
print("
CLUSTER PROFILE")
print(cluster_profile.to_string())
print("
CORRELATION")
print(corr.to_string())
