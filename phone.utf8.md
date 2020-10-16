---
title: "💙🤍 ❤️🖤 💚💙"
subtitle: "📲 - *️⃣*️⃣*️⃣6️⃣4️⃣8️⃣2️⃣7️⃣7️⃣6️⃣ - 《踢出一个未来》🎶"
author: "®γσ ξηg <img src='figure/aaaa.jpg' width='16'>"
date: "2020-10-09"
output:
  html_document: 
    number_sections: yes
    toc: yes
    toc_depth: 4
    toc_float:
      collapsed: yes
      smooth_scroll: yes
    code_folding: hide
---

# ${\color{Black} \circledR} {\color{DarkGreen} \gamma} {\color{Red} \sigma} \; {\color{Blue} \xi} {\color{Red} \eta} {\color{Blue} g}$^[[Latex Equation](https://www.codecogs.com/latex/eqneditor.php)] 🎏^[[Emoji list](https://gist.github.com/rxaviers/7360908)]

<br>

![Quantitative Trading in Sportsbook and financial market](figure/soccer.jpg)

<audio src='music/kick-off a better future.mp3' controls></audio>

<br>

<iframe title="vimeo-player" src="https://player.vimeo.com/video/191464377" width="560" height="315" frameborder="0" allowfullscreen></iframe>

---


```r
## https://www.ip138.com/sj
if(!suppressPackageStartupMessages(require('BBmisc'))) {
  suppressWarnings(suppressMessages(install.packages('BBmisc')))
  }
require('BBmisc')
pkgs <- c('rvest', 'RSelenium', 'reticulate', 'tidyverse', 'magrittr', 
          'RCurl', 'plumber', 'bigQueryR', 'xts', 'forecast', 'purrr', 
          'googleCloudRunner', 'plyr', 'chinese.misc', 'knitr', 'tidyr', 
          'kableExtra', 'plyr', 'dplyr', 'devtools', 'XML', 'xml2', 'formattable')
suppressAll(lib(pkgs))
#(dependencies = TRUE, INSTALL_opts = '--no-lock')
lnk <- 'https://www.ip138.com/sj'
rm(pkgs)

#https://github.com/githubwwwjjj/chinese.misc
#options(tmp_chi_locale=NA)
options(tmp_chi_locale="Chinese (Traditional)_Taiwan.950")
```

<br>

## 中国电讯台

<br>

<figure>
  <img src='figure/中国电信《爱江山更爱美人》.jpg' alt='Trulli' style='width:30%'>
  <figcaption>《爱江山更爱美人》🎶

  <img src='figure/中国联通《皇帝的烦恼》.jpg' alt='Trulli' style='width:32%'>
  <figcaption>《皇帝的烦恼》🎶

  <img src='figure/中国移动《忘情水》.jpg' alt='Trulli' style='width:30%'>
  <figcaption>《忘情水》🎶
</figure>

<br>

## 手机号前三码一览

<br>

- [手机号码段及归属地查询规则](https://blog.csdn.net/pzasdq/article/details/50587428?utm_medium=distribute.pc_aggpage_search_result.none-task-blog-2~all~first_rank_v2~rank_v25-3-50587428.nonecase&utm_term=%E6%89%8B%E6%9C%BA%E5%8F%B7%E6%AE%B5%E6%9F%A5%E8%AF%A2&spm=1000.2123.3001.4430)
- [中国移动，联通，电信各手机号码前三位是多少？](https://zhidao.baidu.com/question/584943266.html)



```python
## https://rstudio.github.io/reticulate/articles/python_packages.html
# create a new environment 
conda_create()
mods <- c('scipy', 'numpy', 'pandas', 'lxml', 'bs4', 'selenium', 'time', 'string', 'random')
mods %>% llply(., function(x) {
    tryCatch({
        y <- conda_install(conda_list()[[1]][2], x)
        y <- import(y)
    }, error = function(e) {
        NULL
    })
})

# import SciPy (it will be automatically discovered in "r-reticulate")
modsip <- mods %>% llply(import)

# indicate that we want to use a specific condaenv
#use_condaenv(conda_list()[[1]][2])

# import SciPy (will use 'r-reticulate' as per call to use_condaenv)
#scipy <- import('scipy')
```


```python
from selenium import webdriver
from bs4 import BeautifulSoup
from webdriver_manager.chrome import ChromeDriverManager

driver = webdriver.Chrome(ChromeDriverManager().install())

url = 'https://www.imdb.com/search/title?release_date=2018&sort=num_votes,desc&page=1'

driver = webdriver.Chrome('/r-reticulate/chromedriver')
driver.get(url)
soup = BeautifulSoup(driver.page_source,"html.parser")

while True:
    items = [itm.get_text(strip=True) for itm in soup.select('.lister-item-p6482776ent a[href^="/title/"]')]
    print(items)

    try:
        driver.find_element_by_xpath('//a[p6482776ains(.,"Next")]').click()
        soup = BeautifulSoup(driver.page_source,"html.parser")
    except Exception: break
```

<br>
<br>

## 手机号查询

<br>

### 样本

<br>

[手机号码归属地查询](https://m.ip138.com/sj.asp)可以查询任何国内手机号。


```r
xxx <- read_html('view-source_https___m.ip138.com_sj.asp_mobile=1316482776.html') %>% 
    html_nodes('td') %>% 
    .[144:152] %>% 
    html_text(trim = T) %>% 
    str_replace_all('^<tr><td class=\"th\"|^<tr><td class=\"th\">|</td<td<span| width=\"36%\"|</td><td>|>|</span></td></tr>$|\\*', '') %>% 
    .[. != ''] %>% 
    str_split('<span')

xxx %>% data.frame %>% 
  t %>% data.frame %>% 
  as_tibble %>% 
  dplyr::rename('类别' = X1, '明细' = X2) %>% 
  kbl('html', caption = '手机号详情', escape = F, align = 'r') %>% 
  kable_styling(
    bootstrap_options = c('striped', 'hover', 'condensed', 'responsive')) %>% 
  scroll_box(height = '400px')
```

<div style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:400px; "><table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
<caption>手机号详情</caption>
 <thead>
  <tr>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> 类别 </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> 明细 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 手机号码段 </td>
   <td style="text-align:right;"> 1316482776 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 卡号归属地 </td>
   <td style="text-align:right;"> 福建 福州市 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 卡 类 型 </td>
   <td style="text-align:right;"> 联通预付费卡 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 区 号 </td>
   <td style="text-align:right;"> 0591 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 邮 编 </td>
   <td style="text-align:right;"> 350000 </td>
  </tr>
</tbody>
</table></div>

<br>

### 靓号


```python
import pandas as pd
import lxml

test = 'https://www.ip138.com/mobile.asp?mobile=1316482776&action=mobile'

tables = pd.read_html(test)
rankings = tables[-1]
rankings.iloc[:200]
```


```r
#https://m.ip138.com/sj.asp?mobile=1316482776
#lnk <- c('https://m.ip138.com/mobile.asp?mobile=', '&action=mobile')

i <- sprintf("%03d", 1:999) %>% 
  paste0(6482776)
lnk <- paste0('https://m.ip138.com/sj.asp?mobile=', i)
rm(i)

p6482776 <- lnk %>% 
  llply(., function(ii) {
    tryCatch({
      ii %>% getURL %>% 
        read_html %>% html_table()
    }, error = function(e) {
        NULL
    }) %>% unlist
  })

p6482776[sapply(p6482776, is.null)] <- NULL
```


```r
#https://m.ip138.com/sj.asp?mobile=1316482776
#lnk <- c('https://m.ip138.com/mobile.asp?mobile=', '&action=mobile')

i <- sprintf("%03d", 120:200) %>% 
  paste0(6482776)
lnk <- paste0('https://m.ip138.com/sj.asp?mobile=', i)
rm(i)

p6482776 <- lnk %>% 
  llply(., function(ii) {
    tryCatch({
      xxx <- ii %>% read_html() %>% 
        html_nodes('td') %>% 
        # .[144:152] %>% 
        html_text(trim = T)# %>% 
        #str_replace_all('^<tr><td class=\"th\"|^<tr><td class=\"th\">|</td<td<span| width=\"36%\"|</td><td>|>|</span></td></tr>$|\\*', '') %>% 
        #.[. != ''] %>% 
        #str_split('<span')

      #xxx %<>% data.frame %>% 
      #  t %>% 
      #  as_tibble %>% 
      #  dplyr::rename('种类' = V1, '明细' = V2)
      cat('\nGet"', ii, '"');
      xxx
    }, error = function(e) {
      cat('\n', ii, 'not exist'); 
      NULL
    })# %>% unlist
  })

p6482776[sapply(p6482776, is.null)] <- NULL
p6482776 <- p6482776[sapply(p6482776, function(x) length(x)>1)]

p6482776 %<>% llply(., function(x) {
    x %>% 
    str_replace_all('\\*', '') %>% 
    matrix(nc = 2, byrow = TRUE) %>% 
    data.frame %>% 
    as_tibble %>% 
    dplyr::rename('类别' = X1, '明细' = X2)
  })

p6482776 %<>% 
    ldply(., function(x) {
        x %>% t %>% data.frame %>% 
            mutate_at(1:ncol(.), funs(str_replace_all(., ' ', ''))) %>% 
        .[-1,]
  }) %>% as_tibble %>% 
    mutate(`手机号码段` = as.numeric(X1), 
           `卡号归属地` = factor(X2), 
           `卡类型` = factor(X3), 
           `区号` = factor(X4),
           `邮编` = as.numeric(X5)) %>% 
    .[-c(1:5)]

## https://rstudio-pubs-static.s3.amazonaws.com/406749_dc4af7298f934812b35b11631abb5ad5.html

saveRDS(p6482776, file = 'data/p6482776.rds')
```




```r
p6482776 <- read_rds('data/p6482776.rds')
p6482776 %>% 
  mutate(
    手机号码段 = color_tile('white', 'darkgrey')(手机号码段), 
    卡类型 = if_else(
      str_detect(卡类型, '联通'), 
  cell_spec(卡类型, background = 'black', color = 'red', italic = TRUE), 
    if_else(
      str_detect(卡类型, '电信'), 
      cell_spec(卡类型, background = 'blue', color = 'white', italic = TRUE), 
    if_else(
      str_detect(卡类型, '移动'), 
  cell_spec(卡类型, background = 'lightgreen', color = 'blue', italic = TRUE), 
  cell_spec(卡类型, background = 'gold', color = 'black'))
  ))) %>% 
  kbl('html', caption = '手机号详情', escape = F, align = 'r') %>% 
  kable_styling(
    bootstrap_options = c('striped', 'hover', 'condensed', 'responsive')) %>% 
  scroll_box(height = '400px')
```

<div style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:400px; "><table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
<caption>手机号详情</caption>
 <thead>
  <tr>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> 手机号码段 </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> 卡号归属地 </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> 卡类型 </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> 区号 </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> 邮编 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffffff">1306482776</span> </td>
   <td style="text-align:right;"> 江苏宿迁市 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: red !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: black !important;">联通130卡</span> </td>
   <td style="text-align:right;"> 0527 </td>
   <td style="text-align:right;"> 223800 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #fdfdfd">1316482776</span> </td>
   <td style="text-align:right;"> 福建福州市 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: red !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: black !important;">联通预付费卡</span> </td>
   <td style="text-align:right;"> 0591 </td>
   <td style="text-align:right;"> 350000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #fcfcfc">1326482776</span> </td>
   <td style="text-align:right;"> 湖北黄冈市 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: red !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: black !important;">联通130卡</span> </td>
   <td style="text-align:right;"> 0713 </td>
   <td style="text-align:right;"> 438000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #fbfbfb">1336482776</span> </td>
   <td style="text-align:right;"> 新疆博尔塔拉蒙古自治州 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: blue !important;">电信CDMA卡</span> </td>
   <td style="text-align:right;"> 0909 </td>
   <td style="text-align:right;"> 833400 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #fafafa">1346482776</span> </td>
   <td style="text-align:right;"> 辽宁阜新市 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: blue !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightgreen !important;">移动全球通卡</span> </td>
   <td style="text-align:right;"> 0418 </td>
   <td style="text-align:right;"> 123000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #f8f8f8">1356482776</span> </td>
   <td style="text-align:right;"> 上海 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: blue !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightgreen !important;">移动全球通卡</span> </td>
   <td style="text-align:right;"> 021 </td>
   <td style="text-align:right;"> 200000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #f7f7f7">1366482776</span> </td>
   <td style="text-align:right;"> 内蒙古兴安盟 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: blue !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightgreen !important;">移动全球通卡</span> </td>
   <td style="text-align:right;"> 0482 </td>
   <td style="text-align:right;"> 137400 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #f6f6f6">1376482776</span> </td>
   <td style="text-align:right;"> 上海 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: blue !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightgreen !important;">移动全球通卡</span> </td>
   <td style="text-align:right;"> 021 </td>
   <td style="text-align:right;"> 200000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #f5f5f5">1386482776</span> </td>
   <td style="text-align:right;"> 山东青岛市 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: blue !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightgreen !important;">移动全球通卡</span> </td>
   <td style="text-align:right;"> 0532 </td>
   <td style="text-align:right;"> 266000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #f3f3f3">1396482776</span> </td>
   <td style="text-align:right;"> 山东青岛市 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: blue !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightgreen !important;">移动全球通卡</span> </td>
   <td style="text-align:right;"> 0532 </td>
   <td style="text-align:right;"> 266000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #f2f2f2">1406482776</span> </td>
   <td style="text-align:right;"> 未知 </td>
   <td style="text-align:right;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: gold !important;">未知</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #f1f1f1">1416482776</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: blue !important;">电信</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #f0f0f0">1426482776</span> </td>
   <td style="text-align:right;"> 未知 </td>
   <td style="text-align:right;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: gold !important;">未知</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #eeeeee">1436482776</span> </td>
   <td style="text-align:right;"> 未知 </td>
   <td style="text-align:right;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: gold !important;">未知</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ededed">1446482776</span> </td>
   <td style="text-align:right;"> 未知 </td>
   <td style="text-align:right;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: gold !important;">未知</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ececec">1456482776</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: red !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: black !important;">联通</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ebebeb">1466482776</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: red !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: black !important;">联通</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #e9e9e9">1476482776</span> </td>
   <td style="text-align:right;"> 湖南长沙市 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: blue !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightgreen !important;">移动数据卡</span> </td>
   <td style="text-align:right;"> 0731 </td>
   <td style="text-align:right;"> 410000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #e8e8e8">1486482776</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: blue !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightgreen !important;">移动</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #e7e7e7">1496482776</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: blue !important;">电信</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #e6e6e6">1506482776</span> </td>
   <td style="text-align:right;"> 山东青岛市 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: blue !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightgreen !important;">移动150卡</span> </td>
   <td style="text-align:right;"> 0532 </td>
   <td style="text-align:right;"> 266000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #e4e4e4">1516482776</span> </td>
   <td style="text-align:right;"> 内蒙古巴彦淖尔市 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: blue !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightgreen !important;">移动151卡</span> </td>
   <td style="text-align:right;"> 0478 </td>
   <td style="text-align:right;"> 15000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #e3e3e3">1526482776</span> </td>
   <td style="text-align:right;"> 山东泰安市 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: blue !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightgreen !important;">移动152卡</span> </td>
   <td style="text-align:right;"> 0538 </td>
   <td style="text-align:right;"> 271000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #e2e2e2">1536482776</span> </td>
   <td style="text-align:right;"> 山西大同市 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: blue !important;">电信CDMA卡</span> </td>
   <td style="text-align:right;"> 0352 </td>
   <td style="text-align:right;"> 37000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #e1e1e1">1546482776</span> </td>
   <td style="text-align:right;"> 未知 </td>
   <td style="text-align:right;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: gold !important;">未知</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #dfdfdf">1556482776</span> </td>
   <td style="text-align:right;"> 山东泰安市 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: red !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: black !important;">山东联通GSM卡</span> </td>
   <td style="text-align:right;"> 0538 </td>
   <td style="text-align:right;"> 271000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #dedede">1566482776</span> </td>
   <td style="text-align:right;"> 陕西西安市 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: red !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: black !important;">联通130卡</span> </td>
   <td style="text-align:right;"> 029 </td>
   <td style="text-align:right;"> 710000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #dddddd">1576482776</span> </td>
   <td style="text-align:right;"> 内蒙古兴安盟 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: blue !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightgreen !important;">移动157卡</span> </td>
   <td style="text-align:right;"> 0482 </td>
   <td style="text-align:right;"> 137400 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #dcdcdc">1586482776</span> </td>
   <td style="text-align:right;"> 山东临沂市 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: blue !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightgreen !important;">移动动感地带卡</span> </td>
   <td style="text-align:right;"> 0539 </td>
   <td style="text-align:right;"> 276000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #dadada">1596482776</span> </td>
   <td style="text-align:right;"> 山东临沂市 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: blue !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightgreen !important;">移动全球通卡</span> </td>
   <td style="text-align:right;"> 0539 </td>
   <td style="text-align:right;"> 276000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #d9d9d9">1606482776</span> </td>
   <td style="text-align:right;"> 未知 </td>
   <td style="text-align:right;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: gold !important;">未知</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #d8d8d8">1616482776</span> </td>
   <td style="text-align:right;"> 未知 </td>
   <td style="text-align:right;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: gold !important;">未知</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #d7d7d7">1626482776</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: blue !important;">电信</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #d5d5d5">1636482776</span> </td>
   <td style="text-align:right;"> 未知 </td>
   <td style="text-align:right;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: gold !important;">未知</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #d4d4d4">1646482776</span> </td>
   <td style="text-align:right;"> 未知 </td>
   <td style="text-align:right;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: gold !important;">未知</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #d3d3d3">1656482776</span> </td>
   <td style="text-align:right;"> 内蒙古兴安盟 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: blue !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightgreen !important;">华翔联信移动</span> </td>
   <td style="text-align:right;"> 0482 </td>
   <td style="text-align:right;"> 137400 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #d2d2d2">1666482776</span> </td>
   <td style="text-align:right;"> 内蒙古兴安盟 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: red !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: black !important;">联通</span> </td>
   <td style="text-align:right;"> 0482 </td>
   <td style="text-align:right;"> 137400 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #d0d0d0">1676482776</span> </td>
   <td style="text-align:right;"> 内蒙古兴安盟 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: red !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: black !important;">联通</span> </td>
   <td style="text-align:right;"> 0482 </td>
   <td style="text-align:right;"> 137400 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #cfcfcf">1686482776</span> </td>
   <td style="text-align:right;"> 未知 </td>
   <td style="text-align:right;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: gold !important;">未知</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #cecece">1696482776</span> </td>
   <td style="text-align:right;"> 未知 </td>
   <td style="text-align:right;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: gold !important;">未知</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #cdcdcd">1706482776</span> </td>
   <td style="text-align:right;"> 广西桂林市 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: blue !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightgreen !important;">虚拟运营商移动</span> </td>
   <td style="text-align:right;"> 0773 </td>
   <td style="text-align:right;"> 541000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #cbcbcb">1716482776</span> </td>
   <td style="text-align:right;"> 广东东莞市 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: red !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: black !important;">虚拟运营商联通</span> </td>
   <td style="text-align:right;"> 0769 </td>
   <td style="text-align:right;"> 523000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #cacaca">1726482776</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: blue !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightgreen !important;">移动</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #c9c9c9">1736482776</span> </td>
   <td style="text-align:right;"> 四川成都市 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: blue !important;">电信173卡</span> </td>
   <td style="text-align:right;"> 028 </td>
   <td style="text-align:right;"> 610000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #c8c8c8">1746482776</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: blue !important;">电信</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #c6c6c6">1756482776</span> </td>
   <td style="text-align:right;"> 河南新乡市 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: red !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: black !important;">联通175卡</span> </td>
   <td style="text-align:right;"> 0373 </td>
   <td style="text-align:right;"> 453000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #c5c5c5">1766482776</span> </td>
   <td style="text-align:right;"> 广东广州市 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: red !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: black !important;">联通176卡</span> </td>
   <td style="text-align:right;"> 020 </td>
   <td style="text-align:right;"> 510000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #c4c4c4">1776482776</span> </td>
   <td style="text-align:right;"> 重庆 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: blue !important;">电信177卡</span> </td>
   <td style="text-align:right;"> 023 </td>
   <td style="text-align:right;"> 400000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #c3c3c3">1786482776</span> </td>
   <td style="text-align:right;"> 山东泰安市 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: blue !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightgreen !important;">移动178卡</span> </td>
   <td style="text-align:right;"> 0538 </td>
   <td style="text-align:right;"> 271000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #c1c1c1">1796482776</span> </td>
   <td style="text-align:right;"> 未知 </td>
   <td style="text-align:right;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: gold !important;">未知</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #c0c0c0">1806482776</span> </td>
   <td style="text-align:right;"> 云南昆明市 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: blue !important;">云南电信CDMA卡</span> </td>
   <td style="text-align:right;"> 0871 </td>
   <td style="text-align:right;"> 650000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #bfbfbf">1816482776</span> </td>
   <td style="text-align:right;"> 贵州贵阳市 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: blue !important;">中国电信天翼卡</span> </td>
   <td style="text-align:right;"> 0851 </td>
   <td style="text-align:right;"> 550000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #bebebe">1826482776</span> </td>
   <td style="text-align:right;"> 山东泰安市 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: blue !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightgreen !important;">移动182卡</span> </td>
   <td style="text-align:right;"> 0538 </td>
   <td style="text-align:right;"> 271000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #bcbcbc">1836482776</span> </td>
   <td style="text-align:right;"> 山东泰安市 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: blue !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightgreen !important;">移动183卡</span> </td>
   <td style="text-align:right;"> 0538 </td>
   <td style="text-align:right;"> 271000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #bbbbbb">1846482776</span> </td>
   <td style="text-align:right;"> 山东泰安市 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: blue !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightgreen !important;">移动184卡</span> </td>
   <td style="text-align:right;"> 0538 </td>
   <td style="text-align:right;"> 271000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #bababa">1856482776</span> </td>
   <td style="text-align:right;"> 山东临沂市 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: red !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: black !important;">联通185卡</span> </td>
   <td style="text-align:right;"> 0539 </td>
   <td style="text-align:right;"> 276000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #b9b9b9">1866482776</span> </td>
   <td style="text-align:right;"> 广东广州市 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: red !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: black !important;">联通186卡</span> </td>
   <td style="text-align:right;"> 020 </td>
   <td style="text-align:right;"> 510000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #b7b7b7">1876482776</span> </td>
   <td style="text-align:right;"> 山东泰安市 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: blue !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightgreen !important;">移动187卡</span> </td>
   <td style="text-align:right;"> 0538 </td>
   <td style="text-align:right;"> 271000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #b6b6b6">1886482776</span> </td>
   <td style="text-align:right;"> 山东泰安市 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: blue !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightgreen !important;">移动188卡</span> </td>
   <td style="text-align:right;"> 0538 </td>
   <td style="text-align:right;"> 271000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #b5b5b5">1896482776</span> </td>
   <td style="text-align:right;"> 上海 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: blue !important;">中国电信天翼卡</span> </td>
   <td style="text-align:right;"> 021 </td>
   <td style="text-align:right;"> 200000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #b4b4b4">1906482776</span> </td>
   <td style="text-align:right;"> 未知 </td>
   <td style="text-align:right;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: gold !important;">未知</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #b2b2b2">1916482776</span> </td>
   <td style="text-align:right;"> 湖南益阳市 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: blue !important;">电信</span> </td>
   <td style="text-align:right;"> 0737 </td>
   <td style="text-align:right;"> 413000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #b1b1b1">1926482776</span> </td>
   <td style="text-align:right;"> 未知 </td>
   <td style="text-align:right;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: gold !important;">未知</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #b0b0b0">1936482776</span> </td>
   <td style="text-align:right;"> 未知 </td>
   <td style="text-align:right;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: gold !important;">未知</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #afafaf">1946482776</span> </td>
   <td style="text-align:right;"> 未知 </td>
   <td style="text-align:right;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: gold !important;">未知</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #adadad">1956482776</span> </td>
   <td style="text-align:right;"> 未知 </td>
   <td style="text-align:right;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: gold !important;">未知</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #acacac">1966482776</span> </td>
   <td style="text-align:right;"> 未知 </td>
   <td style="text-align:right;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: gold !important;">未知</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ababab">1976482776</span> </td>
   <td style="text-align:right;"> 未知 </td>
   <td style="text-align:right;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: gold !important;">未知</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #aaaaaa">1986482776</span> </td>
   <td style="text-align:right;"> 广东广州市 </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: blue !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightgreen !important;">移动</span> </td>
   <td style="text-align:right;"> 020 </td>
   <td style="text-align:right;"> 510000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #a9a9a9">1996482776</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> <span style="  font-style: italic;   color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: blue !important;">电信</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> NA </td>
  </tr>
</tbody>
</table></div>

<br>

## 买号网

- [手机靓号网（网页版）](http://www.1686888.com)
- [手机靓号网（手机版）](http://m.1686888.com)

<br>
<br>

## 手机号维护网站

<br>

![[https://www.ding.com](https://www.ding.com/)可以设定7天、14天、28天、30天自动充值，跨越全球手机号。](figure/ding.png)

<br>
<br>

# 结论

<br>

## 年鉴

<br>

- [科学大家|赌博的乐趣与挑战：不确定性与统计推断](https://tech.sina.cn/scientist/2018-09-11/detail-ihiixyeu6001744.d.html)
- [年鉴｜概率论与数理统计的前世今生](https://www.jianshu.com/p/37158001f8df)

<iframe width="560" height="315" src="https://www.youtube.com/embed/09nf9O3IqDg?start=16&amp;end=41" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

<iframe width="560" height="315" src="https://www.youtube.com/embed/09nf9O3IqDg?start=84&amp;end=114" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

<iframe width="560" height="315" src="https://www.youtube.com/embed/09nf9O3IqDg?start=174&amp;end=219" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

<iframe width="560" height="315" src="https://www.youtube.com/embed/09nf9O3IqDg?start=305&amp;end=342" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

<iframe width="560" height="315" src="https://www.youtube.com/embed/09nf9O3IqDg?start=376&amp;end=410" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

<iframe width="560" height="315" src="https://www.youtube.com/embed/09nf9O3IqDg?start=526&amp;end=553" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

<iframe width="560" height="315" src="https://www.youtube.com/embed/09nf9O3IqDg?start=580&amp;end=606" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

<iframe width="560" height="315" src="https://www.youtube.com/embed/09nf9O3IqDg?start=636&amp;end=666" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

<iframe width="560" height="315" src="https://www.youtube.com/embed/09nf9O3IqDg?start=702&amp;end=728" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

<iframe width="560" height="315" src="https://www.youtube.com/embed/09nf9O3IqDg?start=785&amp;end=820" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

<iframe width="560" height="315" src="https://www.youtube.com/embed/09nf9O3IqDg?start=850&amp;end=880" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

<iframe width="560" height="315" src="https://www.youtube.com/embed/09nf9O3IqDg?start=895&amp;end=919" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

<iframe width="560" height="315" src="https://www.youtube.com/embed/09nf9O3IqDg?start=1092&amp;end=1123" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

<iframe width="560" height="315" src="https://www.youtube.com/embed/09nf9O3IqDg?start=1140&amp;end=1172" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

<iframe width="560" height="315" src="https://www.youtube.com/embed/09nf9O3IqDg?start=1180&amp;end=1207" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

<iframe width="560" height="315" src="https://www.youtube.com/embed/09nf9O3IqDg?start=1239&amp;end=1256" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

<iframe width="560" height="315" src="https://www.youtube.com/embed/09nf9O3IqDg?start=1268&amp;end=1315" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

## 附录


```r
suppressMessages(require('dplyr', quietly = TRUE))
suppressMessages(require('magrittr', quietly = TRUE))
suppressMessages(require('formattable', quietly = TRUE))
suppressMessages(require('knitr', quietly = TRUE))
suppressMessages(require('kableExtra', quietly = TRUE))

sys1 <- devtools::session_info()$platform %>% 
  unlist %>% data.frame(Category = names(.), session_info = .)
rownames(sys1) <- NULL

sys2 <- data.frame(Sys.info()) %>% mutate(Category = rownames(.)) %>% .[2:1]
names(sys2)[2] <- c('Sys.info')
rownames(sys2) <- NULL

if (nrow(sys1) == 9 & nrow(sys2) == 8) {
  sys2 %<>% rbind(., data.frame(
  Category = 'Current time', 
  Sys.info = paste(as.character(lubridate::now('Asia/Tokyo')), 'JST🗾')))
} else {
  sys1 %<>% rbind(., data.frame(
  Category = 'Current time', 
  session_info = paste(as.character(lubridate::now('Asia/Tokyo')), 'JST🗾')))
}

cbind(sys1, sys2) %>% 
  kbl(caption = 'Additional session information:') %>% 
  kable_styling(bootstrap_options = c('striped', 'hover', 'condensed', 'responsive')) %>% 
  row_spec(9, bold = T, color = 'white', background = '#D7261E')
```

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
<caption>Additional session information:</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Category </th>
   <th style="text-align:left;"> session_info </th>
   <th style="text-align:left;"> Category </th>
   <th style="text-align:left;"> Sys.info </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> version </td>
   <td style="text-align:left;"> R version 4.0.2 (2020-06-22) </td>
   <td style="text-align:left;"> sysname </td>
   <td style="text-align:left;"> Linux </td>
  </tr>
  <tr>
   <td style="text-align:left;"> os </td>
   <td style="text-align:left;"> Ubuntu 16.04.6 LTS </td>
   <td style="text-align:left;"> release </td>
   <td style="text-align:left;"> 5.3.0-1017-aws </td>
  </tr>
  <tr>
   <td style="text-align:left;"> system </td>
   <td style="text-align:left;"> x86_64, linux-gnu </td>
   <td style="text-align:left;"> version </td>
   <td style="text-align:left;"> #18~18.04.1-Ubuntu SMP Wed Apr 8 15:12:16 UTC 2020 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ui </td>
   <td style="text-align:left;"> X11 </td>
   <td style="text-align:left;"> nodename </td>
   <td style="text-align:left;"> application-2789879-deployment-7199315-dtltv </td>
  </tr>
  <tr>
   <td style="text-align:left;"> language </td>
   <td style="text-align:left;"> (EN) </td>
   <td style="text-align:left;"> machine </td>
   <td style="text-align:left;"> x86_64 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> collate </td>
   <td style="text-align:left;"> C.UTF-8 </td>
   <td style="text-align:left;"> login </td>
   <td style="text-align:left;"> unknown </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ctype </td>
   <td style="text-align:left;"> C.UTF-8 </td>
   <td style="text-align:left;"> user </td>
   <td style="text-align:left;"> rstudio-user </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tz </td>
   <td style="text-align:left;"> Etc/UTC </td>
   <td style="text-align:left;"> effective_user </td>
   <td style="text-align:left;"> rstudio-user </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;color: white !important;background-color: #D7261E !important;"> date </td>
   <td style="text-align:left;font-weight: bold;color: white !important;background-color: #D7261E !important;"> 2020-10-08 </td>
   <td style="text-align:left;font-weight: bold;color: white !important;background-color: #D7261E !important;"> Current time </td>
   <td style="text-align:left;font-weight: bold;color: white !important;background-color: #D7261E !important;"> 2020-10-09 07:48:06 JST🗾 </td>
  </tr>
</tbody>
</table>

---

<span style='color:RoyalBlue'>**Powered by - Copyright® Intellectual Property Rights of <img src='figure/scibrokes_trading.jpg' width='24'> [Sςιβrοκεrs Trαdιηg®️](http://www.scibrokes.com)経営企業**</span>
