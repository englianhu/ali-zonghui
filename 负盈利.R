## https://stackoverflow.com/a/45344291
require('plyr')
require('dplyr')
pkg <- c('XML', 'RCurl', 'RSelenium', 'rvest', 'decryptr', 
         'wdman', 'webshot', 'tesseract', 'magick', 
         'opencv')
l_ply(pkg, require, quietly = TRUE)
rm(pkg)

## 网址
## 登录网址
lnk <- 'https://m.a80802.com:8760/login'
## 彩种一栏
lnkcz <- 'https://m.cjezllpcecbm.com/allLottery/?source=lottery'
## 1分快三
lnk1k3 <- 'https://m.cjezllpcecbm.com/history/K3/OG1K3/?source=lottery'
## 3分快三
lnk3k3 <- 'https://m.cjezllpcecbm.com/history/K3/OG3K3/?source=lottery'
## 1分快三大小走势
lnk1k3dx <- 'https://m.cjezllpcecbm.com/zoushi/K3/OG1K3/daxiaoratio/?source=lottery'

#lnk1k3 %>% 
#  read_html %>% 
#  html_nodes(xpath = '//*[@id="scrollContainer"]/div[2]') %>% 
#  html_table()

## 打开隐藏浏览器
pDrv <- wdman::phantomjs(port = 4567L, check = FALSE, verbose = FALSE)
remDr <- remoteDriver(browserName = 'phantomjs', port = 4567L)
remDr$open(silent = TRUE)

## 浏览网站
remDr$navigate(lnk)

## 输入账号
webElem <- remDr$findElement(using = 'xpath', value = '//*[@id="app"]/div/div[1]/div[3]/div/table/tbody/tr[1]/td[2]/input')
webElem$clickElement()
webElem$sendKeysToElement(list('leiou004', key = 'enter'))

## 输入密码
webElem <- remDr$findElement(using = 'xpath', value = '//*[@id="app"]/div/div[1]/div[3]/div/table/tbody/tr[2]/td[2]/input')
webElem$clickElement()
webElem$sendKeysToElement(list('leiou123', key = 'enter'))

## 验证码
## https://github.com/decryptr/decryptr
## https://rpubs.com/johndharrison/14707
webElem <- remDr$findElement(using = 'xpath', value = '//*[@id="geetest"]/div/div[2]/div[1]/div[3]')
webElem$clickElement()
webElem$sendKeysToElement(list(key = 'enter'))
#webElem$switchToFrame('div.geetest_popup_box')
hml <- remDr$getPageSource()[[1]] %>% 
  read_html %>% 
  html_node('img') %>%
  html_attr('src')

#imgsmp <- "https://static.geetest.com/nerualpic/word_l1_zh_2020.03.16/abstract/cef6553e85b4ca453be275041172b8a9.jpg?challenge=f3ae5840d4cf49be0526a3afe8c355da"
#imgsmp %>% str_extract_all('.+.jpg') %>% .[[1]]
#https://static.geetest.com/nerualpic/word_l1_zh_2020.03.16/abstract/cef6553e85b4ca453be275041172b8a9.jpg
cpt <- paste0('./img/', dir('./img'))

## remotes::install_github("ropensci/tesseract")
## https://ropensci.org/technotes/2017/08/17/tesseract-16/
#cpt <- download_captcha(hml, n = 1, path = "./img")
hml %>% image_read %>%
  image_trim %>%
  image_ocr

#reticulate::conda_install('tesserocr', envname = 'myenv', pip = TRUE)
#install.packages('rcloud.support', repos=c('http://RForge.net', 'http://R.research.att.com'), type='source')
#sudo apt-get install -y libtesseract-dev libleptonica-dev tesseract-ocr-eng
#conda_install('pytesseract', envname = 'myenv', pip = TRUE)

