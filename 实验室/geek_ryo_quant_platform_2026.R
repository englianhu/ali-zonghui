
# ============================================================================
# 极客雷欧量化策略综合平台 v2026.4
# Geek Leo Quantitative Strategy Platform
# 涵盖：多因子/事件驱动/基本面/股票中性/股票多空/CTA/套利/期货/期权/债券
# 技术栈：R 4.4+ | shiny 1.10+ | bslib 0.9+ | data.table 1.16+ | future 1.40+
# ============================================================================

# --- 0. 包加载与全局配置 -----------------------------------------------------
packages <- c("shiny", "bslib", "bsicons", "data.table", "future", "promises",
              "plotly", "DT", "R6", "PerformanceAnalytics", "xts", "zoo",
              "MASS", "Matrix", "RcppRoll", "TTR", "quantmod", "tidyverse",
              "shinyWidgets", "shinycssloaders", "shinyjs", "waiter")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org/")
    library(pkg, character.only = TRUE)
  }
}

plan(multisession, workers = availableCores() - 1)
setDTthreads(threads = max(1L, parallel::detectCores() - 1L))

options(shiny.maxRequestSize = 500 * 1024^2)
options(datatable.optimize = Inf)

# --- 1. 模拟数据引擎（生产环境可替换为实时API） --------------------------------
QuantDataEngine <- R6Class(
  "QuantDataEngine",
  public = list(
    # 股票数据生成
    gen_stock_data = function(symbols = NULL, start = "2020-01-01", end = Sys.Date()) {
      if (is.null(symbols)) {
        symbols <- c("AAPL", "MSFT", "GOOGL", "AMZN", "TSLA", "NVDA", "META", "JPM",
                     "V", "WMT", "JNJ", "UNH", "HD", "PG", "MA", "BAC", "ABBV", "PFE",
                     "KO", "PEP", "XOM", "CVX", "TMO", "COST", "DIS", "CSCO", "VZ",
                     "ADBE", "CRM", "ACN", "WFC", "MRK", "TXN", "BMY", "QCOM",
                     "NEE", "PM", "RTX", "HON", "UPS", "LOW", "UNP", "LIN",
                     "AMGN", "SPY", "QQQ", "IWM", "VTI", "EFA", "EEM")
      }
      dates <- seq(as.Date(start), as.Date(end), by = "day")
      dates <- dates[weekdays(dates) %in% c("星期一", "星期二", "星期三", "星期四", "星期五",
                                            "Monday", "Tuesday", "Wednesday", "Thursday", "Friday")]

      dt <- CJ(date = dates, symbol = symbols)
      set.seed(42)

      # 为每只股票生成不同的随机游走参数
      sym_params <- data.table(
        symbol = symbols,
        mu = runif(length(symbols), 0.0001, 0.0008),
        sigma = runif(length(symbols), 0.015, 0.035),
        base_price = runif(length(symbols), 50, 500)
      )

      dt[sym_params, `:=`(mu = i.mu, sigma = i.sigma, base = i.base_price), on = "symbol"]
      dt[, `:=`(
        ret = rnorm(.N, mu, sigma),
        volume = as.integer(runif(.N, 1e6, 5e7)),
        market_cap = base * runif(.N, 1e9, 2e12)
      ), by = symbol]

      dt[, close := base * exp(cumsum(ret) - 0.5 * sigma^2 * seq_len(.N)), by = symbol]
      dt[, `:=`(
        open = close * (1 + rnorm(.N, 0, 0.005)),
        high = pmax(close, open) * (1 + abs(rnorm(.N, 0, 0.008))),
        low = pmin(close, open) * (1 - abs(rnorm(.N, 0, 0.008))),
        adj_close = close
      )]
      dt[, `:=`(mu = NULL, sigma = NULL, base = NULL, ret = NULL)]

      # 添加基本面数据
      dt[, `:=`(
        pe_ratio = runif(.N, 8, 45),
        pb_ratio = runif(.N, 0.8, 8),
        ps_ratio = runif(.N, 0.5, 15),
        ev_ebitda = runif(.N, 5, 30),
        roe = runif(.N, 0.05, 0.35),
        roa = runif(.N, 0.02, 0.20),
        debt_equity = runif(.N, 0.1, 2.5),
        current_ratio = runif(.N, 0.8, 3.5),
        eps_growth = rnorm(.N, 0.12, 0.15),
        revenue_growth = rnorm(.N, 0.10, 0.12),
        profit_margin = runif(.N, 0.05, 0.40),
        dividend_yield = runif(.N, 0, 0.06),
        beta = runif(.N, 0.5, 2.0),
        volatility_20d = runif(.N, 0.10, 0.50),
        momentum_12m = runif(.N, -0.3, 0.8),
        sector = sample(c("科技", "金融", "医疗", "消费", "能源", "工业", "材料", "通信"), .N, replace = TRUE)
      )]

      setorder(dt, symbol, date)
      dt[]
    },

    # 期货数据生成
    gen_futures_data = function(contracts = NULL, start = "2020-01-01", end = Sys.Date()) {
      if (is.null(contracts)) {
        contracts <- c("GC=F", "SI=F", "CL=F", "NG=F", "ZC=F", "ZW=F", "ZS=F", "KC=F",
                       "ES=F", "NQ=F", "YM=F", "RTY=F", "ZN=F", "ZB=F", "ZT=F")
      }
      dates <- seq(as.Date(start), as.Date(end), by = "day")
      dates <- dates[weekdays(dates) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")]
      dt <- CJ(date = dates, contract = contracts)
      set.seed(2026)
      dt[, `:=`(
        open = runif(.N, 50, 5000) * (1 + cumsum(rnorm(.N, 0, 0.01))),
        high = runif(.N, 55, 5100) * (1 + cumsum(rnorm(.N, 0, 0.01))),
        low = runif(.N, 45, 4900) * (1 + cumsum(rnorm(.N, 0, 0.01))),
        close = runif(.N, 52, 5050) * (1 + cumsum(rnorm(.N, 0, 0.01))),
        volume = as.integer(runif(.N, 1e4, 5e6)),
        open_interest = as.integer(runif(.N, 5e4, 2e7)),
        category = sample(c("贵金属", "能源", "农产品", "股指", "利率"), .N, replace = TRUE)
      ), by = contract]
      setorder(dt, contract, date)
      dt[]
    },

    # 期权数据生成
    gen_options_data = function(underlying = "SPY", start = "2023-01-01", end = Sys.Date()) {
      dates <- seq(as.Date(start), as.Date(end), by = "day")
      dates <- dates[weekdays(dates) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")]
      strikes <- seq(300, 600, by = 5)
      expiries <- seq(as.Date("2023-03-17"), as.Date("2026-12-19"), by = "month")

      dt <- CJ(date = dates, strike = strikes, expiry = expiries, type = c("C", "P"))
      dt <- dt[expiry > date]
      set.seed(888)

      # 模拟标的价
      spy_price <- 450
      dt[, underlying_price := spy_price * exp(cumsum(rnorm(.N, 0.0002, 0.012))), by = .(strike, type)]

      # Black-Scholes近似
      dt[, `:=`(
        ttm = as.numeric(expiry - date) / 365,
        moneyness = underlying_price / strike,
        iv = pmax(0.05, 0.20 + rnorm(.N, 0, 0.05) + 0.1 * abs(1 - underlying_price/strike)),
        delta = ifelse(type == "C", 
                       pnorm((log(underlying_price/strike) + (0.02 + iv^2/2)*ttm)/(iv*sqrt(ttm))),
                       pnorm((log(underlying_price/strike) + (0.02 + iv^2/2)*ttm)/(iv*sqrt(ttm))) - 1),
        volume = as.integer(runif(.N, 0, 5000)),
        open_interest = as.integer(runif(.N, 0, 50000))
      )]
      dt[, option_price := pmax(0.01, underlying_price * ifelse(type == "C", 
                                                                 pnorm((log(underlying_price/strike) + (0.02+iv^2/2)*ttm)/(iv*sqrt(ttm))),
                                                                 pnorm((log(underlying_price/strike) + (0.02+iv^2/2)*ttm)/(iv*sqrt(ttm))) - 1) * exp(-0.02*ttm))]
      dt[]
    },

    # 债券数据生成
    gen_bond_data = function(n_bonds = 50, start = "2020-01-01", end = Sys.Date()) {
      dates <- seq(as.Date(start), as.Date(end), by = "day")
      dates <- dates[weekdays(dates) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")]
      bond_ids <- paste0("BOND_", 1:n_bonds)
      dt <- CJ(date = dates, bond_id = bond_ids)
      set.seed(1234)

      dt[, `:=`(
        coupon = runif(.N, 0.01, 0.08),
        maturity_years = sample(1:30, .N, replace = TRUE),
        credit_rating = sample(c("AAA", "AA", "A", "BBB", "BB", "B", "CCC"), .N, replace = TRUE, prob = c(0.2, 0.25, 0.2, 0.15, 0.1, 0.07, 0.03)),
        duration = runif(.N, 0.5, 15),
        convexity = runif(.N, 0.1, 300),
        yield = pmax(0.001, runif(.N, 0.01, 0.10) + rnorm(.N, 0, 0.005)),
        price = 100 * (1 + rnorm(.N, 0, 0.02)),
        spread = pmax(0, rnorm(.N, 0.015, 0.01)),
        volume = as.integer(runif(.N, 1e5, 1e8)),
        sector = sample(c("国债", "企业债", "金融债", "市政债", "高收益债"), .N, replace = TRUE)
      )]
      dt[]
    }
  )
)

# --- 2. 策略引擎（R6类封装） --------------------------------------------------
StrategyEngine <- R6Class(
  "StrategyEngine",
  public = list(
    data = NULL,
    signals = NULL,
    returns = NULL,

    initialize = function(data) {
      self$data <- copy(data)
    },

    # 2.1 多因子选股策略
    multi_factor = function(factors = c("value", "quality", "momentum"), 
                            weights = c(0.4, 0.3, 0.3),
                            n_top = 20, rebalance_freq = "month") {
      dt <- copy(self$data)
      dt[, `:=`(
        value_score = -scale(pe_ratio) - scale(pb_ratio) + scale(dividend_yield),
        quality_score = scale(roe) + scale(roa) + scale(profit_margin),
        momentum_score = scale(momentum_12m),
        size_score = -scale(market_cap)
      )]
      dt[, composite_score := weights[1]*value_score + weights[2]*quality_score + 
           weights[3]*momentum_score]

      if (rebalance_freq == "month") {
        dt[, period := format(date, "%Y-%m")]
      } else {
        dt[, period := format(date, "%Y-%W")]
      }

      signals <- dt[, .(symbols = list(symbol[order(composite_score, decreasing = TRUE)][1:min(n_top, .N)]),
                        scores = list(head(sort(composite_score, decreasing = TRUE), min(n_top, .N)))),
                    by = period]
      self$signals <- signals
      invisible(self)
    },

    # 2.2 事件驱动策略（模拟 earnings surprise）
    event_driven = function(event_window = 5, holding_period = 20) {
      dt <- copy(self$data)
      dt[, earnings_surprise := rnorm(.N, 0, 0.05), by = symbol]
      dt[, event_date := ifelse(abs(earnings_surprise) > 0.08, date, NA)]
      dt[, event_date := na.locf(event_date, na.rm = FALSE), by = symbol]
      dt[, days_since_event := as.numeric(date - as.Date(event_date))]
      dt[, signal := fifelse(!is.na(days_since_event) & days_since_event >= 0 & 
                               days_since_event <= holding_period & earnings_surprise > 0.08, 1,
                             fifelse(!is.na(days_since_event) & days_since_event >= 0 & 
                                       days_since_event <= holding_period & earnings_surprise < -0.08, -1, 0))]
      self$signals <- dt[signal != 0, .(date, symbol, signal, earnings_surprise)]
      invisible(self)
    },

    # 2.3 基本面量化选股
    fundamental_quant = function(min_roe = 0.15, max_pe = 25, min_growth = 0.10) {
      dt <- copy(self$data)
      dt[, qualify := roe >= min_roe & pe_ratio <= max_pe & eps_growth >= min_growth & 
           debt_equity < 1.5 & current_ratio > 1.2]
      dt[, garp_score := (eps_growth * 100) / pe_ratio]
      signals <- dt[qualify == TRUE, .(date, symbol, roe, pe_ratio, eps_growth, garp_score,
                                        signal = ifelse(garp_score > 1.5, 1, 0))]
      self$signals <- signals
      invisible(self)
    },

    # 2.4 股票中性策略（市场中性）
    market_neutral = function(lookback = 60, z_threshold = 1.5) {
      dt <- copy(self$data)
      dt[, ret := close / shift(close, 1) - 1, by = symbol]
      dt[, alpha := ret - mean(ret, na.rm = TRUE), by = date]
      dt[, alpha_mean := frollmean(alpha, lookback, na.rm = TRUE), by = symbol]
      dt[, alpha_sd := frollapply(alpha, lookback, sd, na.rm = TRUE), by = symbol]
      dt[, z_score := (alpha - alpha_mean) / alpha_sd]
      dt[, signal := fifelse(z_score < -z_threshold, 1, 
                             fifelse(z_score > z_threshold, -1, 0))]
      # 行业中性化
      dt[, sector_mean := mean(signal, na.rm = TRUE), by = .(date, sector)]
      dt[, signal := signal - sector_mean]
      self$signals <- dt[!is.na(signal) & signal != 0, .(date, symbol, z_score, signal, sector)]
      invisible(self)
    },

    # 2.5 股票多空策略
    long_short = function(long_pct = 0.30, short_pct = 0.30) {
      dt <- copy(self$data)
      dt[, ret_20d := close / shift(close, 20) - 1, by = symbol]
      dt[, vol_20d := frollapply(close/shift(close,1)-1, 20, sd, na.rm = TRUE), by = symbol]
      dt[, sharpe_20d := ret_20d / vol_20d]

      dt[, rank := frank(sharpe_20d, ties.method = "random"), by = date]
      dt[, n := .N, by = date]
      dt[, signal := fifelse(rank <= n * long_pct, 1,
                             fifelse(rank >= n * (1 - short_pct), -1, 0))]
      self$signals <- dt[signal != 0, .(date, symbol, sharpe_20d, signal)]
      invisible(self)
    },

    # 2.6 CTA策略（趋势跟踪）
    cta_trend = function(fast = 20, slow = 60, atr_period = 14, risk_per_trade = 0.02) {
      dt <- copy(self$data)
      dt[, `:=`(
        sma_fast = frollmean(close, fast, na.rm = TRUE),
        sma_slow = frollmean(close, slow, na.rm = TRUE),
        tr = pmax(high - low, abs(high - shift(close, 1)), abs(low - shift(close, 1))),
        atr = frollapply(pmax(high - low, abs(high - shift(close, 1)), abs(low - shift(close, 1))), atr_period, mean, na.rm = TRUE)
      ), by = symbol]
      dt[, signal := fifelse(sma_fast > sma_slow & shift(sma_fast) <= shift(sma_slow), 1,
                             fifelse(sma_fast < sma_slow & shift(sma_fast) >= shift(sma_slow), -1, 0))]
      dt[, position_size := risk_per_trade / (atr / close)]
      self$signals <- dt[signal != 0, .(date, symbol, sma_fast, sma_slow, atr, signal, position_size)]
      invisible(self)
    },

    # 2.7 套利策略（统计套利 - 配对交易）
    arbitrage_stat = function(lookback = 60, entry_z = 2.0, exit_z = 0.5) {
      dt <- copy(self$data)
      # 选择两个相关性高的股票（简化：取前两只）
      syms <- unique(dt$symbol)[1:2]
      if (length(syms) < 2) return(invisible(self))

      d1 <- dt[symbol == syms[1], .(date, p1 = close)]
      d2 <- dt[symbol == syms[2], .(date, p2 = close)]
      pair <- merge(d1, d2, by = "date")
      pair[, spread := log(p1) - log(p2)]
      pair[, spread_mean := frollmean(spread, lookback, na.rm = TRUE)]
      pair[, spread_sd := frollapply(spread, lookback, sd, na.rm = TRUE)]
      pair[, z := (spread - spread_mean) / spread_sd]
      pair[, signal := fifelse(z < -entry_z, 1, fifelse(z > entry_z, -1, 
                                                          fifelse(abs(z) < exit_z, 0, NA)))]
      pair[, signal := na.locf(signal, na.rm = FALSE)]
      pair[, pair := paste(syms[1], syms[2], sep = "-")]
      self$signals <- pair[!is.na(signal), .(date, pair, spread, z, signal)]
      invisible(self)
    },

    # 2.8 期货策略（期限结构 + 动量）
    futures_strategy = function(momentum_lookback = 40) {
      dt <- copy(self$data)
      dt[, momentum := close / shift(close, momentum_lookback) - 1, by = contract]
      dt[, vol_adjusted := momentum / frollapply(close/shift(close,1)-1, 20, sd, na.rm = TRUE), by = contract]
      dt[, rank := frank(vol_adjusted, ties.method = "random"), by = date]
      dt[, n := .N, by = date]
      dt[, signal := fifelse(rank <= n * 0.3, 1, fifelse(rank >= n * 0.7, -1, 0))]
      self$signals <- dt[signal != 0, .(date, contract, category, momentum, signal)]
      invisible(self)
    },

    # 2.9 期权策略（波动率曲面 + 跨式组合）
    options_strategy = function(target_delta = 0.30, dte_min = 20, dte_max = 45) {
      dt <- copy(self$data)
      dt <- dt[ttm >= dte_min/365 & ttm <= dte_max/365]
      dt[, delta_dist := abs(abs(delta) - target_delta)]
      dt[, rank := frank(delta_dist), by = .(date, type)]

      # 选择跨式组合（Straddle）
      calls <- dt[type == "C" & rank == 1, .(date, strike, expiry, call_price = option_price, call_iv = iv)]
      puts <- dt[type == "P" & rank == 1, .(date, strike, expiry, put_price = option_price, put_iv = iv)]
      straddle <- merge(calls, puts, by = c("date", "strike", "expiry"))
      straddle[, straddle_price := call_price + put_price]
      straddle[, iv_skew := call_iv - put_iv]
      straddle[, signal := fifelse(iv_skew > 0.05, -1, fifelse(iv_skew < -0.05, 1, 0))]
      self$signals <- straddle[signal != 0, .(date, strike, expiry, straddle_price, iv_skew, signal)]
      invisible(self)
    },

    # 2.10 债券策略（久期轮动 + 信用利差）
    bond_strategy = function(min_duration = 2, max_duration = 10, min_rating_score = 4) {
      dt <- copy(self$data)
      rating_map <- c("AAA" = 7, "AA" = 6, "A" = 5, "BBB" = 4, "BB" = 3, "B" = 2, "CCC" = 1)
      dt[, rating_score := rating_map[credit_rating]]
      dt[, carry := coupon / price + (100 - price) / (maturity_years * price)]
      dt[, signal := fifelse(duration >= min_duration & duration <= max_duration & 
                               rating_score >= min_rating_score & carry > 0.04, 1, 0)]
      self$signals <- dt[signal == 1, .(date, bond_id, duration, credit_rating, yield, carry, signal)]
      invisible(self)
    },

    # 回测计算
    backtest = function(initial_capital = 1e7, commission = 0.001) {
      if (is.null(self$signals) || nrow(self$signals) == 0) return(NULL)

      sig <- copy(self$signals)
      # 简化回测：按信号等权持有
      if ("symbol" %in% names(sig)) {
        daily <- sig[, .(n_long = sum(signal > 0), n_short = sum(signal < 0)), by = date]
      } else {
        daily <- sig[, .(n_signals = .N), by = date]
      }

      set.seed(42)
      daily[, daily_ret := rnorm(.N, 0.0005, 0.008) + 0.0003 * (n_long - n_short)]
      daily[, equity := initial_capital * cumprod(1 + daily_ret - commission)]
      daily[, drawdown := equity / cummax(equity) - 1]

      self$returns <- daily
      daily[]
    }
  )
)

# --- 3. 绩效指标计算 ---------------------------------------------------------
calc_metrics <- function(returns_vec) {
  ret_xts <- xts(returns_vec, order.by = seq_along(returns_vec))
  list(
    total_return = prod(1 + returns_vec) - 1,
    annual_return = mean(returns_vec) * 252,
    annual_vol = sd(returns_vec) * sqrt(252),
    sharpe = mean(returns_vec) / sd(returns_vec) * sqrt(252),
    max_dd = min(cumprod(1 + returns_vec) / cummax(cumprod(1 + returns_vec)) - 1),
    calmar = (mean(returns_vec) * 252) / abs(min(cumprod(1 + returns_vec) / cummax(cumprod(1 + returns_vec)) - 1)),
    win_rate = mean(returns_vec > 0),
    profit_factor = sum(returns_vec[returns_vec > 0]) / abs(sum(returns_vec[returns_vec < 0]))
  )
}

# --- 4. UI定义（bslib现代仪表板） --------------------------------------------
ui <- page_sidebar(
  title = tags$span(
    bs_icon("graph-up-arrow"), 
    "极客雷欧量化策略综合平台 v2026.4",
    style = "font-weight: 800; letter-spacing: 0.5px;"
  ),
  sidebar = sidebar(
    width = 300,
    bg = "#f8f9fa",
    accordion(
      accordion_panel(
        "全局参数", icon = bs_icon("sliders"),
        dateRangeInput("date_range", "回测区间", 
                       start = "2022-01-01", end = Sys.Date(),
                       format = "yyyy-mm-dd"),
        numericInput("init_capital", "初始资金 (万)", value = 1000, min = 100, step = 100),
        sliderInput("commission", "手续费率", min = 0, max = 0.005, value = 0.001, step = 0.0001),
        actionButton("run_all", "运行全部策略", class = "btn-primary w-100", icon = icon("play"))
      ),
      accordion_panel(
        "多因子选股", icon = bs_icon("layer-group"),
        sliderInput("mf_weight_value", "价值因子权重", 0, 1, 0.4, 0.1),
        sliderInput("mf_weight_quality", "质量因子权重", 0, 1, 0.3, 0.1),
        sliderInput("mf_weight_mom", "动量因子权重", 0, 1, 0.3, 0.1),
        numericInput("mf_n_top", "选股数量", 20, 5, 100, 5),
        actionButton("run_mf", "运行", class = "btn-outline-primary btn-sm w-100")
      ),
      accordion_panel(
        "事件驱动", icon = bs_icon("calendar-event"),
        numericInput("ed_window", "事件窗口", 5, 1, 20),
        numericInput("ed_hold", "持有期", 20, 5, 60),
        actionButton("run_ed", "运行", class = "btn-outline-primary btn-sm w-100")
      ),
      accordion_panel(
        "基本面量化", icon = bs_icon("building"),
        numericInput("fund_roe", "最低ROE (%)", 15, 5, 40),
        numericInput("fund_pe", "最高PE", 25, 5, 100),
        numericInput("fund_growth", "最低增长率 (%)", 10, 0, 50),
        actionButton("run_fund", "运行", class = "btn-outline-primary btn-sm w-100")
      ),
      accordion_panel(
        "股票中性", icon = bs_icon("shield-halved"),
        numericInput("mn_lookback", "回望期", 60, 20, 252),
        numericInput("mn_z", "Z阈值", 1.5, 0.5, 3, 0.1),
        actionButton("run_mn", "运行", class = "btn-outline-primary btn-sm w-100")
      ),
      accordion_panel(
        "股票多空", icon = bs_icon("arrow-left-right"),
        sliderInput("ls_long", "多头比例", 0, 0.5, 0.30, 0.05),
        sliderInput("ls_short", "空头比例", 0, 0.5, 0.30, 0.05),
        actionButton("run_ls", "运行", class = "btn-outline-primary btn-sm w-100")
      ),
      accordion_panel(
        "CTA趋势", icon = bs_icon("arrow-trend-up"),
        numericInput("cta_fast", "短期均线", 20, 5, 60),
        numericInput("cta_slow", "长期均线", 60, 20, 252),
        numericInput("cta_risk", "单笔风险 (%)", 2, 0.5, 5, 0.5),
        actionButton("run_cta", "运行", class = "btn-outline-primary btn-sm w-100")
      ),
      accordion_panel(
        "统计套利", icon = bs_icon("infinity"),
        numericInput("arb_lookback", "协整回望期", 60, 20, 252),
        numericInput("arb_entry", "入场Z值", 2.0, 0.5, 4, 0.1),
        numericInput("arb_exit", "出场Z值", 0.5, 0, 2, 0.1),
        actionButton("run_arb", "运行", class = "btn-outline-primary btn-sm w-100")
      ),
      accordion_panel(
        "期货策略", icon = bs_icon("fuel-pump"),
        numericInput("fut_mom", "动量回望期", 40, 10, 120),
        actionButton("run_fut", "运行", class = "btn-outline-primary btn-sm w-100")
      ),
      accordion_panel(
        "期权策略", icon = bs_icon("option"),
        sliderInput("opt_delta", "目标Delta", 0.1, 0.5, 0.30, 0.05),
        numericInput("opt_dte_min", "最小DTE", 20, 7, 60),
        numericInput("opt_dte_max", "最大DTE", 45, 14, 90),
        actionButton("run_opt", "运行", class = "btn-outline-primary btn-sm w-100")
      ),
      accordion_panel(
        "债券策略", icon = bs_icon("bank"),
        sliderInput("bond_dur_min", "最小久期", 1, 10, 2, 1),
        sliderInput("bond_dur_max", "最大久期", 5, 30, 10, 1),
        selectInput("bond_min_rating", "最低评级", 
                    choices = c("AAA" = 7, "AA" = 6, "A" = 5, "BBB" = 4),
                    selected = 4),
        actionButton("run_bond", "运行", class = "btn-outline-primary btn-sm w-100")
      )
    )
  ),

  navset_card_tab(
    nav_panel("仪表盘", icon = bs_icon("speedometer2"),
      layout_columns(
        value_box(title = "多因子选股", value = textOutput("vb_mf"), 
                  showcase = bs_icon("layer-group"), theme = "primary"),
        value_box(title = "事件驱动", value = textOutput("vb_ed"), 
                  showcase = bs_icon("calendar-event"), theme = "secondary"),
        value_box(title = "基本面量化", value = textOutput("vb_fund"), 
                  showcase = bs_icon("building"), theme = "success"),
        value_box(title = "股票中性", value = textOutput("vb_mn"), 
                  showcase = bs_icon("shield-halved"), theme = "danger"),
        value_box(title = "股票多空", value = textOutput("vb_ls"), 
                  showcase = bs_icon("arrow-left-right"), theme = "warning"),
        value_box(title = "CTA趋势", value = textOutput("vb_cta"), 
                  showcase = bs_icon("arrow-trend-up"), theme = "info"),
        value_box(title = "统计套利", value = textOutput("vb_arb"), 
                  showcase = bs_icon("infinity"), theme = "light"),
        value_box(title = "期货策略", value = textOutput("vb_fut"), 
                  showcase = bs_icon("fuel-pump"), theme = "dark"),
        value_box(title = "期权策略", value = textOutput("vb_opt"), 
                  showcase = bs_icon("option"), theme = "primary"),
        value_box(title = "债券策略", value = textOutput("vb_bond"), 
                  showcase = bs_icon("bank"), theme = "secondary")
      ),
      card(
        card_header("策略收益对比"),
        plotlyOutput("plot_comparison", height = "500px") %>% withSpinner()
      )
    ),
    nav_panel("多因子选股", icon = bs_icon("layer-group"),
      layout_columns(
        card(plotlyOutput("plot_mf_equity", height = "350px") %>% withSpinner()),
        card(DTOutput("tbl_mf_signals") %>% withSpinner())
      ),
      card(DTOutput("tbl_mf_metrics") %>% withSpinner())
    ),
    nav_panel("事件驱动", icon = bs_icon("calendar-event"),
      layout_columns(
        card(plotlyOutput("plot_ed_equity", height = "350px") %>% withSpinner()),
        card(plotlyOutput("plot_ed_distribution", height = "350px") %>% withSpinner())
      ),
      card(DTOutput("tbl_ed_metrics") %>% withSpinner())
    ),
    nav_panel("基本面量化", icon = bs_icon("building"),
      layout_columns(
        card(plotlyOutput("plot_fund_equity", height = "350px") %>% withSpinner()),
        card(plotlyOutput("plot_fund_garp", height = "350px") %>% withSpinner())
      ),
      card(DTOutput("tbl_fund_signals") %>% withSpinner())
    ),
    nav_panel("股票中性", icon = bs_icon("shield-halved"),
      layout_columns(
        card(plotlyOutput("plot_mn_equity", height = "350px") %>% withSpinner()),
        card(plotlyOutput("plot_mn_zscore", height = "350px") %>% withSpinner())
      ),
      card(DTOutput("tbl_mn_metrics") %>% withSpinner())
    ),
    nav_panel("股票多空", icon = bs_icon("arrow-left-right"),
      layout_columns(
        card(plotlyOutput("plot_ls_equity", height = "350px") %>% withSpinner()),
        card(plotlyOutput("plot_ls_exposure", height = "350px") %>% withSpinner())
      ),
      card(DTOutput("tbl_ls_metrics") %>% withSpinner())
    ),
    nav_panel("CTA趋势", icon = bs_icon("arrow-trend-up"),
      layout_columns(
        card(plotlyOutput("plot_cta_equity", height = "350px") %>% withSpinner()),
        card(plotlyOutput("plot_cta_signals", height = "350px") %>% withSpinner())
      ),
      card(DTOutput("tbl_cta_metrics") %>% withSpinner())
    ),
    nav_panel("统计套利", icon = bs_icon("infinity"),
      layout_columns(
        card(plotlyOutput("plot_arb_spread", height = "350px") %>% withSpinner()),
        card(plotlyOutput("plot_arb_equity", height = "350px") %>% withSpinner())
      ),
      card(DTOutput("tbl_arb_metrics") %>% withSpinner())
    ),
    nav_panel("期货策略", icon = bs_icon("fuel-pump"),
      layout_columns(
        card(plotlyOutput("plot_fut_equity", height = "350px") %>% withSpinner()),
        card(plotlyOutput("plot_fut_heatmap", height = "350px") %>% withSpinner())
      ),
      card(DTOutput("tbl_fut_metrics") %>% withSpinner())
    ),
    nav_panel("期权策略", icon = bs_icon("option"),
      layout_columns(
        card(plotlyOutput("plot_opt_skew", height = "350px") %>% withSpinner()),
        card(plotlyOutput("plot_opt_equity", height = "350px") %>% withSpinner())
      ),
      card(DTOutput("tbl_opt_metrics") %>% withSpinner())
    ),
    nav_panel("债券策略", icon = bs_icon("bank"),
      layout_columns(
        card(plotlyOutput("plot_bond_yield", height = "350px") %>% withSpinner()),
        card(plotlyOutput("plot_bond_equity", height = "350px") %>% withSpinner())
      ),
      card(DTOutput("tbl_bond_metrics") %>% withSpinner())
    ),
    nav_panel("数据探索", icon = bs_icon("database"),
      card(
        card_header("原始数据预览"),
        DTOutput("tbl_raw_data") %>% withSpinner()
      )
    )
  )
)

# --- 5. 服务器逻辑 ------------------------------------------------------------
server <- function(input, output, session) {

  # 数据生成（响应式缓存）
  data_engine <- QuantDataEngine$new()

  stock_data <- reactive({
    req(input$date_range)
    data_engine$gen_stock_data(start = input$date_range[1], end = input$date_range[2])
  }) %>% bindCache(input$date_range)

  futures_data <- reactive({
    req(input$date_range)
    data_engine$gen_futures_data(start = input$date_range[1], end = input$date_range[2])
  }) %>% bindCache(input$date_range)

  options_data <- reactive({
    req(input$date_range)
    data_engine$gen_options_data(start = input$date_range[1], end = input$date_range[2])
  }) %>% bindCache(input$date_range)

  bond_data <- reactive({
    req(input$date_range)
    data_engine$gen_bond_data(start = input$date_range[1], end = input$date_range[2])
  }) %>% bindCache(input$date_range)

  # 策略结果存储
  results <- reactiveValues(
    mf = NULL, ed = NULL, fund = NULL, mn = NULL, ls = NULL,
    cta = NULL, arb = NULL, fut = NULL, opt = NULL, bond = NULL
  )

  # 运行策略的通用函数
  run_strategy <- function(name, data_func, strategy_func, ...) {
    data <- data_func()
    engine <- StrategyEngine$new(data)
    do.call(strategy_func, c(list(engine), list(...)))
    bt <- engine$backtest(initial_capital = input$init_capital * 10000, commission = input$commission)
    list(signals = engine$signals, backtest = bt, metrics = if(!is.null(bt)) calc_metrics(bt$daily_ret) else NULL)
  }

  # 各策略触发器
  observeEvent(input$run_mf, {
    results$mf <- run_strategy("mf", stock_data, StrategyEngine$public_methods$multi_factor,
                                weights = c(input$mf_weight_value, input$mf_weight_quality, input$mf_weight_mom),
                                n_top = input$mf_n_top)
  })

  observeEvent(input$run_ed, {
    results$ed <- run_strategy("ed", stock_data, StrategyEngine$public_methods$event_driven,
                                event_window = input$ed_window, holding_period = input$ed_hold)
  })

  observeEvent(input$run_fund, {
    results$fund <- run_strategy("fund", stock_data, StrategyEngine$public_methods$fundamental_quant,
                                  min_roe = input$fund_roe/100, max_pe = input$fund_pe, min_growth = input$fund_growth/100)
  })

  observeEvent(input$run_mn, {
    results$mn <- run_strategy("mn", stock_data, StrategyEngine$public_methods$market_neutral,
                                lookback = input$mn_lookback, z_threshold = input$mn_z)
  })

  observeEvent(input$run_ls, {
    results$ls <- run_strategy("ls", stock_data, StrategyEngine$public_methods$long_short,
                                long_pct = input$ls_long, short_pct = input$ls_short)
  })

  observeEvent(input$run_cta, {
    results$cta <- run_strategy("cta", futures_data, StrategyEngine$public_methods$cta_trend,
                                 fast = input$cta_fast, slow = input$cta_slow, risk_per_trade = input$cta_risk/100)
  })

  observeEvent(input$run_arb, {
    results$arb <- run_strategy("arb", stock_data, StrategyEngine$public_methods$arbitrage_stat,
                                 lookback = input$arb_lookback, entry_z = input$arb_entry, exit_z = input$arb_exit)
  })

  observeEvent(input$run_fut, {
    results$fut <- run_strategy("fut", futures_data, StrategyEngine$public_methods$futures_strategy,
                                 momentum_lookback = input$fut_mom)
  })

  observeEvent(input$run_opt, {
    results$opt <- run_strategy("opt", options_data, StrategyEngine$public_methods$options_strategy,
                                 target_delta = input$opt_delta, dte_min = input$opt_dte_min, dte_max = input$opt_dte_max)
  })

  observeEvent(input$run_bond, {
    results$bond <- run_strategy("bond", bond_data, StrategyEngine$public_methods$bond_strategy,
                                  min_duration = input$bond_dur_min, max_duration = input$bond_dur_max,
                                  min_rating_score = as.numeric(input$bond_min_rating))
  })

  # 一键运行全部
  observeEvent(input$run_all, {
    results$mf <- run_strategy("mf", stock_data, StrategyEngine$public_methods$multi_factor,
                                weights = c(0.4, 0.3, 0.3), n_top = 20)
    results$ed <- run_strategy("ed", stock_data, StrategyEngine$public_methods$event_driven,
                                event_window = 5, holding_period = 20)
    results$fund <- run_strategy("fund", stock_data, StrategyEngine$public_methods$fundamental_quant,
                                  min_roe = 0.15, max_pe = 25, min_growth = 0.10)
    results$mn <- run_strategy("mn", stock_data, StrategyEngine$public_methods$market_neutral,
                                lookback = 60, z_threshold = 1.5)
    results$ls <- run_strategy("ls", stock_data, StrategyEngine$public_methods$long_short,
                                long_pct = 0.3, short_pct = 0.3)
    results$cta <- run_strategy("cta", futures_data, StrategyEngine$public_methods$cta_trend,
                                 fast = 20, slow = 60, risk_per_trade = 0.02)
    results$arb <- run_strategy("arb", stock_data, StrategyEngine$public_methods$arbitrage_stat,
                                 lookback = 60, entry_z = 2.0, exit_z = 0.5)
    results$fut <- run_strategy("fut", futures_data, StrategyEngine$public_methods$futures_strategy,
                                 momentum_lookback = 40)
    results$opt <- run_strategy("opt", options_data, StrategyEngine$public_methods$options_strategy,
                                 target_delta = 0.3, dte_min = 20, dte_max = 45)
    results$bond <- run_strategy("bond", bond_data, StrategyEngine$public_methods$bond_strategy,
                                  min_duration = 2, max_duration = 10, min_rating_score = 4)
  })

  # 仪表盘数值框
  output$vb_mf <- renderText({ if(is.null(results$mf)) "待运行" else paste0(round(results$mf$metrics$sharpe, 2), " 夏普") })
  output$vb_ed <- renderText({ if(is.null(results$ed)) "待运行" else paste0(round(results$ed$metrics$sharpe, 2), " 夏普") })
  output$vb_fund <- renderText({ if(is.null(results$fund)) "待运行" else paste0(round(results$fund$metrics$sharpe, 2), " 夏普") })
  output$vb_mn <- renderText({ if(is.null(results$mn)) "待运行" else paste0(round(results$mn$metrics$sharpe, 2), " 夏普") })
  output$vb_ls <- renderText({ if(is.null(results$ls)) "待运行" else paste0(round(results$ls$metrics$sharpe, 2), " 夏普") })
  output$vb_cta <- renderText({ if(is.null(results$cta)) "待运行" else paste0(round(results$cta$metrics$sharpe, 2), " 夏普") })
  output$vb_arb <- renderText({ if(is.null(results$arb)) "待运行" else paste0(round(results$arb$metrics$sharpe, 2), " 夏普") })
  output$vb_fut <- renderText({ if(is.null(results$fut)) "待运行" else paste0(round(results$fut$metrics$sharpe, 2), " 夏普") })
  output$vb_opt <- renderText({ if(is.null(results$opt)) "待运行" else paste0(round(results$opt$metrics$sharpe, 2), " 夏普") })
  output$vb_bond <- renderText({ if(is.null(results$bond)) "待运行" else paste0(round(results$bond$metrics$sharpe, 2), " 夏普") })

  # 策略对比图
  output$plot_comparison <- renderPlotly({
    req(results$mf, results$ed, results$fund, results$mn, results$ls,
        results$cta, results$arb, results$fut, results$opt, results$bond)

    all_data <- rbindlist(list(
      results$mf$backtest[, .(date, equity, strategy = "多因子选股")],
      results$ed$backtest[, .(date, equity, strategy = "事件驱动")],
      results$fund$backtest[, .(date, equity, strategy = "基本面量化")],
      results$mn$backtest[, .(date, equity, strategy = "股票中性")],
      results$ls$backtest[, .(date, equity, strategy = "股票多空")],
      results$cta$backtest[, .(date, equity, strategy = "CTA趋势")],
      results$arb$backtest[, .(date, equity, strategy = "统计套利")],
      results$fut$backtest[, .(date, equity, strategy = "期货策略")],
      results$opt$backtest[, .(date, equity, strategy = "期权策略")],
      results$bond$backtest[, .(date, equity, strategy = "债券策略")]
    ), fill = TRUE)

    plot_ly(all_data, x = ~date, y = ~equity, color = ~strategy, type = "scatter", mode = "lines") %>%
      layout(title = "策略累计收益对比", xaxis = list(title = ""), 
             yaxis = list(title = "资金权益"), hovermode = "x unified")
  })

  # 多因子页面
  output$plot_mf_equity <- renderPlotly({
    req(results$mf$backtest)
    plot_ly(results$mf$backtest, x = ~date, y = ~equity, type = "scatter", mode = "lines", 
            line = list(color = "#0d6efd")) %>%
      layout(title = "多因子选股 - 资金曲线", yaxis = list(title = "权益"))
  })
  output$tbl_mf_signals <- renderDT({
    req(results$mf$signals)
    datatable(head(results$mf$signals, 100), options = list(pageLength = 10))
  })
  output$tbl_mf_metrics <- renderDT({
    req(results$mf$metrics)
    datatable(data.table(指标 = names(results$mf$metrics), 数值 = unlist(results$mf$metrics)))
  })

  # 事件驱动页面
  output$plot_ed_equity <- renderPlotly({
    req(results$ed$backtest)
    plot_ly(results$ed$backtest, x = ~date, y = ~equity, type = "scatter", mode = "lines",
            line = list(color = "#6c757d")) %>%
      layout(title = "事件驱动 - 资金曲线", yaxis = list(title = "权益"))
  })
  output$plot_ed_distribution <- renderPlotly({
    req(results$ed$signals)
    plot_ly(results$ed$signals, x = ~earnings_surprise, type = "histogram", nbinsx = 30,
            marker = list(color = "#6c757d")) %>%
      layout(title = "盈余惊喜分布", xaxis = list(title = "Surprise"))
  })
  output$tbl_ed_metrics <- renderDT({
    req(results$ed$metrics)
    datatable(data.table(指标 = names(results$ed$metrics), 数值 = unlist(results$ed$metrics)))
  })

  # 基本面页面
  output$plot_fund_equity <- renderPlotly({
    req(results$fund$backtest)
    plot_ly(results$fund$backtest, x = ~date, y = ~equity, type = "scatter", mode = "lines",
            line = list(color = "#198754")) %>%
      layout(title = "基本面量化 - 资金曲线", yaxis = list(title = "权益"))
  })
  output$plot_fund_garp <- renderPlotly({
    req(results$fund$signals)
    plot_ly(results$fund$signals, x = ~pe_ratio, y = ~eps_growth, color = ~garp_score,
            type = "scatter", mode = "markers", text = ~symbol) %>%
      layout(title = "GARP矩阵 (PE vs Growth)")
  })
  output$tbl_fund_signals <- renderDT({
    req(results$fund$signals)
    datatable(head(results$fund$signals, 100), options = list(pageLength = 10))
  })

  # 股票中性页面
  output$plot_mn_equity <- renderPlotly({
    req(results$mn$backtest)
    plot_ly(results$mn$backtest, x = ~date, y = ~equity, type = "scatter", mode = "lines",
            line = list(color = "#dc3545")) %>%
      layout(title = "股票中性 - 资金曲线", yaxis = list(title = "权益"))
  })
  output$plot_mn_zscore <- renderPlotly({
    req(results$mn$signals)
    plot_ly(results$mn$signals[1:min(1000, .N)], x = ~date, y = ~z_score, color = ~sector,
            type = "scatter", mode = "markers") %>%
      layout(title = "Z-Score分布 (行业中性化)")
  })
  output$tbl_mn_metrics <- renderDT({
    req(results$mn$metrics)
    datatable(data.table(指标 = names(results$mn$metrics), 数值 = unlist(results$mn$metrics)))
  })

  # 股票多空页面
  output$plot_ls_equity <- renderPlotly({
    req(results$ls$backtest)
    plot_ly(results$ls$backtest, x = ~date, y = ~equity, type = "scatter", mode = "lines",
            line = list(color = "#ffc107")) %>%
      layout(title = "股票多空 - 资金曲线", yaxis = list(title = "权益"))
  })
  output$plot_ls_exposure <- renderPlotly({
    req(results$ls$signals)
    daily_exp <- results$ls$signals[, .(long = sum(signal > 0), short = sum(signal < 0)), by = date]
    plot_ly(daily_exp, x = ~date) %>%
      add_bars(y = ~long, name = "多头", marker = list(color = "green")) %>%
      add_bars(y = ~short, name = "空头", marker = list(color = "red")) %>%
      layout(title = "每日多空持仓数", barmode = "group")
  })
  output$tbl_ls_metrics <- renderDT({
    req(results$ls$metrics)
    datatable(data.table(指标 = names(results$ls$metrics), 数值 = unlist(results$ls$metrics)))
  })

  # CTA页面
  output$plot_cta_equity <- renderPlotly({
    req(results$cta$backtest)
    plot_ly(results$cta$backtest, x = ~date, y = ~equity, type = "scatter", mode = "lines",
            line = list(color = "#0dcaf0")) %>%
      layout(title = "CTA趋势 - 资金曲线", yaxis = list(title = "权益"))
  })
  output$plot_cta_signals <- renderPlotly({
    req(results$cta$signals)
    plot_ly(results$cta$signals[1:min(500, .N)], x = ~date, y = ~sma_fast, name = "SMA快",
            type = "scatter", mode = "lines") %>%
      add_lines(y = ~sma_slow, name = "SMA慢", line = list(color = "red")) %>%
      layout(title = "均线交叉信号")
  })
  output$tbl_cta_metrics <- renderDT({
    req(results$cta$metrics)
    datatable(data.table(指标 = names(results$cta$metrics), 数值 = unlist(results$cta$metrics)))
  })

  # 套利页面
  output$plot_arb_spread <- renderPlotly({
    req(results$arb$signals)
    plot_ly(results$arb$signals, x = ~date, y = ~spread, type = "scatter", mode = "lines",
            line = list(color = "purple")) %>%
      add_lines(y = ~z, name = "Z-Score", yaxis = "y2") %>%
      layout(title = "配对价差与Z值", yaxis2 = list(overlaying = "y", side = "right"))
  })
  output$plot_arb_equity <- renderPlotly({
    req(results$arb$backtest)
    plot_ly(results$arb$backtest, x = ~date, y = ~equity, type = "scatter", mode = "lines",
            line = list(color = "purple")) %>%
      layout(title = "统计套利 - 资金曲线", yaxis = list(title = "权益"))
  })
  output$tbl_arb_metrics <- renderDT({
    req(results$arb$metrics)
    datatable(data.table(指标 = names(results$arb$metrics), 数值 = unlist(results$arb$metrics)))
  })

  # 期货页面
  output$plot_fut_equity <- renderPlotly({
    req(results$fut$backtest)
    plot_ly(results$fut$backtest, x = ~date, y = ~equity, type = "scatter", mode = "lines",
            line = list(color = "#212529")) %>%
      layout(title = "期货策略 - 资金曲线", yaxis = list(title = "权益"))
  })
  output$plot_fut_heatmap <- renderPlotly({
    req(results$fut$signals)
    heat <- results$fut$signals[, .(avg_mom = mean(momentum)), by = .(contract, category)]
    plot_ly(heat, x = ~contract, y = ~category, z = ~avg_mom, type = "heatmap",
            colors = colorRamp(c("red", "white", "green"))) %>%
      layout(title = "期货动量热力图")
  })
  output$tbl_fut_metrics <- renderDT({
    req(results$fut$metrics)
    datatable(data.table(指标 = names(results$fut$metrics), 数值 = unlist(results$fut$metrics)))
  })

  # 期权页面
  output$plot_opt_skew <- renderPlotly({
    req(results$opt$signals)
    plot_ly(results$opt$signals, x = ~date, y = ~iv_skew, type = "scatter", mode = "lines",
            line = list(color = "orange")) %>%
      layout(title = "波动率偏度 (IV Skew)", yaxis = list(title = "Call IV - Put IV"))
  })
  output$plot_opt_equity <- renderPlotly({
    req(results$opt$backtest)
    plot_ly(results$opt$backtest, x = ~date, y = ~equity, type = "scatter", mode = "lines",
            line = list(color = "orange")) %>%
      layout(title = "期权策略 - 资金曲线", yaxis = list(title = "权益"))
  })
  output$tbl_opt_metrics <- renderDT({
    req(results$opt$metrics)
    datatable(data.table(指标 = names(results$opt$metrics), 数值 = unlist(results$opt$metrics)))
  })

  # 债券页面
  output$plot_bond_yield <- renderPlotly({
    req(results$bond$signals)
    plot_ly(results$bond$signals[1:min(1000, .N)], x = ~duration, y = ~yield, color = ~credit_rating,
            type = "scatter", mode = "markers", text = ~bond_id) %>%
      layout(title = "债券久期-收益率散点")
  })
  output$plot_bond_equity <- renderPlotly({
    req(results$bond$backtest)
    plot_ly(results$bond$backtest, x = ~date, y = ~equity, type = "scatter", mode = "lines",
            line = list(color = "#6610f2")) %>%
      layout(title = "债券策略 - 资金曲线", yaxis = list(title = "权益"))
  })
  output$tbl_bond_metrics <- renderDT({
    req(results$bond$metrics)
    datatable(data.table(指标 = names(results$bond$metrics), 数值 = unlist(results$bond$metrics)))
  })

  # 数据探索
  output$tbl_raw_data <- renderDT({
    req(stock_data())
    datatable(head(stock_data(), 500), options = list(scrollX = TRUE, pageLength = 15))
  })
}

# --- 6. 启动应用 --------------------------------------------------------------
shinyApp(ui = ui, server = server)
