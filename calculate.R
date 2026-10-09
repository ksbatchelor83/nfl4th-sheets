library(dplyr)
library(tibble)
library(nfl4th)
options(timeout = 180)
internal <- function(name) getFromNamespace(name, "nfl4th")
scalar <- function(x, name) {
  if (length(x) != 1 || is.null(x) || is.na(x)) stop(paste("Missing or invalid", name))
  x
}
number <- function(x, name, low, high, whole = FALSE) {
  scalar(x, name)
  if (!is.numeric(x) || !is.finite(x) || x < low || x > high || (whole && x != floor(x)))
    stop(paste("Invalid", name))
  x
}
calculate <- function(x) {
  teams <- c("ARI","ATL","BAL","BUF","CAR","CHI","CIN","CLE","DAL","DEN","DET","GB","HOU","IND","JAX","KC","LA","LAC","LV","MIA","MIN","NE","NO","NYG","NYJ","PHI","PIT","SEA","SF","TB","TEN","WAS")
  if (!scalar(x$posteam, "team") %in% teams || !scalar(x$opponent, "opponent") %in% teams || x$posteam == x$opponent) stop("Choose two different NFL teams.")
  if (!scalar(x$mode,"mode") %in% c("Real game","Hypothetical")) stop("Invalid mode.")
  if (!scalar(x$type,"game type") %in% c("reg","post")) stop("Invalid game type.")
  if (!is.logical(scalar(x$is_home,"home/away")) || !is.logical(scalar(x$possession_received_kickoff,"kickoff"))) stop("Invalid home/away or kickoff value.")
  year <- number(x$season,"season",2014,2100,TRUE)
  q <- number(x$qtr,"quarter",1,4,TRUE)
  seconds <- number(x$quarter_seconds_remaining,"clock",1,900,TRUE)
  yard <- number(x$yardline_100,"distance to goal",1,99,TRUE)
  need <- number(x$ydstogo,"yards to go",1,yard,TRUE)
  margin <- number(x$score_differential,"score differential",-150,150,TRUE)
  pt <- number(x$posteam_timeouts_remaining,"team timeouts",0,3,TRUE)
  dt <- number(x$defteam_timeouts_remaining,"opponent timeouts",0,3,TRUE)
  home <- if (x$is_home) x$posteam else x$opponent
  away <- if (x$is_home) x$opponent else x$posteam
  home_kick <- as.integer(x$is_home == x$possession_received_kickoff)
  play <- tibble(home_team=home, away_team=away, posteam=x$posteam, type=x$type,
    season=year, qtr=q, quarter_seconds_remaining=seconds, ydstogo=need,
    yardline_100=yard, score_differential=margin, home_opening_kickoff=home_kick,
    posteam_timeouts_remaining=pt, defteam_timeouts_remaining=dt)
  context <- NULL
  if (x$mode == "Real game") {
    games <- internal(".games_nfl4th")()
    matches <- games[games$home_team == home & games$away_team == away & games$type == x$type & games$season == year,]
    if (nrow(matches) != 1) stop("No unique scheduled game matches these inputs. Check season, home/away and game type, or use Hypothetical.")
    if (anyNA(matches[,c("spread_line","total_line")])) stop("The scheduled game has no spread or total yet. Use Hypothetical.")
    context <- list(expected_margin=if(x$is_home) matches$spread_line[1] else -matches$spread_line[1],total_line=matches$total_line[1],roof=matches$roof[1],game_id=matches$game_id[1])
    result <- nfl4th::add_4th_probs(play)
  } else {
    expected <- number(x$expected_margin,"expected margin",-100,100)
    total <- number(x$total_line,"expected points",1,200)
    if (total <= abs(expected)) stop("Expected points must exceed absolute expected margin.")
    roof <- scalar(x$roof,"stadium")
    if (!roof %in% c("Outdoors","Dome","Retractable")) stop("Invalid stadium setting.")
    # Build model features directly: standard preparation overwrites manual context.
    half <- seconds + if (q %% 2 == 1) 900 else 0
    remaining <- half + if (q <= 2) 1800 else 0
    home_margin <- if (x$is_home) expected else -expected
    roof_name <- tolower(roof)
    prepared <- play |> mutate(
      down=4, runoff=0L, index=1L,
      half_seconds_remaining=half, game_seconds_remaining=remaining,
      elapsed_share=(3600-remaining)/3600,
      spread_line=home_margin, total_line=total,
      spread_time=home_margin*exp(-4*elapsed_share),
      home_total=(total+home_margin)/2, away_total=(total-home_margin)/2,
      posteam_spread=expected, posteam_total=(total+expected)/2,
      home_timeouts_remaining=if (x$is_home) pt else dt,
      away_timeouts_remaining=if (x$is_home) dt else pt,
      home_receive_2h_ko=if (q>2) 0 else if (home_kick==1) -1 else 1,
      original_posteam=x$posteam,
      era0=0, era1=0, era2=0, era3=as.integer(year<=2017), era4=as.integer(year>2017),
      roof=roof_name, model_roof=roof_name,
      outdoors=as.integer(roof=="Outdoors"), dome=as.integer(roof=="Dome"),
      retractable=as.integer(roof=="Retractable"),
      fg_model_roof=factor(paste0(as.integer(roof=="Outdoors"),as.integer(year>=2020))))
    result <- internal("add_probs")(prepared)
  }
  w <- as.numeric(result[1,c("go_wp","fg_wp","punt_wp")])
  success <- c(result$first_down_prob[1],result$fg_make_prob[1],NA_real_)
  if (any(!is.na(w) & (!is.finite(w) | w<0 | w>1)) || sum(!is.na(w))<2) stop("Model returned invalid probabilities.")
  labels <- c("Go for it","Field goal","Punt")
  ranking <- order(w, decreasing=TRUE, na.last=NA)
  choices <- lapply(1:3,function(i) list(choice=labels[i],win_probability=w[i],success_probability=success[i]))
  list(recommendation=labels[ranking[1]],advantage_pp=100*(w[ranking[1]]-w[ranking[2]]),
    context=context,choices=choices,model_version=as.character(packageVersion("nfl4th")),mode=x$mode)
}

input <- Sys.getenv("SITUATION")
result <- tryCatch(calculate(jsonlite::fromJSON(input)), error=function(e) list(error=conditionMessage(e)))
jsonlite::write_json(result,"result.json",auto_unbox=TRUE,na="null",null="null",pretty=TRUE)
