open Core
open Async
open Analyzer
 
module Mov = MovingAverageAnalyzer(Scraper.BasicScraper)
   
let print_result ~year ~ticker ~profit ~investment = 
  printf "%d [%s]: $%.2f on $%.2f @ %.2f%% return\n"
    year ticker profit investment ((profit /. investment) *. 100.)

let analyze_company ~ticker ~start_date ~end_date =
  let open Deferred.Let_syntax in
  match%bind Scraper.BasicScraper.get_hist_data ["Adj_Close"] ticker start_date end_date with
  | Error e ->
      eprintf "Error retrieving data for %s: %s\n" ticker (Error.to_string_hum e);
      return None
  | Ok data ->
      let%map strategy_result =
        Mov.create_buy_sell_stream data
        |> fun (dates, closes, _, actions) -> return (Analyzer.eval_strategy closes actions, List.hd_exn closes)
      in
      let profit, initial_investment = strategy_result in
      let year = match List.hd dates with
        | Some date -> Date.year date
        | None -> 0
      in
      print_result ~year ~ticker ~profit ~investment:initial_investment;
      Some (ticker, profit, initial_investment)

let analyze_all tickers ~start_date ~end_date =
  let%map results =
    Deferred.List.filter_map tickers ~f:(fun ticker ->
      analyze_company ~ticker ~start_date ~end_date)
  in
  let total_profit =
    List.fold results ~init:0.0 ~f:(fun acc (_, p, _) -> acc +. p)
  in
  printf "\nSUMMARY:\nAnalyzed %d companies\nTotal Profit: $%.2f\n"
    (List.length results) total_profit

let tickers = ["X"; "FDX"; "VZ"; "SPLS"; "T"; "PEP"; "KO"; "AAPL"; "MSFT"; "C"; "YHOO"; "DJIA"]

let () =
  don't_wait_for (
    analyze_all tickers ~start_date:"2013-01-01" ~end_date:"2014-07-01"
  );
  never_returns (Scheduler.go ())
