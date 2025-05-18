open Core 
open Async

type position = [`Long | `Stay | `Short]
type action = [`Buy | `None | `Sell]

type portfolio = (string * float) list

let period = 200
let debug = false

let portfolio_to_string p =
  List.fold p ~init:"" ~f:(fun acc (s, f) -> s ^ ": " ^ Float.to_string f ^ ", " ^ acc)

let mov_avg_comp a b = if a < b then `Short else `Long

let pos_to_act = function
  | `Long -> `Buy
  | `Stay -> `None
  | `Short -> `Sell

let act a b = match (a, b) with
  | (`Long, `Short) -> `Sell
  | (`Short, `Long) -> `Buy
  | _ -> `None

let get_last_action l =
  let rec aux last = function
    | [] -> last
    | `None :: t -> aux last t
    | (`Buy | `Sell) as a :: t -> aux a t
  in
  aux `None l

let eval_strategy ?(conservative=false) closes strategy =
  let stocks_owned = ref 0 in
  let rec aux cs strat acc =
    match cs, strat with
    | c :: c', p :: s ->
        let new_acc =
          match p with
          | `Buy ->
              if debug then printf "Bought @ $%.2f\n" c;
              if conservative then (incr stocks_owned; acc -. c)
              else (stocks_owned := !stocks_owned + 2; acc -. (2. *. c))
          | `Sell ->
              if debug then printf "Sold @ $%.2f\n" c;
              if conservative then (decr stocks_owned; acc +. c)
              else (stocks_owned := !stocks_owned - 2; acc +. (2. *. c))
          | `None -> acc
        in
        aux c' s new_acc
    | _, _ -> acc
  in
  match closes, strategy with
  | c :: c', `Buy :: s ->
      incr stocks_owned;
      aux c' s (-.c)
  | c :: c', `Sell :: s ->
      decr stocks_owned;
      aux c' s c
  | c :: c', `None :: s -> aux c' s 0.
  | _, _ -> 0.
  |> fun res -> res +. (Float.of_int !stocks_owned *. Option.value ~default:0. (List.last closes))

let empty_portfolio = []
let add_to_portfolio p s f = (s, f) :: p

let moving_average l n =
  let arr = Array.of_list l in
  let len = Array.length arr in
  let avgs = ref [] in
  for i = 0 to len - n do
    let sum = ref 0. in
    for j = i to i + n - 1 do
      sum := !sum +. arr.(j)
    done;
    avgs := (!sum /. Float.of_int n) :: !avgs
  done;
  List.rev !avgs

let derivative _ = []
let second_derivative _ = []

module type ANALYZER =
  functor (Scr : Scraper.SCRAPER) ->
    sig
      type data = Scr.data
      type hist_data = Scr.hist_data

      val analyze : data -> portfolio
      val analyze_hist : hist_data -> portfolio
      val create_buy_sell_stream : hist_data -> (float list * float list * float list * action list)
    end

module EmptyAnalyzer : ANALYZER =
  functor (Scr : Scraper.SCRAPER) ->
    struct
      type data = Scr.data
      type hist_data = Scr.hist_data

      let analyze _ = []
      let analyze_hist _ = failwith "Not Implemented"
      let create_buy_sell_stream _ = failwith "Not Implemented"
    end

module EqualAnalyzer : ANALYZER =
  functor (Scr : Scraper.SCRAPER) ->
    struct
      type data = Scr.data
      type hist_data = Scr.hist_data

      let analyze d =
        let tickers = Scr.extract_tickers d in
        let prop = 1. /. Float.of_int (List.length tickers) in
        List.map tickers ~f:(fun t -> (t, prop))

      let analyze_hist _ = failwith "Not Implemented"
      let create_buy_sell_stream _ = failwith "Not Implemented"
    end

module MovingAverageAnalyzer : ANALYZER =
  functor (Scr : Scraper.SCRAPER) ->
    struct
      type data = Scr.data
      type hist_data = Scr.hist_data

      let analyze d =
        let tickers = Scr.extract_tickers d in
        let prop = 1. /. Float.of_int (List.length tickers) in
        List.map tickers ~f:(fun t -> (t, prop))

      let rec compress = function
        | a :: b :: xs -> act a b :: compress (b :: xs)
        | _ -> []

      let get_points compressed prices offset =
        let ind = ref offset in
        List.fold2_exn compressed prices ~init:[] ~f:(fun acc c p ->
          incr ind;
          match c with
          | `Buy -> (!ind, p, 5, Graphics.green) :: acc
          | `Sell -> (!ind, p, 5, Graphics.red) :: acc
          | `None -> acc)

      let extract_field_from_data d field =
        let get_field_val l =
          List.fold l ~init:"" ~f:(fun acc (f, v) -> if f = field then v else acc)
        in
        List.map d ~f:get_field_val

      let create_buy_sell_stream d =
        let l = Scr.hist_data_to_list d in
        let closes = List.map (extract_field_from_data l "Adj_Close") ~f:Float.of_string in
        let mov_avg = moving_average closes period in
        let shifted_closes = List.drop closes (period - 1) in
        let long_short = List.map2_exn shifted_closes mov_avg ~f:mov_avg_comp in
        let buy_sell =
          match long_short with
          | [] -> []
          | hd :: tl -> pos_to_act hd :: compress long_short
        in
        (closes, shifted_closes, mov_avg, buy_sell)

      let analyze_hist d =
        let closes, shifted_closes, mov_avg, buy_sell = create_buy_sell_stream d in
        let%bind () =
          Monitor.try_with (fun () ->
            let g = new Plotter.plotter in
            Graphics.open_graph "";
            Graphics.resize_window 800 500;
            g#set_scale closes;
            g#graph_stock closes;
            g#graph_stock ~offset:(period - 1) ~color:Graphics.red mov_avg;
            g#draw_circles ~text:true (get_points buy_sell shifted_closes (period - 2));
            let%map _ = Clock.after (Time.Span.of_sec 1.5) in
            Graphics.close_graph ()
          ) >>| function
          | Ok () -> ()
          | Error _ -> printf "Graph closed.\n"
        in
        let profit = eval_strategy shifted_closes buy_sell in
        printf "RESULT: $%.2f\n" profit;
        return (List.map closes ~f:(fun t -> (Float.to_string t, 1.)))
    end
