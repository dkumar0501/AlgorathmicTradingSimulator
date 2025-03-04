(* Algorithmic Trading Simulator in OCaml *)

open Printf

(* Define a type for stock market data *)
type stock = {
  symbol: string;
  price: float;
  volume: int;
}

(* Define a type for trading strategy *)
type strategy = {
  name: string;
  buy_condition: stock -> bool;
  sell_condition: stock -> bool;
}

(* Simulate buying and selling based on a strategy *)
let simulate_trading stocks strategy initial_balance =
  let balance = ref initial_balance in
  let holdings = ref 0 in
  List.iter (fun s ->
    if strategy.buy_condition s && !balance >= s.price then (
      holdings := !holdings + 1;
      balance := !balance -. s.price;
      printf "Bought 1 share of %s at $%.2f\n" s.symbol s.price
    );
    if strategy.sell_condition s && !holdings > 0 then (
      holdings := !holdings - 1;
      balance := !balance +. s.price;
      printf "Sold 1 share of %s at $%.2f\n" s.symbol s.price
    )
  ) stocks;
  printf "Final balance: $%.2f, Holdings: %d\n" !balance !holdings

(* Example stock market data *)
let stocks = [
  {symbol = "AAPL"; price = 150.0; volume = 1000};
  {symbol = "AAPL"; price = 155.0; volume = 1200};
  {symbol = "AAPL"; price = 145.0; volume = 1100};
  {symbol = "AAPL"; price = 160.0; volume = 1300};
]

(* Example strategy: Buy if price < 150, sell if price > 155 *)
let simple_strategy = {
  name = "Basic Buy Low Sell High";
  buy_condition = (fun s -> s.price < 150.0);
  sell_condition = (fun s -> s.price > 155.0);
}

(* Run simulation *)
let () = simulate_trading stocks simple_strategy 1000.0
