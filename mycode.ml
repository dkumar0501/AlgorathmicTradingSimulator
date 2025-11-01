(* Algorithmic Trading Simulator machine  *)

open Printf
 
(* Define a type for stock market data  *)
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

(* Define a type for portfolio state *)
type portfolio = {
  mutable balance: float;
  mutable holdings: (string, int) Hashtbl.t;  (* Holdings per stock *)
}

(* Define a type for trade history *)
type transaction = {
  stock: string;
  price: float;
  action: string;
}

(* Trade history storage *)
let trade_history = ref[]  

(* Function to record a trade *)
let record_trade stock action price =
  trade_history := { stock = stock; price = price; action = action } :: !trade_history

(* Simulate buying and selling based on a strategy *)
let simulate_trading stocks strategy initial_balance =
  let portfolio = { balance = initial_balance; holdings = Hashtbl.create 10 } in
  List.iter (fun s ->
    let current_holding = Hashtbl.find_opt portfolio.holdings s.symbol |> Option.value ~default:0 in
    
    (* Buy Condition type *)
    if strategy.buy_condition s && portfolio.balance >= s.price then (
      Hashtbl.replace
