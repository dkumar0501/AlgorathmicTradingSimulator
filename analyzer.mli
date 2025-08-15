type position = [ `Long | `Stay | `Short ]
type action = [ `Buy | `None | `Sell ]
type portfolio = (string * float) list 
    
(** Compare two values to determine long or short signal *)     
val mov_avg_comp : float -> float -> [> `Long | `Short ]     
    
(** Convert a position to a corresponding action *) 
val pos_to_act : position -> action   
 
(** Determine trading action based on position change *)
val act : position -> position -> action  
 
(** Get the last non-None action from an action list *)
val get_last_action : [< `Buy | `None | `Sell ] list -> [> `Buy | `None | `Sell ]
 
(** Convert a portfolio to a printable string *)
val portfolio_to_string : portfolio -> string

(** An empty portfolio *)
val empty_portfolio : portfolio

(** Add a stock and its proportion to the portfolio *)
val add_to_portfolio : portfolio -> string -> float -> portfolio 

(** Compute a simple moving average over a float list with window size *)
val moving_average : float list -> int -> float list

(** Evaluate a trading strategy and compute net profit.
    @param conservative use single share buy/sell (default false) *)
val eval_strategy : ?conservative:bool -> float list -> action list -> float

(** The Analyzer module type allows injecting different scrapers *)
module type ANALYZER =
  functor (Scr : Scraper.SCRAPER) ->
    sig
      type data = Scr.data
      type hist_data = Scr.hist_data

      (** Create a portfolio recommendation from live data *)
      val analyze : data -> portfolio

      (** Analyze historical data and return a portfolio *)
      val analyze_hist : hist_data -> portfolio

      (** Generate trading stream from historical data *)
      val create_buy_sell_stream : hist_data ->
        float list * float list * float list * action list
    end

(** Dummy analyzer that performs no operations *)
module EmptyAnalyzer : ANALYZER

(** Analyzer that allocates equal weights to all tickers *)
module EqualAnalyzer : ANALYZER

(** Analyzer using moving average strategy *)
module MovingAverageAnalyzer : ANALYZER
