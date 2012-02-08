-ifndef(smoke_req).
-define(smoke_req, true).

-record(req, {
          id              :: reference(),
          debug = []      :: [atom()],
          url             :: string(),
          method          :: string(),
          params = []     :: list(),
          time            :: non_neg_integer()
                             }).
        
-endif.
