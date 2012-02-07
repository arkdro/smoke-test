-ifndef(smoke_test).
-define(smoke_test, true).

-record(stat, {sum = 0.0, sum_sq = 0.0, count = 0}).

-record(child, {
          id              :: reference(),
          debug = []      :: [atom()],
          timer           :: reference(),
          stat  = #stat{} :: #stat{},
          url             :: string(),
          hz              :: non_neg_integer(),
          seconds         :: non_neg_integer(),
          cnt             :: non_neg_integer()
}).

% handler's state
-record(sth, {
          debug    = [] :: [atom()],
          children = [] :: [pid()],
          count         :: non_neg_integer(),
          url           :: string(),
          hz            :: non_neg_integer(),
          seconds       :: non_neg_integer()
                           }).

-record(st, {
          url     :: string(),
          hz      :: non_neg_integer(),
          seconds :: non_neg_integer()
                     }).

-endif.
