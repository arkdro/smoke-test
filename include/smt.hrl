-ifndef(smoke_test).
-define(smoke_test, true).

-record(stat, {sum = 0.0, sum_sq = 0.0, count = 0}).
-record(st, {
          url     :: string(),
          hz      :: non_neg_integer(),
          seconds :: non_neg_integer()
                     }).

-endif.
