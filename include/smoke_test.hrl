-ifndef(smoke_test).
-define(smoke_test, true).

-include("child.hrl").
-include("stat.hrl").

-record(child, {
          id              :: reference(),
          serv_tag        :: string(),          % tag of service. E.g. "echo"
          ses_sn          :: non_neg_integer(), % session seq number
          ses_base        :: string(),          % session base part
          jobs = []       :: [#chi{}],
          debug = []      :: [atom()],
          timer           :: reference(),
          stat  = #stat{} :: #stat{},
          timeout         :: non_neg_integer(), % timeout for one job
          url             :: string(),
          host            :: string(),
          method          :: atom(),
          hz              :: non_neg_integer(),
          seconds         :: non_neg_integer(),
          cnt = 0         :: non_neg_integer()
}).

% handler's state
-record(sth, {
          stat = #stat{} :: #stat{},
          debug    = []  :: [atom()],
          children = []  :: [pid()],
          count          :: non_neg_integer(),
          timeout        :: non_neg_integer(), % timeout for one job
          host           :: string(),
          url            :: string(),
          serv_tag       :: string(),
          hz             :: non_neg_integer(),
          seconds        :: non_neg_integer()
                            }).

-record(st, {
          url     :: string(),
          hz      :: non_neg_integer(),
          seconds :: non_neg_integer()
                     }).

-endif.
