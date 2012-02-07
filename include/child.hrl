-ifndef(smoke_child).
-define(smoke_child, true).

-record(chi, {
          pid   :: pid(),
          id    :: reference(),
          start :: tuple(),
          mon   :: reference()
                   }).
        
-endif.
