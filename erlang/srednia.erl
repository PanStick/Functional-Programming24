
-module (srednia).
-export ([srednia/1]).

srednia(X) -> lists:sum(X) / length(X).