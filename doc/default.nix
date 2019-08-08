{ root ? ./. 
, name ? "doc"
, rib ? ../.
, ...
}:

import rib { inherit root name; }
