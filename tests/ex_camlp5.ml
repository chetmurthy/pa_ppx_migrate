
module Ploc= struct
include Ploc

let pp ppf x = Fmt.(const string "<loc>" ppf ())
end

[%%import: MLast.expr
    [@add [%%import: MLast.loc]]
    [@add [%%import: MLast.type_var]]
    [@add [%%import: 'a Ploc.vala]]
    [@with Ploc.vala := vala]
]
