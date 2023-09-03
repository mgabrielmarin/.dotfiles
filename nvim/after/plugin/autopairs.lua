local autopairs = require("nvim-autopairs")


autopairs.get_rule("'")[1].not_filetypes = { "scheme", "lisp" }
autopairs.get_rule("`").not_filetypes = { "scheme", "lisp" }
