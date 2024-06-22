function UseTheme(theme)
    theme = theme or "catppuccin"
    vim.cmd.colorscheme(color)
end

return {
    { 
        "catppuccin/nvim", 
        name = "catppuccin", 
        config = function()
            vim.cmd.colorscheme("catppuccin")
        end,
        priority = 1000 
    },
}
