-- You can add your own plugins here or in other files in this directory!
--  I promise not to create any merge conflicts in this directory :)
--
-- See the kickstart.nvim README for more information
return {
    {
        'kdheepak/lazygit.nvim',
        -- optional for floating window border decoration
        dependencies = {
            'nvim-lua/plenary.nvim',
        },
    },
    {
        'ggandor/leap.nvim',
        config = function()
            require('leap').add_default_mappings()
            require('leap').init_highlight(true)
        end,
    },
    {
        'ThePrimeagen/harpoon',
        branch = 'harpoon2',
        dependencies = { 'nvim-lua/plenary.nvim' },
    },
    {
        "kylechui/nvim-surround",
        version = "*", -- Use for stability; omit to use `main` branch for the latest features
        event = "VeryLazy",
        config = function()
            require("nvim-surround").setup()
        end
    },
    {
        "rose-pine/neovim",
        name = "rose-pine"
    },
    {
        "catppuccin/nvim",
        name = "catppuccin",
        priority = 1000,
        config = function()
            require("catppuccin").setup({
                flavour = "macchiato"
            })
        end
    },
}
