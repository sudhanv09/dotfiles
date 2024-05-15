return {
    'SuperBo/fugit2.nvim',
    opts = {},
    lazy = true,
    dependencies = {
        'MunifTanjim/nui.nvim',
        'nvim-tree/nvim-web-devicons',
        'nvim-lua/plenary.nvim',
        {
            'chrisgrieser/nvim-tinygit',
            dependencies = { 'stevearc/dressing.nvim' }
        }
    },
    cmd = { 'Fugit2', 'Fugit2Graph' },
    keys = {
        { '<leader>gg', mode = 'n', '<cmd>Fugit2<cr>' }
    }
}
