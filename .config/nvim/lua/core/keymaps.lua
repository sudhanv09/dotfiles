-- Oil.nvim
vim.keymap.set('n', '<leader>e', '<cmd>Oil<CR>', { desc = 'Open Explorer' })

-- Lazygit
vim.keymap.set('n', '<leader>gg', '<cmd>LazyGit<CR>', { desc = 'Open Lazygit' })

-- Harpoon
--------------------------------------------------------------------------
local harpoon = require 'harpoon'
vim.keymap.set('n', '<leader>a', function()
    harpoon:list():append()
end, { desc = 'Add to harpoon' })

vim.keymap.set('n', '<C-e>', function()
    harpoon.ui:toggle_quick_menu(harpoon:list())
end, { desc = 'Harpoon menu' })

vim.keymap.set('n', '<C-h>', function()
    harpoon:list():select(1)
end, { desc = 'Harpoon jump to 1' })
vim.keymap.set('n', '<C-t>', function()
    harpoon:list():select(2)
end, { desc = 'Harpoon jump to 2' })
vim.keymap.set('n', '<C-n>', function()
    harpoon:list():select(3)
end, { desc = 'Harpoon jump to 3' })
vim.keymap.set('n', '<C-s>', function()
    harpoon:list():select(4)
end, { desc = 'Harpoon jump to 4' })

vim.keymap.set('n', '<C-S-P>', function()
    harpoon:list():prev()
end, { desc = 'Harpoon previous file' })
vim.keymap.set('n', '<C-S-N>', function()
    harpoon:list():next()
end, { desc = 'Harpoon next file' })
--------------------------------------------------------------------------

-- Motions
--------------------------------------------------------------------------
vim.keymap.set('v', 'J', ":m '>+1<CR>gv=gv")
vim.keymap.set('v', 'K', ":m '>-2<CR>gv=gv")

-- Append next line to current line
vim.keymap.set('n', 'J', 'mzJ`z')

vim.keymap.set('n', '<C-d>', '<C-d>zz')
vim.keymap.set('n', '<C-u>', '<C-u>zz')

vim.keymap.set('x', '<leader>p', '"_dP')
--------------------------------------------------------------------------
-- Default
vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, { desc = 'Go to previous diagnostic message' })
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, { desc = 'Go to next diagnostic message' })
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, { desc = 'Open diagnostics list' })
