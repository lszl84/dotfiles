-- Set leader key to space
vim.g.mapleader = " "

-- Key mappings
vim.keymap.set("n", "<leader>y", '"+y', { desc = "Copy to system clipboard" })
vim.keymap.set("n", "<leader>p", '"+p', { desc = "Paste from system clipboard" })
vim.keymap.set("n", "<F5>", ":!bb<CR>", { desc = "Run bb command" })
vim.keymap.set("n", "<leader>f", ":!cfi %<CR>", { desc = "Reformat current file" })

vim.keymap.set('n', '<C-n>', ':bnext<CR>', { desc = 'Next buffer' })
vim.keymap.set('n', '<C-p>', ':bprev<CR>', { desc = 'Previous buffer' })

-- Map Leader+Enter to open footclient in current directory
vim.keymap.set("n", "<Leader><Enter>", function()
    local current_dir = vim.fn.expand("%:p:h")
    vim.fn.system("footclient --working-directory " .. vim.fn.shellescape(current_dir) .. " &")
end, { noremap = true, silent = true, desc = "Open foot terminal in current directory" })

-- Readline-style navigation in command mode
vim.keymap.set("c", "<M-f>", "<S-Right>", { desc = "Move forward one word" })
vim.keymap.set("c", "<M-b>", "<S-Left>", { desc = "Move backward one word" })
vim.keymap.set("c", "<M-d>", "<S-Right><C-w>", { desc = "Delete one word form the start" })
vim.keymap.set("c", "<C-b>", "<Left>", { desc = "Move backward one character" })
vim.keymap.set("c", "<C-f>", "<Right>", { desc = "Move forward one character" })
vim.keymap.set("c", "<C-a>", "<Home>", { desc = "Move to beginning of line" })
vim.keymap.set("c", "<C-e>", "<End>", { desc = "Move to end of line" })
vim.keymap.set("c", "<C-d>", "<Del>", { desc = "Delete character under cursor" })

-- Basic settings
vim.opt.number = true
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
