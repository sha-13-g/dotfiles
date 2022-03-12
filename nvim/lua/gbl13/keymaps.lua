local opts = { noremap = true, silent = true }

--local term_opts = { silent = true }

-- Shorten function name
local keymap = vim.api.nvim_set_keymap

--Remap space as leader key
keymap("", "<Space>", "<Nop>", opts)
vim.g.mapleader = ";"
vim.g.maplocalleader = ";"

-- Modes
--   normal_mode = "n",
--   insert_mode = "i",
--   visual_mode = "v",
--   visual_block_mode = "x",
--   term_mode = "t",
--   command_mode = "c",

-- Normal --
-- Better window navigation

keymap("n", "dw", "vb\"_d", opts)
keymap("n", "<leader>w", ":w!<CR>", opts)
keymap("n", "<leader>ww", ":wq!<CR>", opts)
keymap("n", "<leader>q", ":q!<CR>", opts)
keymap("n", "<leader>s", ":%s/<C-r><C-w>/<C-r><C-w>/gI<LEFT><LEFT><LEFT>", opts)
keymap("n", "<leader>G", ":Goyo<CR>", opts)

keymap("n", "<leader>y", "\"*y", opts)
keymap("n", "<leader>p", "\"*p", opts)

-- Move between panes
keymap("n", "<space>", "<C-w>w", opts)
keymap("n", "<space>k", "5k", opts)
keymap("n", "<space>j", "5j", opts)

-- Resize with arrows
keymap("n", "<up>", ":resize +2<CR>", opts)
keymap("n", "<down>", ":resize -2<CR>", opts)
keymap("n", "<left>", ":vertical resize +2<CR>", opts)
keymap("n", "<right>", ":vertical resize -2<CR>", opts)

-- Navigate buffers
keymap("n", "<S-l>", ":bnext<CR>", opts)
keymap("n", "<S-h>", ":bprevious<CR>", opts)

-- Move text up and down
keymap("n", "J", "<Esc>:m .+1<CR>==", opts)
keymap("n", "K", "<Esc>:m .-2<CR>==", opts)

-- Split window
keymap("n", "ss", ":split<CR><c-w>w", opts)
keymap("n", "sv", ":vsplit<CR><c-w>w", opts)
--Load config
keymap("n", "<leader>r", ":so ~/.config/nvim/init.lua<CR>", opts)

-- Insert --
-- Press space>k fast to enter
keymap("i", "jj", "<ESC>", opts)

-- Visual --
-- Stay in indent mode
keymap("v", "<", "<gv", opts)
keymap("v", ">", ">gv", opts)

-- Move text up and down
keymap("v", "J", ":m .+1<CR>==", opts)
keymap("v", "K", ":m .-2<CR>==", opts)
keymap("v", "p", '"_dP', opts)

-- Enter to normal mode fast
-- keymap("v", "jj", "<ESC>", opts)

-- Visual Block --
-- Move text up and down
keymap("x", "J", ":move '>+1<CR>gv-gv", opts)
keymap("x", "K", ":move '<-2<CR>gv-gv", opts)
-- -- Better terminal navigation
-- keymap("t", "<C-h>", "<C-\\><C-N><C-w>h", term_opts)
-- keymap("t", "<C-j>", "<C-\\><C-N><C-w>j", term_opts)
-- keymap("t", "<C-k>", "<C-\\><C-N><C-w>k", term_opts)
-- keymap("t", "<C-l>", "<C-\\><C-N><C-w>l", term_opts)
