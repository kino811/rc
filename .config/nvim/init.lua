-- system clipboard
vim.opt.clipboard = "unnamedplus"
vim.keymap.set('t', '<Esc>', [[<C-\><C-n>]], {desc = 'Exit terminal mode'})

-- line number
vim.opt.number = true
vim.opt.relativenumber = true

-- indent
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true

-- search
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.hlsearch = true

-- cursor line highlight
vim.opt.cursorline = true

-- maintain up/down space when scroll
vim.opt.scrolloff = 8

-- leader key
vim.g.mapleader = " "

-- move key convenience
vim.keymap.set("n", "<C-h>", "<C-w>h")
vim.keymap.set("n", "<C-j>", "<C-w>j")
vim.keymap.set("n", "<C-k>", "<C-w>k")
vim.keymap.set("n", "<C-l>", "<C-w>l")

-- turn off highlight after search
vim.keymap.set("n", "<leader>h", ":nohlsearch<CR>") 


-- lazy.nvim bootstrap
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git", "clone", "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- set plugin
require("lazy").setup({
  -- 컬러 스킴
  {
    "catppuccin/nvim",
    name = "catppuccin",
    priority = 1000,
    config = function()
      vim.cmd.colorscheme("catppuccin")
    end,
  },
  -- 파일 탐색기
  {
    "nvim-tree/nvim-tree.lua",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      require("nvim-tree").setup()
      vim.keymap.set("n", "<leader>e", ":NvimTreeToggle<CR>")
    end,
  },
  -- 퍼지 파인더
  {
    "nvim-telescope/telescope.nvim",
    dependencies = { 
      "nvim-lua/plenary.nvim",
      {"nvim-telescope/telescope-fzf-native.nvim", build = "make"}
    },
    config = function()
      local telescope = require("telescope")
      telescope.setup({
        defaults = {
          file_ignore_patterns = {
            "^Library/",
            "^.cache/",
            "^.npm/",
            "^.git/",
            "^.Trash/",
            "^Applications/",
            "^Movies/",
            "^Music/",
            "^Pictures/",
            "^Downloads/",
            "node_modules/",
            "^reveal.js/",
            "%.DS_Store",
          },
          -- fzf를 기본 sorter로 사용하도록 설정
          extensions = {
            fzf = {
              fuzzy = true,                   -- 퍼지 검색 활성화
              override_generic_sorter = true, -- 일반 sorter 덮어쓰기
              override_file_sorter = true,    -- 파일 sorter 덮어쓰기
              case_mode = "smart_case",       -- 대소문자 구분 설정
            }
          },
        },
        pickers = {
          find_files = {
            -- hidden = true,
            -- .gitignore에 등록된 파일도 무시하지 않고 찾고 싶다면 아래 주석 해제
            -- no_ignore = true,

            -- find_command를 명시적으로 지정하여 속도 최적화
            -- find_command = {"fd", "--type", "f", "--strip-cwd-prefix", "--hidden", "--exclude", ".git"},
          },
        },
      })

      -- fzf 확장을 로드하여 검색 속도 대폭 개선
      telescope.load_extension("fzf")

      local builtin = require("telescope.builtin")
      vim.keymap.set("n", "<leader>ff", builtin.find_files)
      vim.keymap.set("n", "<leader>fg", builtin.live_grep)
    end,
  },

  -- lsp
  {
    "williamboman/mason.nvim",
    config = function()
      require("mason").setup()
    end,
  },
  {
    "williamboman/mason-lspconfig.nvim",
    config = function()
      require("mason-lspconfig").setup({
        ensure_installed = {
          "lua_ls",       -- Lua
          "ts_ls",        -- TypeScript
          "pyright",      -- Python
          "rust_analyzer", -- Rust
        },
      })
    end,
  },
  {
    "neovim/nvim-lspconfig",
    config = function()
      local servers = { "lua_ls", "ts_ls", "pyright", "rust_analyzer" }
      for _, server in ipairs(servers) do
        vim.lsp.enable(server)
      end

      -- LSP 키 매핑
      vim.keymap.set("n", "gd", vim.lsp.buf.definition)
      vim.keymap.set("n", "K", vim.lsp.buf.hover)
      vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename)
      vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action)
    end,
  },
})

-- cpp compile
vim.api.nvim_create_autocmd("FileType", {
  pattern = "cpp",
  callback = function()
    vim.keymap.set("n", "<leader>bb", ":w | !clang++ -std=c++20 -Wall -Wextra -g % -o %:r && ./%:r<CR>", {
      buffer = true,
      desc = "Compile and run C++",
    })
  end,
})
