{lib, pkgs, ...}: {

  programs.helix = {
    enable = true;

    settings = {
      theme = "gruvbox_dark_hard";

      keys.normal = {
        "{" = "goto_prev_paragraph";
        "}" = "goto_next_paragraph";
        "X" = "extend_line_above";
        "esc" = ["collapse_selection" "keep_primary_selection"];
        space.space = "file_picker";
        space.w = ":w";
        space.q = ":bc";
        "C-q" = ":xa";
        space.u = {
          f = ":format"; # format using LSP formatter
          w = ":set whitespace.render all";
          W = ":set whitespace.render none";
        };
      };

      keys.insert = {
        A-x = "normal_mode";
        j = { k = "normal_mode"; };
      };

      keys.select = {
        "%" = "match_brackets";
      };

      editor = {
        line-number = "relative";
        cursorline = true;
        auto-completion = true;
        auto-format = true;
        mouse = true;
        color-modes = true;
        idle-timeout = 1;
        scrolloff = 5;
        bufferline = "always";
        true-color = true;
        rulers = [80];
        indent-guides = {
          render = true;
        };
        gutters = ["diagnostics" "line-numbers" "spacer" "diff"];

        statusline = {
          separator = "";
          left = ["mode" "selections" "spinner" "file-name" "total-line-numbers"];
          center = [];
          right = ["diagnostics" "file-encoding" "file-line-ending" "file-type" "position-percentage" "position"];
          mode = {
            normal = "NORMAL";
            insert = "INSERT";
            select = "SELECT";
          };
        };

        whitespace.characters = {
          space = "·";
          nbsp = "⍽";
          tab = "→";
          newline = "⤶";
        };

        cursor-shape = {
          insert = "bar";
          normal = "block";
          select = "block";
        };

        auto-pairs = {
          "(" = ")";
          "{" = "}";
          "[" = "]";
          "\"" = "\"";
          "`" = "`";
          "<" = ">";
        };

        lsp = {
          enable = true;
          display-messages = true;
          display-inlay-hints = true;
          snippets = true;
        };

      };
    };

  };
}
