{ pkgs, lib, ... }:
let
  dimensions = "20,47";
  service =
    name:
    {
      extra,
      text,
      wait,
    }:
    let
      config-file = builtins.toFile "conky-${name}.conf" ''
        conky.config = {
          background = false,
          border_width = 0,
          cpu_avg_samples = 2,
          double_buffer = true,
          draw_borders = false,
          draw_graph_borders = false,
          draw_outline = false,
          draw_shades = false,
          extra_newline = false,
          font = 'CozetteVector:pixelsize=12',
          gap_y = 1,
          minimum_height = 5,
          minimum_width = 2,
          net_avg_samples = 2,
          no_buffers = true,
          out_to_console = false,
          out_to_ncurses = false,
          out_to_stderr = false,
          out_to_x = true,
          own_window = true,
          own_window_class = 'Conky',
          own_window_type = 'dock',
          own_window_argb_visual = true,
          own_window_argb_value = 0,
          show_graph_range = false,
          show_graph_scale = false,
          stippled_borders = 0,
          color0 = 'd9e0ee',
          color1 = '9999ff',
          uppercase = false,
          use_spacer = 'none',
          use_xft = true,
          ${extra}
        }
        conky.text = [[
        ${text}
        ]]
      '';
    in
    {
      name = "conky-${name}";
      value = {
        Unit = {
          Description = "Run conky ${name}";
        };
        Service = {
          ExecStart =
            (pkgs.writeShellScript "conky-${name}" ''
              ${lib.getExe pkgs.conky} -i ${toString wait} -c ${config-file}
              ${lib.getExe pkgs.conky} -c ${config-file}
            '').outPath;
          Restart = "always";
          RestartSec = "10s";
        };
        Install.WantedBy = [ "default.target" ];
      };
    };
in
{
  systemd.user.services = lib.mapAttrs' service {
    status = {
      extra = ''
        alignment = 'top_right',
        text_buffer_size = 2047,
        update_interval = 0.25,
        gap_y = 1,
        gap_x = 150,
      '';
      text = "$alignr\${catp /run/user/1000/status-bar}";
      wait = 100;
    };
    monitor = {
      extra = ''
        alignment = 'bottom_left',
        gap_x = 1,
        gap_y = 300,
        update_interval = 5.0,
      '';
      text = ''
        ''${if_match $cpu > 20}
        $color1
        CPU:
        $color0$cpu%
        $color1''${cpugraph cpu0 ${dimensions}}
        $endif
        ''${if_match $memperc > 50}
        RAM:
        $color0$mem
        $memperc%
        $color1''${memgraph ${dimensions}}
        $endif
        ''${if_match ''${upspeedf ''${gw_iface}} > 1024}
        Up:
        $color0''${upspeed ''${gw_iface}}
        $color1''${upspeedgraph ''${gw_iface} ${dimensions} $color1 $color1 5242880}
        $endif
        ''${if_match ''${downspeedf ''${gw_iface}} > 1024}
        Down:
        $color0''${downspeed ''${gw_iface}}
        $color1''${downspeedgraph ''${gw_iface} ${dimensions} $color1 $color1 12107200}
        $endif
        $color0''${time %Y}
        ''${time %d. %b}
        ''${time KW%V}
        ''${time %a}
      '';
      wait = 5;
    };
  };
}
