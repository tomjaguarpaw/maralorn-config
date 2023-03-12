{
  pkgs,
  lib,
  ...
}: let
  config = extra: ''
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
    	max_text_width = 26,
    	net_avg_samples = 2,
    	no_buffers = true,
    	out_to_console = false,
    	out_to_ncurses = false,
    	out_to_stderr = false,
    	out_to_x = true,
    	own_window = true,
    	own_window_class = 'Conky',
    	own_window_type = 'panel',
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
  '';
  status = builtins.toFile "conky-status.conf" ''
    ${config ''
       alignment = 'top_left',
      text_buffer_size = 2047,
      update_interval = 0.25,
      gap_x = 1,
    ''}
    conky.text = [[
    $color0''${time %Y-%m-%d KW%V %a %H:%M}
    $color1$hr
    ''${catp /run/user/1000/status-bar}
    ]]
  '';
  monitor = let
    dimensions = "20,130";
  in
    builtins.toFile "conky-monitor.conf" ''
      ${config ''
         alignment = 'bottom_left',
        gap_x = 23,
        update_interval = 5.0,
      ''}
      conky.text = [[
      ''${execi 60 cal}
      $color1
      CPU: $color0$cpu%
      $color1''${cpugraph cpu0 ${dimensions}}
      RAM: $color0$mem $memperc%
      $color1''${memgraph ${dimensions}}
      Up: $color0''${upspeed ''${gw_iface}}
      $color1''${upspeedgraph ''${gw_iface} ${dimensions} $color1 $color1 5242880}
      Down: $color0''${downspeed ''${gw_iface}}
      $color1''${downspeedgraph ''${gw_iface} ${dimensions} $color1 $color1 12107200}
      ]]
    '';
in {
  systemd.user.services = {
    conky-status = {
      Unit.Description = "Run conky";
      Service = {
        ExecStart = "${lib.getExe pkgs.conky} -c ${status}";
        Restart = "always";
        RestartSec = "10s";
      };
      Install.WantedBy = ["default.target"];
    };
    conky-monitor = {
      Unit.Description = "Run conky monitor";
      Service = {
        ExecStart = "${lib.getExe pkgs.conky} -c ${monitor}";
        Restart = "always";
        RestartSec = "10s";
      };
      Install.WantedBy = ["default.target"];
    };
  };
}
