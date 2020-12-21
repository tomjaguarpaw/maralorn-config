{ pkgs, ... }: {
  home = {
    packages = [ pkgs.khal ];
      #[view]
      # removed the description from the agend overview. I get a lot of stupid
      # Google ICSs files hat contain massive overhead.
      #agenda_event_format = "{calendar-color}{cancelled}{start-end-time-style} {title}{repeat-symbol}"
      #event_format = "{calendar-color}{cancelled}{start-end-time-style} {title}{repeat-symbol}"
    file.".config/khal/config".text = ''
      [calendars]
      [[all]]
      type = discover
      path = ~/.calendars/*/*
    '';
  };
}
